;;; mac-pseudo-daemon.el --- Daemon mode that plays nice with Mac OS.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/osx-pseudo-daemon
;; Version: 2.0
;; Package-Requires: ((cl-lib "2.00"))
;; Created: 2013-09-20
;; Keywords: convenience osx mac

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; On Mac OS, if you use Cocoa Emacs' daemon mode and then close all
;; GUI frames, the Emacs app on your dock becomes nonfunctional until
;; you open a new GUI frame using emacsclient on the command line.
;; This is obviously suboptimal. This package makes it so that
;; whenever you close the last GUI frame, a new frame is created and
;; the Emacs app is hidden, thus approximating the behvaior of daemon
;; mode while keeping the Emacs dock icon functional. To actually quit
;; instead of hiding Emacs, use CMD+Q (or Alt+Q if you swapped Alt &
;; Command keys).

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defvar macpd-mac-frame-types '(ns mac)
  "Set of frame types considered to be Mac GUI frames.")

;;;###autoload
(defvar macpd-mac-gui-features '(ns mac)
  "Set of features indicating Emacs is running a Mac GUI.")

;; Try to require mac gui features to ensure that `featurep' can find
;; them.
;;;###autoload
(cl-loop
 for feature in macpd-mac-gui-features
 do (require feature nil 'noerror))

;;;###autoload
(defgroup mac-pseudo-daemon nil
  "Emulate daemon mode in Mac OS by hiding Emacs when you kill the last GUI frame."
  :group 'convenience)

(defsubst macpd-mac-gui-feature-is-provided ()
  "Return non-nil if any Mac GUI feature was `provide'-ed."
  (cl-some #'featurep macpd-mac-gui-features))

(defsubst macpd-frame-is-mac-frame (frame)
  "Return non-=nil if FRAME is a Mac GUI frame."
  (memq (framep frame) macpd-mac-frame-types))

(defun macpd-hide-emacs ()
  "Hide all Emacs windows if running.

This works for both `ns' and `mac' frame types."
  (cl-case (framep (selected-frame))
    (ns
     (ns-hide-emacs t))
    (mac
     (call-process
      "osascript" nil nil nil
      "-e" "tell application \"Finder\""
      "-e" "set visible of process \"Emacs\" to false"
      "-e" "end tell"))))

;;;###autoload
(define-minor-mode mac-pseudo-daemon-mode
  "Hide Emacs instead of quitting when you kill the last frame on Mac OS.

On Mac OS, if you use Cocoa Emacs' real daemon mode and then
close all GUI frames, the Emacs app on your dock becomes
nonfunctional until you open a new GUI frame using emacsclient on
the command line. This is obviously suboptimal. This package
implements a fake daemon mode by making it so that whenever you
close the last GUI frame, a new frame is created and the Emacs
app is hidden (like pressing Cmd+H), thus approximating the
behvaior of daemon mode while keeping the Emacs dock icon
functional. This also approximates the behavior of document-based
Mac applications, which can stay open even though there is no
window.

To actually quit Emacs instead of hiding it, use Cmd+Q (or
Option+Q if you swapped the Option & Command keys).

This mode has no effect unless Emacs is running on Mac OS, so if
you have an Emacs config that is shared among multiple operating
systems, it is safe to enable this mode unconditionally."
  :group 'mac-pseudo-daemon
  :global t
  ;; Enable by default on Mac OS
  :init-value nil)

(defun macpd-frame-is-last-mac-frame (frame)
  "Return t if FRAME is the only NS frame."
  (and
   ;; Mac frames supported
   (macpd-mac-gui-feature-is-provided)
   ;; Frame is mac frame
   (macpd-frame-is-mac-frame frame)
   ;; No other frames on same terminal
   (<= (length (filtered-frame-list
               (lambda (frm) (eq (frame-terminal frm)
                            (frame-terminal frame)))))
      1)))

(defun macpd-make-new-default-frame (&optional parameters)
  "Like `make-frame', but select the `*scratch*` buffer in that frame.

Also does not change the currently selected frame.

Arguments PARAMETERS are the same as in `make-frame'."
  (with-selected-frame (make-frame)
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    ;; Return the new frame
    (selected-frame)))

(defun macpd-keep-at-least-one-mac-frame (frame)
  "If FRAME is the last GUI frame on Mac, open a new hidden frame.

This is called immediately prior to FRAME being closed."
  (let ((frame (or frame (selected-frame))))
    (when (macpd-frame-is-last-mac-frame frame)
      ;; If FRAME is fullscreen, un-fullscreen it.
      (when (eq (frame-parameter frame 'fullscreen)
                'fullboth)
        (set-frame-parameter frame 'fullscreen nil)
        ;; Wait for fullscreen animation to complete
        (sit-for 1.5))
      ;; Create a new frame on same terminal as FRAME
      (macpd-make-new-default-frame `((terminal . `(frame-terminal frame))))
      ;; Making a frame might unhide emacs, so hide again
      (sit-for 0.1)
      (macpd-hide-emacs))))

;; TODO: Is `delete-frame-hook' an appropriate place for this?
(defadvice delete-frame (before macpd-keep-at-least-one-mac-frame activate)
  "When the last NS frame is deleted, create a new hidden one first."
  (when mac-pseudo-daemon-mode
    (macpd-keep-at-least-one-mac-frame frame)))

;; This is the function that gets called when you click the X button
;; on the window's title bar.
(defadvice handle-delete-frame (around macpd-never-quit-mac-emacs activate)
  "Never invoke `save-buffers-kill-emacs' when deleting a Mac frame.

Instead, just invoke `delete-frame' as normal. (Has no effect
unless `mac-pseudo-daemon-mode' is active.)"
  (let ((frame (posn-window (event-start event))))
    (if (and mac-pseudo-daemon-mode
             (macpd-frame-is-mac-frame frame))
        (delete-frame frame t)
      ad-do-it)))

(defadvice save-buffers-kill-terminal (around mac-pseudo-daemon activate)
  "When killing an NS terminal, instead just delete all NS frames."
  (let ((frame (selected-frame)))
    (if (and mac-pseudo-daemon-mode
             (macpd-frame-is-mac-frame frame))
        ;; For Mac GUI, just call `delete-frame' on each frame that's
        ;; on the same terminal as the current frame. A new hidden one
        ;; will automatically be spawned by the advice to
        ;; `delete-frame' when the last existing frame is deleted.
        (let ((term (frame-terminal (selected-frame))))
          (mapc 'delete-frame
                (filtered-frame-list
                 (lambda (frm) (eq (frame-terminal frm) term)))))
      ad-do-it)))

(provide 'mac-pseudo-daemon)

;;; mac-pseudo-daemon.el ends here
