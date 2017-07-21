;;; osx-pseudo-daemon.el --- Daemon mode that plays nice with OSX.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/osx-pseudo-daemon
;; Version: 1.0
;; Created: 2013-09-20
;; Keywords: convenience osx

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; On OSX, if you use Cocoa Emacs' daemon mode and then close all GUI
;; frames, the Emacs app on your dock becomes nonfunctional until you
;; open a new GUI frame using emacsclient on the command line. This is
;; obviously suboptimal. This package makes it so that whenever you
;; close the last GUI frame, a new frame is created and the Emacs app
;; is hidden, thus approximating the behvaior of daemon mode while
;; keeping the Emacs dock icon functional. To actually quit instead of
;; hiding Emacs, use CMD+Q (or Alt+Q if you swapped Alt & Command
;; keys).

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
(defvar osxpd-mac-frame-types '(ns mac)
  "Set of frame types considered to be Mac GUI frames.")

;;;###autoload
(defvar osxpd-mac-gui-features '(ns mac)
  "Set of features indicating Emacs is running a Mac GUI.")

;; Try to require mac gui features to ensure that `featurep' can find
;; them.
;;;###autoload
(cl-loop
 for feature in osxpd-mac-gui-features
 do (require feature nil 'noerror))

;;;###autoload
(defgroup osx-pseudo-daemon nil
  "Emulate daemon mode in OSX by hiding Emacs when you kill the last GUI frame.")

(defsubst osxpd-mac-gui-feature-provided ()
  (cl-some #'featurep osxpd-mac-gui-features))

(defsubst osxpd-frame-is-mac-frame (frame)
  (memq (framep frame) osxpd-mac-frame-types))

(defun osxpd-hide-emacs ()
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
(define-minor-mode osx-pseudo-daemon-mode
  "Emulate daemon mode in OSX by hiding Emacs when you kill the last GUI frame.

On OSX, if you use Cocoa Emacs' daemon mode and then close all
GUI frames, the Emacs app on your dock becomes nonfunctional
until you open a new GUI frame using emacsclient on the command
line. This is obviously suboptimal. This package makes it so that
whenever you close the last GUI frame, a new frame is created and
the Emacs app is hidden, thus approximating the behvaior of
daemon mode while keeping the Emacs dock icon functional. To
actually quit instead of hiding Emacs, use CMD+Q (or Alt+Q if you
swapped Alt & Command keys).

This mode has no effect unless Emacs is running on OSX with the
Cocoa GUI, so it is safe to enable it unconditionally on all
platforms."
  :group 'osx-pseudo-daemon
  :global t
  ;; Enable by default on OSX
  :init-value (osxpd-mac-gui-feature-provided))

(defun osxpd-frame-is-last-mac-frame (frame)
  "Returns t if FRAME is the only NS frame."
  (and
   ;; Mac frames supported
   (osxpd-mac-gui-feature-provided)
   ;; Frame is mac frame
   (osxpd-frame-is-mac-frame frame)
   ;; No other frames on same terminal
   (<= (length (filtered-frame-list
               (lambda (frm) (eq (frame-terminal frm)
                            (frame-terminal frame)))))
      1)))

(defun osxpd-keep-at-least-one-mac-frame (frame)
  "If FRAME is the last GUI frame on Mac, open a new hidden frame.

This is called immediately prior to FRAME being closed."
  (let ((frame (or frame (selected-frame))))
    (when (osxpd-frame-is-last-mac-frame frame)
      ;; If FRAME is fullscreen, un-fullscreen it.
      (when (eq (frame-parameter frame 'fullscreen)
                'fullboth)
        (set-frame-parameter frame 'fullscreen nil)
        ;; Wait for fullscreen animation to complete
        (sit-for 1.5))
      ;; Create a new frame on same terminal as FRAME
      (make-frame `(terminal ,(frame-terminal frame)))
      ;; Making a frame might unhide emacs, so hide again
      (sit-for 0.1)
      (osxpd-hide-emacs))))

;; TODO: Is `delete-frame-hook' an appropriate place for this?
(defadvice delete-frame (before osxpd-keep-at-least-one-mac-frame activate)
  "When the last NS frame is deleted, create a new hidden one first."
  (when osx-pseudo-daemon-mode
    (osxpd-keep-at-least-one-mac-frame frame)))

;; This is the function that gets called when you click the X button
;; on the window's title bar.
(defadvice handle-delete-frame (around osxpd-never-quit-mac-emacs activate)
  "Never invoke `save-buffers-kill-emacs' when deleting a Mac frame.

Instead, just invoke `delete-frame' as normal. (Has no effect
unless `osx-pseudo-daemon-mode' is active.)"
  (let ((frame (posn-window (event-start event))))
    (if (and osx-pseudo-daemon-mode
             (osxpd-frame-is-mac-frame frame))
        (delete-frame frame t)
      ad-do-it)))

(defadvice save-buffers-kill-terminal (around osx-pseudo-daemon activate)
  "When killing an NS terminal, instead just delete all NS frames."
  (let ((frame (selected-frame)))
    (if (and osx-pseudo-daemon-mode
             (osxpd-frame-is-mac-frame frame))
        ;; For NS GUI, just call `delete-frame' on each frame that's
        ;; on the same terminal as the current frame. A new hidden one
        ;; will automatically be spawned by the advice to
        ;; `delete-frame' when the last existing frame is deleted.
        (let ((term (frame-terminal (selected-frame))))
          (mapc 'delete-frame
                (filtered-frame-list
                 (lambda (frm) (eq (frame-terminal frm) term)))))
      ad-do-it)))

(provide 'osx-pseudo-daemon)

;;; osx-pseudo-daemon.el ends here
