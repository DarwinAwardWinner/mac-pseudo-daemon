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

;; Try to require ns to ensure that (featurep 'ns) is t if ns is
;; available.
(require 'ns nil 'noerror)

;;;###autoload
(defgroup osx-pseudo-daemon nil
  "Emulate daemon mode in OSX by hiding Emacs when you kill the last GUI frame.")

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
platforms.
"
  :group 'osx-pseudo-daemon
  :global t
  ;; Enable by default on OSX
  :init-value (featurep 'ns))

(defun osxpd-frame-is-last-ns-frame (frame)
  "Returns t if FRAME is the only NS frame."
  (and
   (featurep 'ns)
   ;; Frame is ns frame
   (eq (framep frame) 'ns)
   ;; No other frames on same terminal
   (>= 1 (length (filtered-frame-list 
                 (lambda (frm) (eq (frame-terminal frm)
                              (frame-terminal frame))))))))

(defun osxpd-keep-at-least-one-ns-frame (frame)
  "If FRAME is the last NS frame, open a new hidden NS frame.

This is called immediately prior to FRAME being closed."
  (when (featurep 'ns)
    (let ((frame (or frame (selected-frame))))
      (when (osxpd-frame-is-last-ns-frame frame)
        (progn
          ;; If FRAME is fullscreen, un-fullscreen it.
          (when (eq (frame-parameter frame 'fullscreen)
                    'fullboth)
            (set-frame-parameter frame 'fullscreen nil)
            ;; Wait for fullscreen animation
            (sit-for 1.5))
          ;; Create a new frame on same terminal as FRAME, then restore
          ;; selected frame.
          (let ((sf (selected-frame)))
            (select-frame frame)
            (make-frame)
            (switch-to-buffer "*scratch*")
            (select-frame sf))
          ;; Making a frame might unhide emacs, so hide again
          (sit-for 0.1)
          (ns-hide-emacs t)
          )))))

;; TODO: Is `delete-frame-hook' an appropriate place for this?
(defadvice delete-frame (before osxpd-keep-at-least-one-ns-frame activate)
  "When the last NS frame is deleted, create a new hidden one first."
  (when osx-pseudo-daemon-mode
    (osxpd-keep-at-least-one-ns-frame frame)))

;; This is the function that gets called when you click the X button
;; on the window's title bar.
(defadvice handle-delete-frame (around osxpd-never-quit-ns-emacs activate)
  "Never invoke `save-buffers-kill-emacs' when deleting NS frame.

Instead, just delete the frame as normal."
  (let ((frame (posn-window (event-start event))))
    (if (and osx-pseudo-daemon-mode
             (eq 'ns (framep frame)))
        (delete-frame frame t)
      ad-do-it)))

(defadvice save-buffers-kill-terminal (around osx-pseudo-daemon activate)
  "When killing an NS terminal, instead just delete all NS frames."
  (let ((frame (selected-frame)))
    (if (and osx-pseudo-daemon-mode
             (eq 'ns (framep frame)))
        ;; For NS GUI, just delete all NS frames. A new hidden one
        ;; will automatically be spawned by the advice to
        ;; `delete-frame'.
        (mapc 'delete-frame 
              (filtered-frame-list 
               (lambda (frm) (eq (frame-terminal frm)
                            (frame-terminal frame)))))
      ad-do-it)))

(provide 'osx-pseudo-daemon)

;;; osx-pseudo-daemon.el ends here
