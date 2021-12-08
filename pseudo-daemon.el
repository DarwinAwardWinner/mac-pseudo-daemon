;;; pseudo-daemon.el --- Not an Emacs daemon, but it plays one on TV -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ryan C. Thompson

;; Filename: pseudo-daemon.el
;; Author: Ryan C. Thompson
;; Created: Tue Dec  7 10:57:21 2021 (-0500)
;; Version: 3.0
;; Package-Requires: ((cl-lib "0.1"))
;; URL: https://github.com/DarwinAwardWinner/mac-pseudo-daemon
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Emacs' daemon mode is useful, but it causes a number of problems
;; with various window systems. For example:
;;
;; - On Mac OS, deleting the last GUI frame makes the Emacs app in
;; your dock nonfunctional until a new frame is opened from the
;; command line.
;; - The long-standing bug with closing displays on GTK:
;; https://gitlab.gnome.org/GNOME/gtk/-/issues/221
;; - In the newew pure GTK branch, an Emacs daemon sometimes randomly
;; loses the ability to access the system's clipboard.
;;
;; To work around this, if you're always going to be running a GUI
;; environment, you can use a hidden GUI frame instead of running
;; Emacs as a daemon. This avoids the issues that come from
;; disconnecting and reconnecting to the window system by ensuring
;; that at least one GUI frame exists at all times. At the same time,
;; it approximates the behavior of the emacs daemon: you can "close
;; the last frame", and then when you open a new frame later, you will
;; still be in the same Emacs instance.
;;
;; This package implments this "hidden GUI frame" scheme to emulate
;; daemon mode, hence the name "pseudo-daemon". The hidden frame is
;; only created on demand when the last "real" GUI frame is deleted.
;; The next time you activate Emacs, instead of creating a new frame,
;; this already-existing hidden frame is simply shown.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl-lib)

(defun pseudo-daemon--get-terminal (&optional object allow-dead)
  "Return the terminal associated with OBJECT.

OBJECT can be a terminal, a frame, or a window. If nil, return the
terminal of the currently selected frame.

if OBJECT has no associated terminal, return nil. If ALLOW-DEAD is
nil, additionally return nil in place of any non-live terminal."
  (let ((terminal
         (cond
          ((framep object)
           (frame-terminal object))
          ((null object)
           (frame-terminal (selected-frame)))
          ((terminal-live-p object)
           object)
          ((windowp object)
           (frame-terminal (window-frame object))))))
    (when (or allow-dead (terminal-live-p terminal))
      terminal)))

(defun pseudo-daemon--terminal-frame-list (&optional terminal predicate)
  "Return a list of all frames on a terminal.

TERMINAL can also be a frame or window, for which the associated
terminal will be used instead.

If TERMINAL is nil, use the terminal of the currently selected
frame.

If PREDICATE is non-nil, return only frames that satisfy
PREDICATE."
  (let ((terminal (pseudo-daemon--get-terminal terminal))
        (predicate (or predicate #'identity)))
    (filtered-frame-list
     (lambda (frm) (and (eq (frame-terminal frm) terminal)
                   (funcall predicate frm))))))

;; (defsubst pseudo-daemon-frame-transient-p (frame)
;;   "Returns non-nil if FRAME is a transient frame."
;;   (frame-parameter frame 'pseudo-daemon-transient))

;; ;; TODO: Run this in `after-make-frame-functions'
;; (defun pseudo-daemon-cleanup-transient-frames (new-frame)
;;   "Delete any transient frames on the same terminal as FRAME.

;; This function will never delete FRAME itself.

;; FRAME can also be a terminal, in which case any transient frames
;; on that terminal are deleted."
;;   (when pseudo-daemon-mode
;;     (dolist (other-frame (pseudo-daemon--terminal-frame-list new-frame))
;;       (when (and (not (eq other-frame new-frame))
;;                  (pseudo-daemon-frame-transient-p other-frame))
;;         (delete-frame other-frame)))))

(defun pseudo-daemon-setup-new-frame (frame)
  "Make FRAME look like Emacs' initial frame.

In general this means making FRAME have a single window
containing the scratch buffer or the startup buffer. See
`initial-buffer-choice'."
  (with-selected-frame frame
    ;; Make the transient frame look as much as possible
    ;; like the initial frame of a terminal.
    (delete-other-windows)
    (switch-to-buffer
     (cond ((stringp initial-buffer-choice)
            (find-file-noselect initial-buffer-choice))
           ((functionp initial-buffer-choice)
            (funcall initial-buffer-choice))
           (t ;; Ignore unusual values; real startup will alert the user.
            (startup--get-buffer-create-scratch))))))

(defun pseudo-daemon-make-hidden-frame (&optional terminal parameters)
  "Make a new iconified frame on TERMINAL.

Returns the newly-created frame. PARAMETERS are passed to
`make-frame'. This is called when `pseudo-daemon-mode' is enabled
and the last frame on an eligible terminal is about to be
deleted."
  (let ((terminal (pseudo-daemon--get-terminal terminal))
        (current-frame (selected-frame)))
    ;; If selected frame is not on the specified terminal, then don't
    ;; do anything.
    (when (eq (frame-terminal current-frame) terminal)
      (with-selected-frame current-frame
        (let ((new-frame (pseudo-daemon--internal-make-hidden-frame
                          terminal parameters current-frame)))
          ;; (message "pseudo-daemon made a new frame!")
          (pseudo-daemon-setup-new-frame new-frame))))))

;; The default implementation here should work for non-Mac platforms
(cl-defgeneric pseudo-daemon--internal-make-hidden-frame
    (terminal parameters _current-frame)
  "Internal generic helper for `pseudo-daemon-make-hidden-frame'.

This function should create the new frame, do whatever is
necessary to iconify/hide it, and then return it."
  (make-frame
   (nconc
    `((terminal . ,terminal)
      (visibility . icon))
    parameters)))

;; Method for ns (Mac) window system
(cl-defmethod pseudo-daemon--internal-make-hidden-frame
  (terminal parameters current-frame &context (window-system ns))
  ;; If current frame is fullscreen, un-fullscreen it, or else
  ;; things break.
  (when (eq (frame-parameter current-frame 'fullscreen)
            'fullboth)
    (set-frame-parameter current-frame 'fullscreen nil)
    ;; Wait for the fullscreen animation to finish
    (sit-for 1.5))
  (prog1
      ;; Now make and return the new default frame, not iconified
      ;; because then it shows up in the dock.
      (make-frame
       (nconc
        `((terminal . ,terminal)
          (visibility . t))
        parameters))
    ;; Finally hide the Emacs app, after a short delay
    (sit-for 0.1)
    (ns-hide-emacs t)
    new-frame))

(cl-defmethod pseudo-daemon--internal-make-hidden-frame
  (terminal parameters current-frame &context (window-system mac))
  ;; If current frame is fullscreen, un-fullscreen it, or else
  ;; things break.
  (when (eq (frame-parameter current-frame 'fullscreen)
            'fullboth)
    (set-frame-parameter current-frame 'fullscreen nil)
    ;; Wait for the fullscreen animation to finish
    (sit-for 1.5))
  (prog1
      ;; Now make and return the new default frame, not iconified
      ;; because then it shows up in the dock.
      (make-frame
       (nconc
        `((terminal . ,terminal)
          (visibility . t))
        parameters))
    ;; Finally hide the Emacs app, after a short delay
    (sit-for 0.1)
    ;; This is the only different from the NS method. TODO: Find a
    ;; better way to do this.
    (call-process
     "osascript" nil nil nil
     "-e" "tell application \"Finder\""
     "-e" "set visible of process \"Emacs\" to false"
     "-e" "end tell")))

;; (defun pseudo-daemon-get-transient-frame (&optional terminal create)
;;   "Returns the transient frame for TERMINAL.

;; The terminal has no transient frame, create a new one and return
;; it if CREATE is non-nil, otherwise return nil. If TERMINAL is
;; nil, use the currently active terminal."

;;   (let ((terminal (pseudo-daemon--get-terminal terminal))
;;         (transient-frame
;;          (car-safe
;;           (pseudo-daemon--terminal-frame-list
;;            terminal
;;            #'pseudo-daemon-frame-transient-p))))
;;     (if (and (null transient-frame) create)
;;         (pseudo-daemon-make-transient-frame frame)
;;       transient-frame)))

;; (defsubst pseudo-daemon-make-frame-intransient (frame)
;;   "Mark FRAME as no longer transient.

;; Note that this conversion is one-way. There is no
;; \"pseudo-daemon-make-frame-transient\"."
;;   (set-frame-parameter frame 'pseudo-daemon-transient nil))

(defgroup pseudo-daemon nil
  "[PLACEHOLDER]"
  :group 'convenience)

(define-minor-mode pseudo-daemon-mode
  "[PLACEHOLDER]"
  :group 'pseudo-daemon
  :global t
  :init-value nil
  (when (and pseudo-daemon-mode
             (bound-and-true-p mac-pseudo-daemon-mode))
    (display-warning
     'pseudo-daemon
     "Enabling `pseudo-daemon-mode' and `mac-pseudo-daemon-mode' at the same time may cause unexpected results. You should pick one and disable the other."))
  (if pseudo-daemon-mode
      ;; Activate
      (progn
        (advice-add 'delete-frame :before
                    #'delete-frame@pseudo-daemon)
        (advice-add 'handle-delete-frame :around
                    #'handle-delete-frame@pseudo-daemon)
        (advice-add 'save-buffers-kill-terminal :around
                    #'save-buffers-kill-terminal@pseudo-daemon))
    ;; Deactivate
    (advice-remove 'delete-frame
                   #'delete-frame@pseudo-daemon)
    (advice-remove 'handle-delete-frame
                   #'handle-delete-frame@pseudo-daemon)
    (advice-remove 'save-buffers-kill-terminal
                   #'save-buffers-kill-terminal@pseudo-daemon)))

(defconst pseudo-daemon--invalid-window-systems '(t nil pc)
  "Symbols that do not represent graphical window systems.")

(defcustom pseudo-daemon-window-systems '(ns mac x pgtk)
  "Set of window systems for which to use `pseudo-daemon-mode'.

See `window-system' for the set of valid values. If a frame's
window system is not in this list, `pseudo-daemon-mode' will have
no effect when that frame is deleted."
  :group 'pseudo-daemon
  :type
  `(list
    :tag "Window systems"
    (set
     :inline t
     :tag "Known"
     (const :tag "ns (Mac OS Cocoa)" ns)
     (const :tag "mac (Mac port)" mac)
     (const :tag "x (X windows)" x)
     (const :tag "pgtk (Pure GTK)" pgtk)
     (const :tag "w32 (Windows)" w32))
    (repeat
     :inline t
     :tag "Other"
     (symbol
      :tag "Window system"
      :value WINDOW-SYSTEM
      :validate
      ;; Can't be nil or t
      (lambda (wid)
        (if (memq (widget-value wid) ,pseudo-daemon--invalid-window-systems)
            (progn
              (widget-put
               wid
               :error (format "`%S' is not a valid window system"
                              (widget-value wid)))
	      wid)
          nil))))))

(defsubst pseudo-daemon-frame-eligible-p (frame)
  "Returns non-nil if FRAME is handled by `pseudo-daemon-mode'.

FRAME can also be a window or terminal. If non-nil, the return
value is the same as `terminal-live-p'."
  (when (display-graphic-p frame)
    (let ((terminal-type (terminal-live-p (pseudo-daemon--get-terminal frame))))
      (and (not (memq terminal-type pseudo-daemon--invalid-window-systems))
           (car (memq terminal-type pseudo-daemon-window-systems))))))

(defsubst pseudo-daemon-last-frame-on-terminal-p (&optional frame)
  "Returns non-nil if FRAME is the only frame on its terminal.

FRAME defaults to the selected frame."
  (<= (length (pseudo-daemon--terminal-frame-list frame)) 1))

;; Note that `delete-frame-functions' is not appropriate here because
;; `delete-frame' aborts before running that hook when called on the
;; last frame, so it would never run.
(defun delete-frame@pseudo-daemon (&rest args)
  (let ((frame (car args)))
    (when (and pseudo-daemon-mode
             (pseudo-daemon-frame-eligible-p frame)
             (pseudo-daemon-last-frame-on-terminal-p frame))
    (pseudo-daemon-make-hidden-frame))))

;; (defun pseudo-daemon-before-delete-frame (frame)
;;   "Handle deletion of FRAME for `pseudo-daemon-mode'.

;; This is added to `delete-frame-functions' when
;; `pseudo-daemon-mode' is enabled."
;;   (when (and pseudo-daemon-mode
;;              (pseudo-daemon-frame-eligible-p frame)
;;              (pseudo-daemon-last-frame-on-terminal-p frame))
;;     (pseudo-daemon-make-hidden-frame)))

(defun handle-delete-frame@pseudo-daemon (orig-fun &rest args)
  "Advice implementing `pseudo-daemon-mode'.

Normally, when `handle-delete-frame' is called on the last frame
on a terminal, it calls `save-buffers-kill-emacs' instead of
`delete-frame'. This advice causes it to just call `delete-frame'
as normal, so that the pseudo-daemon can do its work."
  (let* ((event (car args))
         (frame (posn-window (event-start event))))
    (if (and pseudo-daemon-mode
             (pseudo-daemon-frame-eligible-p frame)
             (pseudo-daemon-last-frame-on-terminal-p frame))
        (delete-frame frame t)
      (apply orig-fun args))))

(defsubst pseudo-daemon-delete-all-frames-on-terminal (terminal)
  "Call `delete-frame' on all frames on TERMINAL"
  (mapc #'delete-frame (pseudo-daemon--terminal-frame-list terminal)))

(defun save-buffers-kill-terminal@pseudo-daemon (orig-fun &rest args)
  "Advice implementing `pseudo-daemon-mode'.

When `pseudo-daemon-mode' is enabled, the terminal is \"killed\"
by deleting each of the terminal's frames in sequence, so the
pseudo-daemon can do its work when the last frame is deleted."
   (let ((frame (selected-frame)))
     (if (and pseudo-daemon-mode
             (pseudo-daemon-frame-eligible-p frame))
         (pseudo-daemon-delete-all-frames-on-terminal frame)
       (apply orig-fun args))))

(provide 'pseudo-daemon)

;;; pseudo-daemon.el ends here
