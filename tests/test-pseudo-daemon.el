;;; -*- lexical-binding: t -*-

(require 'pseudo-daemon)
(require 'buttercup)
(require 'cl-lib)

(describe "The pseudo-daemon package"

  :var ((orig-pseudo-daemon-mode pseudo-daemon-mode)
        (orig-pseudo-daemon-window-systems pseudo-daemon-window-systems))

  (before-each
    ;; Start each test with the mode enabled
    (pseudo-daemon-mode 1)
    ;; Spy on and/or disable some functions
    (spy-on 'pseudo-daemon-make-hidden-frame :and-call-through)
    ;; Need to disable this and also keep track of when someone tries
    ;; to call it
    (spy-on 'save-buffers-kill-emacs)
    ;; These probably shouldn't get called, but just in case we'll
    ;; disable them so Emacs doesn't accidentally get killed during
    ;; testing.
    (spy-on 'server-save-buffers-kill-terminal)
    (spy-on 'kill-emacs))

  ;; Restore the original state of all relevant vars after all tests
  ;; are complete
  (after-all
    (setq pseudo-daemon-window-systems orig-pseudo-daemon-window-systems)
    (pseudo-daemon-mode (if orig-pseudo-daemon-mode 1 0)))

  ;; TODO: Can't make frames in batch mode, so this test can't run
  (xit "should only spawn a new frame when closing the last frame"
    (let ((this-term (frame-terminal (selected-frame))))
      (dotimes (_ 5)
        (make-frame `((terminal . ,(frame-terminal (selected-frame))))))
      (let ((this-term-frames
             (filtered-frame-list
              (lambda (frm) (eq (frame-terminal frm) this-term)))))
        (dolist (frm this-term-frames)
          (delete-frame frm)))
      (expect 'make-new-default-frame :to-have-been-called-times 1)
      (expect 'save-buffers-kill-emacs :not :to-have-been-called)
      ;; Terminal should now have exactly one frame -- the
      ;; newly-created one.
      (let ((this-term-frames
             (filtered-frame-list
              (lambda (frm) (eq (frame-terminal frm) this-term)))))
        (expect (length this-term-frames)
                :to-equal 1))))
  (xit "should prevent terminal closure via `handle-delete-frame'")
  (xit "should prevent terminal closure via `save-buffers-kill-terminal'"))

;;; test-pseudo-daemon.el ends here
