;;; -*- lexical-binding: t -*-

(require 'mac-pseudo-daemon)
(require 'buttercup)
(require 'cl-lib)

(describe "The mac-pseudo-daemon package"

  :var ((orig-mac-pseudo-daemon-mode mac-pseudo-daemon-mode)
        (orig-macpd-mac-frame-types macpd-mac-frame-types))

  (before-each
    ;; Start each test with the mode enabled
    (mac-pseudo-daemon-mode 1)
    ;; Start each test with the original values
    (setq macpd-mac-frame-types (get 'macpd-mac-frame-types 'standard-value))
    ;; Spy on and/or disable some functions
    (spy-on 'macpd-make-new-default-frame :and-call-through)
    ;; Need to disable this and also keep track of when someone tries
    ;; to call it
    (spy-on 'save-buffers-kill-emacs)
    ;; These probably shouldn't get called, but just in case we'll
    ;; disable them so Emacs doesn't accidentally get killed.
    (spy-on 'server-save-buffers-kill-terminal)
    (spy-on 'kill-emacs))

  ;; Restore the original state of all relevant vars after all tests
  ;; are complete
  (after-all
    (mac-pseudo-daemon-mode (if orig-mac-pseudo-daemon-mode 1 0))
    (setq (macpd-mac-frame-types orig-macpd-mac-frame-types)))

  (it "should detect whether this frame is a Mac GUI frame"
    ;; To put the function through its paces, we mess with the
    ;; definition of a mac frame
    (let ((macpd-mac-frame-types nil))
      (expect (macpd-frame-is-mac-frame (selected-frame))
              :not :to-be-truthy))
    (let ((macpd-mac-frame-types (list (framep (selected-frame)))))
      (expect (macpd-frame-is-mac-frame (selected-frame))
              :to-be-truthy)))

  (describe "when current frame is (pretending to be) a Mac GUI frame"

    (before-each
      (setq
       ;; This is all the frame types, so every frame now counts as a
       ;; Mac frame.
       macpd-mac-frame-types '(t x w32 ns mac pc)))
    ;; TODO: Can't make frames in batch mode
    (xit "should only spawn a new frame when closing the last frame"
      (let ((this-term (frame-terminal (selected-frame))))
        (dotimes (_ 5)
          (make-frame `((terminal . ,(frame-terminal (selected-frame))))))
        (let ((this-term-frames
               (filtered-frame-list
                (lambda (frm) (eq (frame-terminal frm) this-term)))))
          (dolist (frm this-term-frames)
            (delete-frame frm)))
        (expect 'macpd-make-new-default-frame :to-have-been-called-times 1)
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

  (describe "when current frame is not a Mac GUI frame"
    (before-each
      ;; Pretend all frames are *not* Mac frames.
      (setq
       ;; Now no frames frames count as a Mac frames
       macpd-mac-frame-types '()))
    ;; TODO
    (xit "should not spawn a new frame when closing any frame")
    (xit "should allow terminal closure via `handle-delete-frame'")
    (xit "should allow terminal closure via `save-buffers-kill-terminal'")))

;;; test-macpd.el ends here
