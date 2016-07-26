;;; CMake-mode for editing CMake config files
(require-package 'cmake-mode)



;;; RTags
(when (not (find-if-not #'executable-find '("rc" "rdm" "cmake")))
  (when (maybe-require-package 'rtags)
    ;;(setq-default rtags-socket-file (expand-file-name "~/.rdm"))
    (setq-default rtags-autostart-diagnostics nil)
    ;;(rtags-diagnostics)
    (setq-default rtags-completions-enabled nil)
    (add-hook
     'c-initialization-hook
     (lambda ()
       (after-load 'rtags
         (after-load 'company
           (when rtags-completions-enabled
             (add-hook 'c-mode-hook
                       (lambda () (sanityinc/local-push-company-backend 'company-rtags)))
             (add-hook 'c++-mode-hook
                       (lambda () (sanityinc/local-push-company-backend 'company-rtags))))))))
    (after-load 'cc-mode
      (after-load 'rtags  ;; Needed only if rtags is not required
        ;; Enable guide-key for rtags prefix
        (after-load 'guide-key (add-to-list 'guide-key/guide-key-sequence "C-c r"))
        (rtags-enable-standard-keybindings c-mode-map "C-c r ")
        (rtags-enable-standard-keybindings c++-mode-map "C-c r ")
        (define-key rtags-mode-map (kbd "q") #'quit-window)
        (define-key rtags-dependency-tree-mode-map (kbd "q") #'quit-window)
        (define-key rtags-references-tree-mode-map (kbd "q") #'quit-window)
        (define-key rtags-preprocess-mode-map (kbd "q") #'quit-window)
        (define-key rtags-diagnostics-mode-map (kbd "q") #'quit-window))
      ;; These are the same keys defined in the standard keybinding, but without prefix.
      (define-key c-mode-map (kbd "M-.") #'rtags-find-symbol-at-point)
      (define-key c-mode-map (kbd "M-,") #'rtags-find-references-at-point)
      (define-key c-mode-map (kbd "M-[") #'rtags-location-stack-back)
      (define-key c-mode-map (kbd "M-]") #'rtags-location-stack-forward)
      (define-key c++-mode-map (kbd "M-.") #'rtags-find-symbol-at-point)
      (define-key c++-mode-map (kbd "M-,") #'rtags-find-references-at-point)
      (define-key c++-mode-map (kbd "M-[") #'rtags-location-stack-back)
      (define-key c++-mode-map (kbd "M-]") #'rtags-location-stack-forward))))



;;; Irony-mode
(when (executable-find "cmake")
  (when (maybe-require-package 'irony)
    (add-hook 'c-mode-hook #'irony-mode)
    (add-hook 'c++-mode-hook #'irony-mode)
    (add-hook 'objc-mode-hook #'irony-mode)

    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (add-hook 'irony-mode-hook
              (lambda ()
                (define-key irony-mode-map [remap completion-at-point]
                  #'irony-completion-at-point-async)
                (define-key irony-mode-map [remap complete-symbol]
                  #'irony-completion-at-point-async)))
    ;; Note: cmake-ide is used to configure irony-mode compile options.
    (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

    ;;; Company-irony-c-headers
    ;; Note that `sanityinc/local-push-company-backend' prepends the new
    ;; backend to the list. However, hooks are executed from last to first.
    ;; Hence `company-irony-c-headers' must be added BEFORE `company-irony'
    ;; backend.
    (when (maybe-require-package 'company-irony-c-headers)
      (after-load 'cc-mode
        (after-load 'company
          (add-hook 'irony-mode-hook
                    (lambda () (sanityinc/local-push-company-backend 'company-irony-c-headers))))))

    ;;; Company-irony
    (when (maybe-require-package 'company-irony)
      (after-load 'cc-mode
        (after-load 'company
          (add-hook 'irony-mode-hook
                    (lambda () (sanityinc/local-push-company-backend 'company-irony))))))

    ;;; Flycheck-irony
    (when (maybe-require-package 'flycheck-irony)
      ;; Disable clang and gcc flycheckers if flycheck-irony is available
      (add-hook 'irony-mode-hook
                (lambda () (setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))))
      (after-load 'flycheck
        (flycheck-irony-setup)))

    ;;; Irony-eldoc
    (when (maybe-require-package 'irony-eldoc)
      (add-hook 'irony-mode-hook #'irony-eldoc))

    ))



;;; CMake-ide
(when (executable-find "cmake")
  (when (maybe-require-package 'cmake-ide)
    ;; Rely on cmake-ide to set the correct include paths for company-c-headers
    (setq-default cmake-ide-flags-c '("-I/usr/local/include" "-I/usr/include" "-DNDEBUG")
                  cmake-ide-flags-c++ cmake-ide-flags-c
                  cmake-ide-build-dir "build")
    (after-load 'cc-mode
      ;; Needed so that cmake-ide starts rdm. Tries to detect if rtags.el
      ;; package exists without loading rtags by testing autoloaded function.
      (when (not (find-if-not #'executable-find '("rc" "rdm")))
        (when (fboundp 'rtags-enable-standard-keybindings)
          (require 'rtags)))
      (cmake-ide-setup)

      (define-key c-mode-map (kbd "<f5>") #'cmake-ide-compile)
      (define-key c++-mode-map (kbd "<f5>") #'cmake-ide-compile))))



;;; Indenting
;; Set default to K&R
(setq-default
 c-default-style '((java-mode . "java")
                   (python-mode . "python")
                   (awk-mode . "awk")
                   (other . "k&r")))
;; Set Google indenting style as default if package exists
;; see: https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
(when (maybe-require-package 'google-c-style)
  (autoload #'google-c-lineup-expression-plus-4 "google-c-style" nil)
  (after-load 'cc-vars
    (c-add-style "Google" google-c-style)
    (setf (cdr (assoc 'other c-default-style)) "Google")))


(provide 'init-c)
