;;; CMake-mode for editing CMake config files
(require-package 'cmake-mode)



;;; C headers completion
(require-package 'company-c-headers)
(after-load 'cc-mode
  (after-load 'company
    (after-load 'company-c-headers
      (add-hook 'c-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-c-headers)))
      (add-hook 'c++-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-c-headers))))))



;;; RTags
(when (not (find-if-not #'executable-find '("rc" "rdm" "cmake")))
  (when (maybe-require-package 'rtags)
    ;;(setq-default rtags-socket-file (expand-file-name "~/.rdm"))
    (setq-default rtags-autostart-diagnostics t)
    ;;(rtags-diagnostics)
    (setq-default rtags-completions-enabled t)
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
