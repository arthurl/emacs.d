(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(require-package 'pyvenv)



;;; Python shell commands
(setq-default python-check-command "flake8"
              python-shell-interpreter "python")



;;; Anaconda-mode
(when (maybe-require-package 'anaconda-mode)
  (add-hook 'python-mode-hook
            (lambda () (let ((venv-var (getenv "VIRTUAL_ENV")))
                    (when venv-var
                      (message (concat "Anaconda-mode on virtualenv: " venv-var)))
                    (anaconda-mode)
                    (anaconda-eldoc-mode))))
  (after-load 'anaconda-mode
    (diminish 'anaconda-mode)
    ;; Use standard keys for code navigation
    (define-key anaconda-mode-map (kbd "M-*") nil)
    (define-key anaconda-mode-map (kbd "M-[") #'anaconda-mode-go-back)
    (define-key anaconda-mode-map (kbd "M-/") #'anaconda-mode-find-assignments)
    (define-key anaconda-mode-map (kbd "M-,") #'anaconda-mode-find-references))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))


(provide 'init-python-mode)
