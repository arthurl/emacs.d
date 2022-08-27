;;; init-purescript.el --- Support the Purescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'purescript-mode)
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

  (add-hook 'purescript-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'purescript-sort-imports nil t)))

  (add-hook 'purescript-mode-hook (apply-partially 'prettify-symbols-mode -1))

  (with-eval-after-load 'purescript-mode
    (define-key purescript-mode-map (kbd "C-o") 'open-line)
    (with-eval-after-load 'flymake
      ;; Use compilation-mode's keybindings for flymake
      (define-key purescript-mode-map (kbd "M-g M-n") 'flymake-goto-next-error)
      (define-key purescript-mode-map (kbd "M-g M-p") 'flymake-goto-prev-error)))

  (when (maybe-require-package 'reformatter)
    (reformatter-define purty
      :program "purty" :lighter " purty"))

  (when (maybe-require-package 'eglot)
    (with-eval-after-load 'purescript-mode
      ;; hook must run only after add-node-modules-path
      (add-hook 'purescript-mode-hook 'eglot-ensure)
      (with-eval-after-load 'eglot
        (define-key purescript-mode-map (kbd "C-'") 'eglot-code-actions))))

  (when (maybe-require-package 'psci)
    (add-hook 'purescript-mode-hook 'inferior-psci-mode))

  (when (maybe-require-package 'add-node-modules-path)
    (with-eval-after-load 'purescript-mode
      (add-hook 'purescript-mode-hook 'add-node-modules-path))
    (with-eval-after-load 'psci
      (advice-add 'psci :around (lambda (oldfun &rest args)
                                  (let ((psci/purs-path (or (executable-find "purs")
                                                            psci/purs-path))
                                        (psci/psc-package-path (or (executable-find "psc-package")
                                                                   psci/psc-package-path))
                                        (psci/spago-path (or (executable-find "spago")
                                                             psci/spago-path)))
                                    (apply oldfun args)))))))

(provide 'init-purescript)
;;; init-purescript.el ends here
