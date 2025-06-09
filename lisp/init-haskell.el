;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'haskell-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-cabal-mode 'subword-mode)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (add-auto-mode 'haskell-mode "\\.ghci\\'")

  ;; Indentation
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


  ;; Source code helpers

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  (when (maybe-require-package 'reformatter)
    (reformatter-define hindent
      :program "hindent"
      :lighter " Hin")

    (defalias 'hindent-mode 'hindent-on-save-mode)

    (reformatter-define ormolu
      :program "ormolu"
      :args (when (buffer-file-name)
              `("--stdin-input-file" ,buffer-file-name))
      :lighter " Orm"))

  (with-eval-after-load 'haskell-mode
    (with-eval-after-load 'flymake
      ;; Use compilation-mode's keybindings for flymake
      (define-key haskell-mode-map (kbd "M-g M-n") 'flymake-goto-next-error)
      (define-key haskell-mode-map (kbd "M-g M-p") 'flymake-goto-prev-error))

    (when (maybe-require-package 'eglot)
      ;; if we're using LSP, we don't need other flymake checkers
      ;;(remove-hook 'flymake-diagnostic-functions 'haskell-flymake)
      (add-hook 'haskell-mode-hook #'eglot-ensure)
      (add-hook 'eglot-managed-mode-hook (lambda () (setq-local eldoc-documentation-function #'eldoc-documentation-default)))
      (with-eval-after-load 'eglot
        (define-key haskell-mode-map (kbd "C-'") 'eglot-code-actions)))

    (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
    (define-key haskell-mode-map (kbd "C-o") 'open-line))


  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'haskell-mode)))



(maybe-require-package 'dhall-mode)




(provide 'init-haskell)
;;; init-haskell.el ends here
