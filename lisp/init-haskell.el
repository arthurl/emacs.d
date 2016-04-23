(require-package 'haskell-mode)


;; Use intero for completion and flycheck

(when (maybe-require-package 'intero)
  (after-load 'haskell-mode
    (intero-global-mode)
    (add-hook 'haskell-mode-hook 'eldoc-mode))
  (after-load 'intero
    ;; Don't clobber sanityinc/counsel-search-project binding
    (define-key intero-mode-map (kbd "M-?") nil)
    (after-load 'flycheck
      (flycheck-add-next-checker 'intero
                                 '(warning . haskell-hlint)))))


(add-auto-mode 'haskell-mode "\\.ghci\\'")


;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)



;; Source code helpers

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(when (maybe-require-package 'hindent)
  (add-hook 'haskell-mode-hook 'hindent-mode))

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))


(after-load 'page-break-lines
  (push 'haskell-mode page-break-lines-modes))

;; Don't popup errors
(setq-default haskell-interactive-popup-errors nil)



;; Add quotes to compile path to handle folders names with spaces
(setq-default
 haskell-compile-cabal-build-command
 "cd \"%s\" && cabal build --ghc-option=-ferror-spans"
 haskell-compile-cabal-build-alt-command
 "cd \"%s\" && cabal clean -s && cabal build --ghc-option=-ferror-spans"
 haskell-compile-command
 "ghc -Wall -ferror-spans -fforce-recomp -c \"%s\"")

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "<f5>") 'haskell-compile))



;; Speedbar
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(after-load 'speedbar
  (speedbar-add-supported-extension ".hs")
  (speedbar-add-supported-extension ".lhs")
  (speedbar-add-supported-extension ".hsc"))

;; Set up tagging if hasktags exists
(if (executable-find "hasktags")
    (setq-default haskell-tags-on-save t)
  (after-load 'haskell-mode
    ;; If hasktags does not exist, don't bother to try to use tags.
    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)))


(provide 'init-haskell)
