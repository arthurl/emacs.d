(when (maybe-require-package 'markdown-mode)

  (after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-x C-b") #'org-toggle-checkbox))
  (after-load 'gfm-mode
    (define-key gfm-mode-map (kbd "C-c C-x C-b") #'org-toggle-checkbox))

  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

;; Enable syntax highlighting for math LaTeX fragments
(setq-default markdown-enable-math t)


(provide 'init-markdown)
