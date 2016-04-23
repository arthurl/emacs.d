;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")

  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-x C-b") #'org-toggle-checkbox))
  (with-eval-after-load 'gfm-mode
    (define-key gfm-mode-map (kbd "C-c C-x C-b") #'org-toggle-checkbox))

  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

;; Enable syntax highlighting for math LaTeX fragments
(setq-default markdown-enable-math t)


(provide 'init-markdown)
;;; init-markdown.el ends here
