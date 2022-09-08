;;; Mode customisations

;; Markdown
(setq markdown-command "pandoc -f markdown+tex_math_single_backslash+smart -t html -s --mathjax --highlight-style=pygments")

;; Eglot
(with-eval-after-load 'eglot
  (setq eglot-confirm-server-initiated-edits nil))



;;; Global customisations

;; Disable lax space matching
(setq search-whitespace-regexp nil)



;;; Writing and language
;; Spell check
(setq ispell-dictionary "en_GB")


(provide 'init-local)
