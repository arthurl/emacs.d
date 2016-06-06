;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

(add-hook 'text-mode-hook 'flyspell-mode)

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] nil)))


(provide 'init-flyspell)
