;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ispell)

(when (executable-find ispell-program-name)
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (with-eval-after-load 'flyspell
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

(add-hook 'text-mode-hook 'flyspell-mode)

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] nil)
     (define-key flyspell-mode-map (kbd "C-.") nil)
     (define-key flyspell-mode-map (kbd "C-,") nil)))


(provide 'init-spelling)
;;; init-spelling.el ends here
