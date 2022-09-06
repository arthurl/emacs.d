;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(with-eval-after-load 'python
  (with-eval-after-load 'flymake
    ;; Use compilation-mode's keybindings for flymake
    (define-key python-mode-map (kbd "M-g M-n") 'flymake-goto-next-error)
    (define-key python-mode-map (kbd "M-g M-p") 'flymake-goto-prev-error)))

(with-eval-after-load 'python
  (when (maybe-require-package 'eglot)
    ;; if we're using LSP, we don't need other flymake checkers
    (remove-hook 'flymake-diagnostic-functions 'python-flymake)
    (add-hook 'python-mode-hook 'eglot-ensure)))


(provide 'init-python)
;;; init-python.el ends here
