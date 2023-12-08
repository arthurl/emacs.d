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

(when (maybe-require-package 'flymake-ruff)
  (defun sanityinc/flymake-ruff-maybe-enable ()
    (when (executable-find "ruff")
      (flymake-ruff-load)))
  (add-hook 'python-base-mode-hook 'sanityinc/flymake-ruff-maybe-enable))

(maybe-require-package 'ruff-format)
(reformatter-define ruff-fix
  :program ruff-format-command
  :args (list "check" "--fix-only" "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffFix")

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("\\(poetry\\|uv\\)\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "pyproject.toml"))

(with-eval-after-load 'python
  (with-eval-after-load 'flymake
    ;; for some reason (remove-hook 'flymake-diagnostic-functions 'python-flymake) doesn't work
    (setq-default python-flymake-command nil)
    ;; Use compilation-mode's keybindings for flymake
    (define-key python-base-mode-map (kbd "M-g M-n") 'flymake-goto-next-error)
    (define-key python-base-mode-map (kbd "M-g M-p") 'flymake-goto-prev-error)))

(with-eval-after-load 'python
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (define-key python-base-mode-map (kbd "C-'") 'eglot-code-actions)))

(provide 'init-python)
;;; init-python.el ends here
