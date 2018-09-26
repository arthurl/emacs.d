;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile)

  ;; More reasonable time to expire remote cache
  (setq-default projectile-file-exists-remote-cache-expire (* 3 60))
  ;; Open project dir when switching to project
  (setq-default projectile-switch-project-action #'projectile-dired)

  ;; projectile-project-p is not autoloaded in the library :/
  (autoload #'projectile-project-p "projectile")
  ;; projectile-project-root is not autoloaded in the library
  (autoload #'projectile-project-root "projectile"))


(provide 'init-projectile)
;;; init-projectile.el ends here
