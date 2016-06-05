(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-global-mode)

  ;; The following code means you get a menu if you hit "C-c p" and wait
  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c p"))

  ;; Shorter modeline
  (after-load 'projectile
    (setq-default
     projectile-mode-line
     '(:eval
       (if (file-remote-p default-directory)
           " Pr"
         (format " Pr[%s]" (projectile-project-name))))))

  ;; More reasonable time to expire remote cache
  (setq-default projectile-file-exists-remote-cache-expire (* 3 60))
  ;; Open project dir when switching to project
  (setq-default projectile-switch-project-action #'projectile-dired)

  ;; projectile-project-p is not autoloaded in the library :/
  (autoload #'projectile-project-p "projectile")
  ;; projectile-project-root is not autoloaded in the library
  (autoload #'projectile-project-root "projectile"))


(provide 'init-projectile)
