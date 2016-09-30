;;; Emacs-eclim
(when (maybe-require-package 'eclim)
  (setq-default eclimd-wait-for-process nil
                ;; TODO: Is this generally applicable, outside java-mode?
                ;;help-at-pt-display-when-idle t
                ;;help-at-pt-timer-delay 0.1
                )

  (autoload #'eclim--accepted-p "eclim-common")
  (autoload #'eclim--project-dir "eclim-common")
  (add-hook 'java-mode-hook
            (lambda () (when (and buffer-file-name
                             (eclim--accepted-p buffer-file-name)
                             (eclim--project-dir))
                    (eclim-mode 1))))

  (after-load 'eclim
    (diminish 'eclim-mode "ECL")
    ;;(help-at-pt-set-timer)

    ;;;; Use standard keys for code navigation
    (after-load 'cc-mode
      (define-key java-mode-map (kbd "M-.") #'eclim-java-find-declaration)
      (define-key java-mode-map (kbd "M-[") #'pop-tag-mark)
      (define-key java-mode-map (kbd "M-,") #'eclim-java-find-references)
      (define-key java-mode-map (kbd "C-c r R") #'eclim-java-refactor-rename-symbol-at-point)))

  (when (maybe-require-package 'company-emacs-eclim)
    (setq-default company-emacs-eclim-ignore-case t)
    (after-load 'company
      (add-hook 'java-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-emacs-eclim)))))

  (autoload #'start-eclimd "eclimd" "Start eclimd daemon" t))



;;; Indenting
;; Assumes that init-java is loaded after init-c where google-c-style is loaded
(when (fboundp 'google-c-lineup-expression-plus-4)
  (let ((val (assoc 'java-mode c-default-style)))
    (when val (setf (cdr val) "Google"))))


(provide 'init-java)
