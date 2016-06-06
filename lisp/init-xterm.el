(require 'init-frame-hooks)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))



(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)



;; Make shell output read-only
(add-hook 'comint-preoutput-filter-functions (lambda (text) (propertize text 'read-only t)))

(setq-default comint-scroll-to-bottom-on-input t
              comint-input-ignoredups t)


(provide 'init-xterm)
