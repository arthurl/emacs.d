(require-package 'auctex)

;; AucTeX
(setq-default TeX-auto-save t)
(setq-default TeX-parse-self t)
(setq-default TeX-save-query nil)
; (setq-default TeX-master nil)  ; This somehow screws up
; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
; (setq reftex-plug-into-AUCTeX t)
(setq-default TeX-PDF-mode t)

;; To set up Skim and SyncTeX, see
;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/



;;; Company-mode auto-completion
(when (maybe-require-package 'company-auctex)
  (add-hook 'LaTeX-mode-hook
            (lambda () (sanityinc/local-push-company-backend #'company-auctex-labels)))
  (add-hook 'LaTeX-mode-hook
            (lambda () (sanityinc/local-push-company-backend #'company-auctex-bibs)))
  (add-hook 'LaTeX-mode-hook
            (lambda () (sanityinc/local-push-company-backend
                   '(company-auctex-macros company-auctex-symbols company-auctex-environments)))))


(provide 'init-tex)
