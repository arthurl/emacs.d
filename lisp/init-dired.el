;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (with-eval-after-load 'dired
    (diredfl-global-mode)
    (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t))) ;; Don't truncate lines
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))



;; Use human readable file sizes. Supported by OS X.
(setq-default dired-listing-switches "-alh")

(defvar arthur/OSX-unsupported-ls-switches
  "--group-directories-first "
  "Switches which should be activated but is unsupported on OS X.
Used in arthur/toggle-OSX-supported-ls-switches")

(defun arthur/toggle-OSX-supported-ls-switches ()
  "Toggles whether OS X unsupported switches are passed to ls by dired."
  (interactive)
  (if dired-use-ls-dired
      (setq dired-use-ls-dired nil
            dired-listing-switches (string-remove-prefix
                                    arthur/OSX-unsupported-ls-switches
                                    dired-listing-switches))
    (setq dired-use-ls-dired t
          dired-listing-switches (concat arthur/OSX-unsupported-ls-switches
                                         dired-listing-switches))))

;; Enable switches if system supports it.
(setq dired-use-ls-dired nil)
(when (or (executable-find "gls")
          (not *is-a-mac*))
  (arthur/toggle-OSX-supported-ls-switches))

;; Delete moves to trash
(setq-default delete-by-moving-to-trash t)

;; Enable command that is disabled by default
(put 'dired-find-alternate-file 'disabled nil)


(provide 'init-dired)
;;; init-dired.el ends here
