(require-package 'dired+)
(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t))) ;; Don't truncate lines
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
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
