(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)



;;; Python shell commands
(setq-default python-check-command "flake8"
              python-shell-interpreter "python")



;;; Anaconda-mode
(when (maybe-require-package 'anaconda-mode)
  (add-hook 'python-mode-hook
            (lambda () (let ((venv-var (getenv "VIRTUAL_ENV")))
                    (when venv-var
                      (message (concat "Anaconda-mode on virtualenv: " venv-var)))
                    (anaconda-mode)
                    (anaconda-eldoc-mode))))
  (after-load 'anaconda-mode
    (diminish 'anaconda-mode)
    ;; Use standard keys for code navigation
    (define-key anaconda-mode-map (kbd "M-*") nil)
    (define-key anaconda-mode-map (kbd "M-[") #'anaconda-mode-go-back)
    (define-key anaconda-mode-map (kbd "M-/") #'anaconda-mode-find-assignments)
    (define-key anaconda-mode-map (kbd "M-,") #'anaconda-mode-find-references))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-mode-hook
                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))



;;; Python project management
(when (executable-find "virtualenv")
  ;; Provide capability to work with Python virtualenv, if projectile does not
  ;; exist but `pyvenv-activate' or `pyvenv-workon' are set.
  (when (maybe-require-package 'pyvenv)
    (after-load 'python
      (add-hook 'python-mode-hook #'arthur/setup-virtualenv))))

(defun arthur/setup-virtualenv ()
  "Set up virtualenv and pylint/pyflakes if virtualenv exists.
  User's `pyvenv-activate' or `pyvenv-workon' customisations are
  respected and will not be overwritten. Will query if another
  virtualenv is currently activated."
  (let ((virtualenv-fulldir
         (or
          ;; If `pyvenv-activate' or `pyvenv-workon' is already set to a valid
          ;; path, respect that. Don't second guess even if project directory is
          ;; different. (Because the variables are usually set file or directory
          ;; local. So we can assume that they are accurate.)
          (find-if #'file-directory-p
                   (mapcar #'expand-file-name
                           (remove-if-not #'stringp
                                          '(pyvenv-activate pyvenv-workon))))
          ;; If neither of `pyvenv-activate' or `pyvenv-workon' is set, check if
          ;; the environment variable `VIRTUAL_ENV' is already set to a valid
          ;; directory. If so, return `VIRTUAL_ENV' UNLESS the file is in a
          ;; project AND `VIRTUAL_ENV' is not a sub-directory (at any level) or
          ;; the project. (This check is important as `VIRTUAL_ENV' is not
          ;; "buffer-local".)
          (let ((venv (getenv "VIRTUAL_ENV")))
            (when (and venv (file-directory-p venv))
              (unless (and (projectile-project-p)
                           (not (s-prefix? (projectile-project-root) venv)))
                venv)))
          ;; If none of `pyvenv-activate' or `pyvenv-workon' or `VIRTUAL_ENV' is
          ;; set, try to find the virtualenv directory with
          ;; arthur/find-virtualenv-dir-in-project-root.
          (when (projectile-project-p)
            (arthur/find-virtualenv-dir-in-project-root (projectile-project-root))))))
    ;; See if we finally have a valid virtualenv
    (if virtualenv-fulldir
        (progn
          (pyvenv-activate virtualenv-fulldir)
          (arthur/flycheck-set-pylint-flake8-location virtualenv-fulldir)
          (message (concat "Using python virtualenv: " virtualenv-fulldir)))
      ;; Fail to find a valid virtualenv. Run `pyvenv-deactivate' just in case
      ;; to clear `VIRTUAL_ENV'.
      (pyvenv-deactivate)
      (message "No python virtualenv found"))))

(defun arthur/find-virtualenv-dir-in-project-root (project-root)
  "Automatic detection of python virtualenv. Argument cannot be
  nil. Returns a full path if the virtualenv can be unambiguously
  found, otherwise nil."
  (let ((match-dirs
         (remove-if (lambda (fulldir)
                      (or (not (file-directory-p fulldir)) (file-symlink-p fulldir))
                      ) (mapcar (lambda (dir)
                                  (expand-file-name dir project-root)
                                  ) python-virtualenv-dir-names))))
    (when match-dirs  ;; Some possible virtualenv dirs found!
      ;; Now check that it has the required files
      (let ((match-dirs-compulsory-files-exists
             (remove-if (lambda (fulldir)
                          (find-if-not (lambda (file)
                                         (file-exists-p (expand-file-name file fulldir))
                                         ) python-virtualenv-dir-must-have-all-of)
                          ) match-dirs)))
        (and (= 1 (list-length match-dirs-compulsory-files-exists))
             ;; virtualenv dir unambiguously found!
             (car match-dirs-compulsory-files-exists))))))

(defun arthur/flycheck-set-pylint-flake8-location (venv-fulldir)
  "Make flycheck use pylint/flake8 executable in virtualenv if
  either/both the executables exists."
  (let ((pylint-loc (expand-file-name "bin/pylint" venv-fulldir))
        (flake8-loc (expand-file-name "bin/flake8" venv-fulldir)))
    (when (file-exists-p pylint-loc)
      (setq flycheck-python-pylint-executable pylint-loc))
    (when (file-exists-p flake8-loc)
      (setq flycheck-python-flake8-executable flake8-loc))))

;; User to set this in init-local.el if necessary
(defvar python-virtualenv-dir-names
  '(".venv" ".virtualenv" "venv" "virtualenv")
  "List of possible virtualenv directory names to be tried.")

(defvar python-virtualenv-dir-must-have-all-of
  '("bin/activate" "bin/python")
  "List of files that must exist in a python virtualenv.")


(provide 'init-python-mode)
