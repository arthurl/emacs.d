;;; Code formatter
(when (maybe-require-package 'reformatter)
  (reformatter-define clang-format
    :program "nix-shell"
    :args '("--pure" "-p" "clang" "--run" "clang-format")))

(setq-default flycheck-clang-language-standard "c++23")


;;; Indenting
;; Set default to K&R
(setq-default
 c-default-style '((java-mode . "java")
                   (python-mode . "python")
                   (awk-mode . "awk")
                   (other . "k&r")))
;; Set Google indenting style as default if package exists
;; see: https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
(when (maybe-require-package 'google-c-style)
  (autoload #'google-c-lineup-expression-plus-4 "google-c-style" nil)
  (with-eval-after-load 'cc-vars
    (c-add-style "Google" google-c-style)
    (setf (cdr (assoc 'other c-default-style)) "Google")))


(provide 'init-c)
