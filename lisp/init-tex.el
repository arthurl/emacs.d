;; Latex formatter
(when (maybe-require-package 'reformatter)
  (reformatter-define latexindent
    :program "nix-shell"
    :args '("-p" "texlive.combine { inherit (texlive) scheme-minimal latexindent; }" "--pure" "--run" "latexindent -m")))


(provide 'init-tex)
