;; Latex formatter
(when (maybe-require-package 'reformatter)
  (reformatter-define bibtex-tidy
    :program "bibtex-tidy"
    :args '( "--generate-keys" "--curly" "--numeric" "--months" "--no-align" "--blank-lines"
             "--sort=key" "--duplicates=key,doi" "--drop-all-caps" "--no-escape"
             "--sort-fields=title,shorttitle,author,date,year,month,day,journal,booktitle,location,on,publisher,address,series,volume,number,pages,doi,isbn,issn,url,urldate,copyright,category,note,metadata"
             "--trailing-commas" "--no-remove-dupe-fields" "--wrap=80"
             "--no-modify"))

  (reformatter-define latexindent
    :program "nix-shell"
    :args '("-p" "texlive.combine { inherit (texlive) scheme-minimal latexindent; }" "--pure" "--run" "latexindent -m")))


(provide 'init-tex)
