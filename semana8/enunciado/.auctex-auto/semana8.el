(TeX-add-style-hook
 "semana8"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "brazil") ("fontenc" "T1") ("inputenc" "latin1")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "babel"
    "fontenc"
    "inputenc"
    "amsmath"
    "amsfonts"
    "bussproofs"
    "amssymb"
    "tikz"
    "latexsym"
    "pgf"))
 :latex)

