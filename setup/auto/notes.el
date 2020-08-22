(TeX-add-style-hook
 "notes"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "portuguese") ("inputenc" "utf8")))
   (TeX-run-style-hooks
    "babel"
    "inputenc"
    "fancyhdr"
    "graphicx"
    "hyperref"
    "float"
    "fullpage"
    "proof"
    "tikz"
    "amssymb"
    "amsthm"
    "stmaryrd"
    "color")
   (TeX-add-symbols
    '("TC" 1)
    '("V" 1)
    '("F" 1)
    '("C" 1)
    '("D" 1)
    '("brownBG" 1)
    '("yellowBG" 1)
    '("whiteFG" 1)
    '("blackFG" 1)
    '("brownFG" 1)
    '("yellowFG" 1)
    '("purpleFG" 1)
    '("orangeFG" 1)
    '("blueFG" 1)
    '("greenFG" 1)
    '("redFG" 1)
    "ColourStuff"
    "red"
    "green"
    "blue"
    "orange"
    "purple"
    "yellow"
    "brown"
    "black"
    "white"
    "MonochromeStuff")
   (LaTeX-add-environments
    "Lemma"
    "Theorem"
    "Example"))
 :latex)

