(TeX-add-style-hook "week11_report"
 (lambda ()
    (LaTeX-add-labels
     "sec-1"
     "sec-1-1"
     "fig:1"
     "fig:2"
     "sec-2")
    (TeX-add-symbols
     '("reftbl" 1)
     '("reffig" 1))
    (TeX-run-style-hooks
     "algorithmic"
     "algorithm"
     "amsmath"
     "framed"
     "colortbl"
     "enumerate"
     "comment"
     "fancybox"
     "fancyhdr"
     "xcolor"
     "listings"
     "xeCJK"
     "xltxtra"
     "xunicode"
     "fontspec"
     "hyperref"
     "amstext"
     "amssymb"
     "latexsym"
     "wasysym"
     "marvosym"
     "textcomp"
     "soul"
     "wrapfig"
     "float"
     "longtable"
     "graphicx"
     "fixltx2e"
     "fontenc"
     "T1"
     "inputenc"
     "utf8"
     "latex2e"
     "art10"
     "article")))

