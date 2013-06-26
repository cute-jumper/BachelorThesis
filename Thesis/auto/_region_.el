(TeX-add-style-hook "_region_"
 (lambda ()
    (LaTeX-add-labels
     "chap:translation")
    (TeX-add-symbols
     '("vc" 1)
     '("reftbl" 1)
     '("reffig" 1))
    (TeX-run-style-hooks
     "algpseudocode"
     "algorithmicx"
     "algorithm"
     "bm"
     "tikz"
     "comment"
     "thutils"
     "latex2e"
     "thuthesis10"
     "thuthesis"
     "bachelor"
     "nofonts")))

