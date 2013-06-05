(TeX-add-style-hook "_region_"
 (lambda ()
    (LaTeX-add-labels
     "chap:translation")
    (TeX-run-style-hooks
     "thutils"
     "latex2e"
     "thuthesis10"
     "thuthesis"
     "bachelor"
     "nofonts")))

