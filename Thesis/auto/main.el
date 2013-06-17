(TeX-add-style-hook "main"
 (lambda ()
    (LaTeX-add-bibliographies
     "ref/refs")
    (TeX-add-symbols
     '("vc" 1)
     '("reftbl" 1)
     '("reffig" 1))
    (TeX-run-style-hooks
     "algpseudocode"
     "algorithmicx"
     "algorithm"
     "bm"
     "pgfplots"
     "comment"
     "thutils"
     "latex2e"
     "thuthesis10"
     "thuthesis"
     "bachelor"
     "nofonts"
     "data/cover"
     "data/intro01"
     "data/framework02"
     "data/suffixtree03"
     "data/cluster04"
     "data/template05"
     "data/experiment06"
     "data/future07"
     "data/ack"
     "data/translation")))

