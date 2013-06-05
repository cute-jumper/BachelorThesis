(TeX-add-style-hook "main"
 (lambda ()
    (LaTeX-add-bibliographies
     "ref/refs")
    (TeX-run-style-hooks
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
     "data/translation"
     "data/resume")))

