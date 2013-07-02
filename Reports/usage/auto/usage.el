(TeX-add-style-hook "usage"
 (lambda ()
    (LaTeX-add-labels
     "sec:templateextractor"
     "sec:sbt"
     "sec:code"
     "sec:templ"
     "sec:play")
    (TeX-add-symbols
     "prj")
    (TeX-run-style-hooks
     "geometry"
     "qjpzh"
     "code"
     "latex2e"
     "art10"
     "article")))

