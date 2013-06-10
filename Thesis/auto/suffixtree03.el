(TeX-add-style-hook "suffixtree03"
 (lambda ()
    (LaTeX-add-labels
     "chap:suffixtree"
     "sec:suffixtreeintro"
     "suffixtree:fig:suffix-tree-apple"
     "sec:ukkonen"
     "sec:multipldetect"
     "sec:summarysuffixtree")
    (TeX-run-style-hooks
     "data/suffixtree03-figures"
     "data/suffixtree03-algo")))

