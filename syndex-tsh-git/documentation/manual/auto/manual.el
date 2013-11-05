(TeX-add-style-hook "manual"
 (function
  (lambda ()
    (LaTeX-add-labels
     "characteristics"
     "downloader_specification")
    (TeX-run-style-hooks
     "fullpage"
     "html"
     "latex2e"
     "rep11"
     "report"
     "11pt"
     "twoside"))))

