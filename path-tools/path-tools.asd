(asdf:defsystem "path-tools"
  :description "Some simple manipulation functions on path."
  :version "0.1" 
  :license "The Unlicense"
  :author "Shynur <one.last.kiss@outlook.com>"
  :homepage "https://github.com/Shynur/way-to-CL/path/"
  :components ((:file "path-tools"))
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md")))
