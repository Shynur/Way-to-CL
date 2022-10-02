(defpackage :shynur.path
  (:documentation
"
The basic operations the library supports are:
  getting a list of files in a directory;
  determining whether a file or directory with a given name exists;
  walking a directory hierarchy recursively.

Compatibility: SBCL, CMUCL, LispWorks, OpenMCL, Allegro, CLISP.
" )
  (:use :common-lisp)
  (:export :ls))

(in-package :shynur.path)

;;; Listing a Directory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fun: ls

(defun dir-pathname-p (path)
  "tell whether the given pathname represents a directory"
  (let ((component-present-p #'(lambda (component)
                                 (and component
                                      (not (eq component :unspecific))))))
    (and (not (funcall component-present-p (pathname-name path)))
         (not (funcall component-present-p (pathname-type path))))))

(defun path->dir (path)
  "convert path to a pathname in directory form.
   examples: ./lisp-works/ -> ./lisp-works/
             works/halo.md -> works/halo.md/"
  (when (wild-pathname-p (setf path (pathname path)))
    (error "Cannot reliably convert wild pathnames."))
  (if (dir-pathname-p path)
      path
      (make-pathname :defaults  path
                     :name      nil
                     :type      nil
                     :directory (append (or (pathname-directory path)
                                            '(:relative))
                                        `(,(file-namestring path))))))

;; API
(defun ls (dir)
  "return all files and directories in the given directory"
  (when (wild-pathname-p dir)
    (error "Can only list concrete directory names."))
  (let ((dir/* (make-pathname :defaults (path->dir dir)
                              :name     :wild
                              :type     #-clisp :wild
                                        #+clisp nil)))
    #+(or sbcl cmu lispworks) (directory dir/*)
    #+openmcl                 (directory dir/* :directories           t)
    #+allegro                 (directory dir/* :directories-are-files nil)
    #+clisp                   (nconc (directory dir/*)
                                     (directory (make-pathname :defaults  dir/*
                                                               :name      nil
                                                               :type      nil
                                                               :directory (append (pathname-directory dir/*) '(:wild)))))))

;;; Testing a File’s Existence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fun: path-exists-p

;; API
(defun path-exists-p (path)
  "if the given file or directory exists, return itself or its pathname; else return NIL"
  #+(or sbcl lispworks openmcl) (probe-file path)
  #+(or allegro cmu)            (or (probe-file (path->dir path))
                                    (probe-file path))
  #+clisp                       (or (ignore-errors (probe-file (path->file path)))
                                    (ignore-errors (when (ext:probe-directory (setf path (path->dir path)))
                                                     path))))

(defun path->file (path)
  "convert path to a pathname in file form.
   examples:  ./lisp-works/ -> ./lisp-works
             works/halo.md/ -> works/halo.md"
  (when (wild-pathname-p (setf path (pathname path)))
    (error "Cannot reliably convert wild pathnames."))
  (if (dir-pathname-p path)
      (let* ((path-dir  (pathname-directory path))
             (name&type (pathname (first (last path-dir)))))
        (make-pathname :defaults  path
                       :directory (butlast path-dir)
                       :name      (pathname-name name&type)
                       :type      (pathname-type name&type)))
      path))

;;; Walking a Directory Tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fun: tree@
;;; var: *tree-depth*

;; API
(defvar *tree-depth* 0
  "current depth of tree@'s recursion in a directory tree")

;; API
(defun tree@ (dir fn &key directories (test (constantly t)))
  ":directories permits directories to be passed to fn;
   pathname tested T by :test will be passed to fn.
   NOTICE: this function will NOT call fn on the given dir,
           so the 0-depth of its recursion is when processing the given dir's direct-subpath"
  (labels ((walk (dir)
             (dolist (path (ls dir))
               (cond ((dir-pathname-p path) (when (and directories (funcall test path))
                                              (funcall fn path))
                                            (let ((*tree-depth* (1+ *tree-depth*)))
                                              (walk path)))
                     ((funcall test path) (funcall fn path))))))
    (walk dir)))
