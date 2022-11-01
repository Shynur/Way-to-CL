(cl:defpackage :shynur.path-tools
  (:documentation "
    Utilities this package supports are:
      1. getting a list of files and directories in a directory;
      2. determining whether a file or directory with the specific name exists;
      3. walking in a directory hierarchy recursively as a tree.
    Compatibility: SBCL, CMUCL, LispWorks, OpenMCL, Allegro, CLISP.")
  (:use :cl)
  (:export :list-directory
           :path-exists?
           :tree@directory 
           :*tree-depth*))

(cl:in-package :shynur.path-tools)

;;; Listing a Directory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function: list-directory

(defun directory-pathname? (path)
  "tell whether the given `pathname' represents a `directory'"
  (declare (pathname path))
  (let ((component-present? #'(lambda (component)
                                (and component
                                     (not (eq component :unspecific))))))
    (and (not (funcall component-present? (pathname-name path)))
         (not (funcall component-present? (pathname-type path))))))

(defun path->directory (path)
  "convert path to a `pathname' in `directory' form.
   examples: ./lisp-works/ -> ./lisp-works/
             works/halo.md -> works/halo.md/"
  (when (wild-pathname-p (setf path (pathname path)))
    (error "Cannot reliably convert wild pathnames."))
  (if (directory-pathname? path)
      path
      (make-pathname :defaults  path
                     :name      nil
                     :type      nil
                     :directory (append (or (pathname-directory path)
                                            '(:relative))
                                        `(,(file-namestring path))))))

;; API
(defun list-directory (directory)
  "return all `files' and `directories' in the given directory"
  (when (wild-pathname-p directory)
    (error "Can only list concrete directory names."))
  (let ((dir/* (make-pathname :defaults (path->directory directory)
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
;;; function: path-exists?

;; API
(defun path-exists? (path)
  "if the given file or directory exists, return itself or its pathname; else return NIL"
  #+(or sbcl lispworks openmcl) (probe-file path)
  #+(or allegro cmu)            (or (probe-file (path->directory path))
                                    (probe-file path))
  #+clisp                       (or (ignore-errors (probe-file (path->file path)))
                                    (ignore-errors (when (ext:probe-directory (setf path (path->directory path)))
                                                     path))))

(defun path->file (path)
  "convert path to a `pathname' in `file' form.
   examples:  ./lisp-works/ -> ./lisp-works
             works/halo.md/ -> works/halo.md"
  (when (wild-pathname-p (setf path (pathname path)))
    (error "Cannot reliably convert wild pathnames."))
  (if (directory-pathname? path)
      (let* ((path-dir  (pathname-directory path))
             (name&type (pathname (first (last path-dir)))))
        (make-pathname :defaults  path
                       :directory (butlast path-dir)
                       :name      (pathname-name name&type)
                       :type      (pathname-type name&type)))
      path))

;;; Walking a Directory Tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function: tree@directory
;;; variable: *tree-depth*

;; API
(defvar *tree-depth* 0
  "current `depth' of tree@'s recursion in a directory tree.
   it should NOT be modified by anyone but the function `tree@directory'.")

;; API
(defun tree@directory (directory fn &key directories (test (constantly t)))
  ":directories permits directories to be passed to fn;
   pathname tested T by :test will be passed to fn.
   NOTICE: this function will NOT call fn on the given dir,
           so the 0-depth of its recursion is when processing the given dir's direct-subpath"
  (labels ((walk (directory)
             (dolist (path (list-directory directory))
               (cond ((directory-pathname? path) (when (and directories (funcall test path))
                                              (funcall fn path))
                                            (let ((*tree-depth* (1+ *tree-depth*)))
                                              (walk path)))
                     ((funcall test path) (funcall fn path))))))
    (walk directory)))
