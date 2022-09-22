;;;; A Portable Pathname Library
;;;;
;;;; The basic operations the library will support will be getting a list of
;;;; files in a directory and determining whether a file or directory with a
;;;; given name exists. There is also a function for recursively walking a
;;;; directory hierarchy, calling a given function for each pathname in the
;;;; tree.

(defun show (pathname)
  (format t "host:      ~a
device:    ~a
directory: ~a
name:      ~a
type:      ~a
version:   ~a
" (pathname-host pathname) (pathname-device pathname) (pathname-directory pathname) (pathname-name pathname) (pathname-type pathname) (pathname-version pathname)))

;;; Listing a Directory

(defun component-present-p (val)
  "tell whether a pathname object's component exists"
  (and val
       (not (eq val :unspecific))))

(defun dir-pathname-p (path)
  "tell whether the given pathname is a directory form"
  (and (not (component-present-p (pathname-name path)))
       (not (component-present-p (pathname-type path)))
       p)) ; i do not know why test "p"

(defun path->dir (path)
  "convert any form of path to a pathname in directory form"
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

(defun dir/* (dir)
  "return a wildcard representing any file in the given directory"
  (make-pathname :defaults (path->dir dir)
                 :name :wild
                 :type #-clisp :wild #+clisp nil))

;; API
(defun ls-dir (dir)
  "return all files and directories in the given directory"
  (when (wild-pathname-p dir)
    (error "Can only list concrete directory names."))
  (setf dir (dir/* dir))
  #+(or sbcl cmu lispworks) (directory dir)
  #+openmcl                 (directory dir :directories           t)
  #+allegro                 (directory dir :directories-are-files nil)
  #+clisp            (nconc (directory dir)
                            (directory (clisp-subdir-wildcard dir)))
  #-(or sbcl cmu lispworks openmcl allegro clisp) ; unknown implement
  (error "LS-DIR is not implemented."))

#+clisp
(defun clisp-subdir-wildcard (dir)
  (make-pathname :defaults  dir
                 :name      nil
                 :type      nil
                 :directory (append (pathname-directory dir) '(:wild))))

;;; Testing a File’s Existence

;; API
(defun file-exists-p (path)
  "path, a file or a directory"
  #+(or sbcl lispworks openmcl) (probe-file path)
  #+(or allegro cmu)        (or (probe-file (path->dir path))
                                (probe-file path))
  #+clisp (or (ignore-errors
               (probe-file (path->file path)))
              (ignore-errors
               (when (ext:probe-directory (setf path (path->dir path)))
                 path)))
  #-(or sbcl cmu lispworks openmcl allegro clisp) ; unknown implement
  (error "FILE-EXISTS-P is not implemented."))

(defun path->file (path)
  "convert any form of path to a pathname in file form"
  (when (wild-pathname-p (setf path (pathname path)))
    (error "Cannot reliably convert wild pathnames."))
  (if (dir-pathname-p path)
      (let* ((dir       (pathname-directory path))
             (name&type (pathname (first (last dir)))))
        (make-pathname :defaults  path
                       :directory (butlast dir)
                       :name      (pathname-name name&type)
                       :type      (pathname-type name&type)))
      path))

;;; Walking a Directory Tree

(defvar *tree-depth* 0
  "tell the current recursive depth in a directory tree")

;; API
(defun tree@dir (dir fn &key directories (test (constantly t)))
  ":directories permits directories to be passed to fn; pathname tested T by :test will be passed to fn"
  (labels ((walk (arg-dir)
             (dolist (path (ls-dir arg-dir))
               (cond ((dir-pathname-p path)
                      (when (and directories (funcall test path))
                        (funcall fn path))
                      (let ((*tree-depth* (1+ *tree-depth*)))
                        (walk path)))
                     ((funcall test path)
                      (funcall fn path))))))
    (walk (path->dir dir))))
