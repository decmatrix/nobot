;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/npm
    (:use :cl :yason)
  (:import-from :osicat
                #:read-link)
  (:import-from :cl-fad
                #:directory-exists-p
                #:pathname-parent-directory
                #:pathname-directory-pathname)
  (:import-from :copy-directory
                #:copy)
  (:import-from :nobot/toplevel/error-handling
                #:raise-projectgen-error)
  (:import-from :nobot/utils
                #:copy-file
                #:get-root-dir
                #:get-date-now
                #:get-program-version)
  (:import-from :nobot/projectgen/utils
                #:make-path)
  (:import-from :nobot/projectgen/common
                #:create-project
                #:*project*
                #:get-project-path
                #:get-project-author
                #:get-project-version
                #:get-project-name))

(in-package :nobot/projectgen/npm)

(defvar *project-path*)

(defmethod create-project ((project-type (eql :npm)))
  (let ((*project-path* (get-project-path *project*)))
    (generate-package-json)
    (generate-readme)
    (copy-bot-lib)))

(defun generate-package-json ()
  (let ((file-path (get-file-path
                    :name "package"
                    :type "json")))
    (with-open-file
        (stream
         file-path
         :direction :output
         :if-exists :supersede)
      (with-output (stream :indent t)
        (with-object ()
          (encode-object-elements
           "name"        (get-project-name *project*)
           "author"      (get-project-author *project*)
           "version"     (get-project-version *project*)
           "private"     "true"
           "license"     ""
           "description" "generated bot by NOBOT platform"
           "type"        "module"
           "main"        "index.js")
          (with-object-element ("scripts")
            (with-object ()
              (encode-object-elements
               "start"       "node index.js"
               "postinstall" "cd botlib && npm install")))))
      (format stream "~%"))))

(defun generate-readme ()
  (let ((file-path (get-file-path
                    :name "README"
                    :type "md")))
    (with-open-file
        (stream
         file-path
         :direction :output)
      (format stream "# Generated bot ~a by NOBOT platform ~a in ~a~%"
              (get-project-name *project*)
              (get-program-version)
              (get-date-now))
      (format stream "# Guide how to start bot~%***~%")
      (format stream
              "1. install NodeJS platform on your computer using the next command: `~a`~%"
              (resolve-node-install-command))
      (format stream
              "2. run install dependencies using the next command: `npm install`~%")
      (format stream
              "3. run your bot server using the next command: `npm start`~%")
      (format stream
              "4. enjoy your bot!~%"))))

(defun resolve-node-install-command ()
  (cond
    ((uiop:os-macosx-p)
     "brew install node")
    ((uiop:os-unix-p)
     "sudo apt-get install node")
    (t (raise-projectgen-error
        "unsupported type of operation system: ~a"
        (software-type)))))

;;TODO: here trash, try rewrite it
(defun copy-bot-lib ()
  (let ((lib-path (resolve-lib-path))
        (lib-path-in-project
         (pathname
          (format nil "~abotlib/" *project-path*)))
        (package-json
         (make-pathname
          :name "package"
          :type "json")))
    (ensure-directories-exist lib-path-in-project)
    (copy-file
     (merge-pathnames
      lib-path
      package-json)
     (merge-pathnames
      lib-path-in-project
      package-json))
    (let ((src (format nil "~asrc/" lib-path-in-project)))
      (ensure-directories-exist src)
      (copy
       (pathname
        (format nil "~asrc/" lib-path))
       src))
    (let ((resources (format nil "~aresources/" lib-path-in-project)))
      (copy
       (pathname
        (format nil "~aresources/" lib-path))
       resources))))

(defun resolve-lib-path ()
  (let ((lib-path
         (pathname
          (format nil "~abotlib/wisteria-js/"
                  (pathname-parent-directory (get-root-dir))))))
    (if (directory-exists-p lib-path)
        lib-path
        (cond
          ((or (uiop:os-macosx-p)
               (uiop:os-unix-p))
           (format nil "~abotlib/wisteria-js/"
                   (pathname-parent-directory
                    (pathname-directory-pathname (read-link "/usr/local/bin/nobot")))))
          (t (raise-projectgen-error
              "unsupported type of operation system: ~a"
              (software-type)))))))

(defun get-file-path (&key name type)
  (merge-pathnames
   *project-path*
   (make-pathname
    :name name
    :type type)))
