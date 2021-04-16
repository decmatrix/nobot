;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/npm
    (:use :cl)
  (:import-from :copy-directory
                #:copy)
  (:import-from :alexandria
                #:plist-hash-table)
  (:import-from :yason
                #:encode)
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
         :direction :output)
      (encode
       (plist-hash-table
        `(
          "name"        ,(get-project-name *project*)
          "version"     ,(get-project-version *project*)
          "description" "generatet bot by NOBOT platform"
          "main"        "index.js"
          "type"        "module"
          "author"      ,(get-project-author *project*)
          "scripts"     ,(plist-hash-table
                          '("test" "echo \"Error: no test specified\" && exit 1")
                          :test #'equal)
          "postinstall" "cd botlib && npm install"
          "license"     "<none>")
        :test #'equal)
       stream))))

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
              "2. run instaling dependencies using the next command: `npm install`~%")
      (format stream
              "3. run your bot server using the next command: `npm start`")
      (format stream
              "4. enjoy your bot!~%"))))

(defun resolve-node-install-command ()
  (cond
    ((uiop:os-macosx-p)
     "brew install node")
    ((uiop:os-unix-p)
     "sudo apt-get install node")))

;;TODO: here trash, try rewrite it
(defun copy-bot-lib ()
  (let ((lib-path
         (pathname
          (format nil "~abotlib/wisteria-js/"
                  (get-root-dir))))
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

(defun get-file-path (&key name type)
  (merge-pathnames
   *project-path*
   (make-pathname
    :name name
    :type type)))
