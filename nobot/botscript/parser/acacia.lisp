;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>

;;;;                   ``````                        
;;;;             ```.``..........```````              
;;;;    ````...........`.....................```````` 
;;;; ``.``.......`` `````ACACIA ```````.....```````   
;;;;        `  ``    ```  `      `       `  `         
;;;;             ```````````  ``       ```            
;;;;                  ``` .  ``    ````               
;;;;                    `````    ``                   
;;;;                      `.  ```                     
;;;;                       .``                        
;;;;                       ..                         
;;;;                       ``

;;;; parser generator - acacia: https://github.com/bohdan-sokolovskyi/acacia


(uiop:define-package :nobot/botscript/parser/acacia
    (:use :cl
          :nobot/botscript/parser/acacia/acacia-runner
          :nobot/botscript/parser/acacia/parser-generator
          :nobot/botscript/parser/acacia/tree-tools
          :nobot/botscript/parser/acacia/result-packaging)
  (:export #:with-acacia-runner
           #:define-rule
           #:pack-parse-tree
           #:acacia-packed-result
           #:acacia-get-parse-tree
           #:acacia-get-source-type
           #:acacia-get-source
           #:same-parse-tree-?))
