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
          :nobot/botscript/parser/acacia/configuration
          :nobot/botscript/parser/acacia/parser-generator
          :nobot/botscript/parser/acacia/result-packaging)
  (:export #:with-acacia-process
           #:define-rule
           #:pack-parse-tree
           #:acacia-packed-result
           #:get-parse-tree
           #:get-source-type
           #:get-source))
