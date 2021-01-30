;;;; Copyright (c) 2021 NOBOT
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


(uiop:define-package :nobot/botscript/parser-generator
    (:use :cl
          :nobot/botscript/parser/acacia/configuration
          :nobot/botscript/parser/acacia/parser-generator)
  (:export #:with-init-acacia
           #:define-rule))
