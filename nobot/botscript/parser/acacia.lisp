;;;; Copyright (c) 2021 NOBOT-S
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
          :nobot/botscript/parser/acacia/parser-generator)
  (:export #:with-acacia-process
           #:define-rule))
