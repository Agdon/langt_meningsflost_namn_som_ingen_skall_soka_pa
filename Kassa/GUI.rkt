#lang racket
(require racket/gui/base)
(require "File_handle.rkt")
(require "Database_handle.rkt")
(define *working-database* (read-database-file "working.database" (new database-class)))
(define login-function
  (lambda (user-data-base)
    (define login-func void)
    (define login-func-guest void)
    (define login-frame (new frame%
                             [label "Login"]
                             [width 300]
                             [height 150]))
    (define login-panel (new group-box-panel%
                             [label ""]
                             [parent login-frame]))
    (define user-name-box (new text-field%
                               [label "Användarnamn"]
                               [parent login-panel]
                               [style (list 'single)]
                               [init-value ""]))
    (define user-password-box (new text-field%
                               [label "Passord"]
                               [parent login-panel]
                               [init-value ""]
                               [style (list 'single 'password)]))
    (define login-button (new button%
                              [label "Login"]
                              [parent login-panel]
                              [callback login-func]))
    (define guest-button (new button%
                              [label "Login as guest"]
                              [parent login-panel]
                              [callback login-func-guest]))
    
    (send login-frame show #t)))
    
(define call-primary-window
  (lambda (user)
    (define the-frame (new frame% [label "Kassa"]
                           [width 600]
                           [height 600]))
    (define top-mb (new menu-bar% [parent the-frame]))
    (define m-file (new menu% [label "File"] [parent top-mb]))
    (define m-edit (new menu% [label "Edit"] [parent top-mb]))
    (define m-help (new menu% [label "Help"] [parent top-mb]))
    (append-editor-operation-menu-items m-edit #f)
    (define buy-func
      (lambda (b e)
        (let* ((lst-nr (send selling-list get-selections))
               (index (if (not (eq? lst-nr null))
                       (send selling-list get-data (car lst-nr))
                       'noneselected))
               (num (string->number (send sell-amount get-value))))
          (if (or (eq? index 'noneselected) (not num))
              (void)
              (begin
                (send *working-database* sell index num)
                (if (eq? 'donewrite (create-database-file "working.database" *working-database*))
                    (update-lists)
                    (void)))))))
    (define update-lists
      (lambda ()
        (let ((indexses (send *working-database* get-all-in-use-index)))
          (begin
          (send selling-list set 
                (map number->string indexses) 
                (map
                  (lambda (index)
                    (send *working-database* get-item-name index))
                  indexses)
                (map
                  (lambda (index)
                    (number->string (/ (send *working-database* get-item-price index) 100)))
                  indexses)
                (map
                  (lambda (index)
                    (number->string (send *working-database* get-item-stock index)))
                  indexses)))
          (define data-set 
            (lambda (num lst)
              (if (< num (length indexses))
                  (begin
                    (send selling-list set-data num (car lst))
                    (data-set (+ num 1) (cdr lst)))
                  (void))))
          (data-set 0 indexses))))
    (define tab-selector
      ((lambda (old-tab)
         (lambda (b e)
           (let ((new-tab (send selection-tabs get-selection)))
             (cond
               ((= 0 new-tab)
                (begin
                  (send selection-tabs add-child selling-panel)
                  (cond
                    ((= 1 old-tab) (send selection-tabs delete-child buying-panel))
                    ((= 2 old-tab) (send selection-tabs delete-child admin-panel)))
                  (set! old-tab new-tab)))
               ((= 1 new-tab)
                (begin
                  (send selection-tabs add-child buying-panel)
                  (cond
                    ((= 0 old-tab) (send selection-tabs delete-child selling-panel))
                    ((= 2 old-tab) (send selection-tabs delete-child admin-panel)))
                  (set! old-tab new-tab)))
               ((= 2 new-tab)
                (begin
                  (if #t ;(send user admin?)
                      (send selection-tabs add-child admin-panel)
                      (void))
                  (cond
                    ((= 0 old-tab) (send selection-tabs delete-child selling-panel))
                    ((= 1 old-tab) (send selection-tabs delete-child buying-panel)))
                  (set! old-tab new-tab)))))))
       0))
    (define selection-tabs (new tab-panel%
                                [choices (list "försäljnig" "Inköp" "Administartion")]
                                [parent the-frame]
                                [callback tab-selector]))
    (define selling-panel (new group-box-panel%
                               [label ""] 
                               [parent selection-tabs]
                               [enabled #t]
                               
                               ))
    (define buying-panel (new group-box-panel%
                              [label ""]
                              [parent selection-tabs]
                              [enabled #f]
                              [style (list  'deleted)]))
    (define admin-panel (new group-box-panel%
                             [label ""]
                             [parent selection-tabs]
                             [enabled #f]
                             [style (list  'deleted)]))
    
    (define selling-list (new list-box%
                              [label ""]
                              [choices (list)]
                              [parent selling-panel]
                              [style (list 'single 'column-headers)]
                              [columns (list "Index" "Namn" "Pris" "Antal i lager")]
                              [vert-margin 20]
                              [horiz-margin 20]))
    
    (define sell-amount (new text-field%
                             [parent selling-panel]
                             [vert-margin 5]
                             [horiz-margin 20]
                             [min-width 50]
                             [label "Antal"]
                             [init-value "1"]))
    (define buy-button (new button%
                              [label "Kontant"]
                              [parent selling-panel]
                              [callback buy-func]))
    
                                             
        
                
          
    (begin
      
      
      (update-lists)
      (send the-frame show #t))))