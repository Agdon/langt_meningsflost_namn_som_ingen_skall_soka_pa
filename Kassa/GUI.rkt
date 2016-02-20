#lang racket
(require racket/gui/base)
(require "File_handle.rkt")
(require "Database_handle.rkt")
(require racket/file)
(require racket/date)
(date-display-format 'iso-8601)
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
                                   [label "Password"]
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

(define call-primary-window ;;huvud fönster
  (lambda (user-name rights-level)
    (define working-database (read-database-file "working.database" (new database-class))) ;;läser in databas
    (define the-frame (new frame% [label "Kassa"] ;;fönster
                           [width 600]
                           [height 600]))
    (define top-mb (new menu-bar% [parent the-frame])) ;; skapar meny rad
    (define m-file (new menu% [label "File"] [parent top-mb]))
    (define m-edit (new menu% [label "Edit"] [parent top-mb]))
    (define m-help (new menu% [label "Help"] [parent top-mb]))
    (define m-file-Save (new menu-item%
                             [label "Save as"]
                             [parent m-file]
                             [callback (lambda (b e)
                                           (if (eq? 'donewrite (create-database-file (get-file) working-database))
                                                (update-lists)
                                                (bell)))]))
    (define m-file-open (new menu-item%
                             [label "Open backup"]
                             [parent m-file]
                             [callback (lambda (b e)
                                           (if (eq? 'donewrite (create-database-file (string-append "recover_open" (date->string (current-date)) ".database") working-database))
                                               (begin
                                                (set! working-database (read-database-file (get-file) (new database-class)))
                                                (update-lists))
                                                (bell)))]))
                    
     (define m-file-New (new menu-item%
                             [label "New"]
                             [parent m-file]
                             [callback (lambda (b e)
                                           (if (and (confirm-modal) (eq? 'donewrite (create-database-file (string-append "recover_open" (date->string (current-date)) ".database") working-database)))
                                               (begin
                                                (set! working-database (new database-class))
                                                (update-lists))
                                                (bell)))]))               
                              
    (append-editor-operation-menu-items m-edit #f) ;; slut med meny rad
    (define to-buy-list-handler ;; hanterar inköpslistan
      (new (class object%
             (super-new)
             (init-field
              [index-amount-list '()])
             (define/public add
               (lambda (index amount)
                 (set! index-amount-list (cons (cons index amount) index-amount-list))))
             (define/public remove
               (lambda (index)
                 (set! index-amount-list (filter (lambda (element) (not (eq? (car element) index))) index-amount-list))))
             (define/public clear
               (lambda ()
                 (set! index-amount-list '())))
             (define/public get-indexses
               (lambda ()
                 (map car index-amount-list)))
             (define/public get-amounts
               (lambda () (map cdr index-amount-list)))
             (define/public get-full-list
               (lambda () index-amount-list)))))
    (define confirm-modal ;; simpel popup som kan användas flera gånger
      (lambda ()
        (define return-bol #f)
        (define popup
          (new dialog%
               [label "Bekräfta"]
               [parent the-frame]
               [width 200]
               [height 50]))
        (define vertical-dialog
          (new vertical-panel%
               [parent popup]))
        (define sure-message
          (new message%
               [label "Är du säker?"]
               [parent vertical-dialog]))
        (define horiz-dialog
          (new horizontal-panel%
               [parent vertical-dialog]))
        (define cancel-dialog
          (new button%
               [label "Avbryt"]
               [parent horiz-dialog]
               [callback (lambda (b e)
                           (send popup show #f))]))
        (define OK-dialog
          (new button%
               [label "Bekfräfta"]
               [parent horiz-dialog]
               [callback (lambda (b e)
                           (begin
                             (set! return-bol #t)
                             (send popup show #f)))]))
        (begin
          (send popup show #t) ;(display "done")
          return-bol)))
    (define get-text-modal ;;simpel popup som frågar efter en sträng
      (lambda ()
        (define return-text "")
        (define popup
          (new dialog%
               [label "Ange värde"]
               [parent the-frame]
               [width 200]
               [height 50]))
        (define vertical-dialog
          (new vertical-panel%
               [parent popup]))
        (define modal-text
          (new text-field%
               [label ""]
               [parent vertical-dialog]))
        (define horiz-dialog
          (new horizontal-panel%
               [parent vertical-dialog]))
        (define cancel-dialog
          (new button%
               [label "Avbryt"]
               [parent horiz-dialog]
               [callback (lambda (b e)
                           (send popup show #f))]))
        (define OK-dialog
          (new button%
               [label "OK"]
               [parent horiz-dialog]
               [callback (lambda (b e)
                           (begin
                             (set! return-text (send modal-text get-value))
                             (send popup show #f)))]))
        (begin
          (send popup show #t)
          return-text)))
    
    (define confirm-buy-func ;;använd då ett inköp konfirmeras ATT GÖRA skriva ut ett inköps kvitto
      (lambda (b e)
        (let ((t-price (+ (* 100 (string->number (send buy-total-price-kr get-value))) (string->number (send buy-total-price-ore get-value)))))
          (if (and (confirm-modal) (number? t-price))
              (begin
                ;(display "a")
                (for-each (lambda (element)
                            (begin
                              (send working-database add-item-stock (car element) (cdr element))
                              (display-to-file (string-append 
                                                "Date: " 
                                                (date->string (current-date) #t)
                                                "  Action: Bought "
                                                "  User: " 
                                                user-name 
                                                "  Item: " 
                                                (send working-database get-item-name (car element)) 
                                                "  Amount: "
                                                (number->string (cdr element))
                                                "\r\n")
                                               "Log.txt"
                                               #:exists 'append)))
                          (send to-buy-list-handler get-full-list))
                (send to-buy-list-handler clear)
                (send working-database add-saldo (- t-price))
                (if (eq? 'donewrite (create-database-file "working.database" working-database))
                    (update-lists)
                    (void)))
              (void)))))
    
    (define add-to-buy-list-func
      (lambda (b e) (let* ((lst-nr (send add-to-buy-list-box-list get-selections))
                           (index (if (not (eq? lst-nr null))
                                      (send add-to-buy-list-box-list get-data (car lst-nr))
                                      'noneselected))
                           (num (string->number (send buy-amount get-value))))
                      (if (or (eq? index 'noneselected) (not num))
                          (void)
                          (begin
                            (send to-buy-list-handler add index num)
                            (send add-to-buy-list-frame show #f)
                            (update-lists))))))
    (define buy-list-remove-func
      (lambda (b e) (let* ((lst-nr (send buying-list get-selections))
                           (index (if (not (eq? lst-nr null))
                                      (send buying-list get-data (car lst-nr))
                                      'noneselected)))
                      (if (eq? index 'noneselected)
                          (void)
                          (begin
                            (send to-buy-list-handler remove index)
                            (update-lists))))))   
    (define sell-func
      (lambda (b e)
        (let* ((lst-nr (send selling-list get-selections))
               (index (if (not (eq? lst-nr null))
                          (send selling-list get-data (car lst-nr))
                          'noneselected))
               (num (string->number (send sell-amount get-value))))
          (if (or (eq? index 'noneselected) (not num))
              (void)
              (begin
                (send working-database sell index num)
                (display-to-file (string-append 
                                  "Date: " 
                                  (date->string (current-date) #t) 
                                  "  Action: Sold "
                                  "  User: " 
                                  user-name 
                                  "  Item: " 
                                  (send working-database get-item-name index) 
                                  "  Amount: "
                                  (number->string num)
                                  "\r\n")
                                 "Log.txt"
                                 #:exists 'append)
                (send sell-amount set-value "1")
                (if (eq? 'donewrite (create-database-file "working.database" working-database))
                    (update-lists)
                    (void)))))))
    (define update-lists
      (lambda ()
        (let ((indexses (send working-database get-all-in-use-index)))
          (define update-list
            (lambda (index-list box-list)
              (let ((first-col-list (map number->string index-list))
                    (second-col-list (map
                                      (lambda (index)
                                        (send working-database get-item-name index))
                                      index-list))
                    (third-col-list (map
                                     (lambda (index)
                                       (number->string (/ (send working-database get-item-price index) 100)))
                                     index-list))
                    (forth-col-list (map
                                     (lambda (index)
                                       (number->string (send working-database get-item-stock index)))
                                     index-list)))
                
                (begin
                  (send box-list set first-col-list second-col-list third-col-list forth-col-list)
                  (data-set 0 index-list index-list box-list)))))
          (define data-set 
            (lambda (num lst index-list list-box)
              (if (< num (length index-list))
                  (begin
                    (send list-box set-data num (car lst))
                    (data-set (+ num 1) (cdr lst) index-list list-box))
                  (void))))
          (define update-buy-list
            (lambda (index-list box-list)
              (let ((first-col-list (map number->string index-list))
                    (second-col-list (map
                                      (lambda (index)
                                        (send working-database get-item-name index))
                                      index-list))
                    (third-col-list (map number->string (send to-buy-list-handler get-amounts))))
                (begin
                  (send box-list set first-col-list second-col-list third-col-list)
                  (data-set 0 index-list index-list box-list)))))
          
          (begin
            (update-buy-list (send to-buy-list-handler get-indexses) buying-list)
            (update-list (filter (lambda (index) (send working-database item-for-sale? index)) indexses) selling-list)
            (update-list indexses add-to-buy-list-box-list)
            (update-list indexses item-edit-list)
            (send saldo-message set-label (string-append "Saldo: " 
                                                         (number->string (/ (send working-database get-saldo) 100)) 
                                                         "kr  Värde på inventarier: " 
                                                         (number->string (/ (send working-database get-inventory-value) 100))
                                                         "kr"))))))
    (define buy-list-add-func
      (lambda (b e)
        (send add-to-buy-list-frame show #t)))
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
                                [choices (list "Försäljnig" "Inköp" "Administartion")]
                                [parent the-frame]
                                [callback tab-selector]))
    (define selling-panel (new group-box-panel%
                               [label ""] 
                               [parent selection-tabs]
                               [enabled #t]))
    (define buying-panel (new group-box-panel%
                              [label "Inköps lista"]
                              [parent selection-tabs]
                              [enabled #t]
                              [style (list  'deleted)]))
    (define admin-panel (new group-box-panel%
                             [label ""]
                             [parent selection-tabs]
                             [enabled #t]
                             [style (list  'deleted)]))
    (define item-edit-panel (new group-box-panel%
                                 [label "Ändra varor"]
                                 [parent admin-panel]
                                 [enabled #t]))
    (define item-edit-list (new list-box%
                                [label ""]
                                [choices (list)]
                                [parent item-edit-panel]
                                [style (list 'single 'column-headers)]
                                [columns (list "Index" "Namn" "Pris" "Antal i lager")]
                                [callback (lambda (b e)
                                            (let* ((lst-nr (send item-edit-list get-selections))
                                                   (index (if (not (eq? lst-nr null))
                                                              (send item-edit-list get-data (car lst-nr))
                                                              'noneselected)))
                                              (if (not (eq? index 'noneselected))
                                                  (begin
                                                    (send edit-for-sale-check-box set-value (send working-database item-for-sale? index))
                                                    (send edit-name-text set-value (send working-database get-item-name index))
                                                    (send edit-price-text set-value (number->string (/ (send working-database get-item-price index) 100))))
                                                  (void))))]                               
                                [vert-margin 20]
                                [horiz-margin 20]))
    (define edit-vert-panel
      (new vertical-panel%
           [parent item-edit-panel]))
    (define edit-horiz-panel-1
      (new horizontal-panel%
           [parent edit-vert-panel]))
    (define edit-horiz-panel-2
      (new horizontal-panel%
           [parent edit-vert-panel]))
    (define edit-horiz-panel-3
      (new horizontal-panel%
           [parent edit-vert-panel]))
    (define edit-horiz-panel-4
      (new horizontal-panel%
           [parent edit-vert-panel]))
    (define edit-vert-panel2 (new group-box-panel%
                               [label "Växel"] 
                               [parent edit-vert-panel]))                            
    (define edit-horiz-panel-5
      (new horizontal-panel%
           [parent edit-vert-panel2]))
    (define saldo-message (new message%
                               [label ""]
                               [parent edit-horiz-panel-5]
                               [min-width 300]))
    (define add-saldo-button (new button%
                                  [label "lägg till kr"]
                                  [parent edit-horiz-panel-5]
                                  [callback (lambda (b e)
                                              (let* ((str (get-text-modal))
                                                         (amount (if (number? (string->number str))
                                                                    (* (string->number str) 100)
                                                                    0)))
                                                    (begin
                                                      (display-to-file (string-append 
                                                                         "Date: " 
                                                                         (date->string (current-date) #t) 
                                                                         "  Action: Add saldo "
                                                                         "  User: " 
                                                                         user-name
                                                                         "  Amount: "
                                                                         str
                                                                         "kr Message: "
                                                                         (get-text-modal)
                                                                         "\r\n")
                                                                        "Log.txt"
                                                                        #:exists 'append)
                                                      (send working-database add-saldo amount)
                                                      (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                          (update-lists)
                                                          (void)))))]))
    (define edit-new-item-button (new button%
                                      [label "Ny vara"]
                                      [parent edit-horiz-panel-4]
                                      [callback (lambda (b e)
                                                  (let* ((str (get-text-modal))
                                                         (index (if (number? (string->number str))
                                                                    (string->number str)
                                                                    1)))
                                                    (begin
                                                      (display-to-file (string-append 
                                                                         "Date: " 
                                                                         (date->string (current-date) #t) 
                                                                         "  Action: Create new item "
                                                                         "  User: " 
                                                                         user-name
                                                                         "  Index:"
                                                                         (number->string index)
                                                                         "\r\n")
                                                                        "Log.txt"
                                                                        #:exists 'append)
                                                      (send working-database create-new-item index)
                                                      (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                          (update-lists)
                                                          (void)))))]))
    (define edit-remove-item-button (new button%
                                      [label "Ta bort vara"]
                                      [parent edit-horiz-panel-4]
                                      [callback (lambda (b e)
                                                  (let* ((lst-nr (send item-edit-list get-selections))
                                                      (index (if (not (eq? lst-nr null))
                                                                 (send item-edit-list get-data (car lst-nr))
                                                                 'noneselected)))
                                                    (if (and (number? (string->number (send edit-price-text get-value)))(confirm-modal)  (not (eq? index 'noneselected)))
                                                     (begin
                                                       
                                                       (display-to-file (string-append 
                                                                         "Date: " 
                                                                         (date->string (current-date) #t) 
                                                                         "  Action: Remove-item "
                                                                         "  User: " 
                                                                         user-name 
                                                                         "  Name:" 
                                                                         (send working-database get-item-name index) 
                                                                         "  Price:"
                                                                         (number->string (/ (send working-database get-item-price index) 100))
                                                                         "  Amount in stock:"
                                                                         (number->string (send working-database get-item-stock index))
                                                                         "  For sale? "
                                                                         (if (send working-database item-for-sale? index)
                                                                             "Yes"
                                                                             "No")
                                                                         "\r\n")
                                                                        "Log.txt"
                                                                        #:exists 'append)
                                                       (send working-database remove-item index) 
                                                      
                                                      (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                          (update-lists)
                                                          (void)))
                                                     (bell))))]))
    
    (define edit-name-text (new text-field%
                                [parent edit-horiz-panel-1]
                                [label "Namn"]
                                [init-value "Skriv nytt namn här"]))
    (define edit-price-text (new text-field%
                                 [parent edit-horiz-panel-2]
                                 [label "Pris Kr:"]
                                 [init-value "Skriv nytt pris här"]))
    (define edit-price-button (new button%
                                   [label "Ändra"]
                                   [parent edit-horiz-panel-2]
                                   [callback (lambda (b e)
                                               (let* ((lst-nr (send item-edit-list get-selections))
                                                      (index (if (not (eq? lst-nr null))
                                                                 (send item-edit-list get-data (car lst-nr))
                                                                 'noneselected)))
                                                 (if (and (number? (string->number (send edit-price-text get-value)))(confirm-modal)  (not (eq? index 'noneselected)))
                                                     (begin
                                                       
                                                       (display-to-file (string-append 
                                                                         "Date: " 
                                                                         (date->string (current-date) #t) 
                                                                         "  Action: Price-change "
                                                                         "  User: " 
                                                                         user-name 
                                                                         "  Name: " 
                                                                         (send working-database get-item-name index) 
                                                                         "  New-price: "
                                                                         (send edit-price-text get-value)
                                                                         "\r\n")
                                                                        "Log.txt"
                                                                        #:exists 'append)
                                                       (send working-database set-item-price! index (* 100 (string->number (send edit-price-text get-value))))
                                                       
                                                       (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                           (update-lists)
                                                           (void)))
                                                     (bell))))]))
    (define edit-name-button (new button%
                                  [label "Ändra"]
                                  [parent edit-horiz-panel-1]
                                  [callback (lambda (b e)
                                              (let* ((lst-nr (send item-edit-list get-selections))
                                                     (index (if (not (eq? lst-nr null))
                                                                (send item-edit-list get-data (car lst-nr))
                                                                'noneselected)))
                                                (if (and  (andmap (lambda (ele) (and (< (char->integer ele) 127) (< 31 (char->integer ele)))) (string->list (send edit-name-text get-value)))  (confirm-modal) (not (eq? index 'noneselected)))
                                                    (begin
                                                      
                                                      (display-to-file (string-append 
                                                                        "Date: " 
                                                                        (date->string (current-date) #t)
                                                                        "  Action: Name-change "
                                                                        "  User: " 
                                                                        user-name 
                                                                        "  Old-name: " 
                                                                        (send working-database get-item-name index) 
                                                                        "  New-name: "
                                                                        (send edit-name-text get-value)
                                                                        "\r\n")
                                                                       "Log.txt"
                                                                       #:exists 'append)
                                                      (send working-database set-item-name! index (send edit-name-text get-value))
                                                      
                                                      (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                          (update-lists)
                                                          (void)))
                                                    (bell))))]))
    
    (define edit-for-sale-check-box (new check-box%
                                         [label "Till försäljning"]
                                         [parent edit-horiz-panel-3]
                                         [callback (lambda (b e)
                                                     (begin
                                                       (let* ((lst-nr (send item-edit-list get-selections))
                                                              (index (if (not (eq? lst-nr null))
                                                                         (send item-edit-list get-data (car lst-nr))
                                                                         'noneselected)))
                                                         (if (not (eq? index 'noneselected))
                                                             (send working-database set-item-for-sale! index (send edit-for-sale-check-box get-value))
                                                             (void)))
                                                       (if (eq? 'donewrite (create-database-file "working.database" working-database))
                                                           (update-lists)
                                                           (void)) ))]))
    
    
    
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
    (define sell-button (new button%
                             [label "Kontant"]
                             [parent selling-panel]
                             [callback sell-func]))
    (define buying-list (new list-box%
                             [label ""]
                             [choices (list)]
                             [parent buying-panel]
                             [style (list 'single 'column-headers)]
                             [columns (list "Index" "Namn" "Antal")]
                             [vert-margin 20]
                             [horiz-margin 20]))
    (define buying-lower (new group-box-panel%
                              [label ""]
                              [parent buying-panel]
                              [enabled #t]))
    (define buying-lower-horiz-1 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buying-lower-horiz-2 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buying-lower-horiz-3 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buying-lower-horiz-4 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buying-lower-horiz-5 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buying-lower-horiz-6 (new horizontal-panel%
                                      [parent buying-lower]
                                      [enabled #t]))
    (define buy-total-price-kr (new text-field%
                                    [parent buying-lower-horiz-1]
                                    [vert-margin 5]
                                    [horiz-margin 20]
                                    [min-width 50]
                                    [label "Total pris, Kr:"]
                                    [init-value "0"]))
    (define buy-total-price-ore (new text-field%
                                     [parent buying-lower-horiz-1]
                                     [vert-margin 5]
                                     [horiz-margin 20]
                                     [min-width 50]
                                     [label "Öre:"]
                                     [init-value "0"]))
    (define add-buy-list-button (new button%
                                     [label "Lägg till vara"]
                                     [parent buying-lower-horiz-1]
                                     [callback buy-list-add-func]
                                     ))
    (define remove-buy-list-button (new button%
                                        [label "Ta bort vara"]
                                        [parent buying-lower-horiz-1]
                                        [callback buy-list-remove-func]
                                        ))
    (define confirm-buy-list-button (new button%
                                         [label "Bekräfta inköp"]
                                         [parent buying-lower-horiz-2]
                                         [callback confirm-buy-func]))
    (define add-to-buy-list-frame (new frame%
                                       [label "Lägg till i inköps lista"]
                                       [height 400]
                                       [width 400]))
    (define add-to-buy-list-panel (new group-box-panel%
                                       [label ""]
                                       [parent add-to-buy-list-frame]
                                       [enabled #t]
                                       ))
    (define add-to-buy-list-box-list (new list-box%
                                          [label ""]
                                          [choices (list)]
                                          [parent add-to-buy-list-panel]
                                          [style (list 'single 'column-headers)]
                                          [columns (list "Index" "Namn" "Pris" "Antal i lager")]
                                          [vert-margin 20]
                                          [horiz-margin 20]))
    (define buy-amount (new text-field%
                            [parent add-to-buy-list-panel]
                            [vert-margin 5]
                            [horiz-margin 20]
                            [min-width 50]
                            [label "Antal"]
                            [init-value ""]))
    (define buy-button (new button%
                            [label "Lägg till"]
                            [parent add-to-buy-list-panel]
                            [callback add-to-buy-list-func]))    
    (begin
     (send m-file-open enable #f)
     (send m-file-New enable #f)
      (create-database-file (string-append "Backup_" (date->string (current-date)) ".database") working-database)
      (update-lists)
      (send the-frame show #t))))