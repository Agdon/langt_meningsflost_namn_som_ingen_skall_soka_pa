#lang racket
(require racket/date)
(require racket/file)
(require racket/gui/base)
(date-display-format 'iso-8601)
(define name-amont-list-handler
  (class object%
    (super-new)
    (init-field
     [index-amount-list '()])
    (define/public add
      (lambda (index amount)
        (if (assoc index index-amount-list)
            (let ((element (assoc index index-amount-list)))
              (set! index-amount-list (cons (cons index (+ amount (cdr element))) (remv element index-amount-list))))
            (set! index-amount-list (cons (cons index amount) index-amount-list)))))
    (define/public remove
      (lambda (index)
        (set! index-amount-list (filter (lambda (element) (not (eq? (car element) index))) index-amount-list))))
    (define/public clear
      (lambda ()
        (set! index-amount-list '())))
    (define/public get-indexses
      (lambda ()
        (map car index-amount-list)))
    (define/public get-amont-index
      (lambda (index)
        (cdr (assoc index index-amount-list))))
    (define/public get-amounts
      (lambda () (map cdr index-amount-list)))
    (define/public get-full-list
      (lambda () index-amount-list))))

(define statistics-generator
  (lambda ()
    (define statistic-list (eval (read (open-input-string (string-append (file->string "statistics.txt" #:mode 'text) " )")))))
    (define generate-normal-statistics
      (lambda (start-sec end-sec)
        (let ((working-list (filter (lambda (ele) (and (> (car ele) start-sec) (< (car ele) end-sec))) statistic-list)))
          (define compacted-list-handle (new name-amont-list-handler))
          (for-each (lambda (ele)
                      (send compacted-list-handle add (car (cdr ele)) (car (cdr (cdr ele)))))
                      working-list)
          (define revenue (apply + (map (lambda (ele) (* (car (cdr (cdr ele))) (car (cdr (cdr (cdr (cdr ele))))))) working-list)))
          (define total-cost (apply + (map (lambda (ele) (* (car (cdr (cdr ele))) (car (cdr (cdr (cdr ele)))))) working-list)))
          (define total-profit (- revenue total-cost))
          (define file-name (string-append "Statistics_generated_" (string-replace (date->string (current-date) #t) ":" "_") ".csv"))
          (begin
            (display-to-file (string-append ";Date_from;" (date->string (seconds->date start-sec)) ";Date_until;" (date->string (seconds->date end-sec)) "; \n\r") file-name #:exists 'replace)
            (display-to-file ";Namn;Antal;total inkospkostnad;total forsaljningintakt;total vinst pa vara; \n\r" file-name #:exists 'append)
            (for-each (lambda (ele)
                        (let* ((ele-list (filter (lambda (element) (equal? (car (cdr element)) (car ele))) working-list))
                               (revenue-ele (apply + (map (lambda (ele) (* (car (cdr (cdr ele))) (car (cdr (cdr (cdr (cdr ele))))))) ele-list)))
                               (total-cost-ele (apply + (map (lambda (ele) (* (car (cdr (cdr ele))) (car (cdr (cdr (cdr ele)))))) ele-list)))
                               (total-profit-ele (- revenue-ele total-cost-ele)))
                          (display-to-file (string-append
                                            ";"
                                            (car ele)
                                            ";"
                                            (number->string (cdr ele))
                                            ";"
                                            (number->string (real->double-flonum (/ total-cost-ele 100)))
                                            ";"
                                            (number->string (real->double-flonum (/  revenue-ele 100)))
                                            ";"
                                            (number->string (real->double-flonum (/  total-profit-ele 100)))
                                            ";\n\r")
                                           file-name #:exists 'append)))
                      (send compacted-list-handle get-full-list))
            (display-to-file (string-append
                              ";Total inkopskostnad:;"
                              (number->string (real->double-flonum (/  total-cost 100)))
                              ";Omsattning:;"
                              (number->string (real->double-flonum (/  revenue 100)))
                              ";Vinst;"
                              (number->string (real->double-flonum (/  total-profit 100)))
                              ";")
                             file-name #:exists 'append)))))
    
    (define statistics-frame (new frame%
                                  [label "Ättestupan Statestik"]
                                  [width 400]
                                  [height 50]))
    (define vert-statistics-panel (new group-box-panel%
                                       [label ""]
                                       [parent statistics-frame]))
    (define horiz-statistics-panel (new horizontal-panel%
                                        [parent vert-statistics-panel]))
    (define start-date-YY-t (new text-field%
                                 [label "Start datum 00:00 "]
                                 [parent horiz-statistics-panel]
                                 [init-value "YYYY"]
                                 [style (list 'single)]))
    (define start-date-MM-t (new text-field%
                                 [label ""]
                                 [parent horiz-statistics-panel]
                                 [init-value "MM"]
                                 [style (list 'single)]))
    (define start-date-DD-t (new text-field%
                                 [label ""]
                                 [parent horiz-statistics-panel]
                                 [init-value "DD"]
                                 [style (list 'single)]))
    (define end-date-YY-t (new text-field%
                               [label "Slut datum 24:00 "]
                               [parent horiz-statistics-panel]
                               [init-value "YYYY"]
                               [style (list 'single)]))
    (define end-date-MM-t (new text-field%
                               [label ""]
                               [parent horiz-statistics-panel]
                               [init-value "MM"]
                               [style (list 'single)]))
    (define end-date-DD-t (new text-field%
                               [label ""]
                               [parent horiz-statistics-panel]
                               [init-value "DD"]
                               [style (list 'single)]))
    (define generate-human-button (new button%
                                       [label "skapa statestik för människor"]
                                       [parent vert-statistics-panel]
                                       [callback (lambda (b e)
                                                   (let ((s-Y (string->number (send start-date-YY-t get-value)))
                                                         (s-M (string->number (send start-date-MM-t get-value)))
                                                         (s-D (string->number (send start-date-DD-t get-value)))
                                                         (e-Y (string->number (send end-date-YY-t get-value)))
                                                         (e-M (string->number (send end-date-MM-t get-value)))
                                                         (e-D (string->number (send end-date-DD-t get-value))))
                                                     (generate-normal-statistics (find-seconds 0 0 0 s-D s-M s-Y) (find-seconds 59 59 23 e-D e-M e-Y))))]))
    (begin
      (send statistics-frame show #t))))
                                                                                    
                                       
    