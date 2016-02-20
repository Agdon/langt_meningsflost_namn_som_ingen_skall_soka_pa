#lang racket
(provide database-class)
;;Database handle
(define database-class;;The Database handles Items in the list items-list
  (class object%
    (super-new)
    (init-field
     [items-list '()] ;assoc lista
     [saldo-vaxel 0]
     [saldo-valv 0])
    
    (define/public create-new-item
      (lambda (index)
        (if (assoc index items-list)
            (void) ;replace with proper error message!
            (set! items-list (cons (cons index (new item-class)) items-list)))))
    (define/public set-item-name!
      (lambda (index string)
        (send (cdr (assoc index items-list)) set-name! string)))
    (define/public get-item-name
      (lambda (index)
        (send (cdr (assoc index items-list)) get-name)))
    (define/public set-item-price!
      (lambda (index inte)
        (send (cdr (assoc index items-list)) set-price! inte)))
    (define/public add-item-stock
      (lambda (index inte)
        (send (cdr (assoc index items-list)) add-stock inte)))
    (define/public add-item-stock-cellar
      (lambda (index inte)
        (send (cdr (assoc index items-list)) add-stock-cellar inte)))
    (define/public move-item-stock
      (lambda (index inte)
        (send (cdr (assoc index items-list)) move-stock-cellar->stock inte)));;Postive value moves to stupan
    (define/public set-item-for-sale!
      (lambda (index bol)
        (send (cdr (assoc index items-list)) set-for-sale bol)))
    (define/public add-saldo
      (lambda (inte)
        (set! saldo-vaxel (+ saldo-vaxel inte))))
    (define/public add-saldo-valv
      (lambda (inte)
        (set! saldo-valv (+ saldo-valv inte))))
    (define/public get-item-stock
      (lambda (index)
        (send (cdr (assoc index items-list)) get-stock)))
    (define/public get-item-stock-cellar
      (lambda (index)
        (send (cdr (assoc index items-list)) get-stock-cellar)))
    (define/public get-item-price
      (lambda (index)
        (send (cdr (assoc index items-list)) get-price)))
    (define/public item-for-sale?
      (lambda (index)
        (send (cdr (assoc index items-list)) for-sale?)))
    (define/public get-saldo
      (lambda () saldo-vaxel))
    (define/public get-saldo-valv
      (lambda () saldo-valv))
    (define/public get-inventory-value
      (lambda ()
        (apply + (map (lambda (element) (* (+ (send (cdr element) get-stock-cellar) 
                                              (send (cdr element) get-stock)) (send (cdr element) get-price))) items-list))))
    (define/public get-item-count
      (lambda () (length items-list)))
    (define/public remove-item
      (lambda (index)
        (set! items-list (filter (lambda (element) (not (eq? (car element) index))) items-list))))
    (define/public sell
      (lambda (index num)
        (begin
          (send (cdr (assoc index items-list)) add-stock (- num))
          (add-saldo  (* (send (cdr (assoc index items-list)) get-price) num)))))
    (define/public get-all-in-use-index
      (lambda ()
        (if (null? items-list)
            '()
            (reverse (map car items-list)))))))

(define item-class
  (class object%
    (super-new)
    (init-field
     [name "unknown"]
     [stock 0]
     [price 0]
     [stock-cellar 0]
     [for-sale #f])
    (define/public get-name
      (lambda () name))
    (define/public get-stock
      (lambda () stock))
    (define/public get-stock-cellar
      (lambda () stock-cellar))
    (define/public get-price
      (lambda () price))
    (define/public for-sale?
      (lambda () for-sale))
    (define/public set-name!
      (lambda (name-string)
        (set! name name-string)))
    (define/public add-stock
      (lambda (amount)
        (set! stock (+ stock amount))))
    (define/public add-stock-cellar
      (lambda (amount)
        (set! stock (+ stock-cellar amount))))
    (define/public move-stock-cellar->stock;;Note positive if moving to stupan from the cellar
      (lambda (amount)
        (if (not (> amount stock-cellar))
            (begin
              (set! stock (+ stock amount))
              (set! stock-cellar (- stock-cellar amount)))
            (void))))
    (define/public set-price!
      (lambda (amount)
        (set! price amount)))
    (define/public set-for-sale
      (lambda (bol)
        (set! for-sale bol)))))   
