
#lang racket

(require graph)
(require racklog) 

; define the empty social relation graph and the product relations for purchases and product categories 
(define relations (weighted-graph/undirected '() ))
(define %bought (%rel ()))
(define %products (%rel ()))


;************************************************************************************************************************************
; private functions 


; eliminate duplicates from a list 
(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                    (cdr e))))))


; Return the list of persons no more than maxhops hops away. Do a breadth first search to count hops from a source node. 
(define (hops-away graph start maxhops)
  (begin
    (define friends '())
    (let-values ( [ (first second) (bfs graph start) ] )
      (hash-for-each first (lambda (k v) (when (and (not (equal? v 0)) (<= v maxhops)) (set! friends (cons k friends))))))
    friends)
  )


; sorting function for the weighted social network paths
(define (weightSort x y)
   (< (cadr x) (cadr y)))


; Get the relationship weights for all persons N hops from a person 
(define (getWeightsFromHopSet person hops)
  (begin
    (define set (hops-away relations person hops))
    (define weights '())
    (let-values ( [ (first second) (dijkstra relations person) ] )
      (hash-for-each first (lambda (k v) (when (and (not (equal? v 0)) (member k set)) (set! weights (cons (list k v) weights))))))    
    (sort weights weightSort)   ; return the list of weighted paths in ascending sorted order 
  ))


; the categories purchased by a person
(define (%boughtCategories person)
  (let ((result (%which (categories)
                        (%let (p category)
                              (%bag-of category (%and (%products p category) (%bought person p)) categories)))))
    (if (not (eq? result #f)) (dedupe (cdar result)) '())   ; return an empty list if no categories bought 
    ))


; the products bought by a friend but not by the person, and in a category bought by both 
(define (%notBoughtProductsInSimilarCategories person friend)
  (let ((result (%which (products)
                      (%let (product category)
                            (%bag-of product (%and (%bought friend product)                         ; friend bought a product
                                                   (%not (%bought person product))                  ; that the person didn't 
                                                   (%products product category)                     ; and the product is in a category
                                                   (%member category (%boughtCategories person))    ; bought by the person 
                                                   )
                                     products)))))
    (if (not (eq? result #f)) (dedupe (cdar result)) #f)    ; if not empty, unpack the bag-of into a regular list, else return #f
    ))


(define (consultFriends person friends)
  (cond ((null? friends) #f)
        ((%notBoughtProductsInSimilarCategories person (caar friends))
         (list (caar friends) (car (%notBoughtProductsInSimilarCategories person (caar friends)))))
        (#t (consultFriends person (cdr friends)))))


;************************************************************************************************************************************
; public functions 

; add a relationship to the social graph of a certain weight 
(define (addSocial! person1 person2 weight)
  (add-edge! relations person1 person2 weight))


; declare that a person bought a product 
(define (addPurchase! person product)
  (%assert! %bought () [(person product)]))


; declare that the product is a member of a category 
(define (addProduct! product category)
  (%assert! %products () [(product category)]))


; recommend a product for a person via the strongest friend no more than n hops away 
(define (recommendProduct person hops)
  (begin
    ; get the friends of a person within a certain number of social hops, sorted by strength of connection 
    (define weightedFriends (getWeightsFromHopSet person hops))
    ; for each weighted friend in order, see if the friend bought a product from one of the categories of the person,
    ; but not one the person bought
    (display person) (display ": ")
    (let ((result (consultFriends person weightedFriends)))
      (if (not (eq? result #f))
          (begin (display "DEBUG: ") (display (car result)) (display " recommends ") (display (cadr result)) (display "\n")
                                       (cadr result)) #f)
      )))


;************************************************************************************************************************************
; test 

; add some weighted edges

(addSocial! 'Andy 'Kathy 1)
(addSocial! 'Kathy 'Sal 1)
(addSocial! 'Andy 'Sal 1)
(addSocial! 'Andy 'Dom 1)
(addSocial! 'Andy 'Sal 1)
(addSocial! 'Andy 'Tony 5)
(addSocial! 'Sal 'Hannah 3)
(addSocial! 'Dom 'Hannah 5)
(addSocial! 'Sal 'Tony 10)
(addSocial! 'Dom 'Tony 10)
(addSocial! 'Tony 'Hannah 5)
(addSocial! 'Hannah 'Susan 5)
(addSocial! 'Susan 'Wendy 5)
(addSocial! 'Wendy 'Betty 5)
(addSocial! 'Betty 'Kathy 5)
(addSocial! 'Betty 'Lou 5)
(addSocial! 'Loner 'Loner 1)
(addSocial! 'Cheapo 'Susan 3)
(addSocial! 'Shy 'Cheapo 5)

(addPurchase! 'Andy 'MacBook)
(addPurchase! 'Andy 'Android)
(addPurchase! 'Andy 'Sneakers)
(addPurchase! 'Kathy 'Kindle)
(addPurchase! 'Kathy 'MacBook)
(addPurchase! 'Kathy 'Android)
(addPurchase! 'Kathy 'iPhone)
(addPurchase! 'Kathy 'Yarn)
(addPurchase! 'Kathy 'Glue) 
(addPurchase! 'Sal 'Android)
(addPurchase! 'Sal 'Sneakers)
(addPurchase! 'Dom 'iPhone)
(addPurchase! 'Dom 'Paint)
(addPurchase! 'Tony 'Football)
(addPurchase! 'Tony 'iPhone)
(addPurchase! 'Hannah 'iPhone)
(addPurchase! 'Hannah 'Paint)
(addPurchase! 'Susan 'Yarn)
(addPurchase! 'Susan 'Paint) 
(addPurchase! 'Wendy 'YogaMat)
(addPurchase! 'Betty 'YogaMat)
(addPurchase! 'Betty 'iPhone)
(addPurchase! 'Shy 'iPhone)

(addProduct! 'MacBook 'Electronics)
(addProduct! 'Kindle 'Electronics)
(addProduct! 'Android 'Electronics)
(addProduct! 'iPhone 'Electronics)
(addProduct! 'Yarn 'Crafts)
(addProduct! 'Paint 'Crafts)
(addProduct! 'Glue 'Crafts)
(addProduct! 'Football 'Sporting)
(addProduct! 'YogaMat 'Sporting)
(addProduct! 'Sneakers 'Sporting)
(addProduct! 'Blackberry 'Electronics)


(recommendProduct 'Kathy 2)    
(recommendProduct 'Andy 1)
(recommendProduct 'Loner 2)    ; Loner knows noone in the social graph but himself, bought nothing 
(recommendProduct 'Cheapo 1)   ; Cheapo has friends but bought nothing - so no categories to recommend
(recommendProduct 'Shy 1)      ; Shy knows Cheapo, but Cheapo didn't buy anything, so has nothing to recommend 
(recommendProduct 'Susan 1)    ; Susan knows Cheapo and Wendy, but Wendy didn't buy anything in the same category as Susan
(recommendProduct 'Susan 2)    ; still nothing to recoomend...
(recommendProduct 'Susan 3)    ; Susan knows Wendy, and Wendy knows Betty, and Betty knows Kathy who bought glue 




