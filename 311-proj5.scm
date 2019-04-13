
#lang racket

(require graph)
(require racklog) 

; define the empty social relation graph and the product relations for purchases and product categories 
(define relations (weighted-graph/undirected '() ))
(define %bought (%rel ()))
(define %products (%rel ()))


;************************************************************************************************************************************
; private functions


; Return the list of persons no more than maxhops hops away. Do a breadth first search to count hops from a source node. 
(define (hops-away graph start maxhops)
  (begin
    (define friends '())
    (let-values ( [ (first second) (bfs graph start) ] )
      (hash-for-each first (lambda (k v) (when (and (not (equal? v 0)) (<= v maxhops)) (set! friends (cons k friends))))))
    friends)
  )


; eliminate duplicates from a list 
(define (dedupe e)
  (if (null? e) '()
      (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                    (cdr e))))))


; Get the weights from a person to every other person, return as a list of lists ( (person1 weight1) (person2 weight2) ...)
(define (getWeightsFrom person)
  (begin
    (define weights '())
    ; Run the Dijkstra algorithm to find the weight of the path from the source person to each other person.
    (let-values ( [ (first second) (dijkstra relations person) ] )
      (hash-for-each first (lambda (k v) (when (not (equal? v 0)) (set! weights (cons (list k v) weights))))))
    weights)
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
    (sort weights weightSort)  ; return the list of weighted paths in ascending sorted order 
  ))


; return a list of the products bought by a person 
(define (%boughtProducts person)
  (dedupe (cdar 
           (%which (products)
                   (%let (product)
                         (%bag-of product (%bought person product) products))))))


; the products bought by the friend but not the person 
(define (%notBoughtProducts person friend)
  (%which (products)
          (%let (product)
                (%bag-of product (%and (%bought friend product)
                                       (%not (%bought person product)))
                         products))))


; the categories purchased by a person
(define (%boughtCategories person)
  (dedupe (cdar 
           (%which (categories)
                   (%let (p category)
                         (%bag-of category (%and (%products p category) (%bought person p)) categories))))))


; products in a category 
(define (%productsInCategory category)
  (%which (products)
          (%let (product)
                (%bag-of product (%products product category) products))))



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

 
(define (recommendProduct person hops)
  (begin
    ; get the friends of a person within a certain number of social hops, sorted by strength of connection 
    (define weightedFriends (getWeightsFromHopSet person hops))
    ; get the products this person bought 
    (define personBoughtProducts (%boughtProducts person))
    ; get the categories this person bought 
    (define personBoughtCategories (%boughtCategories person))
    ; for each weighted friend in order, see if the friend bought a product from one of the categories of the person,
    ; but not one the person bought
    
    
    
    ; debug 
    (display weightedFriends)
    (display "\n")
    (display personBoughtProducts)
    (display "\n")
    (display personBoughtCategories)
    (display "\n")

    (display (%notBoughtProducts person 'Andy))
    (display "\n")
    )
  )



;************************************************************************************************************************************
; debug 


;(begin (display "--> ") (%which (category) (%bought-categories 'Andy)))
  
;(begin (display "--> ") (%which () (%bought-category 'Andy 'Electronics)))
;(begin (display "--> ") (%which () (%bought-category 'Andy 'Crafts)))

;(%products-in-category 'Electronics)

;(hops-away relations 'Andy 1)
;(hops-away relations 'Kathy 2)

; (getWeightsFrom 'Andy)

; (getWeightsFromHopSet 'Andy 2)



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

(addPurchase! 'Andy 'MacBook)
(addPurchase! 'Kathy 'Kindle)
(addPurchase! 'Sal 'Android)
(addPurchase! 'Dom 'iPhone)
(addPurchase! 'Kathy 'Yarn)
(addPurchase! 'Dom 'Paint)
(addPurchase! 'Sal 'Sneakers)
(addPurchase! 'Tony 'Football)
(addPurchase! 'Tony 'iPhone)
(addPurchase! 'Hannah 'iPhone)
(addPurchase! 'Hannah 'Paint)
(addPurchase! 'Susan 'Yarn)
(addPurchase! 'Wendy 'YogaMat)
(addPurchase! 'Betty 'YogaMat)
(addPurchase! 'Betty 'iPhone)
(addPurchase! 'Kathy 'MacBook)
(addPurchase! 'Andy 'Android)

(addProduct! 'MacBook 'Electronics)
(addProduct! 'Kindle 'Electronics)
(addProduct! 'Android 'Electronics)
(addProduct! 'iPhone 'Electronics)
(addProduct! 'Yarn 'Crafts)
(addProduct! 'Paint 'Crafts)
(addProduct! 'Football 'Sporting)
(addProduct! 'YogaMat 'Sporting)
(addProduct! 'Sneakers 'Sporting)
(addProduct! 'Blackberry 'Electronics)


(recommendProduct 'Kathy 2)





