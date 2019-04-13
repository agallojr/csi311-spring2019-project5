
#lang racket

(require graph)
(require racklog) 

; define a weighted graph
(define relations (weighted-graph/undirected '() ))
; add some weighted edges 
(add-edge! relations 'Andy 'Kathy 1)
(add-edge! relations 'Kathy 'Sal 1)
(add-edge! relations 'Andy 'Sal 1)
(add-edge! relations 'Andy 'Dom 1)
(add-edge! relations 'Andy 'Sal 1)
(add-edge! relations 'Andy 'Tony 5)
(add-edge! relations 'Sal 'Hannah 3)
(add-edge! relations 'Dom 'Hannah 5)
(add-edge! relations 'Sal 'Tony 10)
(add-edge! relations 'Dom 'Tony 10)
(add-edge! relations 'Tony 'Hannah 5)
(add-edge! relations 'Hannah 'Susan 5)
(add-edge! relations 'Susan 'Wendy 5)
(add-edge! relations 'Wendy 'Betty 5)
(add-edge! relations 'Betty 'Kathy 5)

(define %bought
  (%rel ()
        [('Andy 'MacBook)]
        [('Kathy 'Kindle)]
        [('Sal 'Android)]
        [('Dom 'iPhone)]
        [('Kathy 'Yarn)]
        [('Dom 'Paint)]
        [('Sal 'Sneakers)]
        [('Tony 'Football)]
        [('Tony 'iPhone)]
        [('Hannah 'iPhone)]
        [('Hannah 'Paint)]
        [('Susan 'Yarn)]
        [('Wendy 'YogaMat)]
        [('Betty 'YogaMat)]
        ))

(define %products
  (%rel ()
        [('MacBook 'Electronics)]
        [('Kindle 'Electronics)]
        [('Android 'Electronics)]
        [('iPhone 'Electronics)]
        [('Yarn 'Crafts)]
        [('Paint 'Crafts)]
        [('Football 'Sporting)]
        [('YogaMat 'Sporting)]
        [('Sneakers 'Sporting)]        
        ))  

; Return the list of persons no more than n hops away. Do a breadth first search to count hops from a source node. 
(define (hops-away graph start maxhops)
  (begin
    (define friends '())
    (let-values ( [ (first second) (bfs graph start) ] )
      (hash-for-each first (lambda (k v) (when (and (not (equal? v 0)) (<= v maxhops)) (set! friends (cons k friends))))))
    friends)
  )

;(hops-away relations 'Andy 1)
;(hops-away relations 'Kathy 2)


; Get the weights from a person to every other person, return as a list of lists ( (person1 weight1) (person2 weight2) ...)
(define (getWeightsFrom person)
  (begin
    (define weights '())
    ; Run the Dijkstra algorithm to find the weight of the path from the source person to each other person.
    (let-values ( [ (first second) (dijkstra relations person) ] )
      (hash-for-each first (lambda (k v) (when (not (equal? v 0)) (set! weights (cons (list k v) weights))))))
    weights)
  )

; (getWeightsFrom 'Andy)

; sorting function for the weighted paths
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

; (getWeightsFromHopSet 'Andy 2)


(define (addSocial! person1 person2 weight)
  (add-edge! relations person1 person2 weight))

(addSocial! 'Betty 'Lou 5)

(define (addPurchase! person product)
  (%assert! %bought () [(person product)]))

(addPurchase! 'Betty 'iPhone)

(define (addProduct! product category)
  (%assert! %products () [(product category)]))

(addProduct! 'Blackberry 'Electronics)


(define (%bought-products person)
  (%which (products)
          (%let (product)
                (%bag-of product (%bought person product) products))))

(define (%bought-categories person)
  (%which (categories)
          (%let (p category)
                (%bag-of category (%and (%products p category) (%bought person p)) categories))))

(define (%products-in-category category)
  (%which (products)
          (%let (product)
                (%bag-of product (%products product category) products))))

;(%products-in-category 'Electronics)


(define (recommendProduct person hops)
  (begin
    (define weightedFriends (getWeightsFromHopSet person hops))
    (define boughtProducts (cdar (%bought-products person)))
    (define boughtCategories (cdar (%bought-categories person)))
    (display weightedFriends)
    (display "\n")
    (display boughtProducts)
    (display "\n")
    (display boughtCategories)
    (display "\n")
    
    )
  )


;(begin (display "--> ") (%which (category) (%bought-categories 'Andy)))
  
;(begin (display "--> ") (%which () (%bought-category 'Andy 'Electronics)))
;(begin (display "--> ") (%which () (%bought-category 'Andy 'Crafts)))

(recommendProduct 'Andy 2)





