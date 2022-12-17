;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |15 game solver|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require racket/list)
(require 2htdp/universe)


;; Data definitions:

(@htdd Board)
;; Board is (listof Natural|false) that is (sqr N) elements long
;; interp.
;;  Visually a board is a NxN array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. A natural represents a square block
;;  with a number in [1, (sqr N)-1], and false represents the blank square
;;  to which other squares can be slided to.
;; CONSTRAINT: Exactly 1 square is empty (only one element in Board is false)
;;             A board of size N must have (sqr N) elements
;;             In other words, N = (sqrt (length Board))
;;             All numbers from [1, (sqr N)-1] must be used exactly once

(@htdd Pos)
(define-struct pos (x y))
;; Pos is one of:
;; - Natural
;; - (make-pos Integer Integer)
;; interp.
;;  The position of a square on the board in [0, (sqr N)-1] represented in
;;  two ways:
;;  - a position represented by a single number as the index in the list
;;  - an x y position on a board of size N




;; Constants:

(define B false)   ; B stands for blank square

(define BD2x2s
  (list 1 2
        3 B))   ; solved 2x2 board

(define BD2x2us
  (list 1 3
        2 B))   ; unsolvable 2x2 board (can't go to BD2x2s)

(define BD2x2sr
  (list 1 2
        B 3))   ; easily solvable 2x2 board

(define BD2x2s2
  (list B 2
        1 3))

(define BD4x4s
  (list  1  2  3  4
         5  6  7  8
         9 10 11 12
         13 14 15 B))  ; solved 4x4 board (15 game)

(define BD4x4sr
  (list 1 2 3 4
        5 6 7 8
        9 10 11 12
        13 14 B 15))

(define BD4x4s2
  (list 1 2 3 4
        5 6 7 8
        9 10 11 B
        13 14 15 12))

(define BD4x4us
  (list  1  2  3  4
         5  6  7  8
         9 10 11 12
         13 15 14 B))  ; unsolvable 4x4 board

(define BD4x4r
  (list 10 12 11 4
        1  5  6  9
        B 13 15  7
        3  2  8 14))  ; idk if this is solvable

(define BD4x4r2
  (list 10 12 11 4
        1  5  6  9
        B 13 15  7
        3  2 14  8)) 

(define BD4x4r3
  (list  6 10 11  9
         1  B  7  8
         13 14 15 4
         12  3  2 5))

(define BD3x3s
  (list 1 2 3
        4 5 6
        7 8 B))

(define BD3x3us
  (list 1 2 3
        4 5 6
        8 7 B))

(define BD3x3sr
  (list 1 2 3
        4 5 6
        7 B 8))



(@htdf solve)
(@signature Board Board -> (listof Board))
;; produce the list of boards that go from beg to end; false if impossible
(check-expect (solve BD2x2us BD2x2s) false)
(check-expect (solve  BD2x2s BD2x2s) (list BD2x2s))
(check-expect (solve BD2x2sr BD2x2s) (list BD2x2sr BD2x2s))
(check-expect (solve BD2x2s2 BD2x2s) (list BD2x2s2 BD2x2sr BD2x2s))
;(check-expect (solve BD4x4us BD4x4s) false)
;(check-expect (solve  BD4x4r BD4x4s) false) ;not sure what this should give
;(check-expect (solve BD4x4r2 BD4x4s) false)
(check-expect (solve BD2x2us BD4x4s) false)
;(check-expect (solve BD4x4sr BD4x4s) (list BD4x4sr BD4x4s))
;(check-expect (solve BD4x4s2 BD4x4s) (list BD4x4s2 BD4x4s))
;(check-expect (solve BD3x3sr BD3x3s) (list BD3x3sr BD3x3s))
;(check-expect (solve BD3x3us BD3x3s) false)


;; It terminates. It is finite. However, the search space gets very large,
;; so the program never actually finishes. But theoretically, it would.


(@template-origin genrec arb-tree encapsulated try-catch accumulator)

(define (solve beg end)
  (local [(define (solve-bd bd path)
            (cond [(member bd path) false]
                  [(equal? bd end) (list end)]
                  [else
                   (local [(define try (solve-lobd (next-boards bd)
                                                   (cons bd path)))]
                     (if (not (false? try))
                         (cons bd try)
                         false))]))

          (define (solve-lobd lobd path)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try1 (solve-bd (first lobd) path))
                           (define try2 (solve-lobd (rest lobd) path))]
                     (cond [(false? try1) try2]
                           [(false? try2) try1]
                           [else
                            (if (< (length try1) (length try2))
                                try1
                                try2)]))]))]
    
    (if (= (length beg) (length end))
        (solve-bd beg empty)
        false)))


(@htdf next-boards)
(@signature Board -> (listof Board))
;; generate valid next boards from given board
(check-expect (next-boards BD2x2s)
              (list (list 1 B
                          3 2)
                    (list 1 2
                          B 3)))
(check-expect (next-boards BD4x4s)
              (list (list  1  2  3  4
                           5  6  7  8
                           9 10 11 B
                           13 14 15 12)
                    (list  1  2  3  4
                           5  6  7  8
                           9 10 11 12
                           13 14 B 15)))
(check-expect (next-boards BD4x4r3)
              (list (list  6 B 11  9
                           1 10  7  8
                           13 14 15 4
                           12  3  2 5)
                    (list  6 10 11  9
                           1 14 7  8
                           13 B 15 4
                           12  3  2 5)
                    (list  6 10 11  9
                           B  1  7  8
                           13 14 15 4
                           12  3  2 5)
                    (list  6 10 11  9
                           1  7  B  8
                           13 14 15 4
                           12  3  2 5)))


(@template-origin use-abstract-fn fn-composition)

(define (next-boards bd)
  (local [(define N (sqrt (length bd)))

          (define BLANK-POS (index-of bd B))
          
          (define (next-ps p)
            (local [(define x (pos-x p))
                    (define y (pos-y p))]
              (filter (lambda (p1)
                        (and (<= 0 (pos-x p1) (sub1 N))  ;legal x
                             (<= 0 (pos-y p1) (sub1 N))));legal y
                      (list (make-pos x (sub1 y))        ;up
                            (make-pos x (add1 y))        ;down
                            (make-pos (sub1 x) y)        ;left
                            (make-pos (add1 x) y)))))    ;right

          (define (blank-replace n)
            (local [(define num (list-ref bd n))]
              (list-set (list-set bd n B) BLANK-POS num)))

          (define (pos-col p) (remainder p N))
          (define (pos-row p) (quotient p N))
          (define (pos->r-c p)
            (make-pos (pos-col p) (pos-row p)))
          (define (r-c->pos p)
            (+ (* (pos-y p) N) (pos-x p)))]
    
    (map blank-replace (map r-c->pos (next-ps (pos->r-c BLANK-POS))))))
          
























