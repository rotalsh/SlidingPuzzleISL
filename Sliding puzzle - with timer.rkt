;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Sliding puzzle - with timer|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)


;; Sliding puzzle program
;(@htdw GameState)
;; =================
;; Constants:

(define B false)   ; B stands for blank square
(define START "start")

(define WIDTH 700)
(define HEIGHT WIDTH)
(define MTS (empty-scene WIDTH HEIGHT))





;; =================
;; Data definitions:

;(@htdd Val)
;; Val is one of:
;; - Natural
;; - false
;; CONSTRAINT: if natural, must be in [1, 15]

(define V0 B) ; blank/false
(define V1 1)
(define V9 9)

(define (fn-for-val v)
  (cond [(number? v) (... v)]
        [else
         (...)]))


;(@htdd Board)
;; Board is (listof Val) that is 16 elements long
;; interp.
;;  Visually a board is a NxN array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. A natural represents a square block
;;  with a number in [1, (sqr N)-1], and false represents the blank square
;;  to which other squares can be slided to.
;; CONSTRAINT: Exactly 1 square is empty (only one element in Board is false)
;;             A board has (sqr N) elements
;;             All numbers from [1, (sqr N)-1] must be used exactly once

(define BD2x2s0
  (list 1 2
        3 B))


(define BD4x4s0
  (list  1  2  3  4
         5  6  7  8
         9 10 11 12
         13 14 15 B))  ; solved 4x4 board (15 puzzle)

(define BD4x4s1
  (list 1 2 3 4
        5 6 7 8
        9 10 11 12
        13 14 B 15))

(define BD4x4s2
  (list 1 2 3 4
        5 6 7 8
        9 10 11 B
        13 14 15 12))

(define BD4x4s3
  (list 1 2 3 4
        5 6 B 7
        9 10 11 8
        13 14 15 12))

(define BD4x4s4
  (list B 2 3 4
        1 6 7 8
        5 10 11 12
        9 13 14 15))

(define BD4x4us
  (list 1  2  3  4
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
        3  2 14  8))  ; idk if this is solvable either

(define BD4x4r3
  (list 6 10 11  9
        1  B  7  8
        13 14 15 4
        12  3  2 5)) ; idk if this is solvable either

(define (fn-for-bd bd)
  (cond [(empty? bd) (...)]
        [else
         (... (first bd)
              (fn-for-bd (rest bd)))]))


;(@htdd Pos)
(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. the position of a square on the board in a 4x4 board
;;         as an x y position
;;         the first column and row have x and y of 0 and 0 respectively

(define P0 (make-pos 0 0)) ;first square on board
(define PF (make-pos 3 3)) ;last square on board
(define P1 (make-pos 2 1)) ;some pos on board

(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))


;(@htdd Pair)
(define-struct pair (val ind))
;; Pair is (make-pair Natural Natural)
;; interp. the value of a number and its index in list of naturals
;; CONSTRAINT: pair can only be used to describe a natural in a list of
;;             naturals in which all of the possible indices of the list
;;             appear as values exactly once in the list

(define PA0 (make-pair 0 0))
(define PA1 (make-pair 1 1))
(define PA2 (make-pair 7 2))

(define (fn-for-pair p)
  (... (pair-val p)
       (pair-ind p)))


;(@htdd Cycle)
;; Cycle is (listof Natural)
;; interp. a list of naturals that describe how numbers in a list in which all
;;         of the possible indices of the list appear as values exactly once
;;         in the list cycle around
;;         - a number in a cycle is the position of previous number in the list
;;         Example: (list 1 6 3 1) - 1 is at position 6, 6 at position 3,
;;          and 3 at position 1 in the original list the cycle describes
;; CONSTRAINT: the first and last numebr in Cycle must be identical

(define C0 empty)
(define C1 (list 0 0))
(define C2 (list 1 2 1))
(define C3 (list 1 6 3 1))

(define (fn-for-cycle c)
  (... c))


;(@htdd FalsePage)
(define-struct fp (f page))
;; FalsePage is one of:
;; - (make-fp false 0)
;; - (make-fp false 1)
;; - (make-fp false 2)
;; - (make-fp false 3)
;; interp. the current "page" number of the false state that determines what to
;;         render/where the clicks are
;;         3 indicates that the game has been quit

(define FP0 (make-fp false 0))
(define FP1 (make-fp false 1))

(define (fn-for-fp fp)
  (cond [(zero? (fp-page fp)) (...)]
        [(= 1 (fp-page fp)) (...)]
        [(= 2 (fp-page fp)) (...)]
        [(= 3 (fp-page fp)) (...)]))

;(@htdd Timer)
(define-struct timer (min sec cen))
;; Timer is (make-timer Natural Natural)
;; min is in [0, 99]
;; sec is in [0, 59]
;; cen is in [0, 99]
;; interp. a timer that counts from 0 to 99 minutes and 59 seconds
;;           and 99 centiseconds

(define T0 (make-timer 0 0 0))
(define T1 (make-timer 11 34 8))

(define (fn-for-timer t)
  (... (timer-min t)
       (timer-sec t)
       (timer-cen t)))


;(@htdd TrueBoard)
(define-struct tb (t bd timer))
;; TrueBoard is (make-tb (true Board Timer))
;; interp. the solved state of a board with how long it took to solve

(define TB0 (make-tb true BD2x2s0 T0))
(define TB1 (make-tb true BD4x4s0 T0))

(define (fn-for-tb tb)
  (... (tb-bd tb)
       (tb-timer tb)))


;(@htdd GameState)
(define-struct bs (bd s t))
;; GameState is one of:
;; - "start"
;; - FalsePage
;; - TrueBoard
;; - (make-bs Board Boolean Timer)
;; interp. a game state is a boardstate composed of the current board and
;;           a boolean indicating if the game is paused (true when paused) and
;;           a timer that counts how long much time in game has passed or
;;         "start" when the game is loading or 
;;         FalsePage before the game has started or
;;         TrueBoard if the puzzle is solved

(define GSF (make-fp false 0))
(define GST (make-tb true BD4x4s0 T1))
(define GSF0 (make-bs BD4x4s0 false T1))
(define GSF1 (make-bs BD4x4s1 false T0))
(define GSF2 (make-bs BD4x4s2 false T1))
(define GSF2T (make-bs BD4x4s2 false (make-timer 11 34 9)))
(define GSF3 (make-bs BD4x4s3 false T1))
(define GSF4 (make-bs BD4x4s4 false T0))
(define GST0 (make-bs BD4x4s0 true T0))
(define GST1 (make-bs BD4x4s1 true T0))

(define (fn-for-gs gs)
  (cond [(string? gs) (...)]
        [(fp? gs) (fn-for-fp gs)]
        [(tb? gs) (...)]
        [else
         (... (fn-for-bd (bs-bd gs))
              (bs-s gs)
              (bs-t gs))]))


;; =================
;; Functions:

;(@htdf main)
;(@signature GameState -> GameState)
;; start the world with (main (make-fp false 0))
;; no check-expects for main function

;(@template-origin htdw-main)

(define (main gs)
  (big-bang gs               ; GameState
    (on-tick   tock 0.01)    ; GameState -> GameState
    (to-draw   render)       ; GameState -> Image
    (stop-when quit)         ; GameState -> Boolean
    (on-mouse  handle-mouse) ; GameState Integer Integer MouseEvent -> GameState
    (on-key    handle-key))) ; GameState KeyEvent -> GameState



;(@htdf tock)
;(@signature GameState -> GameState)
;; produce the next gamestate on tick
;; change gamestate to true if the board is solved
(check-expect (tock (make-fp false 0)) (make-fp false 0))
(check-expect (tock (make-tb true BD4x4s0 T1))
              (make-tb true BD4x4s0 T1))
(check-expect (tock GSF0) (make-tb true BD4x4s0 T1))
(check-expect (tock GSF2) GSF2T)
(check-expect (tock "start") (make-fp false 0))
(check-expect (tock GST1) GST1)

;(@template-origin GameState)

(define (tock gs)
  (cond [(string? gs) (make-fp false 0)]
        [(fp? gs) gs]
        [(tb? gs) gs]
        [else
         (cond [(equal? (bs-bd gs)
                        (make-board (build-list (length (bs-bd gs)) identity)))
                (make-tb true (bs-bd gs) (bs-t gs))]
               [(false? (bs-s gs))
                (make-bs (bs-bd gs) false (next-timer (bs-t gs)))]
               [else gs])]))

;(@htdf next-time)
;(@signature Timer -> Timer)
;; move forward in time by one second
(check-expect (next-timer T0) (make-timer 0 0 1))
(check-expect (next-timer (make-timer 0 0 9))
              (make-timer 0 0 10))
(check-expect (next-timer (make-timer 0 0 99))
              (make-timer 0 1 0))
(check-expect (next-timer (make-timer 10 2 23))
              (make-timer 10 2 24))
(check-expect (next-timer (make-timer 22 59 99))
              (make-timer 23 0 0))
(check-expect (next-timer (make-timer 22 59 12))
              (make-timer 22 59 13))
(check-expect (next-timer (make-timer 99 59 99))
              (make-timer 99 59 99))

;(@template-origin Timer)

(define (next-timer t)
  (cond [(and (= (timer-min t) 99) (= (timer-sec t) 59) (= (timer-cen t) 99)) t]
        [(and (= (timer-sec t) 59) (= (timer-cen t) 99))
         (make-timer (add1 (timer-min t)) 0 0)]
        [(= (timer-cen t) 99)
         (make-timer (timer-min t) (add1 (timer-sec t)) 0)]
        [else (make-timer (timer-min t) (timer-sec t) (add1 (timer-cen t)))]))
      


;(@htdf render)
;(@signature GameState -> Image)
;; render the current game state
(check-expect (render (make-fp false 0))
              (render-false (make-fp false 0)))
(check-expect (render (make-tb true BD4x4s0 T1))
              (place-images
               (list (text "Complete!" (round (/ WIDTH 13)) "black")
                     (text (timer-string T1)
                           (round (/ WIDTH 13)) "black")
                     (overlay (text "New Game" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (overlay (text "Continue" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "gray"))
                     (overlay (text "Main Menu" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (rectangle (* WIDTH .4) (* HEIGHT .62)
                                "solid" "white")
                     (rectangle (* WIDTH .43) (* HEIGHT .65)
                                "solid" "blue"))
               (list (make-posn (/ WIDTH 2) (* HEIGHT 0.3))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.4))
                     (make-posn (/ WIDTH 2) (* HEIGHT .5))
                     (make-posn (/ WIDTH 2) (* HEIGHT .6))
                     (make-posn (/ WIDTH 2) (* HEIGHT .7))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
               (render-board BD4x4s0)))
(check-expect (render (make-bs BD4x4s0 false T1)) (render-board BD4x4s0))
(check-expect (render (make-bs BD4x4s0 true T1))
              (render-pause (make-bs BD4x4s0 true T1)))
(check-expect (render "start") MTS)

;(@template-origin GameState)

(define (render gs)
  (cond [(string? gs) MTS]
        [(fp? gs)
         (render-false gs)]
        [(tb? gs)
         (place-images
          (list (text "Complete!" (round (/ WIDTH 13)) "black")
                (text (timer-string (tb-timer gs)) (round (/ WIDTH 13)) "black")
                (overlay (text "New Game" (round (* WIDTH 9/160)) "black")
                         (rectangle (* WIDTH .3) (* HEIGHT .09)
                                    "solid" "green"))
                (overlay (text "Continue" (round (* WIDTH 9/160)) "black")
                         (rectangle (* WIDTH .3) (* HEIGHT .09)
                                    "solid" "gray"))
                (overlay (text "Main Menu" (round (* WIDTH 9/160)) "black")
                         (rectangle (* WIDTH .3) (* HEIGHT .09)
                                    "solid" "green"))
                (rectangle (* WIDTH .4) (* HEIGHT .62)
                           "solid" "white")
                (rectangle (* WIDTH .43) (* HEIGHT .65)
                           "solid" "blue"))
          (list (make-posn (/ WIDTH 2) (* HEIGHT 0.3))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.4))
                (make-posn (/ WIDTH 2) (* HEIGHT .5))
                (make-posn (/ WIDTH 2) (* HEIGHT .6))
                (make-posn (/ WIDTH 2) (* HEIGHT .7))
                (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
          (render-board (tb-bd gs)))]
        [else
         (if (bs-s gs)
             (render-pause gs)
             (render-board (bs-bd gs)))]))


;(@htdf render-false)
;(@signature FalsePage -> Image)
;; produce a render of false pages
(check-expect (render-false (make-fp false 0))
              (place-images
               (list (text "Sliding Puzzle" (round (/ WIDTH 8)) "black")
                     (overlay (text "Play" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "How to Play" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "Quit" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green")))
               (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.6))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.703))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.806)))
               MTS))
(check-expect (render-false (make-fp false 1))
              (place-images
               (list (text "Choose Board Size" (round (/ WIDTH 13)) "black")
                     (overlay (text "3x3" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "4x4" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "5x5" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "6x6" (round (/ WIDTH 15)) "black")
                              (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                         "solid" "green"))
                     (overlay (text "Back" (round (/ WIDTH 18)) "black")
                              (rectangle (/ WIDTH 4.5) (/ HEIGHT 13)
                                         "solid" "green")))
               (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.497))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.6))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.703))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.806))
                     (make-posn (* WIDTH 0.14) (* HEIGHT 0.94)))
               MTS))
(check-expect (render-false (make-fp false 2))
              (place-images
               (list
                (text "How to Play" (round (/ WIDTH 13)) "black")
                (above (text "Slide pieces onto the blank square"
                             (round (/ WIDTH 20)) "black")
                       (text "either by using the arrow keys or"
                             (round (/ WIDTH 20)) "black")
                       (text "by clicking squares adjacent to the"
                             (round (/ WIDTH 20)) "black")
                       (text "blank. The goal is to place the"
                             (round (/ WIDTH 20)) "black")
                       (text "numbers in increasing order, left"
                             (round (/ WIDTH 20)) "black")
                       (text "to right, top to bottom."
                             (round (/ WIDTH 20)) "black")
                       (text "Press Escape to pause the game."
                             (round (/ WIDTH 20)) "black"))
                (overlay (text "Back" (round (/ WIDTH 18)) "black")
                         (rectangle (/ WIDTH 4.5) (/ HEIGHT 13)
                                    "solid" "green"))
                (beside
                 (text "Solved 3x3 board:  " (round (/ WIDTH 26)) "black")
                 (local [(define S (/ WIDTH 25))
                         (define (rs v)
                           (cond [(number? v)
                                  (overlay (text (number->string v)
                                                 (round S) "black")
                                           (square (* 2 S) "outline" "black"))]
                                 [else
                                  (square (* 2 S) "outline" "black")]))]
                   (above (beside (rs 1) (rs 2) (rs 3))
                          (beside (rs 4) (rs 5) (rs 6))
                          (beside (rs 7) (rs 8) (rs B))))))
               (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                     (make-posn (/ WIDTH 2) (* HEIGHT 0.5))
                     (make-posn (* WIDTH 0.14) (* HEIGHT 0.94))
                     (make-posn (* WIDTH 0.65) (* HEIGHT 0.85)))
               MTS))
(check-expect (render-false (make-fp false 3))
              (render-false (make-fp false 0)))

;(@template-origin FalsePage)

(define (render-false fp)
  (cond [(zero? (fp-page fp))
         (place-images
          (list (text "Sliding Puzzle" (round (/ WIDTH 8)) "black")
                (overlay (text "Play" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "How to Play" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "Quit" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green")))
          (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.6))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.703))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.806)))
          MTS)]
        [(= 1 (fp-page fp))
         (place-images
          (list (text "Choose Board Size" (round (/ WIDTH 13)) "black")
                (overlay (text "3x3" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "4x4" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "5x5" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "6x6" (round (/ WIDTH 15)) "black")
                         (rectangle (/ WIDTH 2.7) (/ HEIGHT 11)
                                    "solid" "green"))
                (overlay (text "Back" (round (/ WIDTH 18)) "black")
                         (rectangle (/ WIDTH 4.5) (/ HEIGHT 13)
                                    "solid" "green")))
          (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.497))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.6))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.703))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.806))
                (make-posn (* WIDTH 0.14) (* HEIGHT 0.94)))
          MTS)]
        [(= 2 (fp-page fp))
         (place-images
          (list
           (text "How to Play" (round (/ WIDTH 13)) "black")
           (above (text "Slide pieces onto the blank square"
                        (round (/ WIDTH 20)) "black")
                  (text "either by using the arrow keys or"
                        (round (/ WIDTH 20)) "black")
                  (text "by clicking squares adjacent to the"
                        (round (/ WIDTH 20)) "black")
                  (text "blank. The goal is to place the"
                        (round (/ WIDTH 20)) "black")
                  (text "numbers in increasing order, left"
                        (round (/ WIDTH 20)) "black")
                  (text "to right, top to bottom."
                        (round (/ WIDTH 20)) "black")
                  (text "Press Escape to pause the game."
                        (round (/ WIDTH 20)) "black"))
           (overlay (text "Back" (round (/ WIDTH 18)) "black")
                    (rectangle (/ WIDTH 4.5) (/ HEIGHT 13)
                               "solid" "green"))
           (beside
            (text "Solved 3x3 board:  " (round (/ WIDTH 26)) "black")
            (local [(define S (/ WIDTH 25))
                    (define (rs v)
                      (cond [(number? v)
                             (overlay (text (number->string v)
                                            (round S) "black")
                                      (square (* 2 S) "outline" "black"))]
                            [else
                             (square (* 2 S) "outline" "black")]))]
              (above (beside (rs 1) (rs 2) (rs 3))
                     (beside (rs 4) (rs 5) (rs 6))
                     (beside (rs 7) (rs 8) (rs B))))))
          (list (make-posn (/ WIDTH 2) (* HEIGHT 2/9))
                (make-posn (/ WIDTH 2) (* HEIGHT 0.5))
                (make-posn (* WIDTH 0.14) (* HEIGHT 0.94))
                (make-posn (* WIDTH 0.65) (* HEIGHT 0.85)))
          MTS)]
        [(= 3 (fp-page fp))
         (render-false (make-fp false 0))]))


;(@htdf render-board)
;(@signature Board -> Image)
;; produce a render of the given board
(check-expect (render-board BD4x4s0)
              (local [(define SIZE (/ WIDTH 2 (sqrt (length BD4x4s0))))
                      (define (rs v)
                        (cond [(number? v)
                               (overlay (text (number->string v)
                                              (round SIZE) "black")
                                        (square (* 2 SIZE) "outline" "black"))]
                              [else
                               (square (* 2 SIZE) "outline" "black")]))]
                (place-image/align
                 (overlay (above (beside (rs 1) (rs 2) (rs 3) (rs 4))
                                 (beside (rs 5) (rs 6) (rs 7) (rs 8))
                                 (beside (rs 9) (rs 10) (rs 11) (rs 12))
                                 (beside (rs 13) (rs 14) (rs 15) (rs B)))
                          (overlay/align "left" "top"
                                         (square (sub1 WIDTH) "solid" "white")
                                         (square WIDTH "solid" "black")))
                 0 0 "left" "top" MTS)))
(check-expect (render-board BD4x4us)
              (local [(define SIZE (/ WIDTH 2 (sqrt (length BD4x4us))))
                      (define (rs v)
                        (cond [(number? v)
                               (overlay (text (number->string v)
                                              (round SIZE) "black")
                                        (square (* 2 SIZE) "outline" "black"))]
                              [else
                               (square (* 2 SIZE) "outline" "black")]))]
                (place-image/align 
                 (overlay (above (beside (rs 1) (rs 2) (rs 3) (rs 4))
                                 (beside (rs 5) (rs 6) (rs 7) (rs 8))
                                 (beside (rs 9) (rs 10) (rs 11) (rs 12))
                                 (beside (rs 13) (rs 15) (rs 14) (rs B)))
                          (overlay/align "left" "top"
                                         (square (sub1 WIDTH) "solid" "white")
                                         (square WIDTH "solid" "black")))
                 0 0 "left" "top" MTS)))

;(@template-origin use-abstract-fn fn-composition)

(define (render-board bd)
  (local  [(define SIZE (/ WIDTH 2 (sqrt (length bd))))

           (define (render-square v)
             (cond [(number? v)
                    (overlay (text (number->string v) (round SIZE) "black")
                             (square (* 2 SIZE) "outline" "black"))]
                   [else
                    (square (* 2 SIZE) "outline" "black")]))

           (define (render-row lov)
             (foldr beside empty-image (map render-square lov)))]
    
    (place-image/align
     (overlay (foldr above empty-image
                     (map render-row (separate-bd bd)))
              (overlay/align "left" "top"
                             (square (sub1 WIDTH) "solid" "white")
                             (square WIDTH "solid" "black")))
     0 0 "left" "top" MTS)))


;(@htdf separate-bd)
;(@signature Board -> (listof (listof Val)))
;; separate board into 4 lists of 4 values
(check-expect (separate-bd BD4x4s0)
              (list (list 1 2 3 4)
                    (list 5 6 7 8)
                    (list 9 10 11 12)
                    (list 13 14 15 B)))
(check-expect (separate-bd BD4x4us)
              (list (list 1 2 3 4)
                    (list 5 6 7 8)
                    (list 9 10 11 12)
                    (list 13 15 14 B)))

;(@template-origin Board accumulator)

(define (separate-bd bd0)
  ;; rsf is (listof (listof Val));
  ;;  list of complete rows of length (sqrt (length bd0)) seen so far
  ;; cur is (listof Val); list of vals in current row
  ;; acc is Natural;
  ;;  number of vals to add to cur before adding cur to rsf
  (local [(define len (sqrt (length bd0)))
          (define (separate-bd bd rsf cur acc)
            (cond [(empty? bd) (append rsf (list cur))]
                  [else
                   (if (zero? acc)
                       (separate-bd (rest bd)
                                    (append rsf (list cur))
                                    (list (first bd))
                                    (sub1 len))
                       (separate-bd (rest bd)
                                    rsf
                                    (append cur (list (first bd)))
                                    (sub1 acc)))]))]
    (separate-bd (rest bd0) empty (list (first bd0)) (sub1 len))))


;(@htdf render-pause)
;(@signature Board -> Image)
;; produce render of a paused state with board
(check-expect (render-pause (make-bs BD4x4s1 true T1))
              (place-images
               (list (text "Paused" (round (/ WIDTH 13)) "black")
                     (text (timer-string T1) (round (/ WIDTH 13)) "black")
                     (overlay (text "New Game" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (overlay (text "Continue" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (overlay (text "Main Menu" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (rectangle (* WIDTH .4) (* HEIGHT .62)
                                "solid" "white")
                     (rectangle (* WIDTH .43) (* HEIGHT .65)
                                "solid" "blue"))
               (list (make-posn (/ WIDTH 2) (* HEIGHT .3))
                     (make-posn (/ WIDTH 2) (* HEIGHT .4))
                     (make-posn (/ WIDTH 2) (* HEIGHT .5))
                     (make-posn (/ WIDTH 2) (* HEIGHT .6))
                     (make-posn (/ WIDTH 2) (* HEIGHT .7))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
               (render-board BD4x4s1)))
(check-expect (render-pause (make-bs BD4x4s2 true T0))
              (place-images
               (list (text "Paused" (round (/ WIDTH 13)) "black")
                     (text (timer-string T0) (round (/ WIDTH 13)) "black")
                     (overlay (text "New Game" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (overlay (text "Continue" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (overlay (text "Main Menu" (round (* WIDTH 9/160)) "black")
                              (rectangle (* WIDTH .3) (* HEIGHT .09)
                                         "solid" "green"))
                     (rectangle (* WIDTH .4) (* HEIGHT .62)
                                "solid" "white")
                     (rectangle (* WIDTH .43) (* HEIGHT .65)
                                "solid" "blue"))
               (list (make-posn (/ WIDTH 2) (* HEIGHT .3))
                     (make-posn (/ WIDTH 2) (* HEIGHT .4))
                     (make-posn (/ WIDTH 2) (* HEIGHT .5))
                     (make-posn (/ WIDTH 2) (* HEIGHT .6))
                     (make-posn (/ WIDTH 2) (* HEIGHT .7))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                     (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
               (render-board BD4x4s2)))

;(@template-origin Board)

(define (render-pause gs)
  (place-images
   (list (text "Paused" (round (/ WIDTH 13)) "black")
         (text (timer-string (bs-t gs)) (round (/ WIDTH 13)) "black")
         (overlay (text "New Game" (round (* WIDTH 9/160)) "black")
                  (rectangle (* WIDTH .3) (* HEIGHT .09)
                             "solid" "green"))
         (overlay (text "Continue" (round (* WIDTH 9/160)) "black")
                  (rectangle (* WIDTH .3) (* HEIGHT .09)
                             "solid" "green"))
         (overlay (text "Main Menu" (round (* WIDTH 9/160)) "black")
                  (rectangle (* WIDTH .3) (* HEIGHT .09)
                             "solid" "green"))
         (rectangle (* WIDTH .4) (* HEIGHT .62)
                    "solid" "white")
         (rectangle (* WIDTH .43) (* HEIGHT .65)
                    "solid" "blue"))
   (list (make-posn (/ WIDTH 2) (* HEIGHT .3))
         (make-posn (/ WIDTH 2) (* HEIGHT .4))
         (make-posn (/ WIDTH 2) (* HEIGHT .5))
         (make-posn (/ WIDTH 2) (* HEIGHT .6))
         (make-posn (/ WIDTH 2) (* HEIGHT .7))
         (make-posn (/ WIDTH 2) (/ HEIGHT 2))
         (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
   (render-board (bs-bd gs))))


;(@htdf timer-string)
;(@signature Timer -> String)
;; convert timer to a string
(check-expect (timer-string T0) "00:00.00")
(check-expect (timer-string T1) "11:34.08")

;(@template-origin Timer)

(define (timer-string t)
  (string-append (two-digit (timer-min t)) ":" 
                 (two-digit (timer-sec t)) "."
                 (two-digit (timer-cen t))))

;(@htdf two-digit)
;(@signature Number -> String)
;; convert a one or two digit number into a two digit string
(check-expect (two-digit 3) "03")
(check-expect (two-digit 13) "13")

;(@template-origin Number)

(define (two-digit n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))



;(@htdf quit)
;(@signature GameState -> Boolean)
;; produce true if bs-s of the current gamestate is true
(check-expect (quit (make-fp false 0)) false)
(check-expect (quit (make-fp false 3)) true)
(check-expect (quit (make-tb true BD4x4s0 T1)) false)
(check-expect (quit GSF2) false)
(check-expect (quit GST0) false)
(check-expect (quit "start") false)

;(@template-origin GameState)

(define (quit gs)
  (cond [(string? gs) false]
        [(fp? gs)
         (if (= 3 (fp-page gs))
             true
             false)]
        [(tb? gs) false]
        [else false]))


;(@htdf handle-mouse)
;(@signature GameState Integer Integer MouseEvent -> GameState)
;; on mouse event call appropriate function
(check-expect (handle-mouse (make-fp false 0) 1 2 "button-down")
              (click (make-fp false 0) 1 2))
(check-expect (handle-mouse (make-tb true BD4x4s0 T1) 32 5 "button-down")
              (click (make-tb true BD4x4s0 T1) 32 5))
(check-expect (handle-mouse GSF2 5 9 "button-down") (click GSF2 5 9))
(check-expect (handle-mouse GSF3 0 0 "button-up") GSF3)
(check-expect (handle-mouse false 1 2 "drag") false)
(check-expect (handle-mouse (make-tb true BD4x4s0 T1) 32 5 "move")
              (make-tb true BD4x4s0 T1))

;(@template-origin MouseEvent)

(define (handle-mouse gs x y me)
  (cond [(string=? me "button-down") (click gs x y)]
        [else gs]))


;(@htdf click)
;(@signature GameState Integer Integer -> GameState)
;; on click change gamestates based on current gamestate and mouse position
(check-expect (click GSF0 0 0) GSF0)
(check-expect (click GSF0 0 (* HEIGHT 5/8)) GSF0)
(check-expect (click GSF0 (* WIDTH 5/8) 0) GSF0)

(check-expect (click GSF2 (* WIDTH 7/8) (* HEIGHT 7/8)) GSF0)
(check-expect (click GSF2 WIDTH (* HEIGHT 7/8)) GSF2)
(check-expect (click GSF2 (- WIDTH 1) (* HEIGHT 7/8)) GSF0)
(check-expect (click GSF2 (+ WIDTH 1) (* HEIGHT 7/8)) GSF2)
(check-expect (click GSF2 (* WIDTH 3/4) (* HEIGHT 7/8)) GSF2)
(check-expect (click GSF2 (- (* WIDTH 3/4) 1) (* HEIGHT 7/8)) GSF2)
(check-expect (click GSF2 (+ (* WIDTH 3/4) 1) (* HEIGHT 7/8)) GSF0)
(check-expect (click GSF2 (* WIDTH 7/8) HEIGHT) GSF2)
(check-expect (click GSF2 (* WIDTH 7/8) (- HEIGHT 1)) GSF0)
(check-expect (click GSF2 (* WIDTH 7/8) (+ HEIGHT 1)) GSF2)
(check-expect (click GSF2 (* WIDTH 7/8) (* HEIGHT 3/4)) GSF2)
(check-expect (click GSF2 (* WIDTH 7/8) (- (* HEIGHT 3/4) 1)) GSF2)
(check-expect (click GSF2 (* WIDTH 7/8) (+ (* HEIGHT 3/4) 1)) GSF0)

(check-expect (click GSF2 (* WIDTH 7/8) (* HEIGHT 3/8))
              (swap GSF2 (make-pos 0 -1)))
(check-expect (click GSF3 (* WIDTH 3/8) (* HEIGHT 3/8))
              (swap GSF3 (make-pos -1 0)))
(check-expect (click GSF3 (* WIDTH 7/8) (* HEIGHT 3/8))
              (swap GSF3 (make-pos +1 0)))

(check-expect (click "START" 2 3) "START")
(check-expect (click (make-fp false 1) 4 1)
              (click-false (make-fp false 1) 4 1))
(check-expect (click GST0 2 5) (game-solved 2 5 GST0))

;(@template-origin GameState)

(define (click gs x y)
  (cond [(string? gs) gs]
        [(fp? gs) (click-false gs x y)]
        [(tb? gs) (game-solved x y gs)]
        [else
         (if (bs-s gs)
             (click-pause x y gs)
             (cond [(overlapping? (bs-bd gs) (make-pos 0 +1) x y)
                    (swap gs (make-pos 0 +1))]
                   [(overlapping? (bs-bd gs) (make-pos 0 -1) x y)
                    (swap gs (make-pos 0 -1))]
                   [(overlapping? (bs-bd gs) (make-pos +1 0) x y)
                    (swap gs (make-pos +1 0))]
                   [(overlapping? (bs-bd gs) (make-pos -1 0) x y)
                    (swap gs (make-pos -1 0))]
                   [else gs]))]))


;(@htdf click-false)
;(@signature FalsePage Integer Integer -> GameState)
;; produce correct gamestate based on falsepage and mouse pos
(check-expect (click-false (make-fp false 0) 3 1)
              (click-f0 3 1))
(check-expect (click-false (make-fp false 1) 13 4)
              (click-f1 13 4))
(check-expect (click-false (make-fp false 2) 5 2)
              (click-f2 5 2))
(check-expect (click-false (make-fp false 3) 7 8)
              (make-fp false 3))

;(@template-origin FalsePage)

(define (click-false fp x y)
  (cond [(zero? (fp-page fp)) (click-f0 x y)]
        [(= 1 (fp-page fp)) (click-f1 x y)]
        [(= 2 (fp-page fp)) (click-f2 x y)]
        [(= 3 (fp-page fp)) fp]))


;(@htdf click-f0)
;(@signature Integer Integer -> GameState)
;; produce correct gamestate based on mouse pos
(check-expect (click-f0 (/ WIDTH 2) (* HEIGHT 0.6))
              (make-fp false 1))
(check-expect (click-f0 (/ WIDTH 2) (* HEIGHT 0.703))
              (make-fp false 2))
(check-expect (click-f0 (/ WIDTH 2) (* HEIGHT 0.806))
              (make-fp false 3))
(check-expect (click-f0 0 0)
              (make-fp false 0))

;(@template-origin Integer)

(define (click-f0 x y)
  (cond [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.6) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.6) (/ HEIGHT 22))))
         (make-fp false 1)]
        [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.703) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.703) (/ HEIGHT 22))))
         (make-fp false 2)]
        [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.806) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.806) (/ HEIGHT 22))))
         (make-fp false 3)]
        [else (make-fp false 0)]))


;(@htdf click-f1)
;(@signature Integer Integer -> GameState)
;; produce correct gamestate based on mouse pos
(check-random (click-f1 (/ WIDTH 2) (* HEIGHT 0.497))
              (random-board 0 9))
(check-random (click-f1 (/ WIDTH 2) (* HEIGHT 0.6))
              (random-board 0 16))
(check-random (click-f1 (/ WIDTH 2) (* HEIGHT 0.703))
              (random-board 0 25))
(check-random (click-f1 (/ WIDTH 2) (* HEIGHT 0.806))
              (random-board 0 36))
(check-expect (click-f1 (* WIDTH 0.14) (* HEIGHT 0.94))
              (make-fp false 0))
(check-expect (click-f1 0 0)
              (make-fp false 1))

;(@template-origin Integer)

(define (click-f1 x y)
  (cond [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.497) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.497) (/ HEIGHT 22))))
         (random-board 0 9)]
        [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.6) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.6) (/ HEIGHT 22))))
         (random-board 0 16)]
        [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.703) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.703) (/ HEIGHT 22))))
         (random-board 0 25)]
        [(and (< (- (/ WIDTH 2) (/ WIDTH 5.4)) x
                 (+ (/ WIDTH 2) (/ WIDTH 5.4)))
              (< (- (* HEIGHT 0.806) (/ HEIGHT 22)) y
                 (+ (* HEIGHT 0.806) (/ HEIGHT 22))))
         (random-board 0 36)]
        [(and (< (- (* WIDTH 0.14) (/ WIDTH 9)) x
                 (+ (* WIDTH 0.14) (/ WIDTH 9)))
              (< (- (* HEIGHT 0.94) (/ HEIGHT 26)) y
                 (+ (* HEIGHT 0.94) (/ HEIGHT 26))))
         (make-fp false 0)]
        [else (make-fp false 1)]))
        
       
;(@htdf click-f2)
;(@signature Integer Integer -> GameState)
;; produce correct gamestate based on mouse pos
(check-expect (click-f2 (* WIDTH 0.14) (* HEIGHT 0.94))
              (make-fp false 0))
(check-expect (click-f2 0 0)
              (make-fp false 2))

;(@template-origin Integer)

(define (click-f2 x y)
  (cond [(and (< (- (* WIDTH 0.14) (/ WIDTH 9)) x
                 (+ (* WIDTH 0.14) (/ WIDTH 9)))
              (< (- (* HEIGHT 0.94) (/ HEIGHT 26)) y
                 (+ (* HEIGHT 0.94) (/ HEIGHT 26))))
         (make-fp false 0)]
        [else (make-fp false 2)]))


;(@htdf game-solved)
;(@signature Integer Integer GameState -> GameState)
;; start a new game, return to main menu, or stay based on mouse position
(check-random (game-solved (* WIDTH .5) (* HEIGHT 0.5) GST)
              (random-board 0 (length (tb-bd GST))))
(check-expect (game-solved (* WIDTH .5) (* HEIGHT 0.7) GST)
              (make-fp false 0))
(check-expect (game-solved 0 0 GST) GST)

;(@template-origin Integer)

(define (game-solved x y gs)
  (cond [(and (< (- (* WIDTH .5) (* WIDTH .15))
                 x
                 (+ (* WIDTH .5) (* WIDTH .15)))
              (< (- (* HEIGHT .5) (* HEIGHT .045))
                 y
                 (+ (* HEIGHT .5) (* HEIGHT .045))))
         (random-board 0 (length (tb-bd gs)))]
        [(and (< (- (* WIDTH .5) (* WIDTH .15))
                 x
                 (+ (* WIDTH .5) (* WIDTH .15)))
              (< (- (* HEIGHT .7) (* HEIGHT .045))
                 y
                 (+ (* HEIGHT .7) (* HEIGHT .045))))
         (make-fp false 0)]
        [else gs]))


;(@htdf overlapping?)
;(@signature Board Pos Integer Integer -> Boolean)
;; produce true if mouse is in the pos square relative to blank square
(check-expect (overlapping? BD4x4s3 (make-pos +1 0) 0 0) false)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (* HEIGHT 1/4)) false)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (- (* HEIGHT 1/4) 1)) false)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (+ (* HEIGHT 1/4) 1)) true)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (* HEIGHT 1/2)) false)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (+ (* HEIGHT 1/2) 1)) false)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (- (* HEIGHT 1/2) 1)) true)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 7/8) (* HEIGHT 3/8)) true)
(check-expect (overlapping? BD4x4s3 (make-pos +1 0)
                            (* WIDTH 5/8) (* HEIGHT 1/8)) false)
(check-expect (overlapping? BD4x4s3 (make-pos 0 -1)
                            (* WIDTH 5/8) (* HEIGHT 1/8)) true)
(check-expect (overlapping? BD4x4s3 (make-pos 0 -1)
                            (* WIDTH 3/8) (* HEIGHT 3/8)) false)
(check-expect (overlapping? BD4x4s3 (make-pos -1 0)
                            (* WIDTH 3/8) (* HEIGHT 3/8)) true)
(check-expect (overlapping? BD4x4s3 (make-pos -1 0)
                            (* WIDTH 5/8) (* HEIGHT 5/8)) false)
(check-expect (overlapping? BD4x4s3 (make-pos 0 +1)
                            (* WIDTH 5/8) (* HEIGHT 5/8)) true)
(check-expect (overlapping? BD4x4s3 (make-pos 0 +1)
                            (* WIDTH 7/8) (* HEIGHT 3/8)) false)

;(@template-origin Pos)

(define (overlapping? bd pos x y)
  (local [(define index (index-of bd B))
          (define size (length bd))
          (define N (sqrt size))
          (define SIZE (/ WIDTH 2 (sqrt size)))
          
          (define (pos-col i) (remainder i N))
          (define (pos-row i) (quotient i N))

          (define new-pos
            (make-pos (+ (pos-col index) (pos-x pos))
                      (+ (pos-row index) (pos-y pos))))]
    
    (and (< (* SIZE 2 (pos-x new-pos)) x (* SIZE 2 (add1 (pos-x new-pos))))
         (< (* SIZE 2 (pos-y new-pos)) y (* SIZE 2 (add1 (pos-y new-pos)))))))


;(@htdf click-pause)
;(@signature Integer Integer GameState -> GameState)
;; produce correct gamestate from mouse pos
(check-random (click-pause (* WIDTH .5) (* HEIGHT .5) GST1)
              (random-board 0 (length (bs-bd GST1))))
(check-expect (click-pause (* WIDTH .5) (* HEIGHT .6) GST1)
              (make-bs (bs-bd GST1) false (bs-t GST1)))
(check-expect (click-pause (* WIDTH .5) (* HEIGHT .7) GST1)
              (make-fp false 0))
(check-expect (click-pause 0 0 GST1) GST1)

;(@template-origin Integer)

(define (click-pause x y gs)
  (cond [(and (< (- (* WIDTH .5) (* WIDTH .15))
                 x
                 (+ (* WIDTH .5) (* WIDTH .15)))
              (< (- (* HEIGHT .5) (* HEIGHT .045))
                 y
                 (+ (* HEIGHT .5) (* HEIGHT .045))))
         (random-board 0 (length (bs-bd gs)))]
        [(and (< (- (* WIDTH .5) (* WIDTH .15))
                 x
                 (+ (* WIDTH .5) (* WIDTH .15)))
              (< (- (* HEIGHT .6) (* HEIGHT .045))
                 y
                 (+ (* HEIGHT .6) (* HEIGHT .045))))
         (make-bs (bs-bd gs) false (bs-t gs))]
        [(and (< (- (* WIDTH .5) (* WIDTH .15))
                 x
                 (+ (* WIDTH .5) (* WIDTH .15)))
              (< (- (* HEIGHT .7) (* HEIGHT .045))
                 y
                 (+ (* HEIGHT .7) (* HEIGHT .045))))
         (make-fp false 0)]
        [else gs]))


;(@htdf random-board)
;(@signature Natural Natural -> GameState)
;; produce random board with length n based on parity (0 or 1) with false
(check-random (random-board 0 16)
              (make-bs (make-board
                        (correct-parity (random-list 16) 0))
                       false T0))
(check-random (random-board 1 9)
              (make-bs (make-board
                        (correct-parity (random-list 9) 1))
                       false T0))

;(@template-origin fn-composition)

(define (random-board n len)
  (make-bs (make-board
            (correct-parity (random-list len) n))
           false
           (make-timer 0 0 0)))


;(@htdf make-board)
;(@signature (listof Natural) -> Board)
;; make a board from list of naturals; add1 to all and change max num to false
(check-expect (make-board (build-list 16 identity))
              (list 1 2 3 4
                    5 6 7 8
                    9 10 11 12
                    13 14 15 B))
(check-expect (make-board (build-list 4 identity))
              (list 1 2
                    3 B))
(check-expect (make-board (list 2 0
                                1 3))
              (list 3 1
                    2 B))

;(@template-origin use-abstract-fn)

(define (make-board lon)
  (map (λ (n) (if (= n (sub1 (length lon)))
                  B
                  (add1 n)))
       lon))


;(@htdf correct-parity)
;(@signature (listof Natural) Natural -> (listof Natural))
;; correct the parity based on given lon and given parity (0 or 1, even or odd)
(check-expect (correct-parity (build-list 16 identity) 0)
              (build-list 16 identity))
(check-expect (correct-parity (build-list 16 identity) 1)
              (0-1-switch (build-list 16 identity)))
(check-expect (correct-parity (list 0 1 2 3
                                    4 5 6 7
                                    8 9 10 11
                                    12 13 15 14) 0)
              (list 0 1 2 3
                    4 5 6 7
                    8 9 10 11
                    12 13 15 14))   

;(@template-origin Natural)

(define (correct-parity lon n)
  (if (= 1 (+ (parity lon) n))
      (0-1-switch lon)
      lon))


;(@htdf parity)
;(@signature (listof Natural) -> Natural)
;; produce parity (0 or 1, even or odd) of given list of natural
(check-expect (parity (build-list 16 identity)) 0)
(check-expect (parity (list 1 0 2 3
                            4 5 6 7
                            8 9 10 11
                            12 13 14 15)) 1)

;(@template-origin fn-composition)

(define (parity lon)
  (local [(define (cycle-parity loc)
            (foldr + 0 (map length loc)))]
    
    (remainder (+ (cycle-parity (pair-cycle (list-pair lon)))
                  (blank-parity (sqrt (length lon))
                                (index-of lon (sub1 (length lon))))) 2)))


;(@htdf blank-parity)
;(@signature Natural Natural -> Natural)
;; produce parity of position of blank in board of size n x n
(check-expect (blank-parity 4 0) 0)
(check-expect (blank-parity 4 1) 1)
(check-expect (blank-parity 4 4) 1)
(check-expect (blank-parity 4 5) 0)

;(@template-origin Natural)

(define (blank-parity n p)
  (if (zero? (remainder (- (remainder p n) (quotient p n)) 2))
      0
      1))


;(@htdf list-pair)
;(@signature (listof Natural) -> (listof Pair))
;; produce a list of pairs that describe each number in the list of natural
(check-expect (list-pair (list 0 1 2 3))
              (list (make-pair 0 0) (make-pair 1 1)
                    (make-pair 2 2) (make-pair 3 3)))
(check-expect (list-pair (list 3 2 1 0))
              (list (make-pair 3 0) (make-pair 2 1)
                    (make-pair 1 2) (make-pair 0 3)))
(check-expect (list-pair (list 10 11 2 0
                               1 3 5 9
                               8 7 13 14
                               4 15 6 12))
              (list (make-pair 10 0) (make-pair 11 1)
                    (make-pair 2 2) (make-pair 0 3)
                    (make-pair 1 4) (make-pair 3 5)
                    (make-pair 5 6) (make-pair 9 7)
                    (make-pair 8 8) (make-pair 7 9)
                    (make-pair 13 10) (make-pair 14 11)
                    (make-pair 4 12) (make-pair 15 13)
                    (make-pair 6 14) (make-pair 12 15)))

;(@template-origin use-abstract-fn)

(define (list-pair lon)
  (map (λ (n) (make-pair n (index-of lon n))) lon))


;(@htdf pair-cycle)
;(@signature (listof Pair) -> (listof Cycle))
;; from list of pairs produce list of cycles that represents the pairs
(check-expect (pair-cycle empty) empty)
(check-expect (pair-cycle (list-pair (list 0 1 2 3)))
              (list (list 0 0) (list 1 1) (list 2 2) (list 3 3)))
(check-expect (pair-cycle (list-pair (list 3 2 1 0)))
              (list (list 3 0 3) (list 2 1 2)))
(check-expect (pair-cycle (list-pair (list 3 0 1 2)))
              (list (list 3 0 1 2 3)))
(check-expect (pair-cycle (list-pair (list 10 11 2 0
                                           1 3 5 9
                                           8 7 13 14
                                           4 15 6 12)))
              (list (list 10 0 3 5 6 14 11 1 4 12 15 13 10)
                    (list 2 2) (list 9 7 9) (list 8 8)))

;(@template-origin accumulator (listof Pair))

(define (pair-cycle lop0)
  ;; acc is Pair; the previously seen pair
  ;; cyc is Cycle; current cycle of numbers from pairs
  ;; rsf is (listof Cycle); all cycles seen so far
  (local [(define (pair-cycle lop acc cyc rsf)
            (local [(define ind (pair-ind acc))]
              (cond [(empty? lop)
                     (append rsf (list (append cyc (list ind))))]
                    [(= (first cyc) ind)
                     (pair-cycle
                      (rest lop)
                      (first lop)
                      (list (pair-val (first lop)))
                      (append rsf (list (append cyc (list ind)))))]
                    [else
                     (local [(define npair
                               (find-npair lop ind))]
                       (pair-cycle (remove npair lop)
                                   npair
                                   (append cyc (list (pair-val npair)))
                                   rsf))])))

          (define (find-npair lop i)
            (if (= i (pair-val (first lop)))
                (first lop)
                (find-npair (rest lop) i)))]
    
    (if (empty? lop0)
        empty
        (pair-cycle (rest lop0)
                    (first lop0)
                    (list (pair-val (first lop0)))
                    empty))))


;(@htdf 0-1-switch)
;(@signature (listof Natural) -> (listof Natural))
;; produce a list with the positions of 0 and 1 switched in given list
(check-expect (0-1-switch (build-list 16 identity))
              (list 1 0 2 3
                    4 5 6 7
                    8 9 10 11
                    12 13 14 15))
(check-expect (0-1-switch (reverse (build-list 16 identity)))
              (list 15 14 13 12
                    11 10 9 8
                    7 6 5 4
                    3 2 0 1))

;(@template-origin (listof Natural)) ; racket/list functions

(define (0-1-switch lon)
  (local [(define 0index (index-of lon 0))
          (define 1index (index-of lon 1))]
    (list-set (list-set lon 0index 1) 1index 0)))


;(@htdf random-list)
;(@signature Natural -> (listof Natural))
;; produce a random list of length n with numbers from 0 to n-1
(check-random (random-list 4)
              (local [(define (random-list n rem)
                        (cond [(zero? n) empty]
                              [else
                               (local [(define NUM (list-ref rem (random n)))]
                                 (cons NUM
                                       (random-list (sub1 n)
                                                    (remove NUM rem))))]))]
                (random-list 4 (build-list 4 identity))))
(check-random (random-list 16)
              (local [(define (random-list n rem)
                        (cond [(zero? n) empty]
                              [else
                               (local [(define NUM (list-ref rem (random n)))]
                                 (cons NUM
                                       (random-list (sub1 n)
                                                    (remove NUM rem))))]))]
                (random-list 16 (build-list 16 identity))))

;(@template-origin Natural accumulator)

(define (random-list n0)
  ;; rem is (listof Natural);
  ;; list of remaining naturals not yet added to the list being built
  (local [(define (random-list n rem)
            (cond [(zero? n) empty]
                  [else
                   (local [(define NUM (list-ref rem (random n)))]
                     (cons NUM
                           (random-list (sub1 n)
                                        (remove NUM rem))))]))]
    (random-list n0 (build-list n0 identity))))


;(@htdf handle-key)
;(@signature GameState KeyEvent -> GameState)
;; on key press call appropriate function with appropriate parameter
(check-expect (handle-key (make-fp false 0) "down") (make-fp false 0))
(check-expect (handle-key (make-tb true BD4x4s0 T1) "down")
              (make-tb true BD4x4s0 T1))
(check-expect (handle-key (make-fp false 0) "n") (make-fp false 0))
(check-expect (handle-key (make-tb true BD4x4s0 T1) "n")
              (make-tb true BD4x4s0 T1))
(check-expect (handle-key GSF1 "up") (swap GSF1 (make-pos 0 +1)))
(check-expect (handle-key GSF2 "down") (swap GSF2 (make-pos 0 -1)))
(check-expect (handle-key GSF1 "left") (swap GSF1 (make-pos +1 0)))
(check-expect (handle-key GSF2 "right") (swap GSF2 (make-pos -1 0)))
(check-random (handle-key GSF1 "escape") GST1)
(check-random (handle-key (make-fp false 0) "escape")
              (make-fp false 0))
(check-random (handle-key (make-tb true BD4x4s0 T1) "escape")
              (make-tb true BD4x4s0 T1))
(check-expect (handle-key GSF3 "e") GSF3)

;(@template-origin KeyEvent)

(define (handle-key gs ke)
  (cond [(string=? ke     "up") (swap gs (make-pos 0 +1))]
        [(string=? ke   "down") (swap gs (make-pos 0 -1))]
        [(string=? ke   "left") (swap gs (make-pos +1 0))]
        [(string=? ke  "right") (swap gs (make-pos -1 0))]
        [(string=? ke "escape") (key-pause gs)]
        [else gs]))


;(@htdf swap)
;(@signature GameState Pos -> GameState)
;; swap blank with square indicated by pos relative to blank if possible
(check-expect (swap GST0 (make-pos 0 +1)) GST0)
(check-expect (swap GSF0 (make-pos +1 0)) GSF0)
(check-expect (swap GSF2 (make-pos 0 +1)) GSF0)
(check-expect (swap GSF4 (make-pos 0 -1)) GSF4)
(check-expect (swap GSF4 (make-pos -1 0)) GSF4)
(check-expect (swap GSF3 (make-pos -1 0))
              (make-bs (list 1 2 3 4
                             5 B 6 7
                             9 10 11 8
                             13 14 15 12) false (bs-t GSF3)))
(check-expect (swap GSF3 (make-pos 0 -1))
              (make-bs (list 1 2 B 4
                             5 6 3 7
                             9 10 11 8
                             13 14 15 12) false (bs-t GSF3)))
(check-expect (swap GSF1 (make-pos +1 0)) (make-bs (bs-bd GSF0) false T0))
(check-expect (swap (make-fp false 0) (make-pos +1 0)) (make-fp false 0))
(check-expect (swap (make-tb true BD4x4s0 T1) (make-pos -1 0))
              (make-tb true BD4x4s0 T1))
(check-expect (swap "start" (make-pos +1 0)) "start")

;(@template-origin GameState)

(define (swap gs pos)
  (cond [(string? gs) gs]
        [(fp? gs) gs]
        [(tb? gs) gs]
        [else
         (local [(define bd (bs-bd gs))
                 (define index (index-of bd B))
                 (define size (length bd))
                 (define N (sqrt size))
          
                 (define (pos-col i) (remainder i N))
                 (define (pos-row i) (quotient i N))
          
                 (define (pos->index p)
                   (+ (* (pos-y p) N) (pos-x p)))

                 (define (blank-replace n)
                   (local [(define num (list-ref bd n))]
                     (list-set (list-set bd n B) index num)))

                 (define new-pos
                   (make-pos (+ (pos-col index) (pos-x pos))
                             (+ (pos-row index) (pos-y pos))))]
           (cond [(and (<= 0 (pos-x new-pos) (sub1 N))
                       (<= 0 (pos-y new-pos) (sub1 N)))
                  (make-bs (blank-replace (pos->index new-pos))
                           (bs-s gs)
                           (bs-t gs))]
                 [else gs]))]))
           

;(@htdf key-pause)
;(@signature GameState -> GameState)
;; produce correct gamestate based on current gamestate after escape is clicked
(check-expect (key-pause (make-fp false 0)) (make-fp false 0))
(check-expect (key-pause (make-tb true BD4x4s0 T1)) (make-tb true BD4x4s0 T1))
(check-expect (key-pause (make-bs BD4x4s1 false T0)) (make-bs BD4x4s1 true T0))
(check-expect (key-pause (make-bs BD4x4s1 true T1)) (make-bs BD4x4s1 true T1))
(check-expect (key-pause "start") "start")

;(@template-origin GameState)

(define (key-pause gs)
  (cond [(string? gs) gs]
        [(fp? gs) gs]
        [(tb? gs) gs]
        [else
         (if (bs-s gs)
             gs
             (make-bs (bs-bd gs) true (bs-t gs)))]))