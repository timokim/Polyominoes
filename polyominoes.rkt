;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSIGNMENT 10 POLYOMINOES!

;; Provided Files
(require "a10.rkt")

;; Provided Data/Structure Definitions

;; A Grid is a (ne-listof (ne-listof Character))

;; A Pos is a (make-pos Int Int)
(define-struct pos (x y))

;; A State is a (make-state Grid (listof Grid))
(define-struct state (puzzle pieces))


;; Question 1a

;; build-2dlist: Nat Nat (Nat Nat -> X) -> (listof (listof X))

;; Purpose: To consume two numbers, which represent the width and the
;; height of the resultant 2D list, as well as a function that takes
;; two numbers as inputs and produces a value, and ultimately produce
;; a 2D list with the outputs of the function on each pair of numbers
;; in each position.

;; Examples:
(check-expect (build-2dlist 2 3 +) '((0 1)
                                     (1 2)
                                     (2 3)))
(check-expect (build-2dlist 4 4 >) (list (list false true true true)
                                         (list false false true true)
                                         (list false false false true)
                                         (list false false false false)))

;; Definition
(define (build-2dlist width height function)
  (build-list height (lambda (x) (build-list width (lambda (y) (function y x))))))

;; Tests:
(check-expect (build-2dlist 0 0 +) empty)
(check-expect (build-2dlist 3 3 expt) '((1 1 1)
                                        (0 1 2)
                                        (0 1 4)))
(check-expect (build-2dlist 7 6 (lambda (x y) #\.))
              '((#\. #\. #\. #\. #\. #\. #\.)
                (#\. #\. #\. #\. #\. #\. #\.)
                (#\. #\. #\. #\. #\. #\. #\.)
                (#\. #\. #\. #\. #\. #\. #\.)
                (#\. #\. #\. #\. #\. #\. #\.)
                (#\. #\. #\. #\. #\. #\. #\.)))



;; Question 1b

;; all-positions: Nat Nat -> (listof Pos)

;; Purpose: To consume two numbers and produce the list of all possible
;; pos values that are defined in a grid with width and height as being
;; the given two numbers.

;; Examples:
(check-expect (all-positions 1 1) (list (make-pos 0 0)))
(check-expect (all-positions 2 3) (list (make-pos 0 0) (make-pos 1 0)
                                        (make-pos 0 1) (make-pos 1 1)
                                        (make-pos 0 2) (make-pos 1 2)))

;; Definition
(define (all-positions width height)
  (foldr (lambda (x y) (append x y)) empty (build-2dlist width height make-pos)))

;; Tests:
(check-expect (all-positions 0 0) empty)
(check-expect (all-positions 5 5) (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0) (make-pos 3 0) (make-pos 4 0)
                                        (make-pos 0 1) (make-pos 1 1) (make-pos 2 1) (make-pos 3 1) (make-pos 4 1) 
                                        (make-pos 0 2) (make-pos 1 2) (make-pos 2 2) (make-pos 3 2) (make-pos 4 2)
                                        (make-pos 0 3) (make-pos 1 3) (make-pos 2 3) (make-pos 3 3) (make-pos 4 3)
                                        (make-pos 0 4) (make-pos 1 4) (make-pos 2 4) (make-pos 3 4) (make-pos 4 4)))


;; Question 2

;; all-orientations: Grid -> (listof Grid)

;; Purpose: To consume a single grid and produce a list of grids that are simply
;; rotations or reflections (as well as combinations of rotations and reflections) 
;; of the given piece, which could be a list of simply the given grid or a list of
;; 8 grids that are variants of the consumed grid.

;; Examples:
(check-expect (lists-equiv? (all-orientations '((#\a #\a #\.)
                                                (#\. #\a #\a)))
                            (list '((#\a #\a #\.)
                                    (#\. #\a #\a))
                                  '((#\. #\a #\a)
                                    (#\a #\a #\.))
                                  '((#\a #\.)
                                    (#\a #\a)
                                    (#\. #\a))
                                  '((#\. #\a)
                                    (#\a #\a)
                                    (#\a #\.)))) true)
(check-expect (lists-equiv? (all-orientations '((#\a #\. #\a)
                                                (#\. #\a #\.)
                                                (#\a #\. #\a)))
                            (list '((#\a #\. #\a)
                                    (#\. #\a #\.)
                                    (#\a #\. #\a)))) true)

;; Definition
(define (all-orientations Polyno)
  (local
    [;; transpose: Grid -> Grid
     ;; Purpose: To consume a Grid and simply transpose
     ;; the grid as if one would transpose a matrix
     (define (transpose mat)
       (cond
         [(empty? (first mat)) empty]
         [else (cons (map first mat)
                     (transpose (map rest mat)))]))
     ;; remove-duplicates: (listof Any) -> (listof Any)
     ;; Purpose: To consume a list and output the identical
     ;; list except with all the duplicates removed, leaving
     ;; only the first occurrence of each unique element.
     (define (remove-duplicates lst)
       (local [;; filter-rest: Any (listof Any) -> (listof Any)
               ;; Purpose: To consume a single value as well as a
               ;; list and remove any occurrence of the given value
               ;; in the list.
               (define (filter-rest a-grid lst)
                 (filter (lambda (x) 
                           (not (equal? a-grid x))) lst))]
         (foldr (lambda (x y) 
                  (cons x (filter-rest x y))) empty lst)))]
    (remove-duplicates
     (list Polyno (transpose Polyno) (reverse Polyno)
           (reverse (transpose Polyno)) (map reverse (reverse Polyno))
           (map reverse Polyno) (map reverse (transpose Polyno))
           (map reverse (reverse (transpose Polyno)))))))

;; Tests:
(check-expect (lists-equiv? (all-orientations '((#\a #\a #\a)))
                            (list '((#\a #\a #\a))
                                  '((#\a)
                                    (#\a)
                                    (#\a)))) true)
(check-expect (lists-equiv? (all-orientations '((#\a #\a #\.)
                                                (#\. #\a #\a)
                                                (#\. #\a #\.)))
                            (list '((#\a #\a #\.)
                                    (#\. #\a #\a)
                                    (#\. #\a #\.))
                                  '((#\. #\a #\a)
                                    (#\a #\a #\.)
                                    (#\. #\a #\.))
                                  '((#\. #\a #\.)
                                    (#\a #\a #\a)
                                    (#\a #\. #\.))
                                  '((#\. #\a #\.)
                                    (#\a #\a #\a)
                                    (#\. #\. #\a))
                                  '((#\. #\a #\.)
                                    (#\a #\a #\.)
                                    (#\. #\a #\a))
                                  '((#\. #\a #\.)
                                    (#\. #\a #\a)
                                    (#\a #\a #\.))
                                  '((#\a #\. #\.)
                                    (#\a #\a #\a)
                                    (#\. #\a #\.))
                                  '((#\. #\. #\a)
                                    (#\a #\a #\a)
                                    (#\. #\a #\.)))) true)


;; Question 3

;; grid-get: Nat Nat Grid -> Char

;; Purpose: To consume two numbers, representing the x and
;; y coordinate of the position, and produce the character
;; in the Grid in that specific position.

;; Examples:
(check-expect (grid-get 0 0 '((#\a #\b)
                              (#\c #\d))) #\a)
(check-expect (grid-get 1 1 '((#\a #\b)
                              (#\c #\d))) #\d)
(check-expect (grid-get 0 1 '((#\a #\b)
                              (#\c #\d))) #\c)

;; Definition
(define (grid-get x y a-Grid)
  (cond
    [(and (zero? x) (zero? y)) (first (first a-Grid))]
    [(zero? x) (grid-get 0 (sub1 y) (rest a-Grid))]
    [else (grid-get (sub1 x) y (map rest a-Grid))]))

;; Tests:
(check-expect (grid-get 0 2 '((#\A #\A #\A #\B #\.)
                              (#\A #\. #\B #\B #\B)
                              (#\. #\B #\B #\C #\C)
                              (#\. #\. #\C #\C #\C))) #\.)
(check-expect (grid-get 2 3 '((#\A #\A #\A #\B #\.)
                              (#\A #\. #\B #\B #\B)
                              (#\. #\B #\B #\C #\C)
                              (#\. #\. #\C #\C #\C))) #\C)
(check-expect (grid-get 4 1 '((#\A #\A #\A #\B #\.)
                              (#\A #\. #\B #\B #\B)
                              (#\. #\B #\B #\C #\C)
                              (#\. #\. #\C #\C #\C))) #\B)
(check-expect (grid-get 2 0 '((#\A #\A #\A #\B #\.)
                              (#\A #\. #\B #\B #\B)
                              (#\. #\B #\B #\C #\C)
                              (#\. #\. #\C #\C #\C))) #\A)


;; first-empty-pos: Grid -> Pos

;; Purpose: To consume a grid and determine the position in
;; which the first empty character (i.e. #\.) occurs.

;; Examples:
(check-expect (first-empty-pos '((#\.))) (make-pos 0 0))
(check-expect (first-empty-pos '((#\a #\. #\b)
                                 (#\. #\. #\.)
                                 (#\. #\. #\.))) (make-pos 1 0))

;; Definition
(define (first-empty-pos a-Grid)
  (local
    [;; fep: (listof Pos) -> Pos
     ;; Purpose: To consume a list of positions
     ;; and determine in which position the first
     ;; empty character #\. occurs.
     (define (fep a-list)
       (cond
         [(empty? a-list) false]
         [(char=? #\. (grid-get (pos-x (first a-list))
                                (pos-y (first a-list))
                                a-Grid))
          (first a-list)]
         [else (fep (rest a-list))]))]
    (fep (all-positions (length (first a-Grid)) (length a-Grid)))))

;; Tests:
(check-expect (first-empty-pos '((#\A #\A #\A #\B #\.)
                                 (#\A #\. #\B #\B #\B)
                                 (#\. #\B #\B #\C #\C)
                                 (#\. #\. #\C #\C #\C))) (make-pos 4 0))
(check-expect (first-empty-pos '((#\A #\A #\A #\B #\B)
                                 (#\A #\. #\B #\B #\B)
                                 (#\. #\B #\B #\C #\C)
                                 (#\. #\. #\C #\C #\C))) (make-pos 1 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\B #\B)
                                 (#\A #\A #\B #\B #\B)
                                 (#\. #\B #\B #\C #\C)
                                 (#\. #\. #\C #\C #\C))) (make-pos 0 2))



;; Question 4

;; superimpose: Grid Grid Pos -> Grid

;; Purpose: to consume two grids and a Pos value representing the 
;; offset and superimpose the second given grid on top of the first
;; grid after the top grid has been translated by the given offset.

;; Examples:
(check-expect (superimpose '((#\a #\b)
                             (#\c #\d))
                           '((#\e)
                             (#\f))
                           (make-pos 0 0))
              '((#\e #\b)
                (#\f #\d)))
(check-expect (superimpose '((#\a #\b)
                             (#\c #\d))
                           '((#\e)
                             (#\f))
                           (make-pos 1 0))
              '((#\a #\e)
                (#\c #\f)))

;; Definition
(define (superimpose base top offset)
  (local
    [;; determining: Nat Nat -> Char
     ;; Purpose: to consume two numbers, representing
     ;; the x and y value of a position, and determines, and
     ;; outputs, the character that needs to occupy that position
     ;; after deciding which grid to extract the value from.
     (define (determining posx posy)
       (cond
         [(or (>= (- posx (pos-x offset)) (length (first top)))
              (< (- posx (pos-x offset)) 0)
              (>= (- posy (pos-y offset)) (length top))
              (< (- posy (pos-y offset)) 0)
              (char=? #\. (grid-get (- posx (pos-x offset))
                                    (- posy (pos-y offset))
                                    top)))
          (grid-get posx posy base)]
         [else (grid-get (- posx (pos-x offset))
                         (- posy (pos-y offset))
                         top)]))]
    (build-2dlist (length (first base)) 
                  (length base)
                  (lambda (x y) (determining x y)))))

;; Tests:
(check-expect (superimpose '((#\A #\A #\A #\B #\B)
                             (#\A #\A #\B #\B #\B)
                             (#\. #\B #\B #\C #\C)
                             (#\. #\. #\C #\C #\C))
                           '((#\0 #\0 #\.)
                             (#\. #\0 #\0)) (make-pos 1 2))
              '((#\A #\A #\A #\B #\B)
                (#\A #\A #\B #\B #\B)
                (#\. #\0 #\0 #\C #\C)
                (#\. #\. #\0 #\0 #\C)))
(check-expect (superimpose '((#\A #\A #\A #\B #\B)
                             (#\A #\A #\B #\B #\B)
                             (#\. #\B #\B #\C #\C)
                             (#\. #\. #\C #\C #\C))
                           '((#\0 #\0 #\.)
                             (#\. #\0 #\0)) (make-pos -5 -7))
              '((#\A #\A #\A #\B #\B)
                (#\A #\A #\B #\B #\B)
                (#\. #\B #\B #\C #\C)
                (#\. #\. #\C #\C #\C)))
(check-expect (superimpose '((#\A #\A #\A #\B #\B)
                             (#\A #\A #\B #\B #\B)
                             (#\. #\B #\B #\C #\C)
                             (#\. #\. #\C #\C #\C))
                           '((#\0 #\0 #\.)
                             (#\. #\0 #\0)) (make-pos 3 -1))
              '((#\A #\A #\A #\B #\0)
                (#\A #\A #\B #\B #\B)
                (#\. #\B #\B #\C #\C)
                (#\. #\. #\C #\C #\C)))



;; Question 5

;; neighbours: State -> (listof State)

;; Purpose: To consume a state, with a specific puzzle-grid and a list
;; of grids representing the remaning pieces yet to be filled in, and produce
;; a list of states representing the next possible direction that the process
;; of solving the puzzle could go. (i.e. a list of states with the puzzle-grid 
;; that has one more piece superimposed and that same piece removed from the
;; list of remaining grids/pieces.

;; Example/Test:
(check-expect (lists-equiv? (neighbours (make-state '((#\A #\A #\A #\. #\.)
                                                      (#\A #\A #\B #\. #\.)
                                                      (#\. #\B #\B #\C #\C)
                                                      (#\. #\. #\C #\C #\C))
                                                    (list '((#\a #\a))
                                                          '((#\b #\.)
                                                            (#\b #\b)))))
                            (list (make-state '((#\A #\A #\A #\a #\a)
                                                (#\A #\A #\B #\. #\.)
                                                (#\. #\B #\B #\C #\C)
                                                (#\. #\. #\C #\C #\C))
                                              (list '((#\b #\.)
                                                      (#\b #\b))))
                                  (make-state '((#\A #\A #\A #\a #\.)
                                                (#\A #\A #\B #\a #\.)
                                                (#\. #\B #\B #\C #\C)
                                                (#\. #\. #\C #\C #\C))
                                              (list '((#\b #\.)
                                                      (#\b #\b))))
                                  (make-state '((#\A #\A #\A #\b #\b)
                                                (#\A #\A #\B #\b #\.)
                                                (#\. #\B #\B #\C #\C)
                                                (#\. #\. #\C #\C #\C))
                                              (list '((#\a #\a))))
                                  (make-state '((#\A #\A #\A #\b #\b)
                                                (#\A #\A #\B #\. #\b)
                                                (#\. #\B #\B #\C #\C)
                                                (#\. #\. #\C #\C #\C))
                                              (list '((#\a #\a))))
                                  (make-state '((#\A #\A #\A #\b #\.)
                                                (#\A #\A #\B #\b #\b)
                                                (#\. #\B #\B #\C #\C)
                                                (#\. #\. #\C #\C #\C))
                                              (list '((#\a #\a)))))) true)

;; Definition
(define (neighbours state)
  (local
    [;; superimposable?: Grid Grid Pos -> Boolean
     ;; Purpose: To consume two grids, the first one representing the base
     ;; and the second one representing a piece, as well as an offset and 
     ;; and outputs a Boolean value that shows whether or not the piece can
     ;; be legally superimposed, meaning it is not out-of-bounds, isn't covering
     ;; any piece of the base grid and the first empty position of the base grid
     ;; is covered.
     (define (superimposable? a-Grid Polyno offset)
       (local
         [;; blocksize: Grid -> Nat
          ;; Purpose: Determines the size of the grid, i.e. the number of positions
          ;; in the grid where it's not empty.
          (define (blocksize a-Grid)
            (length (filter (lambda (x) (not (char=? x #\.))) 
                            (foldr append empty a-Grid))))]
         (cond
           [(or (not (= (blocksize (superimpose a-Grid Polyno offset))
                        (+ (blocksize a-Grid) (blocksize Polyno))))
                (char=? #\. (grid-get (pos-x (first-empty-pos a-Grid))
                                      (pos-y (first-empty-pos a-Grid))
                                      (superimpose a-Grid Polyno offset))))
            false]
           [else true])))
     ;; legal-offset: Grid Grid -> Pos
     ;; Purpose: to consume two grids, first being the base and the second being
     ;; the top piece, and produces the position value which represents the offset
     ;; by which the top piece can be translated to be legally superimposed on top
     ;; of the base grid.
     (define (legal-offset a-Grid Polyno)
       (filter (lambda (offset) (superimposable? a-Grid Polyno offset))
               (all-positions (length (first a-Grid)) (length a-Grid))))
     ;; same-piece?: Grid Grid -> Boolean
     ;; Purpose: To determine if the two given grids are in fact the same piece,
     ;; i.e. simply a rotated/reflected variant of each other.
     (define (same-piece? piece1 piece2)
       (lists-equiv? (all-orientations piece1)
                     (all-orientations piece2)))]
    (foldr 
     (lambda (x y) 
       (cond
         [(empty? (legal-offset (state-puzzle state) x)) y]
         [else (cons (make-state 
                      (superimpose 
                       (state-puzzle state)
                       x
                       (first (legal-offset (state-puzzle state) x)))
                      (filter (lambda (z) (not (same-piece? x z)))
                              (state-pieces state))) y)]))
     empty (foldr (lambda (x y) (append (all-orientations x) y)) 
                  empty (state-pieces state)))))


;; solve-puzzle: Grid (listof Grid) Symbol -> (union (listof String) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.



(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Short Test

(check-expect (solve-puzzle pent-grid-8 pent-pieces-8 'offline)
              (list
               "FFIIIIILZZ"
               "VFFYLLLLZN"
               "VFYYYYXZZN"
               "VVVTWXXXNN"
               "PPPTWWXUNU"
               "PPTTTWWUUU"))