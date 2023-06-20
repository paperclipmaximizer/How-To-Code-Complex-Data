;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze-2w-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Solve simple square mazes

;; maze-v1.rkt


;; Constants:

;; Data definitions:

;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;;



(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3            
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower left   "  "   "

;; 1. each side length is (sqrt (length <maze>)) -> It is arbatrarily wide and long
;; 2. each element is t/f
#;
(define (fn-for-b b)
  (... b))
;; Element is bool
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) ...]
        [else
         (... (first lob)
              (fn-for-lob (rest lob)))]))
;; ListOfBoolean is one of:
;; - empty
;; - (cons Boolean (listof Boolean))
#;
(define (fn-for-p p)
  (... (pos-x p)
       (pos-y p)))
;; Pos is struct

;; We know:
;; Maze has a set of next-mazes that are (- x 1) shorter in the case of right move and (- y 1) in case of downward move
;; the arity is [0,2]
;; The next-mazes have to be generated
;; gen-rec
;; backtracking search over the generated mazes?

;; Functions:

;; Maze -> Boolean
;; consumes maze and produces true if solvable
;; solvable means can move a combination of right or down to reach end
;; ASSUME: maze is valid

(check-expect (solvable? M4) #f)
(check-expect (solvable? M0) #f)
(check-expect (solvable? M1) #t)

(define (solvable? m) 
  ;<template from arbitrary-arity tree, for m and lom>
  (local [(define (solve--p p)                 ;a maze
            (if (solved? p m)
                #t
                (solve--lop (next-posz m p))))     ;m-subs doesnt exist because maze doesn't contain a list of mazes so we have to generate the list live

          (define (solve--lop lop)                ;a list of mazes (0,1 or 2) long
            (cond [(empty? lop) #f]
                  [else
                   (local [(define try (solve--p (first lop)))]
                     (if (not (false? try))
                         try
                         (solve--lop (rest lop))))]))]
    (solve--p (make-pos 0 0))))

;; Pos Maze -> Boolean
;; !!!
(define (solved? p m)
  (if (and (= (sqrt (length m))
              (pos-x p))
           (= (sqrt (length m))
              (pos-y p)))
      (mref m p)
      #f))

;; Maze Pos -> (pairof Pos) 
;; generate 2 mazes
(check-expect (next-posz M0 (make-pos 0 0)) empty)
              
(check-expect (next-posz M1 (make-pos 0 0)) (list (make-pos 0 1)))

(define (next-posz m p)
  (keep-only-valid (next-pz p) m))

;; Pos Maze -> Pos
;; consumes Pos Maze and produces length and height of the final maze
(check-expect (next-pz (make-pos 0 0)) (list (make-pos 1 0)
                                             (make-pos 0 1)))

(define (next-pz p)
  (list (make-pos (add1 (pos-x p)) (pos-y p))
        (make-pos (pos-x p) (add1 (pos-y p)))))

;; (listof pos) Maze -> (listof Pos) | Empty
;; consumes pair of pos's and maze and checks if they are still in range or false
(check-expect (keep-only-valid (list (make-pos 1 0)
                                     (make-pos 0 1)) M0) empty)
(check-expect (keep-only-valid (list (make-pos 1 0)
                                     (make-pos 0 1)) M1) (list (make-pos 0 1)))
(define (keep-only-valid pp m)
         (local [(define (oob? p)
                   (and (or (<= (pos-x p)(sqrt (length m)))
                            (<= (pos-y p)(sqrt (length m))))
                        (mref m p)))]
           (filter oob? pp)))

;; Maze Pos -> Boolean
;; produce contents of given square in given maze
(check-expect (mref (list #t #f
                          #f #f) (make-pos 0 0)) #t)
  
(check-expect (mref (list #t #t
                          #f #f) (make-pos 0 1)) #f)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))