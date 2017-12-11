(use-modules (ice-9 rdelim)
             (srfi srfi-9 gnu))

;; program is (make-program String Integer ListOfProgram)
;; a program is a tower of programs balanced on discs in a trie structure
(define-record-type <program>
  (make-program name weight disc)
  program?
  (name      program-name)
  (weight   program-weight   set-program-weight)
  (disc program-disc set-program-disc))

;; the input:
;; pbga (66)
;; xhth (57)
;; ebii (61)
;; havc (66)
;; ktlj (57)
;; fwft (72) -> ktlj, cntj, xhth
;; qoyq (66)
;; padx (45) -> pbga, havc, qoyq
;; tknk (41) -> ugml, padx, fwft
;; jptl (61)
;; ugml (68) -> gyxo, ebii, jptl
;; gyxo (61)
;; cntj (57)
;; should turn into this:

(define pbga (make-program "pbga" 66 '()))
(define xhth (make-program "xhth" 57 '()))
(define ebii (make-program "ebii" 61 '()))
(define havc (make-program "havc" 66 '()))
(define ktlj (make-program "ktlj" 57 '()))
(define qoyq (make-program "qoyq" 66 '()))
(define jptl (make-program "jptl" 61 '()))
(define gyxo (make-program "gyxo" 61 '()))
(define cntj (make-program "cntj" 57 '()))
(define fwft (make-program "fwft" 72 (cons ktlj (cons cntj (cons xhth '())))))
(define padx (make-program "padx" 45 (cons pbga (cons havc (cons qoyq '())))))
(define ugml (make-program "ugml" 68 (cons gyxo (cons ebii (cons jptl '())))))
(define tknk (make-program "tknk" 41 (cons ugml (cons padx (cons fwft '())))))

;; (list Program) -> String
;; return the name of the heaviest tower
(define (heaviest-tower towers)
  (define (find-heaviest towers heaviest)
    (cond ((null? towers) (program-name heaviest))
          ((> (total-weight (car towers))
              (total-weight heaviest))
           (find-heaviest (cdr towers) (car towers)))
          (else (find-heaviest (cdr towers) heaviest))))
  (find-heaviest towers (car towers)))

;; Program -> Integer
;; calculate the total weight of a tower of programs
(define (total-weight tower)
  (define (add-program n)
    (+ (program-weight n)
       (add-discs (program-disc n))))
  (define (add-discs disc)
    (cond ((null? disc) 0)
          (else
           (+ (add-program (car disc))
              (add-discs (cdr disc))))))
  (add-program tower))
