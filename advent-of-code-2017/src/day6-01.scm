(use-modules (ice-9 rdelim))

(define test-bank '(0 2 7 0))

(define (memory-reallocation)
  (define (get-memory-banks)
    (let ((line (read-line)))
      (map (lambda (x) (string->number x)) (string-split line #\tab))))

  ;; (list Integer) (list (list Integer)) Integer -> Integer
  ;; return the amount of redistribution cycles that can be done
  ;; before a repeat configuration is found
  (define (reallocate state history cycles)
    (let ((max-bank (find-nth state (apply max state))))
      (if (member state history)
          cycles
          (reallocate (redistribute state max-bank)
                      (cons state history)
                      (1+ cycles)))))

  ;; (list Integer) Integer -> List
  ;; redistrubute the memory blocks until the given bank is equal to end 
  ;; then return the state
  (define (redistribute state max-bank)
    (let ((state-length (length state)))
      (define (redistribute state next-bank end)
        (if (= (list-ref state max-bank) end)
            state
            (redistribute
             (increase-next (decrease-largest state max-bank) next-bank)
             (get-next next-bank)
             end)))

      ;; Integer
      (define (get-next current-bank)
        (if (< current-bank (1- state-length))
            (1+ current-bank)
            0))

      (redistribute state (get-next max-bank) (if (max-tied? state) 0 1))))

  ;; (list Integer) -> Bool
  ;; check to see if the max element appears twice in a list
  (define (max-tied? state)
    (let ((max-value (apply max state)))
      (> (length
          (filter (lambda (x) (= x max-value)) state))
         1)))

  (define (increase-next state next-bank)
    (modify-nth 1+ state next-bank))

  (define (decrease-largest state max-bank)
    (modify-nth 1- state max-bank))

  (let ((state (get-memory-banks)))
    (reallocate state '() 0)))

(define (main)
  (with-input-from-file "input.day6.txt" memory-reallocation))

;; List Integer -> Integer
;; find the index of a value in a list
(define (find-nth lst value)
  (define (find-nth lst value index)
    (if (= (car lst) value)
        index
        (find-nth (cdr lst) value (1+ index))))
  (find-nth lst value 0))

;; List Integer Integer -> List
;; set the element of a list at the given index to the given value 
(define (set-nth lst index value)
  (if (> index 0)
      (cons (car lst)
            (set-nth (cdr lst) (1- index) value))
      (cons value (cdr lst))))

;; List Integer Fn -> List
;; perform an operation on the value at the given index of a list
(define (modify-nth op lst index)
  (set-nth lst index (op (list-ref lst index))))
