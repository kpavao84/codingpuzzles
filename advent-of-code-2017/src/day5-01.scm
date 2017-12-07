(use-modules (ice-9 rdelim))

(define (maze-steps)
  ;; List -> List
  ;; read the file and return a list of the numbers in the maze
  (define (generate-maze maze)
    (let ((line (read-line)))
      (if (eof-object? line)
          (map (lambda (x) (string->number x)) (reverse maze))
          (generate-maze (cons line maze)))))

  ;; List Inter Integer Integer -> Integer
  ;; traverse the maze until reaching the end
  ;; return the number of steps it took
  (define (traverse-maze maze position steps end)
    (if (>= position end)
        steps
        (traverse-maze (increase-maze-position maze position)
                       (jump maze position)
                       (1+ steps)
                       end)))

  ;; Integer List -> Integer
  ;; return the new position after jumping
  (define (jump maze position)
    (+ position (list-ref maze position)))

  ;; Integer List -> List
  ;; return a list with the given position increased by 1
  (define (increase-maze-position maze position)
    (set-nth maze position
             (if (>= 3 (list-ref maze position))
                 (1- (list-ref maze pozition))
                 (1+ (list-ref maze position)))))

    (let ((maze (generate-maze '())))
      (traverse-maze maze 0 0 (length maze))))

(define (main)
  (with-input-from-file "input.day5.txt" maze-steps))

;; List Integer Integer -> List
;; set the element of a list at the given index to the given value 
(define (set-nth lst index value)
  (if (> index 0)
      (cons (car lst)
            (set-nth (cdr lst) (1- index) value))
      (cons value (cdr lst))))
