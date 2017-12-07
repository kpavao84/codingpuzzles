(use-modules (ice-9 rdelim))

(define (maze-steps)
  ;; List -> List
  ;; read the file and return a list of the numbers in the maze
  (define (generate-maze maze)
    (let ((line (read-line)))
      (if (eof-object? line)
          (map (lambda (x) (string->number x)) (reverse maze))
          (generate-maze (cons line maze)))))

  ;; List Integer Integer Integer -> Integer
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
    (let ((offset (list-ref maze position)))
      (set-nth maze position (if (>= offset 3)
                                 (- offset 1)
                                 (1+ offset)))))

  ;; start traversing the maze
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
