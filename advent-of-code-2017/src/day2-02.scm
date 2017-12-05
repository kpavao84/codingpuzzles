(use-modules (ice-9 rdelim))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (corruption-checksum)
  (define (calculate-row row)
    (car (filter (lambda (x) (> x 0))
                 (flatten
                  (map (lambda (i)
                         (map (lambda (j)
                                (if (and (not (= i j)) (= (modulo i j) 0)) 
                                    (/ i j)
                                    0))
                              row))
                       row)))))

  (define (parse-row row)
    (map (lambda (x) (string->number x))
         (string-split row #\tab)))

  (define (calculate-checksum sum)
    (let ((row (read-line)))
      (if (eof-object? row)
          sum
          (calculate-checksum (+ sum
                                 (calculate-row (parse-row row)))))))

  (calculate-checksum 0))

(define (main)
  (with-input-from-file "input.day2.txt" corruption-checksum))
