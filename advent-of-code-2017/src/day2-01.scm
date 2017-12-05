
(use-modules (ice-9 rdelim))

(define (corruption-checksum)
  (define (calculate-row row)
    (- (apply max row) (apply min row)))

  (define (calculate-checksum sum)
    (let ((row (read-line)))
      (if (eof-object? row)
          sum
          (calculate-checksum (+ sum
                                 (calculate-row (map (lambda (x) (string->number x))
                                                     (string-split row #\tab))))))))

  (calculate-checksum 0))

(define (main)
  (with-input-from-file "input.day2.txt" corruption-checksum))
