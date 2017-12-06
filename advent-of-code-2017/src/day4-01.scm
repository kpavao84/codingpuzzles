
(define (high-entropy-passphrases)
  (define (parse-row row)
    (string-split row #\space))

  (define (valid? row)
    (cond ((null? row) #t)
          ((member (car row) (cdr row)) #f)
          (else (valid? (cdr row)))))

  (define (sum-valid-passphrases sum)
    (let ((row (read-line)))
      (if (eof-object? row)
          sum
          (sum-valid-passphrases (+ sum
                                    (if (valid? (parse-row row)) 1 0))))))

  (sum-valid-passphrases 0))

(define (main)
  (with-input-from-file "input.day4.txt" high-entropy-passphrases))
