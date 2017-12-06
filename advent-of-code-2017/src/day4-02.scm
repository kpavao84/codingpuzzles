
(define (high-entropy-passphrases)
  (define (parse-row row)
    (alphabetize-list (string-split row #\space)))

  (define (valid? row)
    (cond ((null? row) #t)
          ((member (car row) (cdr row)) #f)
          (else (valid? (cdr row)))))

  (define (alphabetize-list words)
    (map (lambda (word) (sort-word word)) words))

  (define (sort-word word)
    (accumulate string-append ""
                (map (lambda (letter) (string letter))
                     (sort (string->list word) char-ci<?))))

  (define (sum-valid-passphrases sum)
    (let ((row (read-line)))
      (if (eof-object? row)
          sum
          (sum-valid-passphrases (+ sum
                                    (if (valid? (parse-row row)) 1 0))))))

  (sum-valid-passphrases 0))

(define (main)
  (with-input-from-file "input.day4.txt" high-entropy-passphrases))

;; from SICP
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
