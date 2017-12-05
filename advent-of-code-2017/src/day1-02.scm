;; Integer -> Integer
(define (inverse-captcha captcha)
  ;; List Integer -> Integer
  (define (next captcha-list halfway)
    (cond ((null? captcha-list) '())
          ((= halfway 0) (car captcha-list))
          (else (next (cdr captcha-list) (- halfway 1)))))

  ;; List Integer Integer -> Integer
  (define (iter captcha-list current halfway)
    (cond ((null? (next captcha-list halfway)) (* 2 current))
          ((= (car captcha-list) (next captcha-list halfway))
           (iter (cdr captcha-list) (+ (car captcha-list) current) halfway))
          (else
           (iter (cdr captcha-list) current halfway))))

  (let* ((captcha-list (number->list captcha))
         (halfway (/ (length captcha-list) 2)))
    (iter captcha-list 0 halfway)))

;; from https://stackoverflow.com/questions/8014453/convert-number-to-list-of-digits
(define (number->list n . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((n n) (d '()))
      (if (zero? n) d
          (loop (quotient n b)
                (cons (modulo n b) d))))))

;; tests
(= (inverse-captcha 1212) 6)
(= (inverse-captcha 1221) 0)
(= (inverse-captcha 123425) 4)
(= (inverse-captcha 123123) 12)
(= (inverse-captcha 12131415) 4)
