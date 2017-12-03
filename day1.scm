;; Integer -> Integer
(define (inverse-captcha captcha)
  ;; List -> Bool
  (define (first-last-match? captcha-list)
    (= (car captcha-list) (car (reverse captcha-list))))

  ;; List Integer -> Integer
  (define (iter captcha-list current)
    (cond ((null? (cdr captcha-list)) current)
          ((= (car captcha-list) (cadr captcha-list))
           (iter (cdr captcha-list) (+ (car captcha-list) current)))
          (else
           (iter (cdr captcha-list) current))))

  (if (first-last-match? (number->list captcha))
      (iter (number->list captcha) (car (number->list captcha)))
      (iter (number->list captcha) 0)))

;from https://stackoverflow.com/questions/8014453/convert-number-to-list-of-digits
(define (number->list n . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((n n) (d '()))
      (if (zero? n) d
          (loop (quotient n b)
                (cons (modulo n b) d))))))

;; tests
(= (inverse-captcha 1122) 3)
(= (inverse-captcha 1111) 4)
(= (inverse-captcha 1234) 0)
(= (inverse-captcha 91212129) 9)
