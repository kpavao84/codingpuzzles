(use-modules (ice-9 rdelim))

(define (parse-input)
  ;; List -> (list (list String Integer ListOfInteger))
  (define (generate-programs programs)
    "read the input and create a list of programs"
    (let* ((line (read-line)))
      (if (eof-object? line)
          programs
          (generate-programs (cons (get-program-info line)
                                 programs)))))

  ;; String -> (list String Integer ListOfString)
  (define (get-program-info input)
    "parse the input into a list of strings to make it easier to create programs"
    (let ((split (string-split input #\space)))
      (define (get-name)
        (car split))
      (define (get-weight)
        (string-drop-right (string-drop (cadr split) 1) 1))
      (define (get-disc)
        (if (> (length split) 2)
            (map (λ (c) (string-trim-right c #\,)) (cdddr split))
            '()))
      (list (get-name) (get-weight) (get-disc))))

  (generate-programs '()))

(define (main)
  (let* ((programs (with-input-from-file "input.day7.txt" parse-input))
         (program-list (map (λ (x) (car x)) programs))
         (disc-list (create-disc-list programs)))
    (set-difference program-list disc-list)))

;; (list (list String Number ListOfString)) -> ListOfString
(define (create-disc-list programs)
  "create a list of all the programs in discs in a given list of programs"
  (let ((holding-discs (map (λ (x) (car x))
                            (filter (λ (x) (> (length (caddr x)) 0)) programs))))
    (flatten (map (λ (x) (caddr x))
                  (filter (λ (x) (member (car x) holding-discs)) programs)))))

;; helpers
(define (set-difference s1 s2)
  (cond ((null? s1) '())
        ((not (member (car s1) s2)) (cons (car s1) (set-difference (cdr s1) s2)))
        (else (set-difference (cdr s1) s2))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))
