;; -----------------------------------------------------
;; character-value, value-character mapping

(define ctv  
  (lambda (x)
    (cond
      ((eq? x 'a) 0)
      ((eq? x 'b) 1)
      ((eq? x 'c) 2)
      ((eq? x 'd) 3)
      ((eq? x 'e) 4)
      ((eq? x 'f) 5)
      ((eq? x 'g) 6)
      ((eq? x 'h) 7)
      ((eq? x 'i) 8)
      ((eq? x 'j) 9)
      ((eq? x 'k) 10)
      ((eq? x 'l) 11)
      ((eq? x 'm) 12)
      ((eq? x 'n) 13)
      ((eq? x 'o) 14)
      ((eq? x 'p) 15)
      ((eq? x 'r) 16)
      ((eq? x 's) 17)
      ((eq? x 't) 18)
      ((eq? x 'u) 19)
      ((eq? x 'v) 20)
      ((eq? x 'y) 21)
      ((eq? x 'z) 22))))

(define vtc  
  (lambda (x)
    (cond
      ((eq? x 0) 'a)
      ((eq? x 1) 'b)
      ((eq? x 2) 'c)
      ((eq? x 3) 'd)
      ((eq? x 4) 'e)
      ((eq? x 5) 'f)
      ((eq? x 6) 'g)
      ((eq? x 7) 'h)
      ((eq? x 8) 'i)
      ((eq? x 9) 'j)
      ((eq? x 10) 'k)
      ((eq? x 11) 'l)
      ((eq? x 12) 'm)
      ((eq? x 13) 'n)
      ((eq? x 14) 'o)
      ((eq? x 15) 'p)
      ((eq? x 16) 'r)
      ((eq? x 17) 's)
      ((eq? x 18) 't)
      ((eq? x 19) 'u)
      ((eq? x 20) 'v)
      ((eq? x 21) 'y)
      ((eq? x 22) 'z))))


(define reduce
  (lambda (op l id)
    (if (null? l)
       id
       (op (car l) (reduce op (cdr l) id)) )))
