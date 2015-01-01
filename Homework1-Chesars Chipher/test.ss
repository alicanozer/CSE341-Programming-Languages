;; some test cases..
;;
;; Run scheme and load with (load "test1.ss")
;;
;; (test4 4) will run the complete test using Gen-Decoder-A (using n=4 for Cipher)
;; (test5 4) will run the complete test using Gen-Decoder-A


(load "decode.ss")

;; test encode-d
(define test1 (lambda () (encode-d test-document (encode-n 3))))

;; test spell-checker
(define test2 (lambda () (spell-checker '(t h i s))))

;; test Gen-Decoder-A
(define test3 (lambda () (Gen-Decoder-A (encode-d test-document (encode-n 3)))))

;; test Code-Breaker with Gen-Decoder-A with everything else
(define test4
  (lambda (n)
    (let 
      ((ds1e (encode-d ds1 (encode-n n)))) ;; first encode the document content in "ds1" using the input n.
      (display ds1) (newline)
      (display ds1e)
      (Code-Breaker ds1e (Gen-Decoder-A (car ds1e))) ;; now decode it
    )))

;; test Code-Breaker with Gen-Decoder-B with everything else
(define test5
  (lambda (n)
    (let 
      ((ds1e (encode-d ds1 (encode-n n)))) ;; first encode the document content in "ds1" using the input n.
      (display ds1) (newline)
      (display ds1e)
      (Code-Breaker ds1e (Gen-Decoder-B (car ds1e))) ;; now decode it
    )))

;; some test data
(define ps1 '( (t e s t) (s o z l u k) (b u) (o l s u n) (t e s t) (s c h e m e) ) )
(define ps2 '( (s c h e m e) (b u) (g e n c) (t e s t) (s o z l u k) ) )

(define ds1 (list ps1 ps2))