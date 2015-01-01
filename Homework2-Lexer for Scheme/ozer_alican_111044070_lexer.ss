;;
;; A lex for subset of the scheme language.
;; filename:ozer_alican_111044070_lexer.ss
;; Alican OZER
;; Nu:111044070
;; Programming-Languages-Homework 2
;; Lexical Analyzer for subset of Scheme Programming Language
;; -------------------------------------------
;; Helper functions ...
;; definition of variables and values
;; INPUT: a string
;; OUTPUT: #t if found,#f otherwise
(define (findMatch token)
	(cond
		((equal? "(" token) (display " ( LPAREN '(' )\n") #t)
		((equal? ")" token) (display " ( RPAREN ')' )\n") #t)
		((equal? "0" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "1" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "2" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "3" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "4" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "5" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "6" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "7" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "8" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "9" token) (display " ( INTEGER_LITERAL '" )(display token) (display "')\n") #t)
		((equal? "\""token) (display " ( STRING_LITERAL '"  )(display token) (display "')\n") #t)
		((equal? "quote" token) (display " ( QUOTE_KEYWORD 'quote' )\n") #t)
		((equal? "and" token) (display " ( AND_KEYWORD 'and' )\n") #t)
		((equal? "lambda" token) (display " ( LAMBDA_KEYWORD 'lambda' )\n") #t)
		((equal? "if" token) (display " ( IF_KEYWORD 'if' )\n") #t)
		((equal? "define" token) (display " ( DEFINE_KEYWORD 'define' )\n") #t)
		((equal? "or" token) (display " ( OR_KEYWORD 'or' )\n") #t)
		((equal? "not" token) (display " ( NOT_KEYWORD 'not' )\n") #t)
		((equal? "#t" token) (display " ( BOOLEAN_LITERAL '#t' )\n") #t)
		((equal? "#f" token) (display " ( BOOLEAN_LITERAL '#f' )\n") #t)
		((equal? " " token ) #t)
		(else #f)
))
;--------------------------------------
;helper function for lexer function
;INPUT str: a string
;INPUT ind1 and ind2 : begin and end index
;INPUT range; size of substring for compare with define variables
;INPUT max; size of given string for reached end of string
;returns #t ; finished lexical analysing
(define lex (lambda (str ind1 ind2 range max)
	(cond
		((= ind2 max) (findMatch ")") #t) ;kapat ve bitir
		((equal? (substring str ind1 ind2) " ") (lex str ind2 (+ ind2 1) 1 max))
		((or (equal? (substring str ind2 (+ ind2 1)) " ") (equal? (substring str ind2 (+ ind2 1)) ")"));yanındaki boşluksa
			(cond
				((> range 6)
					(display " ( IDENTIFIER '")
					(display (substring str ind1 ind2))
					(display "' )\n") 
					(lex str ind2 (+ ind2 1) 1 max);sıfırla devam et
				)(else ((cond
						((not (equal? (string->number (substring str ind1 ind2)) #f));sayı ise
							(cond 
								((= range 1) (findMatch (substring str ind1 ind2));sıfırla devam et
									(lex str ind2 (+ ind2 1) 1 max))
								(else
									(display " ( INTEGER_LITERAL '")
									(display (substring str ind1 ind2))
									(display "' )\n")			;sıfırla devam et 
									(lex str ind2 (+ ind2 1) 1 max))))
						((eq? (findMatch (substring str ind1 ind2)) #t) ;listede varsa
								(lex str ind2 (+ ind2 1) 1 max);sıfırla devam et
						)
						((= range 1) ;sayı değil ve listede yok ise
							(display " ( identifier-character '")
							(display (substring str ind1 ind2) )
							(display "' )\n")
							(lex str ind2 (+ ind2 1) 1 max);sıfırla devam et							
						)(else
							(display " ( IDENTIFIER '")
							(display (substring str ind1 ind2))
							(display "' )\n");sıfırla devam et
							(lex str ind2 (+ ind2 1) 1 max)))))));yanıdaki boşluk yada parantez değilse okumaya devam et
		(else (cond
				((equal? (substring str ind1 ind2) "(")
					(findMatch (substring str ind1 ind2))
					(lex str ind2 (+ ind2 1) 1 max);sıfırla devam et
				)
				((equal? (substring str ind1 ind2) ")") 
					(findMatch (substring str ind1 ind2))
					(lex str ind2 (+ ind2 1) 1 max))
				(else (lex str ind1 (+ ind2 1) (+ range 1) max )))))))
;---------------------------------------------------------------------------
;; Reads a program from the string argument and returns the token type and the corresponding lexeme...
(define lexer 
  	(lambda (p)
	  	(lex p 0 1 1 (string-length p))
	  	(display "\nLexical Analyzing Finished...\n")
))
;;-------------------ozer_alican_111044070_lexer.ss---------------------------
;//////some test
(define mytest "(define spell-checker (lambda (w) (if (or (member 158 dictionary) #f) note #t)))")
(define mytest2 "(define factorial (lambda (n) (if (= n 1) 1 (* n (factorial (- n 1))))))")