    
    ;;ozer_alican_111044070_decode.ss
    ;;PROGRAMLAMA DILLERI HOMEWORK 1 SEZARIN SIFRESI
    ;;KULLANMA KILAVUZU
    ;;GEN-DECODER-A VE GEN-DECODER-B bir dokuman alır(paragraf degil)
    ;;bu yuzden test.ss dosyasındaki test4 ve test5 kısımlarında bulunan
    ;;ONEMLi::(car ds1e) kısımlarını direkt olarak ds1e olarak DEGiSTiRiNiZ 



    ;; contains "ctv", "vtc",and "reduce" definitions
    (load "include.ss")

    ;; contains a test document consisting of a number of paragraphs. 
    (load "document.ss")

    ;; contains a test-dictionary, which has a much smaller dictionary for testing
    ;; the dictionary is needed for spell checking
    ;;(load "test-dictionary.ss")

     (load "dictionary.ss") ;; the real thing with 18,000 words


    ;; -----------------------------------------------------
    ;; HELPER FUNCTIONS

    ;; *** YOUR CODE FOR ANY HELPER FUNCTION GOES HERE ***

    ;;map fonksiyonunun döngü sayısını ayarlamak icin alfabe
    (define alphabet '(a b c d e f g h i j k l m n o p r s t u v y z))

    
    ;;verilen dokumandaki kelimeler ile sözlüğü karşılaştırır
    ;;eğer sözlükte varsa yerine 1 yoksa 0 yazar
    ;;INPUT: a document
    ;;OUTPUT: a list that includes 1's and 0's
    (define getlength 
        (lambda (dcmnt)
            (map (lambda (doc)
                (map (lambda (sntnce)
                    (if
                        (eq? (member sntnce dictionary) #f)
                        0
                        1
                    )
                    )doc
                )               
                )dcmnt
            )
    ))

    ; -----------------------------------------------------
    ;;verilen sayı listesinin elemanlarını toplar
    ;;INPUT: a number list(only numbers)
    ;;OUTPUT a number that sum of number list
    (define sumlist 
        (lambda (doc)
        (cond
            ((null? doc) 0)
            ((pair? (car doc)) (+(sumlist (car doc)) (sumlist (cdr doc))))
            (else (+ (car doc) (sumlist (cdr doc))))
        )
    ))

    ; -----------------------------------------------------
    ;;her shift işlemi için uyuşan kelime sayısını list olarak döndürür
    ;;INPUT:a document
    ;;OUTPUT:a number list(that how many times used each word)
    (define shiftchecker
    (lambda (freq)
        (map
            (lambda (lttr)
                (sumlist(getlength(encode-d freq (encode-n (ctv lttr)))))
            )alphabet
        ) 
    ))

    ; -----------------------------------------------------
    ;;verilen listedeki en buyuk elemanı(değerini) dondurur
    ;;INPUT:a number list
    ;;OUTPUT: max element of given list
    (define findmaxoflist
        (lambda (lst)
            (findmaxoflist2 (cdr lst) (car lst))
    ))
    ;;wrapper function of above 
    (define findmaxoflist2
        (lambda (lst buyuk)
            (cond
                ((equal? '() lst) buyuk)
                (else (findmaxoflist2 (cdr lst) (max (car lst) buyuk))))

    ))

    ; -----------------------------------------------------
    ;;verilen elemanın listede kaçıncı index te olduğunu döndürür
    ;;INPUT :a list ,a number,a number(1)
    ;;OUTPUT:an number that indexof given number in the given list
    (define indexbul
    (lambda (lst enbuyuk index)
        (cond 
            ((equal? (car lst)  '())   index)
            ((equal? (car lst) enbuyuk) index)
            (else (indexbul (cdr lst) enbuyuk (+ index 1)))

        )


    ))

    ; -----------------------------------------------------
    ;;en çok kullanılan harfi bulmak için GEN-DECODER-B de kullanılacak
    ;;dokuman icinde verilen harfin verilen listede bulunduğu yere 1 atar
    ;;INPUT: a document list and a letter
    ;;OUTPUT: a list that includes only 1's and 0's
    (define harfsayisibul
        (lambda (lst harf)
            (define count)
            (map (lambda (paragraph);;document to paragraph
                (map (lambda (word);;paragraph to word
                    (map (lambda (letter);;word to letter
                        (cond
                            ((equal? letter harf) 1)
                            (else 0)
                            )
                        )word
                    )
                    )paragraph
                )   
                )lst
            )
    ))

    ; -----------------------------------------------------
    ;;verilen documanda hangi harften kactane varsa onu dondurur
    ;;INPUT:a document list for frequance analyser
    ;;OUTPUT:a number list (that each letter used how many times)
    (define harfleritopla
        (lambda (lst)
            (map (lambda (letters)
                    (sumlist (harfsayisibul lst letters))
                )alphabet
            )
    ))

    ; -----------------------------------------------------
    ;;harfsayisibul fonksiyonun dictionary ile kullanılması için
    ;;INPUT:a paragraph list and a letter
    ;;OUTPUT:a paragraph list that includes only 1's and 0's
    (define harfsayisibul-dic
        (lambda (lst harf)
            (define count)
                (map (lambda (word)
                    (map (lambda (letter)
                        (cond
                            ((equal? letter harf) 1)
                            (else 0)
                            )
                        )word
                    )
                    )lst
                )   
    ))

    ; -----------------------------------------------------
    ;;harfleritopla fonksiyonunun dictionary ile kullanılması için
    ;;INPUT:a paragraph list
    ;;a number list (that each letter used how many times)
    (define harfleritopla-dic
        (lambda (lst)
            (map (lambda (letters)
                    (sumlist (harfsayisibul-dic lst letters))
                )alphabet
            )
    ))   


    ;; -----------------------------------------------------
    ;; SPELL CHECKER FUNCTION

    ;;check a word's spell correctness
    ;;INPUT:a word(a global variable "dictionary" is included 
    ;;in the file "test-dictionary.ss", and can be used directly here)
    ;;OUTPUT:true(#t) or false(#f)
    (define spell-checker
        (lambda (w)
            (if
                (eq? (member w dictionary) #f)
                #f
                #t
            )
        )
    )

    ;; -----------------------------------------------------
    ;; ENCODING FUNCTIONS

    ;;generate an Caesar Cipher single word encoder
    ;;INPUT:a number "n"
    ;;OUTPUT:a function, whose input=a word, output=encoded word
    (define encode-n
        (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
            (lambda (w);;"w" is the word to be encoded
                (map 
                    (lambda (lttr)
                        (vtc (modulo (+ (ctv lttr) n) 23 ))
                    )w
                )
            )
        )
    )
;;----------------------------------------------------------------
    ;;encode a document
    ;;INPUT: a document "d" and a "encoder"
    ;;OUTPUT: an encoded document using a provided encoder
    (define encode-d;;this encoder is supposed to be the output of "encode-n"
        (lambda (d encoder)
            (map 
                (lambda (sntnce);;paragraph to sentences
                    (map
                        (lambda (wrd);;sentence to words
                            (encoder wrd)
                        )sntnce     ;;sentences
                    )      
                )d          ;;paragraphs
            )            
        )        
    )
    ;;map ile parçaladığımız paragraftaki cümleyi 
    ;;kelimelere ayırmak için tekrar mapa yolluyoruz

    ;; (encode-d test-document (encode-n 4))
        
;;----------------------------------------------------------------
    ;; DECODE FUNCTION GENERATORS
    ;; 2 generators should be implemented, and each of them 
    ;;returns a decoder

    ;generate a decoder using brute-force-version spell-checker
    ;;INPUT:an encoded paragraph "p"
    ;;OUTPUT:a decoder, whose input=a word, output=decoded word
    (define Gen-Decoder-A
      (lambda (p)
        (define encodedlist p);;kodlanmıs listeyi alıyoruz
        (define numberlist (shiftchecker p));;shift edip kelimeleri kontrol ediyoruz
        (define maxelement (findmaxoflist numberlist));;en fazla uyusma sayısını alıyoruz
        (define index (indexbul numberlist maxelement 1));;uyusmanın shift edilme sayısını buluyoruz

        ;;dokumentın kac adım shift edildigini biliyoruz artık
        ;;orjinalini bulmak icin geri shift ediyoruz
        (define original-doc (encode-d encodedlist (encode-n (- index 1))))
        original-doc
            

    ))

    ;;DOKUMAN BUYUDUKCE DOGRU SONUC BULMA IHTIMALI KESINLIKLE ARTAR
    ;;LUTFEN BUYUK DOKUMANLAR ICIN KULLANINIZ
    ;;generate a decoder using frequency analysis
    ;;INPUT:same as above
    ;;OUTPUT:same as above
    (define Gen-Decoder-B
      (lambda (p)
        (define harfsayilari (harfleritopla p));;;harflarin sayılarını tutan liste
        (define maxharf (findmaxoflist harfsayilari));en cok kullanılan harften kactane kullanılmış
        
        ;encok kullanılan harfin alfabedeki sırası
        (define harfindex (indexbul harfsayilari maxharf 1))

        ;verilen sözlükteki ençok kullanılan harfin sırası
        (define orj-harfler (harfleritopla-dic dictionary))
        (define orj-index (indexbul orj-harfler (findmaxoflist orj-harfler) 1))
        (define jumpcount (- harfindex orj-index))
        (define original-doc (encode-d p (encode-n (- 23 jumpcount))))
        original-doc
    

        ))

    ;; -----------------------------------------------------
    ;; CODE-BREAKER FUNCTION

    ;;a codebreaker
    ;;INPUT: an encoded document(of course by a Caesar's Cipher), 
    ;;a decoder(generated by functions above)
    ;;OUTPUT: a decoded document
    (define Code-Breaker
      (lambda (d decoder)
        
         decoder
         
         ))

    ;; -----------------------------------------------------
    ;; EXAMPLE APPLICATIONS OF FUNCTIONS
    ;;(spell-checker '(h e l l o))
    ;;(define add5 (encode-n 5))
    ;;(encode-d document add5)
    ;;(define decoderSP1 (Gen-Decoder-A document))
    ;;(define decoderFA1 (Gen-Decoder-B document))
    ;;(Code-Breaker document decoderSP1)