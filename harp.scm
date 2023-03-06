#!/usr/bin/chezscheme

; C# - Cs, Cb - Cb
(define tones '(
	(C . 0) (Bs . 0)
	(Cs . 1) (Db . 1)
	(D . 2)
	(Ds . 3) (Eb . 3)
	(E . 4)
	(F . 5)
	(Fs . 6) (Gb . 6)
	(G . 7)
	(Gs . 8) (Ab . 8)
	(A . 9)
	(As . 10) (Bb . 10)
	(B . 11) (Cb . 11)
))
(define (tone t) (cdr (assoc t tones)))
(define (tone-rev n) (cdr (assoc n (map (lambda(k) (cons (cdr k) (car k))) tones))))

(define ionian			'(2 2 1 2 2 2 1)) ; major
(define dorian			'(2 1 2 2 2 1 2))
(define phrygian		'(1 2 2 2 1 2 2))
(define lydian			'(2 2 2 1 2 2 1))
(define mixolydian		'(2 2 1 2 2 1 2))
(define aeolian			'(2 1 2 2 1 2 2)) ; minor (natural)
(define locrian			'(1 2 2 1 2 2 2))

(define major 			ionian)
(define natural-minor 		aeolian)
(define harmonic-minor 		'(2 1 2 2 1 3 1))
(define blues '(3 2 1 1 3 2))
(define jazz '(2 1 2 2 2 2 1))

(define (M-show mod beg) (list-head (reverse (map tone-rev 
	(fold-left (lambda(v p) (cons (modulo (+ p (car v)) 12) v)) (list beg) mod)))
	(length mod)))
(define (M-totones Notelist) (map (lambda(a b) (abs (- 12 (- (+ a (if (> a b) 0 12)) b))))
	(map tone Notelist)
	(map tone (append (list-tail Notelist 1) (list-head Notelist 1)))))

; если i - C то d - D, p - E, l - F, m - G, a - A, l - B
(define natural-modes (list ionian dorian phrygian lydian mixolydian aeolian locrian))
(define natural-modes-names '(ionian dorian phrygian lydian mixolydian aeolian locrian))
(define (M-by beg) (map (lambda(m n t) (list t n (M-show m (tone t)))) natural-modes natural-modes-names 
	(M-show (car natural-modes) beg)))
; функция струпеней (аккордов построеных от них) лада
;I	тоника
;II	нисходящий вводной звук
;III	медианта
;IV	субдоминатна
;V	домананта
;VI	субмедианта
;VII	восходящий вводной звук

(define Interv '(
	(- (6 0 тритон)
	)(ч
		(0 1 унисон)
		(5 4	кварта)
		(7 5 квинта)
		(12 8 октава)
	)(м
		(1 2 секунда)
		(3 4 терция)
		(8 6 секста)
		(10	7 септима)
	)(б
		(2 2 секунда)
		(4 4 терция)
		(9 6 секста)
		(11	7 септима)
)))
(define (find-interv Notes) #f)

(define Akk2 (map (lambda(a) (cons (cdr a) (car a))) '(
	(ч1	1)
	(m2	1)
	(M2	2)
	(m3	3)
	(M3	4)
	(ч4	5)
	(4/5 6)
	(ч5	7)
	(m6	8)
	(M6	9)
	(m7	10)
	(M7	11)
	(ч8	12)
)))
(define Akk3 (map (lambda(a) (cons (cdr a) (car a))) '(
	(dim	3 6) ; m3 + m3
	(m	3 7) ; m3 + M3
	(M	4 7) ; M3 + m3
	(aug	4 8) ; M3 + M3
	(sus2 2 7)
	(sus4 5 7)
; какието новые аккорды
;	(s 7 11) ; трезвучие 2 четверти
;	(S 7 8) ; трезвучие 4 четверти
)))
(define Akk4 (map (lambda(a) (cons (cdr a) (car a))) '(
; Септакорды
	(M7	4 7 10) ; Akk + m3
	(m7	3 7 10)
	(dim7	3 6 9)
	(aug7	4 8 11)

	(M7maj	4 7 11) ; Akk + M3
	(m7maj	3 7 11)
	(dim7maj	3 6 10)

	(M7-5	4 10)
	;(dim7s	3 9)
	(m7-5	3 10)
)))
(define Akkords (append Akk4 Akk3 Akk2))


(define (roundN N) (lambda(d) (modulo (* N d) 12)))
(define (matrix l . axs)
	(do ((i 0 (+ i 1)) (outs (make-vector (car axs)) outs)) ((= i (car axs)) outs)
		(do ((j 0 (+ j 1)) (out (make-vector (cadr axs)) out)) ((= j (cadr axs)) (vector-set! outs i out))
			(vector-set! out j (l i j)))))

; научная теория музыки
(define (intmat By) (lambda(r5 u4) (tone-rev ((roundN 5) (-((roundN 4) (- (tone By) u4)) r5)))))
(define Chords3 '(
	(M ((0 0) (0 1) (1 0)))
	;(aug ((0 0) (1 0) (-1 0)))
	(m ((0 0) (1 -1) (1 0)))
	;(dim ((0 0) (1 -1) (2 -2)))
	(S ((0 0) (0 -1) (1 0)))
	(s ((0 0) (1 1) (1 0)))
))
(define Chords4 '(
	(M ((0 0) (0 1) (1 0)))
	;(aug ((0 0) (1 0) (-1 0)))
	(m ((0 0) (1 -1) (1 0)))
	;(dim ((0 0) (1 -1) (2 -2)))
	;(S ((0 0) (0 -1) (1 0)))
	;(s ((0 0) (1 1) (1 0)))
))
(define (chord-by chord By) (map (lambda(c) (apply (intmat By) c)) (if (symbol? (car chord)) (cadr chord) chord)))
; квинтовый круг
(define (round7 dt) (map tone-rev (map (lambda(d) (modulo (* 7 (+ d dt)) 12)) (iota 12))))
(define (penta dt) (list-head (round7 dt) 5))

(define (St->tones St) (map (lambda(h) (map (lambda(t) (if (boolean? t) t (tone t))) h)) St))
(define (tones->St tns dt) (map (lambda(h) (map (lambda(t) (if (boolean? t) t (tone-rev (modulo (+ t dt) 12)))) h)) tns))
(define (St-blow St) (map (lambda(h) (cdr (member #f (reverse h)))) St))
(define (St-draw St) (map (lambda(h) (cdr (member #f h))) St))
(define (St-musk Stb Sta) ; from b to a 
	(define (pts ta tb) (display (- (car ta) (car tb))) (display "\t"))
	(define (mi f . als) (let rc((o als)) (if (null? (car o)) #f (begin (apply f (map car o)) (rc (map cdr o))))))
	(mi pts (St-blow (St->tones Sta)) (St-blow (St->tones Stb))) (newline)
	(mi (lambda(i) (display (+ i 1)) (display "\t")) (iota (length Sta))) (newline)
	(mi pts (St-draw (St->tones Sta)) (St-draw (St->tones Stb))) (newline))
(define (list-out out lst) (let rec ((i out) (ost lst)) (cond 
	((null? ost) '()) 
	((= 0 i) (cdr ost)) 
	(else (cons (car ost) (rec (- i 1) (cdr ost)))))))

(define (print . X) (let rc((x X)) (unless (null? x) (begin (display (car x)) (rc (cdr x))))) (newline) X)
(define (dls lst d) (append (list-tail lst d) (list-head lst d)))
(define (eqp? a b) (let rec ((i (length a))) (if (= 0 i) #f (or (equal? (dls a i) b) (rec (- i 1))))))
(define (assoc-c v l e?) (if (null? l) #f (if (e? (caar l) v) (car l) (assoc-c v (cdr l) e?))))
(define (akk-aplk hole akk aplk) (let* (
		(aplk (map (lambda(ap) (cons (cdr ap) (car ap))) aplk))
		(t (assoc-c (car akk) aplk eqp?)))
	(and t (list (tone-rev (cdr t)) (cdr akk) hole (+ hole (length (car t)))))))

(define (-d a b m) (- (+ a (if (> a b) 0 m)) b))
(define (p m a) (map (lambda(t) (-d t (list-ref a m) 12)) (list-out m a)))
(define (f tns) (map (lambda(m) (cons (list-ref tns m) (p m tns))) (iota (length tns))))
(define (akk-find akks tns) (filter (lambda(n) (not (null? n))) (let rec ((t tns) (i 1)) 
	(if (null? t) '() (cons (filter values (map 
		(lambda(a) (cond 
			((> (+ 1 (length (car a))) (length t)) #f)
			(else (akk-aplk i a (f (map tone (list-head t (+ 1 (length (car a))))))))))
		akks)) (rec (cdr t) (+ i 1)))))))

(define (St-trans St htones) (tones->St (St->tones St) htones))
(define (St-tones-print St)
	(define (comp up tup over step out) (cond
		((null? tup) out)
		((boolean? (car tup)) (comp up (cdr tup) #t step out))
		(else (vector-set! out up (if over (string-append (symbol->string (car tup)) "*") (car tup)))
			(comp (+ up step) (cdr tup) over step out))))
	(let* (
			(Sba (St-blow St))
			(Sda (St-draw St))
			(midle (apply max (map (lambda(t) (length (filter (lambda(p) (not (boolean? p))) t))) Sba)))
			(low (apply max (map (lambda(t) (length (filter (lambda(p) (not (boolean? p))) t))) Sda)))
			(rows (make-vector  (length St))))
	(let rc ((d Sda) (b Sba) (hole 0))
		(if (null? d) rows (begin (vector-set! rows hole 
			(comp (- midle 1) (car b) #f -1 
			(comp (+ midle 1) (car d) #f 1
			((lambda(v) (vector-set! v midle (+ 1 hole)) v)
			(make-vector (+ 1 midle low) " ")))))
			(rc (cdr d) (cdr b) (+ 1 hole)))))
	(do ((s 0 (+ s 1))) ((= s (+ 1 midle low)))
		(do ((h 0 (+ h 1))) ((= h (length St)))
			(display (vector-ref (vector-ref rows h) s)) 
			(display "\t"))
		(newline))))
(define (St-chord St Akklist)
	(let rc ((ost (map (lambda(c) (cons (list-head c 2) (cddr c))) (append 
		(map (lambda(p) (append p '(b))) (apply append (akk-find Akklist (map car (St-blow St))))) 
		(map (lambda(p) (append p '(d))) (apply append (akk-find Akklist (map car (St-draw St))))))))
			(base '())) (cond
		((null? ost) base)
		(else ((lambda(t) (rc (filter (lambda(c) (not (equal? (car c) t))) ost) (cons 
			(cons t (map cdr (filter (lambda(c) (equal? (car c) t)) ost))) base)))
			(caar ost))))))
(define (St-all-tones St)
	(define tns '())
	(map (lambda(k) (if (member k tns) k (set! tns (cons k tns))))
		(append (map car (St-draw St)) (map car (St-blow St))))
	(newline) (display "Notes: ")
	(display (sort (lambda(a b) (< (tone a) (tone b))) tns))
	(newline))

			

(define (St-show St)
	(define (AkkShow Akk) (map (lambda(t)
			(display (caar t)) (display "\t") (display (cadar t))
			(display "\t")
			(apply print (apply append (map list (cdr t) (make-list (length (cdr t)) " "))))
		) (St-chord St Akk)))
	(St-tones-print St)
	(St-all-tones St)
	(newline) (print "x2 chord:") (AkkShow Akk2)
	(newline) (print "x3 chord:") (AkkShow Akk3)
	(newline) (print "x4 chord:") (AkkShow Akk4)
	#f)

(define (vector-find vec val veq?) (do ((i 0 (+ i 1)) 
		(ids '() (if (veq? (vector-ref vec i) val) (cons i ids) ids)))
	((= i (vector-length vec)) ids)))
(define (St-find-note St note) 
	(define (neq? n n0) (= (tone n) (tone n0)))
	(define (grep mark mod)
		(map (lambda(k) (cons (+ k 1) mark)) (vector-find (list->vector (map car (mod St))) note neq?)))
	(append (grep 'b St-blow) (grep 'd St-draw)))
(define (notelist-trans notelist dt) (map (lambda(d) (tone-rev (modulo (+ (tone d) dt) 12))) notelist))
(define (St-where St notelist) (let rc((nl notelist))
	(if (null? nl) #f (begin (apply print (car nl) "\t" (St-find-note St (car nl)))
		(rc (cdr nl))))))

(define (St-natural-modes St key) (let rc ((ost (M-by key)))
	(unless (null? ost) (let ((l (car ost)))
		(print "\t" (car l) "\t" (cadr l)) 
		(St-where St (caddr l))
	(rc (cdr ost))))))


	


; overbends #t bends blow #f draw bends #t overbends
(define C-major '(
(Eb 	#t 	C #f D Db)
(Ab 	#t 	E #f G Gb F)
(C  	#t 	G #f B Bb A Ab)
(Eb 	#t 	C #f D Db)
(Gb 	#t 	E #f F)
(Bb 	#t 	G #f A Ab)
	(	C #f B 	#t Db)
	(Eb 	E #f D 	#t F)
	(Gb 	G #f F 	#t Ab)
	(B Bb 	C #f A 	#t Db)
))
(define C-puddy '(
(Eb 	#t 	C #f D Db)
(Ab 	#t 	E #f G Gb F)
(C  	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(Gb 	#t 	E #f F)
(Bb 	#t 	G #f A Ab)
	(	C #f B 	#t Db)
	(Eb 	E #f D 	#t F)
	(Gb 	G #f F 	#t Ab)
	(B Bb 	C #f A 	#t Db)
))
(define C-dim '(
(Eb 	#t 	C #f D Db)
(Gb 	#t Eb 	  #f F E)
(A  	#t Gb 	  #f Ab G)
(C 	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(Gb 	#t Eb 	  #f F E)
(A  	#t Gb 	  #f Ab G)
(C 	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(Gb 	#t Eb 	  #f F E)
))

;*
(define C-LeeOskar '(
(Eb 	#t 	C #f D Db)
(Ab 	#t 	E #f G Gb F)
(C  	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(Gb 	#t 	E #f Fs)
(Bb 	#t 	G #f A Ab)
	(	C #f B 	#t Db)
	(Eb 	E #f D 	#t F)
	(Gb 	G #f Fs #t Ab)
	(B Bb 	C #f A 	#t Db)
))


(define C-unkminor (St-trans '( ;где взял не помню
(C 	#t 	G #f B Bb)
(F 	#t 	C #f E Eb D Db)
(Ab  	#t 	E #f G Gb F)
(C 	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(G 	#t 	E #f Gb F)
	(Ab	A #f G 		#t Bb)
	( 	C #f B 		#t Db)
	(E 	F #f D 		#t F)
(Ab G Gb	A #f Gb 	#t Bb)
) 3))
(define C-open-minor '( ;дорийско-миксолидийская
(Eb 	#t 	C #f D Db)
(Ab 	#t 	E #f G Gb F)
(C  	#t 	G #f Bb A Ab)
(Eb 	#t 	C #f D Db)
(Gb 	#t 	E #f F)
(Bb 	#t 	G #f A Ab)
	(	C #f Bb	#t Db)
	(Eb 	E #f D 		#t F)
	(Gb 	G #f F 		#t Ab)
	(B Bb 	C #f A 		#t Db)
))
(define C-natural-minor (St-trans '( ; transform from A
(C 	#t 	A #f B Bb)
(F 	#t 	C #f E Eb D Db)
(Ab  	#t 	E #f G Gb F)
(C 	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(G 	#t 	E #f Gb F)
	(Ab	A #f G 		#t Bb)
	( 	C #f B 		#t Db)
	(Eb 	E #f D 		#t F)
(Ab G Gb	A #f Gb 	#t Bb)
) 3))
(define C-harmonic-minor (St-trans '(
(C 	#t 	A #f B Bb)
(F 	#t 	C #f E Eb D Db)
(A  	#t 	E #f Ab G Gb F)
(C 	#t 	A #f B Bb)
(Eb 	#t 	C #f D Db)
(Gb 	#t 	E #f F)
	(Ab	A #f Ab	#t Bb)
	( 	C #f B 		#t Db)
	(Eb 	E #f D 		#t F)
(Ab G Gb	A #f F	 	#t Bb)
) 3))
(define C-harmonic-minor-cross '(
	(C  #f D )
	(Eb #f G )
	(G  #f Bb)
	(C  #f D )
	(Eb #f Fs)
	(G  #f Ab)
	(C  #f B )
	(Eb #f D )
	(G  #f Fs)
	(C  #f Ab)
))
(define C-arabic '(
	(Bb #f C )
	(Db #f E )
	(F  #f G )
	(Ab #f B )
	(C  #f Db)
	(E  #f F )
	(G  #f Ab)
	(B  #f C )
	(Db #f E )
	(F  #f G )
))
(define C-natural-minor-straight '(
	(Eb #t C #f D Db)
	(Ab #t Eb #f G Gb F)
	(C  #t G #f Bb Bb A Ab)
	(Eb #t C #f D Db)
	(Gb #t Eb #f F)
	(Bb #t G #f Ab B)
	(C #f Bb #t Db)
	(D Eb #f D #t E)
	(Gb G #f F #t Ab)
	(B Bb C #f Ab #t Db)
))
