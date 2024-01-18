#lang racket

(provide (all-defined-out))

; O funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosește recursivitate pe stivă.

(define (get-men mpref)
  (if (null? mpref)
      null
      (append (list(car (car mpref))) (get-men (cdr mpref)))))


; O funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosește recursivitate pe coadă.

(define (get-women wpref)
  (get-women-helper wpref null))

(define (get-women-helper wpref acc)
  (if (null? wpref)
      (reverse acc)
      (get-women-helper (cdr wpref) (cons (car (car wpref)) acc))))


; O funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.

(define (get-pref-list pref person)
  (if (equal? (car (car pref)) person)
      (cdr (car pref))
           (get-pref-list (cdr pref) person)))


; O funcție recursivă care primește o listă de tipul
; întors de funcția precedentă (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosește operatori condiționali, folosește în schimb operatori
; logici pentru a obține același efect.

(define (preferable? L x y)
(or (equal? (car L) x) (and (not (equal? (car L) y)) (preferable? (cdr L) x y))))

; O funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.

(define (get-partner engagements person)
  (cond [(null? engagements) #f]
        [(equal? (car (car engagements)) person) (cdr (car engagements))]
        [else (get-partner (cdr engagements) person)]))


; O funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond [(null? p1-list) #f]
        [(preferable? p1-list p2 (car p1-list)) #f]
        [(preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list))) #t]
        [else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)]))
