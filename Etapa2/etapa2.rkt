#lang racket

(provide (all-defined-out))

; Ca în etapa 1, o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosește funcționala map.

(define (get-men mpref)
  (map car mpref))


; Ca în etapa 1, o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosește foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.

(define (get-women wpref)
  (foldr (λ (x acc) (cons (car x) acc)) null wpref))


; Ca în etapa 1, o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Folosiți minim o funcțională și minim o funcție anonimă.

(define (get-pref-list pref person)
  (foldr (λ (x acc) (if (equal? (car x) person) (append (cdr x) acc) (append acc null))) null pref))


; Ca în etapa 1, o funcție care primește o listă de tipul
; întors de funcția precedentă (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosește funcția member.

(define (preferable? pref-list x y)
  (if (list? (member y (member x pref-list))) #t #f))


; Implementarea recursivă a funcționalei find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea este eficientă în sensul că nu continuă explorarea 
; listei odată ce s-a găsit elementul.

(define (find-first p L)
  (cond [(null? L) #f]
        [(p (car L)) (car L)]
        [else (find-first p (cdr L))]))


; Ca în etapa 1, o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosește find-first, fără să îl apeleze de 2 ori.

(define (get-partner engagements person)
  (define find-first-result (find-first (λ (x) (equal? person (car x))) engagements))
    (if (equal? find-first-result #f) #f (cdr find-first-result)))
  

; Implementarea recursivă a funcționalei change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.

(define (change-first p L val)
  (change-first-helper p L val '()))

(define (change-first-helper p L val acc)
  (if (null? L)
      acc
      (if (p (car L)) (change-first-helper p null val (append (append acc (list val)) (cdr L)))
          (change-first-helper p (cdr L) val (append acc (list (car L)))))))


; Implementarea funcției update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Folosește change-first.

(define (update-engagements engagements p1 p2)
  (change-first (λ (x) (equal? p1 (car x))) engagements (cons p1 p2)))


; Implementarea funcției better-match-exists? din etapa 1.

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond [(null? p1-list) #f]
        [(preferable? p1-list p2 (car p1-list)) #f]
        [(preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list))) #t]
        [else (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)]))


; Implementarea funcției stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):

(define or-stable-match (λ x (if (null? x) #f
                                 (if (car x) #t (apply or-stable-match (cdr x))))))

(define (stable-match? engagements mpref wpref)
  (not (apply or-stable-match (foldr (λ (x acc) (append acc (append (list (better-match-exists? (car x) (cdr x) (get-pref-list wpref (car x)) mpref engagements))
                                                                    (list (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))))) null engagements))))
