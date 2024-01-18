#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1 V
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (letrec ([girls-helper-reverse (λ (x) (map (λ (y) (cons (cdr y) (car y))) x))] 
           [girls (λ (x) (better-match-exists? (car x) (cdr x) (get-pref-list wpref (car x)) mpref (girls-helper-reverse engagements)))]
           [boys (λ (x) (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))]
           [rez (λ (x) (or (girls x) (boys x)))])
    (filter (λ (x) (rez x)) engagements)))

; TODO 2 V
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let engage-aux ([free-men-aux free-men] [engagements-aux engagements])
    (if (null? free-men-aux) engagements-aux
          (let engage-alternate ([girl (get-pref-list mpref (car free-men-aux))])
                  (cond [(equal? #f (get-partner engagements-aux (car girl)))
                         (engage-aux (cdr free-men-aux) (cons (cons (car girl) (car free-men-aux)) engagements-aux))]
                        [(preferable? (get-pref-list wpref (car girl)) (car free-men-aux) (get-partner engagements-aux (car girl)))
                         (engage-aux (cons (get-partner engagements-aux (car girl)) (cdr free-men-aux)) (update-engagements engagements-aux (car girl) (car free-men-aux)))]
                        [else (engage-alternate (cdr girl))])))))


; TODO 3 V
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) null mpref wpref))


; TODO 4 V
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (λ (x acc) (append acc (list (car x)) (list (cdr x)))) null pair-list))

