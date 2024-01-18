#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; Implementarea funcției match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur

(define (match person engagements pref1 pref2 queue)
  (let match-aux ([engagements-aux engagements] [person-list (get-pref-list pref1 person)])
    (cond [(null? person-list) (cons (cons #f person) engagements-aux)]
          [(equal? #f (not (member (car person-list) queue))) (match-aux engagements-aux (cdr person-list))]
          [(preferable? (get-pref-list pref2 (car person-list)) person (get-partner engagements-aux (car person-list)))
           (match (get-partner engagements-aux (car person-list)) (update-engagements engagements-aux (car person-list) person) pref1 pref2 queue)]
          [else (match-aux engagements-aux (cdr person-list))])))


; Implementarea funcției path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
;   - persoanele nelogodite din cameră apar în engagements sub forma
;     (#f . nume-bărbat) sau (nume-femeie . #f)

(define (path-to-stability engagements mpref wpref queue)
  (let ([w-reverse (λ (x) (map (λ (y) (cons (cdr y) (car y))) x))]) 
    (cond [(null? queue) engagements]
          [(not (member (car queue) (get-men mpref)))
           (let match-w ([person-list (get-pref-list wpref (car queue))])
             (cond [(null? person-list)
                    (path-to-stability (cons (cons (car queue) #f) engagements) mpref wpref (cdr queue))]
                   [(equal? #f (not (member (car person-list) (cdr queue)))) (match-w (cdr person-list))]
                   [(equal? #f (get-partner (w-reverse engagements) (car person-list)))
                    (path-to-stability (w-reverse (update-engagements (w-reverse engagements) (car person-list) (car queue))) mpref wpref (cdr queue))]
                   [(preferable? (get-pref-list mpref (car person-list)) (car queue) (get-partner (w-reverse engagements) (car person-list)))
                    (path-to-stability (w-reverse (update-engagements (w-reverse engagements) (car person-list) (car queue))) mpref wpref (cons (get-partner (w-reverse engagements) (car person-list)) (cdr queue)))]
                   [else (match-w (cdr person-list))]))]
          [else (let match-m ([person-list (get-pref-list mpref (car queue))])
                  (cond [(null? person-list)
                         (path-to-stability (cons (cons #f (car queue)) engagements) mpref wpref (cdr queue))]
                        [(equal? #f (not (member (car person-list) (cdr queue)))) (match-m (cdr person-list))]
                        [(equal? #f (get-partner engagements (car person-list)))
                         (path-to-stability (update-engagements engagements (car person-list) (car queue)) mpref wpref (cdr queue))]
                        [(preferable? (get-pref-list wpref (car person-list)) (car queue) (get-partner engagements (car person-list)))
                         (path-to-stability (update-engagements engagements (car person-list) (car queue)) mpref wpref (cons (get-partner engagements (car person-list)) (cdr queue)))]
                        [else (match-m (cdr person-list))]))])))


; Implementarea funcției update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability

(define (update-stable-match engagements mpref wpref)
  (letrec ([unstable (get-unstable-couples engagements mpref wpref)]
           [queue (get-couple-members unstable)]
           [room-engagements (let re-aux ([stable null] [engagements-aux engagements])
                               (cond [(null? engagements-aux) stable]
                                     [(not (member (car engagements-aux) unstable)) (re-aux (cons (car engagements-aux) stable) (cdr engagements-aux))]
                                     [else (re-aux stable (cdr engagements-aux))]))])
    (path-to-stability room-engagements mpref wpref queue)))
