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


(define engage-couple cons)
(define add-couple cons)
(define add-person cons)
(define first-person car)
(define rest-of-list cdr)
(define second-person cdr)

; TODO 1
; Implementați funcția match care primește o persoană person care
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
  ;lista cu persoana curenta ce va trece prin algoritm
  (let ((person-list (list person)))
  ;iteratie prin aceasta lista
  (let loop-person ((person-list person-list) (engagements engagements))
    (if (null? person-list)
        engagements
        (let ((current-person (first-person person-list)) (rest-person-list (rest-of-list person-list)))
        ;iteratie prin lista de preferati ai persoanei curente  
        (let loop-pref-person ((pref-list-person (get-pref-list pref1 current-person)))
          ;atunci cand persoana nu gaseste un partener se logodeste cu "fals"
          (let ((no-eng (engage-couple #f current-person)))
          (if (null? pref-list-person)
              (loop-person rest-person-list (add-couple no-eng engagements))
              (let ((pref-person (first-person pref-list-person)) (rest-pref-list-person (rest-of-list pref-list-person)))
                ;se cauta cuplul din camera ce il contine pe unul din preferatii persoanei curent
                (let ((filtered-eng (filter (lambda (L) (equal? pref-person (car L))) engagements)))
                    ;daca nu exista un cuplu cu aceasta persoana, se itereaza prin lista de preferati
                    (if (null? filtered-eng)
                        (loop-pref-person rest-pref-list-person)
                        (let* ((couple (car filtered-eng)) (couple-head (first-person couple)) (couple-tail (second-person couple)))
                          ;se verifica daca persoana din camera e logodita cu fals
                          (if (equal? #f couple-tail)
                              ;daca este, atunci se inlocuieste fals cu persoana curenta in lista cuplurilor din camera
                              (update-engagements engagements couple-head current-person)
                              ;se verifica daca se prefera persoana curenta in locul partenerului actual
                              (if (preferable? (get-pref-list pref2 pref-person) current-person couple-tail)
                                  ;daca da, atunci se formeaza un nou cuplu in camera, iar fostul partener devine "current-person"
                                  ;si urmeaza sa treaca prin acelasi algoritm
                                  (loop-person (add-person couple-tail rest-person-list) (update-engagements engagements couple-head current-person))
                                  ;daca nu, se itereaza prin lista de preferinte a persoanei curente
                                  (loop-pref-person rest-pref-list-person)))))))))))))))    
                
        

; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)
(define (path-to-stability engagements mpref wpref queue)
  ;functie care inverseaza cuplurile dintr-o lista de cupluri
  (define (rev LIST)
    (foldr (lambda (L res) (cons (cons (cdr L) (car L)) res)) null LIST))

  ;functie care verifica daca o persoana este barbat
  (define (male? person)
    ;se itereaza prin lista de barbati
    (let loop ((men-list (get-men mpref)))
      ;daca lista a ajuns goala atunci se intoarce fals, persoana fiind femeie
      (if (null? men-list)
          #f
          (let ((first-men (first-person men-list)) (rest-men (rest-of-list men-list)))
          ;daca persoana coincide cu un membru al listei de barbati se intoarce true  
          (if (equal? person first-men)
              #t
              (loop rest-men))))))

  ;iteratie prin coada
  (let iter ((queue queue) (result engagements))
    (if (null? queue)
        result
        (let ((current-person (first-person queue)) (rest-of-queue (rest-of-list queue)))
          ;daca persoana curenta este barbat atunci se apeleaza match cu parametrii corespunzatori
          ;adica in engagements pe prima pozitie sunt femeile
          (if (male? current-person)
              (iter rest-of-queue (match current-person result mpref wpref queue))
              ;daca persoana curent a este femeie, atunci se inverseaza lista rezultata pentru
              ;ca match sa se apeleze cu barbatii pe prima pozitie in cuplurile din engagements
              (iter rest-of-queue (rev (match current-person (rev result) wpref mpref queue))))))))


; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
 ;unstable = lista cu cuplurile instabile din camera
 ;queue = persoanele din cuplurile din unstable sub forma de list
 (let* ((unstable (get-unstable-couples engagements mpref wpref)) (queue (get-couple-members unstable)))
   ;functie care returneaza true daca un cuplu este stabil si false daca este instabil
   (define (f L)
     (let iter ((unstable unstable))
       (if (null? unstable)
           #t
           (let ((first-couple (first unstable)) (rest-of-couples (rest unstable)))
             (if (equal? L first-couple)
                 #f
                 (iter rest-of-couples))))))
   ;se filtreaza cuplurile din camera pentru a ramane doar cele stabile
   (let ((room-engagements (filter f engagements)))
     (path-to-stability room-engagements mpref wpref queue))))
    
                   
; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (build-stable-matches-stream pref-stream)
  (if (stream-empty? pref-stream)
      empty-stream
      (stream-cons (gale-shapley (car (stream-first pref-stream)) (cdr (stream-first pref-stream)))
                   (build-stable-matches-stream (stream-rest pref-stream))))) 


