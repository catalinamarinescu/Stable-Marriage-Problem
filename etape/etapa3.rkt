#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
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
  (let ((save engagements) (save2 (foldr (lambda (L res) (cons (cons (cdr L) (car L)) res)) null engagements)))
    (let iter ((engagements engagements) (result '()))
    (if (null? engagements)
        result
        (let* ((pair (car engagements)) (male (cdr pair)) (female (car pair)))
          (if (equal? (nor (better-match-exists? male female (get-pref-list mpref male) wpref save)
                    (better-match-exists? female male (get-pref-list wpref female) mpref save2)) #f) 
              (iter (cdr engagements) (cons pair result))
              (iter (cdr engagements) result)))))))




; TODO 2
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
  (let iter ((free-men free-men) (engagements engagements)) ;iteram prin barbatii liberi
    (if (null? free-men)
        engagements
        (let ((currMale (car free-men)))
        (let iter2 ((pref-female (get-pref-list mpref currMale))) 
          (let* ((currFemale (car pref-female)) (eng-pair (filter (lambda (L) (equal? currFemale (car L))) engagements)))
            (if (null? pref-female)
                (iter (cdr free-men) engagements)
                (let ((newPair (cons currFemale currMale)))
                  (if (null? eng-pair)
                      (iter (cdr free-men) (cons newPair engagements)) 
                      (let* ((pair (car eng-pair)) (female (car pair)) (femaleEx (cdr pair))) 
                        (if (preferable? (get-pref-list wpref female) currMale femaleEx) 
                            (iter (cons femaleEx (cdr free-men)) (update-engagements engagements female currMale)) 
                            (iter2 (cdr pref-female))))))))))))) 
            


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
   (foldr (lambda (x acc) (cons (cdr x) (cons (car x) acc))) null pair-list))

