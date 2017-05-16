#lang scheme

;------------------------------------------------------------------------------------------------------ Définition des lignes

;;définition de la ligne1  : Thanusan et Aljoscha

(define ligne1 '(("La Défense")
 ("Esplanade de la Défense")
 ("Pont de Neuilly")
 ("Les Sablons")
("Porte Maillot")
("Argentine")
("Charles de Gaulle - Etoile" ("ligne2" "ligne6"))
("George V")
("Franklin D. Roosevelt")
("Champs-Elysées - Clemenceau")
("Concorde")
("Tuileries")
("Palais Royal - Musée du Louvre")
("Louvre - Rivoli")
("Châtelet" ("ligne4"))
("Hotel de Ville")
("Saint-Paul")
("Bastille" ("ligne5"))
("Gare de Lyon")
("Reuilly - Diderot")
("Nation" ("ligne2" "ligne6"))
("Porte de Vincennes")
("Saint-Mandé - Tourelle")
("Bérault")
("Chateau de Vincennes")))

;;définition de la ligne2 : Aljoscha

(define ligne2 '(("Porte Dauphine")
("Victor Hugo")
("Charles de Gaulle - Etoile" ("ligne1" "ligne6"))
("Ternes")
("Courcelles")
("Monceau")
("Villiers")
("Rome")
("Place de Clichy")
("Blanche")
("Pigalle")
("Anvers")
("Barbès - Rochechouart" ("ligne4"))
("La Chapelle")
("Stalingrad" ("ligne5"))
("Jaurès" ("ligne5"))
("Colonel Fabien")
("Belleville")
("Couronnes")
("Ménilmontant")
("Père Lachaise")
("Philippe Auguste")
("Alexandre Dumas")
("Avron")
("Nation" ("ligne1" "ligne6"))))

;; définition de la ligne6 : Aljoscha

(define ligne6 '(("Charles de Gaulle - Etoile" ("ligne1" "ligne2"))
("Kléber")
("Boissière")
("Trocadéro")
("Passy")
("Bir-Hakeim")
("Dupleix")
("La Motte-Picquet Grenelle")
("Cambronne")
("Sèvres - Lecourbe")
("Pasteur")
("Montparnasse Bienvenue" ("ligne4"))
("Edgar Quinet")
("Raspail" ("ligne4"))
("Denfert-Rochereau" ("ligne4"))
("Saint-Jacques")
("Glacière")
("Corvisart")
("Place d'Italie" ("ligne5"))
("Nationale")
("Chevaleret")
("Quai de la Gare")
("Bercy")
("Dugommier")
("Daumesnil")
("Bel-Air")
("Picpus")
("Nation" ("ligne1" "ligne2"))))

;;définition de la ligne4 : Thanusan

(define ligne4 '(("Porte de Clignancourt")
("Simplon")
("Marcadet  Poissonniers")
("Chateau Rouge")
("Barbès - Rochechouart" ("ligne2"))
("Gare du Nord" ("ligne5"))
("Gare de l'Est" ("ligne5"))
("Chateau d'Eau")
("Strasbourg - Saint-Denis")
("Réaumur - Sébastopol")
("Etienne Marcel")
("Les Halles")
("Châtelet" ("ligne1"))
("Cité")
("Saint-Michel")
("Odéon")
("Saint-Germain-des-Prés")
("Saint-Sulpice")
("Saint-Placide")
("Montparnasse - Bienvenue" ("ligne6"))
("Vavin")
("Raspail" ("ligne6"))
("Denfert-Rochereau" ("ligne6"))
("Mouton-Duvernet")
("Alésia")
("Porte d'Orléans")))

;; définition de la ligne5 : Aljoscha

(define ligne5 '(("Bobigny - Pablo Picasso")
("Bobigny - Pantin Raymond Queneau")
("Eglise de Pantin")
("Hoche")
("Porte de Pantin")
("Ourcq")
("Laumière")
("Jaurès" ("ligne2"))
("Stalingrad" ("ligne2"))
("Gare du Nord" ("ligne4"))
("Gare de l'Est" ("ligne4"))
("Jacques Bonsergent")
("République")
("Oberkampf")
("Richard-Lenoir")
("Bréguet - Sabin")
("Bastille" ("ligne1"))
("Quai de la Rapée")
("Gare d'Austerlitz")
("Saint-Marcel")
("Campo-Formio")
("Place d'Italie" ("ligne6"))))


;-----------------------------------------------------------------------définitions des petits programmes qui serviront d'outils

;;1)
;;only-station : 
;;commentaire: Ceci est un programme outil qui me facilite car si on entre une bonne liste 
;;             il nous permet de seulement avoir le nom de la station
;;             exemple: pour la liste '("Nation" ("ligne2" "ligne6")), il me renverra seulement "Nation".

;;Thanusan
(define (only-station s)   
  (car s))

;;2)
;;listcounter-for-travel
;;commentaire: Ce programme est un compteur de station pour un trajet.
;;             exemple: pour un trajet avec 5 stations, il nous renverra 4.

;;Aljoscha
(define (listcounter-for-travel l)   
  (if (pair? l)
      (if (list? (car l))
          (+ 1 (listcounter-for-travel (cdr l)))
          (+ 0 (listcounter-for-travel (cdr l))))
      -1))


;;3)
;;truelinefinder
;;commentaires : Quels que programmes ne reconnaissent pas les lignes que l'on entre car ils ne reconnaissent pas les lignes 
;;               qui sont entrées comme "ligne..." car ils reconnaissent seulement les lignes entrées tel que ligne... . Donc ce programme 
;;               est comme un traducteur.
;;               par exemple: si on entre "ligne1", il va le remplacer par ligne1.

;;Aljoscha
(define (truelinefinder l)
  (if (eq? l "ligne1")
      ligne1
      (if (eq? l "ligne2")
          ligne2
          (if (eq? l "ligne6")
              ligne6
              (if (eq? l "ligne4")
                  ligne4
                  (if (eq? l "ligne5")
                      ligne5
                      (display "Veuillez entrer une ligne valide.")))))))



;;4)
;;lines
;;commentaires: Ce programme est une liste de toutes les lignes.
;;              Ceci est un outil indispensable pour quelques programmes.

;;Aljoscha
(define lines '("ligne1" "ligne2" "ligne6" "ligne4" "ligne5"))

;;5)
;;existe?
;; commentaire: un prédicat qui renvoie true si il existe au moins une fois, ligne entrée (l1) dans la liste
;; de ligne entrée (l2)(define (existe? l1 l2)

;;Aljoshcha
(define (existe? l1 l2)
  (if (pair? l2)
  (if (eq? l1 (car l2))
      true
      (existe? l1 (cdr l2)))
  false))


;-------------------------------------------------------------------------Définition des grands programmes qui serviront d'outil

;;1)
;;lignesearcher
;;commentaire: lignesearcher nous renvoie la liste de toutes les lignes où l'on peut trouver la station entrée

;;Thanusan
(define (lignesearcher s)
  (define (iterer ligne1 ligne2 ligne6 ligne4 ligne5 acc)
    (if (pair? ligne1)
        (if (eq? s (only-station (car ligne1)))
            (iterer (cdr ligne1) ligne2 ligne6 ligne4 ligne5 (cons "ligne1" acc))
            (iterer (cdr ligne1) ligne2 ligne6 ligne4 ligne5 acc))
        (if (pair? ligne2)
            (if (eq? s (only-station (car ligne2)))
                (iterer ligne1 (cdr ligne2) ligne6 ligne4 ligne5 (cons "ligne2" acc))
                (iterer ligne1 (cdr ligne2) ligne6 ligne4 ligne5 acc))
            (if (pair? ligne6)
                (if (eq? s (only-station (car ligne6)))
                    (iterer ligne1 ligne2 (cdr ligne6) ligne4 ligne5 (cons "ligne6" acc))
                    (iterer ligne1 ligne2 (cdr ligne6) ligne4 ligne5 acc))
                (if (pair? ligne4)
                    (if (eq? s (only-station (car ligne4)))
                        (iterer ligne1 ligne2 ligne6 (cdr ligne4) ligne5 (cons "ligne4" acc))
                        (iterer ligne1 ligne2 ligne6 (cdr ligne4) ligne5 acc))
                    (if (pair? ligne5)
                        (if (eq? s (only-station (car ligne5)))
                            (iterer ligne1 ligne2 ligne6 ligne4 (cdr ligne5) (cons "ligne5" acc))
                            (iterer ligne1 ligne2 ligne6 ligne4 (cdr ligne5) acc))
                       (reverse acc)))))))
  (iterer ligne1 ligne2 ligne6 ligne4 ligne5 empty))


;1)intervalle
;;objectif: créer une fonction qui nous permettra de trouver l'intervalle entre le départ et l'arrivée

;;intervalle
;;commentaires: La fonction intervalle nous donne une liste des stations qui sont entre la station de départ (d) et la station
;;              d'arrivée (a) dans une ligne entrée (l). Cette fonction nous donne la liste des stations dans l'ordre de 
;;              départ et d'arrivée (c'est à dire en respectant la direction).

;;Thanusan
(define (intervalle d a l)
  (define (iterer enleve-avant-s-et-apres-s enleve-avant-s-et-apres-sRESET accl acc)
    (if (pair? accl)
        (if (pair? enleve-avant-s-et-apres-s)
            (if (eq? (car accl) (car enleve-avant-s-et-apres-s))
                (iterer enleve-avant-s-et-apres-sRESET enleve-avant-s-et-apres-sRESET (cdr accl) acc)
                (iterer (cdr enleve-avant-s-et-apres-s) enleve-avant-s-et-apres-sRESET accl acc))
            (iterer enleve-avant-s-et-apres-sRESET enleve-avant-s-et-apres-sRESET (cdr accl) (cons (car accl) acc)))
        (if (bonnedirection? d a l)
            (reverse acc)
             acc)))
  (iterer (enleve-avant-s-et-apres-s d a l) (enleve-avant-s-et-apres-s d a l) l empty))

;;les sous fonctions pour la fonction intervalle

;;enleve-avant-d 
;;commentaires: cette fonction nous donne la liste des stations qui sont avant la station entrée (d)
;;              dans la ligne entrée (l). La station entrée n'apparaitra pas dans la liste des stations que cette 
;;              fonction nous donnera.

;;Thanusan
(define (enleve-avant-s d l)
  (define (iterer d l acc1 acc2)
    (if (pair? l)
        (if (eq? d (only-station (car l)))
            (if (pair? acc1)
                (reverse acc1)
                acc2)
            (iterer d (cdr l) (cons (car l) acc1) acc2))
        (reverse acc1)))
    (iterer d l empty empty)) 

;;enleve-après-s
;;commentaires: Cette fonction nous donne la liste des stations qui sont après la station entrée (a) dans
;;              une ligne entrée (l). Et la station entrée n'apparaitra pas dans la liste des stations que 
;;              cette fonction donnera.

;;Thanusan
(define (enleve-après-s a l)
 (reverse (enleve-avant-s a (reverse l))))

;;enleve-avant-s-et-après-s
;;commentaires: Cette fonction nous donne la liste des stations qui ne sont pas dans l'intervalle de la station entrée dans (d) 
;;              et de la station entrée dans (a) dans une ligne entrée (l).
;;              D'ailleurs, cette fonction nous donne une liste dans l'ordre de départ et d'arrivée.

;;Thanusan
(define (enleve-avant-s-et-apres-s d a l)
  (let* ((acc 
          (if (bonnedirection? d a l) 
              (append (enleve-avant-s d l) (enleve-après-s a l)) 
              (append (reverse (enleve-après-s a (reverse l))) (reverse (enleve-avant-s d (reverse l))))))) (if (bonnedirection? d a l)
                                                                                             acc
                                                                                             (reverse acc))))



;;3)quel-direction
;;objectif= créer une fonction qui nous trouve la bonne direction par rapport au trajet sur une ligne.


;;quel-direction
;;commentaires: Ce programme nous donne la direction à prendre selon le trajet à faire sur une ligne.

;;Thanusan
(define (quel-direction d a l)
  (if (eq? (bonnedirection? d a l) true)
      (car (car (reverse l)))
      (car (car l))))
 

;;bonnedirection?
;;commentaires: Ce prédicat nous renvoie true si la station entrée dans (d) est avant la station entrée dans (a) (de gauche à droite) et
;;             false si la station entrée dans (a) est avant la station entrée dans (d) (de gauche à droite) dans une ligne entrée.
;;             En effet, cette fonction sert de sous-fonction pour d'autres fonctions en donnant des détails sur les directions.

;;Thanusan
(define (bonnedirection? d a l)
  (define (iterer compteurdl compteural)
    (if (oneline? d a)
    (if (< compteurdl compteural)
        true
        false)
    (display "Veuillez entrer deux stations qui sont sur la meme ligne.")))
  (iterer (compteurbd d l) (compteurbd a l)))

;;un outil indispensable pour bonnedirection?, le compteur.

;;compteurbd 
;;commentaires: ce compteur nous donne l'emplacement de la station entrée en (d) par rapport à la première station
;;              (de gauche à droite) dans la ligne entrée dans (l)

;;Thanusan
(define (compteurbd s l)
  (define (iterer accs accl acc)
  (if (pair? accl)
  (if (eq? accs (only-station (car accl)))
      acc
      (iterer accs (cdr accl) (+ 1 acc)))
 (display "Veuillez entrer une station qui correspond à votre ligne")))
  (iterer s l 1))


;------------------------------------------------------------------------Définition des fonctions pour les correspondances

;1) correspondances
;;objectif : afficher la liste des correspondances qu'on peut trouver dans une ligne entrée.

;;   correspondances ;;
;;commentaires : Le programme correspondances affiche la liste des lignes qu'on peut
;;               trouver en correspondances dans une ligne entrée. Plusieurs versions cités en dessous ont
;;               étés nécessaire pour l'aboutissement du programme correspondances.

;;Thanusan
(define (correspondances l)
   (define (iterer carlinkedlinesl acc)
     (if (pair? carlinkedlinesl) 
             (iterer (cdr carlinkedlinesl) (append (car carlinkedlinesl) acc))
             (reverse acc)))
          (iterer (correspondances3 l) empty))

;;correspondances version1;;
;;commentaires : La première version du "correspondances" affiche les stations dans lesquelles il y'à au moins une correspondance

;;Thanusan
(define (correspondances1 l)
 (define (iterer accl acc)
   (if (pair? accl) 
       (if (pair? (cdr (car accl)))
           (iterer (cdr accl) (cons (car accl) acc))
           (iterer (cdr accl) acc))
       (reverse acc)))
   (iterer l empty))

;;correspondances version2;; 
;;commentaires : La deuxième version du "correspondances" affiche la première version de correspondance :s
;;               sans les noms des stations devant.

;;Thanusan
(define (correspondances2 l)
  (define (iterer lfl acccarl) 
    (if (pair? lfl)
        (iterer (cdr lfl) (cons (cdar lfl) acccarl))
        acccarl))
  (iterer (correspondances1 l) empty))

;;correspondances version3;;
;;commentaires : La troixième version du "correspondances" affiche ce qu'affiche correspondances2 
;;              en mieux (c'est à dire avec moins de parenthèses)

;;Thanusan
(define (correspondances3 l)
  (define (iterer linkedlinel carlinkedlinel acc)
    (if (pair? linkedlinel)
        (iterer (cdr linkedlinel) (cons (car (car linkedlinel)) carlinkedlinel) acc)
        carlinkedlinel))
  (iterer (correspondances2 l) empty empty))



;2)Ou-est-l-dans-ml
;;Objectif : Créer une fonction qui nous permet de donner les stations où on peut prendre une correspondance entrée 
;;           dans une ligne entrée.

;; Ou-est-l-dans-ml
;;commentaire: Le programme Ou-est-l-dans-ml (nommé Ou-est-l-dans-ml) nous renvoie les noms des différentes stations de la ligne entrée en ml,
;;             qui nous permettront de prendre une ligne entrée (l). Quels petit programmes cités en dessous ont été nécessaires
;;             pour l'aboutissement d'Ou-est-l-dans ml.

;;Thanusan
(define (Ou-est-l-dans-ml l ml)
  (miniorganiseur (Ou-est-l-dans-ml1 l ml)))

;;La premiere version d'Ou-est-l-dans-ml
;;commentaire: une fonction qui nous affiche la ou les stations qui nous permettront de prendre la correspondance (donné dans l) 
;;             une liste de ligne entrée (donné dans ml)

;;Thanusan
(define (Ou-est-l-dans-ml1 l ml)
  (define (iterer lfml acc)
    (if (pair? lfml)
        (if (existe? l (car (cdr (car lfml))))
            (iterer (cdr lfml) (cons (car lfml) acc))
            (iterer (cdr lfml) acc))
        acc))
    (iterer (correspondances1 ml) empty))

;; miniorganiseur est une sous fonction pour Ou-est-l-dans-ml
;; commentaire: ce programme nous permet d'afficher seulement les noms des stations dans la liste que donne Ou-est-l-dans-ml1 (dans l)

;;Thanusan
(define (miniorganiseur l)
  (define (iterer accl acc)
    (if (pair? accl)
        (iterer (cdr accl) (cons (car (car accl)) acc))
        acc))
 (iterer l empty))







;------------------------------------------------------------------------------------Les programmes indispensable pour le trajet






;------------------------------------------------------------projet sur un trajet sur une ligne

;;projet nommé Oneline
;;objectif créer une fonction qui donne les informations nécessaires aux voyageurs pour un trajet sur une seule ligne.






;;oneline? 
;;commentaires: Ce prédicat renvoie true si on peut trouver au moins une fois la station entrée dans départ (d) 
;;              et la station entrée dans arrivée (a) sur une même ligne. 
;;              Sinon, ce prédicat nous renverra false.

;;Aljoscha
(define (oneline? d a)
  (define (iterer lignesearcherd lignesearchera lignesearcheraRESET)
    (if (pair? lignesearcherd)
        (if (pair? lignesearchera)
            (if (eq? (car lignesearcherd) (car lignesearchera))
                true
                (iterer lignesearcherd (cdr lignesearchera) lignesearcheraRESET))
                (iterer (cdr lignesearcherd) lignesearcheraRESET lignesearcheraRESET))
        false))
  (iterer (lignesearcher d) (lignesearcher a) (lignesearcher a)))

;;onelinev1
;;commentaires: Ce programme nous donne la liste des lignes où l'on peut trouver les deux stations entrées (dans (d) et (a)).  
 
;;Aljoscha
(define (onelinev1 d a)
  (define (iterer lignesearcherd lignesearchera lignesearcheraRESET acc)
    (if (oneline? d a)
    (if (pair? lignesearcherd)
        (if (pair? lignesearchera) 
            (if (eq? (car lignesearcherd) (car lignesearchera))
                (iterer lignesearcherd (cdr lignesearchera) lignesearcheraRESET (cons (car lignesearcherd) acc))
                (iterer lignesearcherd (cdr lignesearchera) lignesearcheraRESET acc))
            (iterer (cdr lignesearcherd) lignesearcheraRESET lignesearcheraRESET acc))
        acc)
    (display "Veuillez entrer deux stations qui sont sur la meme ligne")))
  (iterer (lignesearcher d) (lignesearcher a) (lignesearcher a) empty))

;;intervalle-oneline
;;commentaires: ce programme affiche la liste des stations qui sont entre la station entrée dans le départ (d) 
;;              et la station entrée dans l'arrivée (a) pour chaque ligne de la liste que onelinev1 renverra pour les deux stations
;;              (d et a) entrées.       
;;              (Les deux stations d et a, seront dans la ou les listes).

;;Aljoscha
(define (intervalle-oneline d a)
  (define (iterer onelinev1da acc)
    (if (pair? onelinev1da)
       (iterer (cdr onelinev1da) (cons (intervalle d a (truelinefinder (car onelinev1da))) acc))
       acc))
  (iterer (onelinev1 d a) empty))

;;compteur-intervalle-oneline 
;;commentaires: Ce programme affiche dans l'ordre, le nombre de stations que contient chaque sous-listes 
;;              de la liste intervalle-oneline. Ce programme peut servir d'horaire pour les trajets 
;;              correspondances.

;;Aljoscha
(define (compteur-intervalle-oneline d a)
  (define (iterer onelinev1da acccompteur)
    (if (pair? onelinev1da)
        (iterer (cdr onelinev1da) (cons (listcounter-for-travel (intervalle d a (truelinefinder (car onelinev1da)))) acccompteur))
        acccompteur))
  (iterer (onelinev1 d a) empty))

;;oneline-organisateur
;;commentaires: Ce programme est un organisateur pour la version finale de oneline. En effet, il trie 
;;              les informations donnée par le programme onelinev1, compteur-intervalle-oneline, et intervalle-oneline dans l'ordre
;;              qu'il le faut. le tri que ce programme nous donnera, nous permettra d'avoir le temps du trajet, la ligne
;;              d'un trajet, ainsi que l'intervalle entre le départ et l'arrivée sur cette ligne.

;;Aljoscha
(define (oneline-organisateur d a)
  (define (iterer compteur-intervalle-oneline onelinev1da intervalle-oneline acc)
    (if (pair? compteur-intervalle-oneline)
        (iterer (cdr compteur-intervalle-oneline) (cdr onelinev1da) (cdr intervalle-oneline) (cons (cons (car compteur-intervalle-oneline) (cons (car onelinev1da) (list (car intervalle-oneline)))) acc))
        acc))
  (iterer (compteur-intervalle-oneline d a) (reverse (onelinev1 d a)) (intervalle-oneline d a) empty))

;;minimum-oneline-organisateur 
;; commentaire:     Ce programme affiche la sous-liste qui possède le minimum de temps du trajet parmi les différentes sous-listes que               
;;                  contient la liste donné par le programme oneline-organisateur.
 
;;Aljoscha
(define (minimum-oneline-organisateur d a)
  (define (iterer oneline-organisateurda acc)
     (if (pair? oneline-organisateurda)
         (if (<  (car (car oneline-organisateurda)) (car acc))
             (iterer (cdr oneline-organisateurda) (car oneline-organisateurda))
             (iterer (cdr oneline-organisateurda) acc))
         acc))
    (iterer (oneline-organisateur d a) (car (oneline-organisateur d a))))

;;oneline-info
;;commentaire: Ce programme, on peut aussi l'appeler la version finale de oneline.
;;             En effet, son rôle est de trier les différentes informations de la liste que minimum-oneline-organisateur renvoie.
;;             pour pouvoir afficher à l'utilisateur le trajet à faire et le temps de trajet (pour un trajet sur une seule ligne).

;;Aljoscha
(define (oneline-info d a)
  (define (iterer minimum-oneline-organisateur accligne accdépart accarrival acchoraire accdirection)
       (write "temps de trajet estimé :") (display acchoraire) (write " minutes. Voici le trajet à suivre: à la station ") (display d) (write ", veuillez prendre la ") (display accligne) (write " vers la direction de ") (display (quel-direction d a accdirection)) (write ". Et enfin, veuillez descendre à la station ") (display a) (write " pour arriver à votre destination"))
  (iterer (minimum-oneline-organisateur d a) (car (cdr (minimum-oneline-organisateur d a))) (car (car (car (cdr (cdr (minimum-oneline-organisateur d a)))))) (car (last (car (cdr (cdr (minimum-oneline-organisateur d a)))))) (car (minimum-oneline-organisateur d a)) (truelinefinder (car (cdr (minimum-oneline-organisateur d a))))))




;--------------------------------------------------------------------projet sur un trajet sur deux lignes

;;projet nommé twolines
;;objectif: Donné les information nécessaires aux voyageurs sur un trajet sur deux lignes.

;;twolines?
;;commentaires: Ce prédicat a pour rôle d'envoyer true si on peut utiliser la fonction twolines pour un trajet.
;;              Si ce trajet n'est pas possible par la fonction twolines, il renverra false.

;;Thanusan
(define (twolines? d a)
  (if (pair? (quel-stations-pour-twolines d a))
      true 
      false))



;;twolines-v1
;;commentaires: twolines-v1, est un programme qui cherche les différentes lignes dans les quelles les stations (d) et (a)
;;              peuvent se trouver et il regarde si la ligne d'arrivée est dans les correspondances de la ligne de départ. Si oui, ils
;;              va créer une sous-liste avec la ligne de départ et d'arrivée pour faciliter la version finale de twolines.
;;              Ce programme est très intéressant. En effet, il est une solution pour plusieurs problèmes que nous avons rencontré
;;              pour la création d'un programme qui gère un trajet sur deux lignes. 
;;             (quelques exemples des problèmes : la station de départ ou d'arrivé peut être sur plusieurs lignes. Parfois, sur une ligne de départ
;;              nous pouvons trouver plusieurs correspondances pour accéder à la ligne d'arrivée...)

;;Thanusan
(define (twolines-v1 d a)
  (define (iterer lignesearcherd lignesearcherdRESET lignesearchera acc)
    (if (pair? lignesearchera)
        (if (pair? lignesearcherd)
        (if (existe? (car lignesearchera) (correspondances (truelinefinder (car lignesearcherd))))
            (iterer (cdr lignesearcherd) lignesearcherdRESET lignesearchera (cons (cons (car lignesearchera) (cons (car lignesearcherd) empty)) acc))
            (iterer (cdr lignesearcherd) lignesearcherdRESET lignesearchera acc))
        (iterer lignesearcherdRESET lignesearcherdRESET (cdr lignesearchera) acc))
        acc))
  (iterer (lignesearcher d) (lignesearcher d) (lignesearcher a) empty))

;;quel-stations-pour-twolines-v1
;;commentaires: Ce programme cherche les stations qui permettent d'accéder d'une ligne
;;              de départ à la ligne d'arrivée pour chaque sous liste de la liste que twolines-v1 donne. 
;;              D'ailleurs, ce programme nous donne une liste propre avec la liste des stations appropriés à chaque sous liste de
;;              twolines-v1.

;;Thanusan
(define (quel-stations-pour-twolines d a)
  (define (iterer twolines-v1da acc)
    (if (pair? twolines-v1da)
        (iterer (cdr twolines-v1da) (cons (cons (Ou-est-l-dans-ml (car (car twolines-v1da)) (truelinefinder (car (cdr  (car twolines-v1da))))) (cons (car twolines-v1da) empty)) acc))
        acc))
  (iterer (twolines-v1 d a) empty))

;;quel-stations-pour-twolines-v2
;;commentaires: Ce programme est une évolution du programme quel-stations-pour-twolines. En effet, il a pour rôle de trier
;;              les informations de quel-stations-pour-twolines, afin de faciliter la version finale de twolines.
;;              exemple: si la premiere version du quel-stations-pour-twolines affiche '(("g" "t") ("ligne2" "ligne5"))
;;              la deuxieme affichera '("g" ("ligne2" "ligne5")) ("t" ("ligne2" "ligne5"))).

;;Thanusan
(define (quel-stations-pour-twolines-v2 d a)
  (define (iterer quel-stations-pour-twolinesda acccar acccdr acc)
    (if (pair? quel-stations-pour-twolinesda)
        (if (pair? acccar)
            (iterer quel-stations-pour-twolinesda (cdr acccar) acccdr (cons (cons (car acccar) (cons acccdr empty)) acc))
            (iterer (cdr quel-stations-pour-twolinesda) (car (car quel-stations-pour-twolinesda)) (car (cdr (car quel-stations-pour-twolinesda))) acc))
        (if (pair? acccar)
            (iterer quel-stations-pour-twolinesda (cdr acccar) acccdr (cons (cons (car acccar) (cons acccdr empty)) acc))
            (reverse acc))))
  (iterer (quel-stations-pour-twolines d a) empty empty empty))

;;intervalle-twolines
;;commentaires: Ce programme est très intéressant. En effet, ce programme est la version adapté du programme intervalle pour le programme twolines
;;                                                 on peut le servir pour avoir la liste des stations qui sont entre la station de départ
;;                                                 et la station de correspondance dans la ligne de départ, ou la liste des stations qui sont 
;;                                                 entre la station de correspondance et la station d'arrivée dans la ligne d'arrivée.
;;                                                 C'est le programme indispensable pour la version finale de twolines.

;;Thanusan
(define (intervalle-twolines d a s l)
  (define (iterer acclignearrivé acclignedépart accs acc)
    (cons (intervalle d accs (truelinefinder acclignedépart)) (cons (intervalle accs a (truelinefinder acclignearrivé)) acc)))
  (iterer (car l) (car (cdr l)) s empty))

;;intervalle-de-d-à-c-puis-de-c-à-a
;;commentaires: Ce programme introduit l'une des meilleures évolution qui permet d'accéder à la version finale de twolines.
;;              En effet, grace à l'aide du programme intervalle-twolines, ce programme donne pour chaque sous liste du 
;;              programme quel-stations-pour-twolines-v2, l'intervalle entre la station de départ et la station 
;;              de la correspondance dans la ligne de départ, et l'intervalle entre la station de la correspondance et la
;;              station d'arrivée dans la ligne d'arrivée sous forme de sous liste en respectant l'ordre des sous listes.

;;Thanusan
(define (intervalle-de-d-à-c-puis-de-c-à-a d a)
  (define (iterer quel-stations-pour-twolines-v2da acc)
    (if (pair? quel-stations-pour-twolines-v2da)
        (iterer (cdr quel-stations-pour-twolines-v2da) (cons (intervalle-twolines d a (car (car quel-stations-pour-twolines-v2da)) (car (cdr (car quel-stations-pour-twolines-v2da)))) acc))
       (reverse acc)))
  (iterer (quel-stations-pour-twolines-v2 d a) empty))


;;compteur-twolines
;;commentaires: Ce programme est une mini fonction définie spécialement pour calculer le temps du trajet d'une sous-liste
;;              de la liste donné par le programme intervalle-de-d-à-c-puis-de-c-à-a.

;;Thanusan
(define (compteur-twolines l)
  (define (iterer accl acc)
    (if (pair? accl)
        (iterer (cdr accl) (+ (listcounter-for-travel (car accl)) acc))
        acc))
  (iterer l 5))

;;horaire-twolines
;;commentaires: Ce programme nous donne en effet la liste du programme compteur-twolines pour chaque sous liste de la liste donné
;;              par le programme intervalle-de-d-à-c-puis-de-c-à-a. (Il a la meme fonction que map avec compteur-twolines sur une liste
;;              de sous-liste donné par le programme intervalle-de-d-à-c-puis-de-c-à-a).

;;Thanusan
(define (horaire-twolines d a)
  (define (iterer intervalle-de-d-à-c-puis-de-c-à-ada acc)
    (if (pair? intervalle-de-d-à-c-puis-de-c-à-ada)
        (iterer (cdr intervalle-de-d-à-c-puis-de-c-à-ada) (cons (compteur-twolines (car intervalle-de-d-à-c-puis-de-c-à-ada)) acc))
        (reverse acc)))
  (iterer (intervalle-de-d-à-c-puis-de-c-à-a d a) empty))

;;horaire-avec-chaque-trajet-twolines
;;commentaires: Ce programme nous donne une liste avec l'horaire respectif des sous-listes de la liste donné par le programme
;;              intervalle-de-d-à-c-puis-de-c-à-a grâce au programme horaire-twolines.

;;Thanusan
(define (horaire-avec-chaque-trajet-twolines d a)
  (define (iterer intervalle-de-d-à-c-puis-de-c-à-ada horaire-twolinesda acc)
    (if (pair? intervalle-de-d-à-c-puis-de-c-à-ada)
        (iterer (cdr intervalle-de-d-à-c-puis-de-c-à-ada) (cdr horaire-twolinesda) (cons (cons (car horaire-twolinesda) (cons (car intervalle-de-d-à-c-puis-de-c-à-ada) empty)) acc))
       (reverse acc)))
  (iterer (intervalle-de-d-à-c-puis-de-c-à-a d a) (horaire-twolines d a) empty))

;;info-trajet-pour-twolines
;;commentaires: Ce programme est indispensable pour avoir des informations sur un trajet avec deux lignes.
;;              En effet, ce programme crée à l'aide du programme quel-stations-pour-twolines-v2, une sous-liste qui a dans elle, deux autres
;;              sous-listes, dont la premiere qui contient dans l'ordre, le nom de la station de correspondance, la ligne d'arrivée et la ligne de départ et
;;              et la deuxieme sous-liste est la sous-liste du programme horaire-avec-chaque-trajet-twolines qui correspond à la première sous-liste.
;;              Ce programme aura autant de sous-liste que le programme horaire-avec-chaque-trajet-twolines.

;;Thanusan
(define (info-trajet-pour-twolines d a)
  (define (iterer horaire-avec-chaque-trajet-twolinesda quel-stations-pour-twolines-v2da acc)
    (if (pair? horaire-avec-chaque-trajet-twolinesda)
        (iterer (cdr horaire-avec-chaque-trajet-twolinesda) (cdr quel-stations-pour-twolines-v2da) (cons (cons (car quel-stations-pour-twolines-v2da) (cons (car horaire-avec-chaque-trajet-twolinesda) empty)) acc))
        (reverse acc)))
  (iterer (horaire-avec-chaque-trajet-twolines d a) (quel-stations-pour-twolines-v2 d a) empty))

;;minimum-d'-info-trajet-pour-twolines
;;commentaires: Ce programme donne le minimum par rapport à l'horaire des différentes sous-listes donné par le programme
;;              info-trajet-pour-twolines.

;;Thanusan
(define (minimum-d-info-trajet-pour-twolines d a)
  (define (iterer info-trajet-pour-twolinesda acc)
    (if (pair? info-trajet-pour-twolinesda)
        (if (< (car (car (cdr (car info-trajet-pour-twolinesda)))) (car (car (cdr acc))))
            (iterer (cdr info-trajet-pour-twolinesda) (car info-trajet-pour-twolinesda))
            (iterer (cdr info-trajet-pour-twolinesda) acc))
        acc))
  (iterer (info-trajet-pour-twolines d a) (car (info-trajet-pour-twolines d a))))

;;finaltwolines
;;commentaires: Ce programme est la version finale de twolines et il trie les différentes informations donné par le programme 
;;              minimum-d'-info-trajet-pour-twolines afin de donner les informations nécessaires sur le trajet 
;;              et le temps du trajet.

;;Thanusan
(define (finaltwolines d a)
  (define (iterer acchoraire accstationdecorrespondance acclignedépart acclignearrivé accdirectionld accdirectionla)
    (write "Temps du trajet estimé :") (display acchoraire) (write "minutes. Voici le trajet à suivre : Sur la station ") (display d) (write ", veuillez prendre la ") (display acclignedépart) (write " vers la direction de ") (display accdirectionld) (write ". Puis, veuillez descendre à la station ") (display accstationdecorrespondance) (write " .Et enfin, il ne vous restera plus qu'à prendre la ") (display acclignearrivé) (write " vers la direction de ") (display accdirectionla) (write " et descendre à la station ") (display a) (write ", pour arriver à votre destination")) 
    (iterer (car (car (cdr (minimum-d-info-trajet-pour-twolines d a)))) (car (car (minimum-d-info-trajet-pour-twolines d a))) (car (cdr (car (cdr (car (minimum-d-info-trajet-pour-twolines d a)))))) (car (car (cdr (car (minimum-d-info-trajet-pour-twolines d a))))) (quel-direction d (car (car (minimum-d-info-trajet-pour-twolines d a))) (truelinefinder (car (cdr (car (cdr (car (minimum-d-info-trajet-pour-twolines d a)))))))) (quel-direction (car (car (minimum-d-info-trajet-pour-twolines d a))) a (truelinefinder (car (car (cdr (car (minimum-d-info-trajet-pour-twolines d a)))))))))
                                                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
;---------------------------------------------------------------------------------------programme utilisé par l'utilisateur
;;Thanusan et Aljoscha
(define (itinéraire d a)
  (if (eq? (oneline? d a) true)
      (oneline-info d a)
      (if (eq? (twolines? d a) true)
           (finaltwolines d a)
           (write "Veuillez entrer des stations valides. Merci"))))
      

