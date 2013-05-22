Motivation
==========

J'ai essentiellement programmé ce OCaml Pong pour me familiariser avec OCaml et avec certains concepts de la programmation fonctionnelle.

Récursion terminale (tail recursion)
-------------------

`main` utilise la récursion terminale pour ne pas faire déborder la pile.


Fermeture (closures)
---------------

`balle` et `joueur` sont des enregistrements composés de fonctions donnant accès à des popriétés privées contenus dans des fermetures.


Fonctions comme valeurs de première classe (first-class functions)
-------------------------------
`tballe.b_applique_sur_direction` et `tjoueur.jr_passe_rect` prennent une fonction en paramètre.


Types et filtres
----------------

L'état du jeu est représenté à l'aide d'un type pouvant être de forme `Splash | EnJeu | Pause | Quitter`. À l'aide de filtres, les fonctions `entree`, `engin` et `rendu` agissent différemment dépendamment de l'état du jeu.


Idées d'extension
==========

1. Pointage.
3. Effets visuels (sur contact entre la balle et un joueur, sur rebond au mur, traînée derrière la balle, etc.).
4. Pouvoir changer de joueur en pleine partie.
5. État "but marqué" marquant un temps d'arrêt après qu'un but soit marqué.