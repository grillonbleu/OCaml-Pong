(**
 *  PONG OCaml 
 *  Voir README.md
 *
 *  François Moreau
 *  mai 2013 
 *
 *  Usage : Chargez le tout dans l'interpréteur et tapez "OCamlPong.main();;"
 *)

#load "graphics.cma";;
#load "unix.cma";; 

module type PONG = 
  sig
    val main : unit -> unit
  end;;

module OCamlPong : PONG = struct

(* Constantes *)
let pi = acos (-. 1.);;
let largeur_fenetre, hauteur_fenetre = 640, 480;;
let maxfps = 40.;;


(* État de l'application *)
type etat = Splash | EnJeu | Pause | Quitter ;; 

(* Toutes les fonctions membres des types tjoueur et tballe sont préfixés
   car deux types ne peuvent pas partagés des champs de mêmes noms 
   (ça brise l'inférence de types).

   http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#labels_surcharge *)

(* Les joueurs
   Les fonctions des joueurs donnent accès à leurs
   propriétés, protégées et partagées entre les fonctions
   à partir d'une fermeture par instance.
   Joueur 1 est le joueur 
   Joueur 2 est l'adversaire *)
type tjoueur = {
    (* http://stackoverflow.com/a/1936981 *)
    jr_passe_rect : 'a. (int -> int -> int -> int -> 'a) -> 'a ;

    jr_hauteur_relative : int -> float ;

    jr_getyf : unit -> float ; 
    jr_getvitesse : unit -> float ;
    jr_incyf : float -> tjoueur ;
    jr_sety : int -> tjoueur ; 
  };;

let rec batiJoueur x y  = let 
    largeur = 25 and
    hauteur = 100 and
    x = x and (* coordonnée à gauche de rectangle *)
    y = y and (* coordonnée verticale au milieu du rectangle *)
    vitesse = 100. in {

      (* Reçoit en paramètre une fonction qu'on appliquera aux paramètres :
          - x,
          - y, (du coin supérieur gauche), 
          - largeur,
          - hauteur
         Est utilisé pour le rendu et la détection de collision avec la balle. *)
      jr_passe_rect = (fun f -> f 
      (int_of_float x) 
      ((int_of_float y) - (hauteur / 2))
      largeur
      hauteur );

      (* Prend une coordonnée verticale et évalue sa différence par rapport au milieu du joueur.
         Un résultat de -1.0 indique que le paramètre est sur la limite inférieure du joueur. 
         Un résultat de 1.0 indique que le paramètre est sur la limite supérieure du joueur. 
         Est utilisé pour le calcul d'angle de rebond.  *)
      jr_hauteur_relative = (fun i -> ((float_of_int i) -. y) /. 
                                   ((float_of_int hauteur) /. 2.) ) ;

      jr_getyf = (fun () -> y); 

      jr_incyf = (fun dy -> batiJoueur x (y +. dy)) ;

      jr_sety = (fun nouvy -> batiJoueur x (float_of_int nouvy) );

      jr_getvitesse = (fun () -> vitesse);
};;
    


(* La balle
   Les fonctions de la balle donnent accès à ses
   propriétés, protégées et partagées entre les fonctions
   à partir d'une fermeture. *)
type tballe = {
    b_get_rayon : unit -> int ;
    b_getx : unit -> int ;
    b_getxf : unit -> float ;
    b_getvitessexf : unit -> float ;
    b_getvitesseyf : unit -> float ;
    b_gety : unit -> int ;
    b_getyf : unit -> float ;

    b_set_pos : int -> int -> tballe ;
    b_set_angle : float -> tballe ;
    b_deplace : float -> tballe ;
    b_applique_sur_direction : (float -> float -> (float * float)) -> tballe ;
  };;

let rec batiBalle x y vitessex vitessey =
  let
      x = x and
      y = y and
      vitessex = vitessex and
      vitessey = vitessey and 
      vitesse = 300. and
      rayon = 5 in {

    (* Fonctions d'accès *)
    b_get_rayon = (fun () -> rayon) ;
    b_getx = (fun () -> (int_of_float x)) ;
    b_getxf = (fun () -> x) ;
    b_getvitessexf = (fun () -> vitessex) ;
    b_getvitesseyf = (fun () -> vitessey) ;
    b_gety = (fun () -> (int_of_float y)) ;
    b_getyf = (fun () -> y) ;

    (* Déplace la balle vers une nouvelle position *)
    b_set_pos = (fun nouvx nouvy ->
      batiBalle (float_of_int nouvx)
                (float_of_int nouvy)
            vitessex
            vitessey ) ;
  
    (* Modifie l'angle de déplacement de la balle *)
    b_set_angle = (fun rad ->
      batiBalle x y 
    ((cos rad) *. vitesse)
    ((sin rad) *. vitesse) );

    (* Calcule et applique la position dans laquelle la balle se trouve une 
       fois que "secondes_ecoulees" se sont écoulées. *)
    b_deplace = (fun secondes_ecoulees ->  
      batiBalle (x +. vitessex *. secondes_ecoulees)
            (y +. vitessey *. secondes_ecoulees)
            vitessex
            vitessey );

  (* Applique la fonction "f" au vecteur de vélocité représenté par 
     vitessex et vitessey *)
  b_applique_sur_direction = (fun f ->
    let nouv_vx, nouv_vy = f vitessex vitessey in
    batiBalle x y nouv_vx nouv_vy );
};;



(* Détermine si un point est dans un rectangle. 
   Utilisé pour la détection de collision. *)
let point_dans_rect px py rx ry rw rh = 
  px < rx + rw &&
  px > rx &&
  py < ry + rh &&
  py > ry ;;



(* Opérateur pipe *)
let (|>) x f = f x;;


(* Attente de moins d'une seconde *)
let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x;;

let minisleep (sec: float) =
    (ignore (restart_on_EINTR (Unix.select [] [] []) sec));;


(* Gestion des entrées clavier et souris *)
let entree etat balle joueur1 = match etat with
  | Splash ->
    (if (Graphics.key_pressed ()) then 
      match Graphics.read_key() with
      | 'q' -> Quitter, balle, joueur1
      | ' ' -> EnJeu,
           ((balle.b_set_pos (largeur_fenetre/2) (hauteur_fenetre/2)).b_set_angle 0.),
           joueur1
      | _ -> etat, balle, joueur1
    else etat, balle, joueur1)

  | EnJeu -> let joueur1 = joueur1.jr_sety (snd (Graphics.mouse_pos())) in
    (if (Graphics.key_pressed ()) then 
      match Graphics.read_key() with
      | 'q' -> Quitter, balle, joueur1
      | ' ' -> Pause, balle, joueur1
      | _ -> etat, balle, joueur1
    else etat, balle, joueur1)

  | Pause ->
    (if (Graphics.key_pressed ()) then 
      match Graphics.read_key() with
      | 'q' -> Quitter, balle, joueur1
      | ' ' -> EnJeu, balle, joueur1
      | _ -> etat, balle, joueur1
    else etat, balle, joueur1)

  | Quitter -> etat, balle, joueur1;;



(* Rendu de l'état courant à l'écran *)
let rendu etat balle joueur1 joueur2 =  
  match etat with
  | Splash -> begin
    Graphics.set_color Graphics.black;
    Graphics.fill_rect 0 0 largeur_fenetre hauteur_fenetre;
    Graphics.set_color Graphics.white;
    Graphics.moveto (largeur_fenetre - 300) 10;
    Graphics.draw_string "Appuyez sur [ESPACE] pour commencer";
    Graphics.moveto 10 70;
    Graphics.draw_string "COMMANDES";
    Graphics.moveto 10 50;
    Graphics.draw_string "[SOURIS] -> Deplacer la raquette";
    Graphics.moveto 10 30;
    Graphics.draw_string "[ESPACE] -> Prendre une pause";
    Graphics.moveto 10 10;
    Graphics.draw_string "[TOUCHE Q] -> Quitter";
    Graphics.moveto 275 300;
    Graphics.draw_string "P O N G";
  end

  | EnJeu | Pause -> begin
    if etat = EnJeu then
      Graphics.set_color Graphics.black
    else 
      Graphics.set_color (Graphics.rgb 128 128 128);
    Graphics.fill_rect 0 0 largeur_fenetre hauteur_fenetre;
    Graphics.set_color Graphics.white;
    Graphics.draw_segments [| (largeur_fenetre / 2,  0, largeur_fenetre/2, hauteur_fenetre) |];

    joueur1.jr_passe_rect (Graphics.fill_rect);
    joueur2.jr_passe_rect (Graphics.fill_rect);
    Graphics.fill_rect 
       ((balle.b_getx()) - (balle.b_get_rayon()))
       ((balle.b_gety()) - (balle.b_get_rayon()))
       ((balle.b_get_rayon()) * 2)
       ((balle.b_get_rayon()) * 2)
  end
    
  | Quitter -> ();; 



(* Détecte une collision entre la balle et un joueur et, si c'est le cas,
   dévie la balle en conséquence. *)
let engin_collision_balle_joueur joueur1 joueur2 balle = 
  (* Joueur 1*)
  if balle.b_getvitessexf() < 0.0 &&
    joueur1.jr_passe_rect (point_dans_rect (balle.b_getx()) (balle.b_gety())) then
    balle.b_set_angle ((pi /. 4.) *. joueur1.jr_hauteur_relative (balle.b_gety()))
  (* Joueur 2 *)
  else if balle.b_getvitessexf() > 0.0 &&
    joueur2.jr_passe_rect (point_dans_rect (balle.b_getx()) (balle.b_gety())) then
    balle.b_set_angle (pi -. ((pi /. 4.) *. joueur2.jr_hauteur_relative (balle.b_gety())))
  (* Aucune collision avec un joueur*)
  else
    balle;;



(* Détecte une collision entre la balle et le mur du haut ou du bas et, si c'est le cas,
   dévie la balle en conséquence. *)
let engin_collision_mur balle = 
  (* Mur du bas *)
  if balle.b_gety() < 0 then
    balle.b_applique_sur_direction (fun vx vy -> (vx, abs_float vy) )
  (* Mur du haut *)
  else if balle.b_gety() > hauteur_fenetre - 40 then
    balle.b_applique_sur_direction (fun vx vy -> (vx, -. abs_float vy) )
  (* Aucune collision avec un mur *)
  else
    balle;;



(* Détecte un but marqué par un des joueur et, si c'est le cas, procède à la remise
   au jeu. *)
let engin_but balle =
  (* But par joueur 1 *)
  if balle.b_getx() > largeur_fenetre then 
      (balle.b_set_pos (largeur_fenetre / 2) (hauteur_fenetre / 2)).b_set_angle pi
  (* But par joueur 2 *)
  else if balle.b_getx() < 0 then 
    (balle.b_set_pos (largeur_fenetre / 2) (hauteur_fenetre / 2)).b_set_angle 0.
  (* Aucun but *)
  else 
    balle;;



let engin estampille etat balle joueur1 joueur2 = 
  match etat with
  | Splash | Pause | Quitter ->  balle, joueur1, joueur2

  | EnJeu -> let temps_ecoule = ((Unix.gettimeofday()) -. estampille) in 

             (* Déplacement de joueur2 *)
             let dist_balle_j2 = (joueur2.jr_getyf()) -. (balle.b_getyf()) and
                 step_j2 = temps_ecoule *. (joueur2.jr_getvitesse()) in
             let joueur2 = if dist_balle_j2 > step_j2 then
                 joueur2.jr_incyf (-. step_j2)
               else if dist_balle_j2 < -. step_j2 then
                 joueur2.jr_incyf step_j2
               else
                 joueur2 in

             (* Déplacement de la balle *)
             let balle = (balle.b_deplace temps_ecoule)
             (* Gestion des collisions de la balle *)
             |> engin_collision_balle_joueur joueur1 joueur2
             |> engin_collision_mur
             |> engin_but in
             
             (balle, joueur1, joueur2)
;;



(**** Fonction principale ****)
let main () = 
  begin
    Graphics.open_graph (Printf.sprintf " %dx%d" largeur_fenetre hauteur_fenetre);
    Graphics.auto_synchronize false;

    (* Boucle principale implémentée par une fonction récursive terminale *)
    let rec aux estampille etat balle joueur1 joueur2 =
      begin
        (* Rendu *)
        Graphics.clear_graph ();
        rendu etat balle joueur1 joueur2;
        Graphics.synchronize();

        (* Gestion d'entrée utilisateur *)
        let (etat, balle, joueur1) = entree etat balle joueur1 in

        (* Limiter le nombre de boucles par secondes *)
        let temps_ecoule = (Unix.gettimeofday()) -. estampille in
        if temps_ecoule < (1. /. maxfps) then minisleep ((1. /. maxfps) -. temps_ecoule);
        
        (* Mouvement de la balle, de l'adversaire et gestion des collisions*)
        let (balle, joueur1, joueur2) = engin estampille etat balle joueur1 joueur2 in

        (* Appel récursif conditionnel à l'état du jeu *)
        if etat <> Quitter then aux (Unix.gettimeofday()) etat balle joueur1 joueur2;
      end in
    aux (Unix.gettimeofday()) 
      Splash
      ((batiBalle (float_of_int (largeur_fenetre/2))
	      (float_of_int (hauteur_fenetre/2)) 0. 0.).b_set_angle 0.)
      (batiJoueur (float_of_int 10) (float_of_int (hauteur_fenetre/2)))
      (batiJoueur (float_of_int (largeur_fenetre - 40)) (float_of_int (hauteur_fenetre/2)));
    Graphics.close_graph();
  end;;

end;; (* Fin module OCaml Pong *)
