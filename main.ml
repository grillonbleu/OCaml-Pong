(**
 *  PONG OCaml
 *
 *  François Moreau
 *  mai 2013 
 *  TODO Instructions pour quitter, pause et souris
 *)

#load "graphics.cma";;
#load "unix.cma";; 

(* Constantes *)

let pi = acos (-. 1.);;
let largeur_fenetre, hauteur_fenetre = 640, 480;;


(* Joueurs 
   Joueur 1 est le joueur 
   Joueur 2 est l'adversaire *)

let largeur_joueur = 10;;
let hauteur_joueur = 100;;

let joueur1_x = 20;;
let joueur1_y = ref (hauteur_fenetre / 2);;

let joueur2_x = (largeur_fenetre - 30);;
let joueur2_y = ref (float_of_int (hauteur_fenetre / 2));;
let joueur2_vitesse = 80.;; (* en pixels par seconde *)



(* La balle
   Les fonctions de la balle donnent accès à ses
   propiétés, protégées et partagées entre les fonctions
   à partir d'une fermeture commune. *)
type tballe = {
    set_pos : int -> int -> unit ;
    set_angle : float -> unit ;
    get_rayon : unit -> int ;
    getx : unit -> int ;
    getxf : unit -> float ;
    getvitessexf : unit -> float ;
    gety : unit -> int ;
    getyf : unit -> float ;
    deplace : float -> unit ;
    applique_sur_direction : (float -> float -> (float * float)) -> unit ;
  };;

let balle = (function() -> begin
  (* Propriétés privées *)
  let x, y, vitessex, vitessey, vitesse, rayon = ref 0., ref 0., ref 0., ref 0., 300., 5 in {

    (* Fonctions d'accès *)
    get_rayon = (fun () -> rayon) ;
    getx = (fun () -> (int_of_float !x)) ;
    getxf = (fun () -> !x) ;
    getvitessexf = (fun () -> !vitessex) ;
    gety = (fun () -> (int_of_float !y)) ;  
    getyf = (fun () -> !y) ;

    (* Déplace la balle vers une nouvelle position *)
    set_pos = (fun nouvx nouvy ->
      x := (float_of_int nouvx) ; 
      y := (float_of_int nouvy) ;
    ) ;
  
    (* Modifie l'angle de déplacement de la balle *)
    set_angle = (fun rad ->
      vitessex := (cos rad) *. vitesse ;
      vitessey := (sin rad) *. vitesse ;
    );

   (* Calcule et applique la position dans laquelle la balle se trouve une 
      fois que "secondes_ecoulees" se sont écoulées. *)
   deplace = (fun secondes_ecoulees ->  
     x := !x +. !vitessex *. secondes_ecoulees ; 
     y := !y +. !vitessey *. secondes_ecoulees ; 
   );

  (* Applique la fonction "f" au vecteur de vélocité représenté par 
     vitessex et vitessey *)
  applique_sur_direction = (fun f ->
    let nouv_vx, nouv_vy = f !vitessex !vitessey in
    vitessex := nouv_vx ;
    vitessey := nouv_vy ;
  );
}
end)();;



(* Détermine si un point est dans un rectangle. 
   Utilisé pour la détection de collision. *)
let point_dans_rect px py rx ry rw rh = 
  px < rx + rw &&
  px > rx &&
  py < ry + rh &&
  py > ry ;;



(* État de l'application *)
type etat = Splash | EnJeu | Pause | Quitter ;; 
let etat_courant = ref Splash;;


(* Gestion des entrées clavier et souris *)
let entree () =  match !etat_courant with

  | Splash -> begin
    if (Graphics.key_pressed ()) then 
	  match Graphics.read_key() with
	  | 'q' -> (etat_courant := Quitter)
	  | ' ' -> begin
        (etat_courant := EnJeu);
	balle.set_pos (largeur_fenetre / 2) (hauteur_fenetre / 2) ;
	balle.set_angle 0. ;
      end
	  | _ -> ()
  end

  | EnJeu -> begin
    (if (Graphics.key_pressed ()) then 
	  match Graphics.read_key() with
	  | 'q' -> (etat_courant := Quitter)
	  | ' ' -> (etat_courant := Pause)
	  | _ -> ());
    joueur1_y := snd (Graphics.mouse_pos());
  end

  | Pause -> begin
    if (Graphics.key_pressed ()) then 
	  match Graphics.read_key() with
	  | 'q' -> (etat_courant := Quitter)
	  | ' ' -> (etat_courant := EnJeu)
	  | _ -> ()
  end

  | Quitter -> ();;



(* Rendu de l'état courant à l'écran *)
let rendu () =  
  match !etat_courant with
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
    if !etat_courant = EnJeu then
      Graphics.set_color Graphics.black
    else 
      Graphics.set_color (Graphics.rgb 128 128 128);
      Graphics.fill_rect 0 0 largeur_fenetre hauteur_fenetre;
      Graphics.set_color Graphics.white;
      Graphics.draw_segments [| (largeur_fenetre / 2,  0, largeur_fenetre/2, hauteur_fenetre) |];
      Graphics.fill_rect joueur1_x (!joueur1_y                - hauteur_joueur/2) largeur_joueur hauteur_joueur;
      Graphics.fill_rect joueur2_x ((int_of_float !joueur2_y) - hauteur_joueur/2) largeur_joueur hauteur_joueur;
      Graphics.fill_rect
	((balle.getx()) - (balle.get_rayon()))
	((balle.gety()) - (balle.get_rayon()))
	((balle.get_rayon()) * 2)
	((balle.get_rayon()) * 2)
  end
	
  | Quitter -> ();; 



(* Calcul et application des déplacements, détection de collisions et comportement de l'adversaire. *)
let engin estampille = 
  match !etat_courant with
  | Splash -> ()

  | EnJeu -> let temps_ecoule = (Unix.gettimeofday()) -. estampille in begin
    (* Déplacement de la balle *)
    balle.deplace temps_ecoule ;

    (* Déplacement du joueur 2 *)
    let dist_balle_j2, step_j2 = !joueur2_y -. (balle.getyf()), temps_ecoule *. joueur2_vitesse in
    if dist_balle_j2 > step_j2 then
      joueur2_y := !joueur2_y -. step_j2
    else if dist_balle_j2 < -. step_j2 then
      joueur2_y := !joueur2_y +. step_j2;
    
    (* Collision balle/joueur1 (angles de rebond de de pi/4 à -pi/4) *)
    if balle.getvitessexf() < 0.0 &&
        point_dans_rect 
	(balle.getx()) (balle.gety())
	joueur1_x (!joueur1_y - hauteur_joueur/2)
	largeur_joueur hauteur_joueur then
      begin
	balle.set_angle ((pi /. 4.) *. 
			 (float_of_int (balle.gety() - !joueur1_y)) /.
			 (float_of_int (hauteur_joueur / 2)));
      end;

    (* Collision balle/joueur2 (angles de rebond de 3pi/4 à 5pi/4)  *)
    if balle.getvitessexf() > 0.0 &&
         point_dans_rect 
	(balle.getx()) (balle.gety())
	joueur2_x ((int_of_float !joueur2_y) - hauteur_joueur/2)
	largeur_joueur hauteur_joueur then
      begin
	balle.set_angle (pi -. ((pi /. 4.) *. 
				(balle.getyf() -. !joueur2_y) /.
				(float_of_int (hauteur_joueur / 2))));
      end;
    
    (* Collision entre la balle et le mur du haut *)
    if balle.gety() > hauteur_fenetre - 40 then
      balle.applique_sur_direction (fun vx vy -> (vx, -. abs_float vy) );

    (* Collision entre la balle et le mur du bas *)
    if balle.gety() < 0 then
      balle.applique_sur_direction (fun vx vy -> (vx, abs_float vy) );

    (* But par joueur 1 *)
    if balle.getx() > largeur_fenetre then begin
      balle.set_pos (largeur_fenetre / 2) (hauteur_fenetre / 2);
      balle.set_angle pi ;
    end ;

    (* But par joueur 2 *)
    if balle.getx() < 0 then begin
      balle.set_pos (largeur_fenetre / 2) (hauteur_fenetre / 2);
      balle.set_angle 0. ;
    end ;
  end

  | Pause -> ()

  | Quitter -> ();; 



(**** Fonction principale ****)
let main () = 
  begin
    Graphics.open_graph (Printf.sprintf " %dx%d" largeur_fenetre hauteur_fenetre);
    Graphics.auto_synchronize false;
    etat_courant := Splash;

    (* Boucle principale implémentée par une fonction récursive terminale *)
    let rec aux estampille =
      begin
	(* Rendu *)
        Graphics.clear_graph ();
	rendu ();
        Graphics.synchronize();

	(* Gestion d'entrée utilisateur *)
	entree ();

        (* Déplacements, collisions et comportement de l'adversaire *)
        engin estampille;

        (* Appel récursif conditionnel à l'état du jeu *)
        if !etat_courant <> Quitter then aux (Unix.gettimeofday());
      end in
    aux (Unix.gettimeofday());
    Graphics.close_graph();
  end;;
