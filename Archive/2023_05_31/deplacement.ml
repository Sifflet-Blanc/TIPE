open Graphics
open Bvh_tree
open Test
open Couleur
(*open Raytracer*)

let dt = 1.0 /. 20.0(*flux d'information visuel le plus rapide*)
let epsilon0 : float = 0.000000000008854187 (*permitivitée du vide*)
let champ_vision = [|1.91986;(*vision monoculaire*)
					 1.04720;(*vision binoculaire*)
					 0.52360;(*discrimination des couleurs*)
					 0.34907;(*reconnaissance des symboles*)
					 0.17453|](*lecture*)

let maxX : float = 22. (*largeur de la carte en metre*)
let maxY : float = 20. (*hauteur de la carte en metre*)
let maxX_i : int = (int_of_float maxX)
let maxY_i : int = (int_of_float maxY)
let prop_i : int = 1000 / maxX_i (*affichage proportionnel 1 metre = prop pixel*)
let prop : float = (float_of_int prop_i) 
let vitesse_max_humain : float = 9.5 (*m.s-1*)
let acceleration_max_humain : float = 2. (*m.s-2*)
let dmax : float = 8. (*m*)
let k : float = 1000.

type people = {
	largeur : float; (*en metre represente la largeur de la personne epaule droite à epaule gauche*)
	epaisseur : float; (*en metre represente l'epaisseur de la personne la profondeur du corp*)
	rayon : float; (*en metre*)
	masse : float; (*en kilogramme represente la masse de la personne*)
	allure_voulue : float;
	pos_voulue : float * float;
	mutable position : float * float; (*en metre*)
	mutable allure : float; (*en m/s*)
	mutable direction : float; (*en radiant*)
}
(** rayon en m float
	masse en kg float
	position en x y en m float
	allure en m/s float 
	direction en rad float
	int [people] *)

let people_init (l :float) (ep:float) (m:float) (allure : float) (pos: float * float) (allure_voulue:float) (pos_voulue : float * float) (dir:float) : people = 
	{largeur = l; epaisseur = ep; rayon = dist_euclidienne (l, ep) (0., 0.) /. 2.; masse = m; allure_voulue = allure_voulue; pos_voulue = pos_voulue; position = pos; allure = allure; direction = dir}

let set_allure (p : people) a = p.allure <- min a vitesse_max_humain

let set_dir (p : people) d =  p.direction <- float_mod d

let print_people (lp : people list) lcouleur : unit=
	let rec aux lp  lcouleur j n =
		match lp, lcouleur with 
		|p::qp, couleur::qcouleur -> 
					(let x, y = p.position in
					set_color couleur;
					fill_circle (int_of_float (x *. prop)) (int_of_float (y *. prop)) (int_of_float((dist_euclidienne (p.largeur, p.epaisseur) (0., 0.) /. 2.) *. prop));
					set_color (rgb 0 0 0);
					moveto (int_of_float (x *. prop)) (int_of_float (y *. prop));
					lineto (int_of_float ((x +. cos p.direction *. p.rayon) *. prop)) (int_of_float ((y +. sin p.direction *. p.rayon) *. prop));
					aux qp qcouleur (j +. 1.) n)
		|_, _ -> () in
	aux lp lcouleur 0. (float_of_int (List.length lp))

let random_diff0 f = 
	let temp = ref (Random.float f) in
	while !temp < 0.000001 do
		temp := Random.float f
	done;
	!temp

let set_pos lp t _ = 
	let rec aux lp t (acc : ((float * float) * (float * float)) list) : ((float * float) * (float * float)) list =
		match lp with 
		|[] -> List.rev acc
		|p::q ->
			let dis_p = p.allure *. t in
			let x, y = p.position in 
			let newx = x +. (cos p.direction)*.dis_p in 
			let newy = y +. (sin p.direction)*.dis_p in
			p.position <- (newx, newy);
			let a1 = asin (p.largeur /. (2. *. p.rayon)) in
			let a2 = pi/.2. +. asin (p.epaisseur /. (2. *. p.rayon)) in
			aux q t (((newx, newy +. (sin (float_mod (p.direction -. a2))) *. p.rayon) , (newx +. (cos (float_mod (p.direction -. a2))) *. p.rayon, newy))::
					   ((newx, newy -. (sin (float_mod (p.direction -. a1))) *. p.rayon) , (newx -. (cos (float_mod (p.direction -. a1))) *. p.rayon, newy))::
					   ((newx -. (cos (float_mod (p.direction +. a2))) *. p.rayon, newy) , (newx, newy -. (sin (float_mod (p.direction +. a2))) *. p.rayon))::
					   ((newx +. (cos (float_mod (p.direction +. a1))) *. p.rayon, newy) , (newx, newy +. (sin (float_mod (p.direction +. a1))) *. p.rayon))::acc) in
	aux lp t []

let min_abs a b = if min (Float.abs a) (Float.abs b) = Float.abs a then a else b


let print_bvh_tree a =
	let rec aux a =
		match a with
		|Nil -> ()
		|Noeud (g,o,d) -> set_color (rgb 65 0 125);
						fill_rect (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (fst o)) *. prop))
									(abs(int_of_float (((fst (fst o))-.(fst (snd o))) *. prop))) (abs(int_of_float (((snd (fst o))-.(snd (snd o))) *. prop)));
						aux g;
						aux d;
		|Feuille o -> set_color (rgb 125 0 0);
						fill_rect (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (fst o)) *. prop))
									(abs(int_of_float (((fst (fst o))-.(fst (snd o))) *. prop))) (abs(int_of_float (((snd (fst o))-.(snd (snd o))) *. prop))) in
	aux a

let print_obj l =
	let rec aux l =
		set_color (rgb 0 0 0);
		match l with
		|[] -> ()
		|o::q ->
			let x = min (fst (fst o)) (fst (snd o)) in 
			let y = min (snd (fst o)) (snd (snd o)) in 
			let w = max (fst (fst o)) (fst (snd o)) -. x in 
			let h = max (snd (fst o)) (snd (snd o)) -. y in 
			fill_rect (int_of_float (x *. prop)) (int_of_float (y *. prop)) (int_of_float (w *. prop)) (int_of_float (h *. prop));
			aux q
		in
	match l with 
	| [] -> ()
	| _::q -> aux q


let rm lp lc =
	let rec aux lp lc acclp acclc accn =
		match lp, lc with
		| p::qp, c::qc ->
			(*if dist_euclidienne p.position p.pos_voulue < p.rayon*)
			if fst p.position >= fst p.pos_voulue
			then
				aux qp qc acclp acclc accn
			else
				aux qp qc (p::acclp) (c::acclc) (1+accn)
		| _, _ -> ((acclp, acclc), accn) in
	aux lp lc [] [] 0

let changement_direction_allure (lp : people list) (ll : (float * (float * float) * float) list list) (obj : ((float * float) * (float * float)) list) (completobj : ((float * float) * (float * float)) list) : unit =
	(*	lp : liste des personne presente
		ll : liste de (liste des point vu par une personne) par personne
		obj : liste des objects sur le terrain *)
	let rec somme_fij lp ri mi posi i j accx accy = 
		match lp with 
		| [] -> (k *. accx /. mi, k *. accy /. mi) 
		| p::qlp -> if i = j 
			then
				somme_fij qlp ri mi posi i (j+1) accx accy
			else begin 
				let g = ri +. p.rayon -. dist_euclidienne posi p.position in
				(*Printf.printf "%f\n" g;*)
				if g <= 0.
				then 
					somme_fij qlp ri mi posi i (j+1) accx accy
				else begin 
					let alphaij = atan2 (snd posi -. snd p.position) (fst posi -. fst p.position) in
					somme_fij qlp ri mi posi i (j+1) (accx +. g *. cos alphaij) (accy +. g *. sin alphaij) 
				end
			end
		in
	let rec somme_fiw obj ri mi xi yi (accx: float) accy =
		match obj with 
		| [] -> (k *. accx /. mi, k *. accy /. mi) 
		| ((x1, y1), (x2, y2))::qobj -> 
			let gx = min x1 x2 in
			let dx = max x1 x2 in
			let by = min y1 y2 in
			let hy = max y1 y2 in
			let x = ref xi in
			let y = ref yi in
			if dx < xi then x := dx;
			if xi < gx then x := gx;
			if hy < yi then y := hy;
			if yi < by then y := by;
			(*set_color (rgb 0 255 0);
			moveto (int_of_float (prop *. xi)) (int_of_float (prop *. yi));
			lineto (int_of_float (prop *. !x)) (int_of_float (prop *. !y));*)
			let g = ri -. dist_euclidienne (xi, yi) (!x, !y) in
			(*Printf.printf "%f\n" g;*)
			if g <= 0.
			then
				somme_fiw qobj ri mi xi yi accx accy
			else begin 
				let alphaij = atan2 (yi -. !y) (xi -. !x) in
				somme_fiw qobj ri mi xi yi (accx +. g *. cos alphaij) (accy +. g *. sin alphaij) 
			end
		in
 	let rec aux auxlp ll acc =
 		(*appel recursif sur chaque homme*)
		match auxlp,ll with 
		|p::qp,l::ql ->
			(*Printf.printf "%f = op=%f + atan2 y=%f x=%f\n" (0. -. p.direction +. atan2 (snd p.pos_voulue -. snd p.position) (fst p.pos_voulue -. fst p.position)) p.direction (snd p.pos_voulue -. snd p.position) (fst p.pos_voulue -. fst p.position);*)
			let alpha_des = p.direction +. new_orient l dmax (atan2 (snd p.pos_voulue -. snd p.position) (fst p.pos_voulue -. fst p.position) -. p.direction) (*prop (fst p.position) (snd p.position) p.direction*); in
			let (vix, viy) = p.allure *. (cos p.direction), p.allure *. (sin p.direction) in
			let tmp = raytracing_vecteur alpha_des (fst p.position) (snd p.position) p.rayon (sqrt (maxX *. maxX +. maxY *. maxY)) completobj in
			let dh = dist_euclidienne p.position (snd tmp) in
			let v_des = min p.allure_voulue (dh /. dt) in
			let (vx_des, vy_des) = (v_des *. cos alpha_des, v_des *. sin alpha_des) in
			(*set_color (rgb 255 0 0);
			moveto (int_of_float (prop *. fst p.position)) (int_of_float (prop *. snd p.position));
			rlineto (int_of_float (prop *. vx_des)) (int_of_float (prop *. vy_des));
			set_color (rgb 0 0 0);
			moveto (int_of_float (prop *. fst p.position)) (int_of_float (prop *. snd p.position));
			rlineto (int_of_float (prop *. vix)) (int_of_float (prop *. viy));
			rlineto (int_of_float (prop *. (vx_des -. vix) *. dt)) (int_of_float (prop *. (vy_des -. viy) *. dt));*)
			(*calcul des nouvelles vitesse a adopter en fonction des point de contact detecte*)
			let (afijx, afijy) = somme_fij lp p.rayon p.masse p.position acc 0 0. 0. in
			let (afiwx, afiwy) = somme_fiw obj p.rayon p.masse (fst p.position) (snd p.position) 0. 0. in
			let (vx, vy) = (vix +. (vx_des -. vix) +. afijx +. afiwx, viy +. (vy_des -. viy) +. afijy +. afiwy) in
			(*set les nouvelles caractéristiques de l'humain*)
			let new_allure = sqrt (vx *. vx +. vy *. vy) in
			set_allure p (new_allure);
			set_dir p (atan2 vy vx);
			aux qp ql (acc+1)
		|_,_ -> () in 
	aux lp ll 0

let rec detection_pt_physique (lpeople : people list) (obj : ((float*float)*(float*float)) list) : (float * (float * float) * float) list list=
	(*lpeople : liste des humains presents
		obj : tableau contenant les objets qu'il y a sur le terrain*)
	match lpeople with
	| [] -> []
	| p::q ->
		let orient_ray = ref 0. in
		let pt_vu = ref [] in
		moveto (int_of_float(fst p.position *. prop)) (int_of_float(snd p.position *. prop));
		set_color (rgb 150 25 25);
		for _ = 0 to 255 do
			let r1 = random_diff0 1. in (*float aleatoire entre 0. et 1. different de .0*)
			let r2 = Random.float 1. in
			let z = (sqrt((-2.) *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) /. 1.348172 in
			orient_ray := asin (z);
			(*orient_ray := float_of_int i *. 2. *. pi /. 255.;*)
			(match raytracing_vecteur (p.direction +. !orient_ray) (fst p.position) (snd p.position) p.rayon dmax obj with
			|Some _,(x,y) ->
							(*lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float (fst p.position *. prop)) (int_of_float (snd p.position *. prop));*)
							let dist = dist_euclidienne (x,y) p.position in
							pt_vu := (dist, (x,y), !orient_ray) :: !pt_vu
			|None,_ -> ());
			(match raytracing_vecteur (p.direction -. !orient_ray) (fst p.position) (snd p.position) p.rayon dmax obj with
			|Some _,(x,y) ->
							(*lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float (fst p.position *. prop)) (int_of_float (snd p.position *. prop));*)
							let dist = dist_euclidienne (x, y) p.position in 
							pt_vu := (dist, (x,y), 0. -. !orient_ray) :: !pt_vu

			|None,_ -> ());
		done;
		!pt_vu :: (detection_pt_physique q obj)

let randomarray n max =
	if max < n then failwith "impossible ligne 280";
	let rep = Array.make n (-1) in
	for i = 0 to n-1 do
		let tmp = ref (Random.int max) in
		while Array.exists (fun i -> i = !tmp) rep do
			tmp := Random.int max
		done;
		rep.(i) <- !tmp
	done;
	rep

let test_prog () = 
	(*let start_time = Sys.time () in*)
	Random.init 90;

	open_graph (Printf.sprintf " %dx%d" (prop_i*maxX_i) (prop_i*maxY_i));
	set_window_title "Deplacement test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	set_color (rgb 255 255 255);

	let n = ref 90 in
	let tab = randomarray !n (maxY_i * maxY_i) in
	let ltest = ref (List.init !n (fun i -> people_init 0.579 0.33 70. 
		0. (float_of_int (tab.(i)/maxY_i) +. 0.5, float_of_int (tab.(i) mod maxY_i) +. 0.5)
		4. (maxX -. 1.5, maxY /. 2.) (Random.float (2. *. pi)))) in
	let lcouleur = ref (List.init !n (fun i -> 
		let c = float_of_int (tab.(i)) /. float_of_int (maxY_i * maxY_i) in 
		rgb (r c) (g c) (b c))) in

	let obj = [((-1., Float.neg_infinity), (0., Float.infinity));
		  ((maxX, Float.neg_infinity), (maxX+.1., Float.infinity));
		  ((Float.neg_infinity, -1.), (Float.infinity, 0.));
		  ((Float.neg_infinity, maxY), (Float.infinity, maxY+.1.));
		  (*((0.,0.),(maxX, maxY));*)
		  (((maxX -. 2.), 0.), ((maxX -. 1.), (maxY /. 2.) -. 1.5));
		  (((maxX -. 2.), (maxY /. 2.) +. 1.5), ((maxX -. 1.), maxY));
		  (*((0., 1.), (maxX, 2.));
		  ((0., maxY -. 2.), (maxX, maxY -. 1.));
		  ((1., 0.), (2., maxY))*)] in
	let people_block = ref [] in 
	let bvh = construit_bvh 1 obj in
	print_bvh bvh;
	let continue = ref true in 
	while !n > 0 do 
		let time = Sys.time() in
		if key_pressed () then continue := false;
		set_color (rgb 250 250 250);	
		fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	   	(*print_donne_people ltest;*)
		let tmp = rm !ltest !lcouleur in 
	   	ltest := fst (fst tmp);
	   	lcouleur := snd (fst tmp);
	   	n := snd tmp;
		people_block := set_pos !ltest dt bvh;
	   	print_people !ltest !lcouleur;
		print_obj (obj (*@ !people_block*));
		let lo = detection_pt_physique !ltest (!people_block @ obj) in
		(*let lo = detection_pt_physique_bvh !ltest (construit_bvh 1 (obj @ !people_block)) in*)
		changement_direction_allure !ltest lo obj (!people_block @ obj);
		while Sys.time () < time +. (dt *. 1.) do if key_pressed () then continue := false done;
   done;
   close_graph ()

let _ =
	Random.self_init ();
	test_prog ()

(*dune build --watch --terminal-persistence=clear-on-rebuild*)
(*OCAMLRUNPARAM=b ./_build/default/deplacement.exe*)
(*pb =	-le mur du bord*)