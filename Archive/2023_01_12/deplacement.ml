open Graphics
open Bvh_tree

let minimal_frame_time = 1.0 /. 10.0(*flux d'information visuel le plus rapide*)
let pi : float = acos(-1.0)
let alpha : float = 0.0003(*detail visible le plus petit en rad*)
let epsilon0 : float = 0.000000000008854187 (*permitivitée du vide*)
let champ_vision = [|1.91986;(*vision monoculaire*)
					 1.04720;(*vision binoculaire*)
					 0.52360;(*discrimination des couleurs*)
					 0.34907;(*reconnaissance des symboles*)
					 0.17453|](*lecture*)

let maxX : float = 10.
let maxY : float = 10.
let maxX_i : int = (int_of_float maxX)
let maxY_i : int = (int_of_float maxY)
let prop_i : int = 100 (*affichage proportionnel 1 metre = prop pixel*)
let prop : float = 100. 

let init_graphics_engine =
    open_graph (Printf.sprintf " %dx%d" (prop_i*maxX_i) (prop_i*maxY_i))

type people = {
	largeur : float; (*en metre represente la largeur de la personne epaule droite à epaule gauche*)
	epaisseur : float; (*en metre represente l'epaisseur de la personne la profondeur du corp*)
	rayon : float; (*en metre*)
	masse : float; (*en kilogramme represente la masse de la personne*)
	mutable position : float * float; (*en metre*)
	mutable allure : float; (*en m/s*)
	mutable direction : float; (*en radiant*)
	mutable sens : int (*1 ou -1*)
}
(** rayon en m float
	masse en kg float
	position en x y en m float
	allure en m/s float 
	direction en rad float
	sens 1 si positif et -1 si negative  int [people] *)

let rec orient_mod orient = 
	if orient > 2. *. pi then orient_mod (orient -. 2. *. pi)
	else if orient < 0. then orient_mod (orient +. 2. *. pi)
	else orient

let dist_euclidienne (x,y) (x',y') =
   sqrt ((x-.x')*.(x-.x') +. (y-.y')*.(y-.y'))

let people_init (l :float) (ep:float) (m:float) (pos: float * float) (allure:float) (dir:float) (sens:int) : people = 
	{largeur = l; epaisseur = ep; rayon = dist_euclidienne (l, ep) (0., 0.) /. 2.; masse = m; position = pos; allure = allure; direction = dir; sens = sens}

let set_allure (p : people) a = if a < 15. then p.allure <- a else p.allure <- 15.

let rec set_dir (p : people) d = 
	if d > 2.0 *. pi then set_dir p (d -. 2.0 *. pi)
	else if d < 0.0 then set_dir p (d +. 2.0 *. pi)
	else p.direction <- d

let set_sens (p : people) s = p.sens <- s

let rec print_people (lp : people list) (lcarre : ((float * float) * (float * float)) list) : unit=
	set_color (rgb 0 200 0);
	match lp, lcarre with 
	|p::qp, a::b::c::d::qcarre -> 
				(let x, y = p.position in
				let tab = [|a; b; c; d|] in
				fill_circle (int_of_float (x *. prop)) (int_of_float (y *. prop)) (int_of_float((dist_euclidienne (p.largeur, p.epaisseur) (0., 0.) /. 2.) *. prop));
				set_color (rgb 255 20 20);
				for i = 0 to 3 do
					let e = min (fst (fst tab.(i))) (fst (snd tab.(i))) in 
					let f = min (snd (fst tab.(i))) (snd (snd tab.(i))) in 
					let w = max (fst (fst tab.(i))) (fst (snd tab.(i))) -. e in 
					let h = max (snd (fst tab.(i))) (snd (snd tab.(i))) -. f in 
					fill_rect (int_of_float (e *. prop)) (int_of_float (f *. prop)) (int_of_float (w *. prop)) (int_of_float (h *. prop))
				done;
				set_color (rgb 0 0 200);
				moveto (int_of_float (fst (snd a) *. prop)) (int_of_float (snd (fst a) *. prop));
				lineto (int_of_float (fst (snd b) *. prop)) (int_of_float (snd (fst b) *. prop)); 
				lineto (int_of_float (fst (fst d) *. prop)) (int_of_float (snd (snd d) *. prop)); 
				lineto (int_of_float (fst (fst c) *. prop)) (int_of_float (snd (snd c) *. prop));  
				lineto (int_of_float (fst (snd a) *. prop)) (int_of_float (snd (fst a) *. prop));
				moveto (int_of_float (x *. prop)) (int_of_float (y *. prop));
				lineto (int_of_float ((x +. cos p.direction *. p.rayon) *. prop)) (int_of_float ((y +. sin p.direction *. p.rayon) *. prop));
				set_color (rgb 255 255 255);
				print_people qp qcarre)
	|_, _-> ()

let print_donne_people (lp : people list) =
	let rec aux lp acc = 
		match lp with 
		|[] -> ()
		|p::q -> Printf.printf "people %d position = %f; %f\nallure = %f\ndirection = %f\nsens = %d\n\n" acc (fst p.position) (snd p.position) p.allure p.direction p.sens;
				flush stdout;
				aux q (acc-1) in 
	aux lp (List.length lp)

let random_diff0 f = 
	let temp = ref (Random.float f) in
	while !temp < 0.00005 do
		temp := Random.float f
	done;
	!temp

let rec intersection a bvh : 'a option =
	let x = fst a in
	let y = snd a in
	match bvh with
	|Nil -> None 
	|Noeud (g, o, d) -> if fst (fst o) <= x && x <= fst (snd o) && snd (fst o) <= y && y <= snd (snd o)
						then begin 
							set_color (rgb 0 255 0);
							let i = intersection a g in
							if i <> None
							then i 
							else intersection a d
						end
						else None
	|Feuille o -> if fst (fst o) <= x && x <= fst (snd o) && snd (fst o) <= y && y <= snd (snd o)
					then Some o
				else None

let set_pos lp t _ = 
	let rec aux lp t (acc : ((float * float) * (float * float)) list) : ((float * float) * (float * float)) list =
		match lp with 
		|[] -> acc
		|p::q ->
			let dis_p = p.allure *. t in (*norme de la distance parcourue*)
			let x, y = p.position in 
			(*if (intersection (x +. (cos p.direction)*.dis_p, y +. (sin p.direction)*.dis_p) bvh) != None
			then*) 
			let newx = x +. (cos p.direction)*.dis_p in 
			let newy = y +. (sin p.direction)*.dis_p in
			p.position <- (newx, newy);
			let a1 = asin (p.largeur /. (2. *. p.rayon)) in
			let a2 = pi/.2. +. asin (p.epaisseur /. (2. *. p.rayon)) in
			aux q t (((newx, newy +. (sin (orient_mod (p.direction -. a2))) *. p.rayon) , (newx +. (cos (orient_mod (p.direction -. a2))) *. p.rayon, newy))::
					   ((newx, newy -. (sin (orient_mod (p.direction -. a1))) *. p.rayon) , (newx -. (cos (orient_mod (p.direction -. a1))) *. p.rayon, newy))::
					   ((newx -. (cos (orient_mod (p.direction +. a2))) *. p.rayon, newy) , (newx, newy -. (sin (orient_mod (p.direction +. a2))) *. p.rayon))::
					   ((newx +. (cos (orient_mod (p.direction +. a1))) *. p.rayon, newy) , (newx, newy +. (sin (orient_mod (p.direction +. a1))) *. p.rayon))::acc) in
	aux lp t []

let changement_direction_allure (lp : people list) (ll: (float * (float * float)) list list) : unit =
	(*calculer les forces de coulomb entre p et tout les pt dans l pour obtenir l'acceleration v(t1) = a*(t1-t0) + v(t0)*)
	let q = 0.05 in
	let q' = 0.05 in
	(*Printf.printf "%f %f\n" p.allure p.direction;*)
	(*ici toute les point de contact seront géré*)
	let rec traitement_contacta contact (ax : float) (ay : float)=
		match contact with
		|[] -> Printf.printf "\n";(ax, ay)
		|(a,b)::l ->(*a - ((a|po) / (po|po)) po avec a le vecteur acceleration et po le vecteur de la personne a l'objet*)
					let pa = (((a -. ax) *. ax) +. ((b -. ay) *. ay)) /. ((a -. ax) *. (a -. ax) +. (b -. ay) *. (b -. ay)) in
					let ax' = ax -. pa *. (a -. ax) in
					let ay' = ay -. pa *. (b -. ay) in
					(*Printf.printf "obj : (%f %f) new_acceleration : %f %f\n" a b ax' ay';*)
					traitement_contacta l ax' ay' in
	let rec traitement_contactv contact vx vy =
		match contact with
		|[] -> Printf.printf "\n";(vx, vy)
		|(a,b)::l ->(*a - ((a|po) / (po|po)) po avec a le vecteur acceleration et po le vecteur de la personne a l'objet*)
					let pv = (((a -. vx) *. vx) +. ((b -. vy) *. vy)) /. ((a -. vx) *. (a -. vx) +. (b -. vy) *. (b -. vy)) in
					let vx' = vx -. pv *. (a -. vx) in
					let vy' = vy -. pv *. (b -. vy) in
					(*Printf.printf "obj : (%f %f) new_vitesse : %f %f\n" a b vx' vy';*)
					traitement_contactv l vx' vy' in
	(*calcul du pfd par itération sur tout les points vu*)
	let rec pfd (l : (float * (float * float)) list) (acceleration : float * float) (x : float) (y : float) (contact : (float * float) list) p =
		match l with
		|[] -> 
				let ax, ay = traitement_contacta contact (fst acceleration) (snd acceleration) in
				((ax *. p.masse, ay *. p.masse), contact)
		|(d,(a,b))::l' -> 	if p.rayon >= d 
							then begin 
								pfd l' acceleration x y ((a,b)::contact) p
							end
							else begin
								let angle = atan ((a -. x) /. (b -. y)) in
								if angle <> angle 
								then begin Printf.printf "pb angle %f %f %f %f\n" a b x y end;
								let f_repultion = ref ((q *. q') /. (4. *. pi *. d *. d)) in
								if f_repultion <> f_repultion
								then begin Printf.printf "pb f_repultion %f\n" d end;
								if !f_repultion > 0.04
								then f_repultion := 0.04;
								pfd l' ((fst acceleration) -. !f_repultion *. (cos angle), 
									(snd acceleration) -. !f_repultion *. (sin angle)) x y contact p
							end in
	let rec aux lp ll =
		match lp,ll with 
		|p::qp,l::ql ->
			(*on prend l'acceleration issue d'un pfd*)
			let ((ax,ay),contact) = pfd l (0., 0.) (fst p.position) (snd p.position) [] p in
			(*la vitesse de la perosonne en base cartesienne*)
			let (vx,vy) = p.allure *. (cos p.direction), p.allure *. (sin p.direction) in
			(*calcul des nouvelles vitesse a adopter*)
			let (new_vx, new_vy) = traitement_contactv contact (ax *. minimal_frame_time +. vx)  (ay *. minimal_frame_time +. vy) in
			(*set les caractéristique de l'humain*)
			let new_allure = sqrt (new_vx *. new_vx +. new_vy *. new_vy) in
			set_allure p (new_allure);
			(*ici on corrige les cas de 0*)
			if new_vy = 0. 
			then begin 
				if new_vx > 0.
				then set_dir p 0.
				else set_dir p pi 
			end
			else begin
				if new_vx = 0.
				then begin set_dir p (((Float.abs new_vy) /. new_vy) *. (pi /. 2.)) end
				else set_dir p (tan (new_vy /. new_vx));
			end;
			Printf.printf "changement_allure dir : %f new_vy : %f new_vx : %f new_all : %f\nax : %f ay : %f vx : %f vy : %f\n"
							 p.direction new_vy new_vx new_allure ax ay vx vy;
			(*Printf.printf "après %f\n" p.direction*) 
			aux qp ql 
		|_,_ -> () in 
	aux lp ll

let raytracing_bvh orient pos bvh = 
	let x = ref (fst pos) in
	let y = ref (snd pos) in
	let rep = ref None in
	(*Printf.printf "__%f %f %f \n" orient (cos orient) (sin orient);*)
	while !rep == None do
		(*if !x +. (cos orient)*. (1. /. prop) <> !x +. (cos orient)*. (1. /. prop)
		then begin Printf.printf "pb raytracing_bvh x %f %f %f" !x (cos orient) orient end;
		if !y +. (sin orient)*. (1. /. prop) <> !y +. (sin orient)*. (1. /. prop)
		then begin Printf.printf "pb raytracing_bvh y %f %f %f" !x (cos orient) orient end;*)
		x := !x +. (cos orient)*. (1. /. prop);
		y := !y +. (sin orient)*. (1. /. prop);
		(*Printf.printf "%d %d\n" (int_of_float (!x)) (int_of_float (!y));*)
		if 0. < !x && !x < maxX && 0. < !y && !y < maxY
		then begin
			(*let c = point_color (int_of_float (!x)) (int_of_float (!y)) in
			let r = ref (((c lsr 16)) mod 256) in
			let g = ref (((c lsr 8)) mod 256) in
			let b = ref ((c) mod 256) in
			if !r < 155
			then r := !r+100
			else begin
				if !b < 255
				then b := !b+3
				else g := !g+1
			end;
			set_color (rgb !r !g !b);*)
			(*
			for i = 0 to Array.length obj -1 do
				match obj.(i) with 
				|((a,b),(c,d)) -> if (a < !x && !x < c) && (b < !y && !y < d) then rep := Some i 
			done*)
			plot (int_of_float (!x *. prop)) (int_of_float (!y *. prop));
			rep := intersection (!x, !y) bvh
		end
		else 
			rep := Some ((0.,0.),(0.,0.))
	done;
	!rep,(!x,!y)

let raytracing_vecteur orient p obj =
	let o = orient_mod orient in
	let a = tan o in
	let b = (snd p.position) -. a *. (fst p.position) in
	let rep = ref (Some ((0., 0.), (0., 0.))) in
	let x = ref None in
	let y = ref None in
	(*Printf.printf "orient = %f;\ta = %f;\tb = %f" o a b;*)
	let rec aux obj a b =
		match obj with 
		|[] -> ()
		|obji::q ->
			let c, d, e, f = (fst (fst obji)), (snd (fst obji)), (fst (snd obji)), (snd (snd obji)) in
			(*Printf.printf "%f\n%f\n%f\t%f\n" f d c e;*)
			(*
			f	.-------*
				|       |
				|  obj  |
				|       |
			d	*-------.
			
				c       e
			*)
			(*verification de l'intersection avec les segments verticaux*)
			(*verif de l'intersection
				verif que l'intersection se face du bon cote (droite si l'humain regarde a droite et gauche si non)
				verif que l'intersection se face sur la bonne hauteur (haut si l'humain regarde en haut et bas si non) *)
			if (min d f) <= a *. c +. b && a *. c +. b <= (max d f) && 
				((c -. (fst p.position) >= 0. && (o <= pi/. 2. || o >= 3. *. (pi /. 2.))) || (c -. (fst p.position) <= 0. && o >= pi/. 2. && o <= 3. *. (pi /. 2.))) && 
				((a *. c +. b -. (snd p.position) >= 0. && o <= pi) || (a *. c +. b -. (snd p.position) <= 0. && o >= pi)) && dist_euclidienne (c,a*. c +. b) p.position > p.rayon
			then begin 
				if (!x <> None && dist_euclidienne (c,a*. c +. b) p.position <= dist_euclidienne (Option.get !x, Option.get !y) p.position) || !x = None
				then begin
					x := Some c;
					y := Some (a *. c +. b);
					rep := Some obji 
				end
			end;
			if (min d f) <= a *. e +. b && a *. e +. b <= (max d f) && 
				((e -. (fst p.position) >= 0. && (o <= pi/. 2. || o >= 3. *. (pi /. 2.))) || (e -. (fst p.position) <= 0. && o >= pi/. 2. && o <= 3. *. (pi /. 2.))) && 
				((a *. e +. b -. (snd p.position) >= 0. && o <= pi) || (a *. e +. b -. (snd p.position) <= 0. && o >= pi)) && dist_euclidienne (e,a*. e +. b) p.position > p.rayon
			then begin
				if (!x <> None && dist_euclidienne (e,a*. e +. b) p.position <= dist_euclidienne (Option.get !x, Option.get !y) p.position) || !x = None
				then begin
					x := Some e;
					y := Some (a *. e +. b);
					rep := Some obji 
				end
			end;
			(*verification de l'intersection avec les segments horisontaux*)
			(*Printf.printf "(min %f %f) <= (%f -. %f) /. %f && (%f -. %f) /. %f <= (max %f %f)\n" c e d b a d b a c e;
			Printf.printf "%f <= %f && %f <= %f\n" (min c e) ((d -. b) /. a) ((d -. b) /. a) (max c e);
			Printf.printf "%f > %f\n" (dist_euclidienne ((d -. b) /. a,d) p.position) p.rayon;*)
			if (min c e) <= (d -. b) /. a && (d -. b) /. a <= (max c e) && 
				(((d -. b) /. a -. (fst p.position) >= 0. && (o <= pi/. 2. || o >= 3. *. (pi /. 2.))) || ((d -. b) /. a -. (fst p.position) <= 0. && o >= pi/. 2. && o <= 3. *. (pi /. 2.))) && 
				((d -. (snd p.position) >= 0. && o <= pi) || (d -. (snd p.position) <= 0. && o >= pi)) && dist_euclidienne ((d -. b) /. a,d) p.position > p.rayon
			then begin
				if (!x <> None && dist_euclidienne ((d -. b) /. a,d) p.position <= dist_euclidienne (Option.get !x, Option.get !y) p.position) || (!x = None && !y = None)
				then begin
					x := Some ((d -. b) /. a);
					y := Some d;
					rep := Some obji 
				end
			end;
			(*Printf.printf "(min %f %f) <= (%f -. %f) /. %f && (%f -. %f) /. %f <= (max %f %f)\n" c e f b a f b a c e;
			Printf.printf "%f <= %f && %f <= %f\n" (min c e) ((f -. b) /. a) ((f -. b) /. a) (max c e);
			Printf.printf "%f > %f\n" (dist_euclidienne ((f -. b) /. a,f) p.position) p.rayon;*)
			if (min c e) <= (f -. b) /. a && (f -. b) /. a <= (max c e) && 
				(((f -. b) /. a -. (fst p.position) >= 0. && (o <= pi/. 2. || o >= 3. *. (pi /. 2.))) || ((f -. b) /. a -. (fst p.position) <= 0. && o >= pi/. 2. && o <= 3. *. (pi /. 2.))) && 
				((f -. (snd p.position) >= 0. && o <= pi) || (f -. (snd p.position) <= 0. && o >= pi)) && dist_euclidienne ((f -. b) /. a,f) p.position > p.rayon
			then begin
				if (!x <> None && dist_euclidienne ((f -. b) /. a,f) p.position <= dist_euclidienne (Option.get !x, Option.get !y) p.position) || (!x = None && !y = None)
				then begin
					x := Some ((f -. b) /. a);
					y := Some f;
					rep := Some obji 
				end
			end;
			aux q a b
		in
	aux (((-1., Float.neg_infinity), (0., Float.infinity))::
		  ((maxX, Float.neg_infinity), (maxX+.1., Float.infinity))::
		  ((Float.neg_infinity, -1.), (Float.infinity, 0.))::
		  ((Float.neg_infinity, maxY), (Float.infinity, maxY+.1.))::
		  obj) a b;
	if !x = None then begin x := Some 10.; y := Some 10. end;
	(*Printf.printf "%f %f %f %f \t %f %f\n\n*----------------------------------------------------------------------------------------------------*\n\n" (fst p.position) (snd p.position) (Option.get !x) (Option.get !y) p.rayon (dist_euclidienne (Option.get !x, Option.get !y) p.position);*)
	lineto (int_of_float (Option.get !x *. prop)) (int_of_float(Option.get !y *. prop));
	moveto (int_of_float (fst p.position *. prop)) (int_of_float(snd p.position *. prop));
	!rep, (Option.get !x, Option.get !y)

let rec raytracing_vecteur_bvh a b bvh =
	(*let x = ref (fst pos) in
	let y = ref (snd pos) in*)
	match bvh with
	|Nil -> None 
	|Noeud (g, o, d) -> if fst (fst o) <= ( (snd (fst o)) -. b) /. a && ( (snd (fst o)) -. b) /. a <= fst (snd o)
											|| fst (fst o) <= ( (snd (snd o)) -. b) /. a && ( (snd (snd o)) -. b) /. a <= fst (snd o) 
						then begin 
							set_color (rgb 0 255 0);
							let i = raytracing_vecteur_bvh a b g in
							if i <> None
							then i 
							else raytracing_vecteur_bvh a b d
						end
						else None
	|Feuille o -> if fst (fst o) <= ( (snd (fst o)) -. b) /. a && ( (snd (fst o)) -. b) /. a <= fst (snd o)
											|| fst (fst o) <= ( (snd (snd o)) -. b) /. a && ( (snd (snd o)) -. b) /. a <= fst (snd o) 
					then Some o
				else None

let detection_objet orient pos bvh =
	(*orient : direction de l'humain en radian
		pos : position de l'humain
		bvh : tableau contenant les objet qu'il i a sur le terrain*)
	let orient_ray = ref alpha in
	let obj_vu = ref [] in
	(*while !orient_ray < 1.91986 do *)
	(*let a = ref 0. in
	let b = ref 0. in*)
	for _ = 0 to 250 do
		moveto (int_of_float(fst pos *. prop)) (int_of_float(snd pos *. prop));
		let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
		let r2 = Random.float 1. in
		let z = (sqrt(-2. *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) in
		orient_ray := asin (z*.z);
		(match raytracing_bvh (orient +. !orient_ray) pos bvh with
		|Some o,(x,y) ->
						let dist = dist_euclidienne (x,y) pos in 
						if not (List.exists (fun (v,_) -> o=v) !obj_vu) 
						then 
							obj_vu := (o,(dist,(x,y))) :: !obj_vu
						else
							obj_vu := List.map (fun (v,(d,point)) -> if o=v && d>dist then (v,(dist,(x,y))) else (v,(d,point))) !obj_vu
		|None,_ -> ());
		set_color (rgb 255 255 255);
		(match raytracing_bvh (orient -. !orient_ray) pos bvh with
		|Some o,(x,y) ->
						let dist = dist_euclidienne (x,y) pos in 
						if not (List.exists (fun (v,_) -> o=v) !obj_vu) 
						then 
							obj_vu := (o,(dist,(x,y))) :: !obj_vu
						else
							obj_vu := List.map (fun (v,(d,point)) -> if o=v && d>dist then (v,(dist,(x,y))) else (v,(d,point))) !obj_vu
		|None,_ -> ());
		set_color (rgb 255 255 255);
		(*a := tan (orient -. !orient_ray);
		b := (snd pos) -. (!a *. (fst pos));
		set_color (rgb 255 255 255);
		(match raytracing_vecteur !a !b obj with
		|Some x -> set_color (rgb 255 0 255);
					if not (List.mem x !obj_vu) then obj_vu := x :: !obj_vu
		|None -> ());
		lineto (1000*prop_i) (int_of_float ((!a *. 1000. +. !b) *. prop));
		moveto (int_of_float(fst pos)) (int_of_float(snd pos));
		a := tan (orient +. !orient_ray);
		b := (snd pos) +. (!a *. (fst pos));
		set_color (rgb 255 255 255);
		(match raytracing_vecteur !a !b obj with
		|Some x -> set_color (rgb 255 0 255);
					if not (List.mem x !obj_vu) then obj_vu := x :: !obj_vu
		|None -> ());
		lineto (1000*prop_i) ((int_of_float ((!a *. 1000. +. !b) *. prop));*)
		(*Printf.printf "%f %f %f\n" !orient_ray (orient +. !orient_ray) (orient -. !orient_ray);*)
		(* ancien changement de rayon orient_ray := !orient_ray +. alpha *. 100. *. !orient_ray*)
	done;
	!obj_vu

let rec detection_pt_physique lpeople (obj : ((float*float)*(float*float)) list) =
	(*lpeople : liste des humains present
		obj : tableau contenant les objet qu'il i a sur le terrain*)
	match lpeople with
	|p::q ->
		let orient_ray = ref alpha in
		let pt_vu = ref [] in
		moveto (int_of_float(fst p.position *. prop)) (int_of_float(snd p.position *. prop));
		for _ = 0 to 249 do
			let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
			let r2 = Random.float 1. in
			let z = (sqrt((-2.) *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) in
			orient_ray := asin (z);
			(match raytracing_vecteur (p.direction +. !orient_ray) p obj with
			|Some _,(x,y) ->
							let dist = dist_euclidienne (x,y) p.position in 
							if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
							then 
								pt_vu := (dist,(x,y)) :: !pt_vu
							else
								pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
			|None,_ -> ());
			set_color (rgb 255 255 255);
			(match raytracing_vecteur (p.direction -. !orient_ray) p obj with
			|Some _,(x,y) ->
							let dist = dist_euclidienne (x,y) p.position in 
							if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
							then 
								pt_vu := (dist,(x,y)) :: !pt_vu
							else
								pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
			|None,_ -> ());
			set_color (rgb 255 255 255);
		done;
		!pt_vu :: (detection_pt_physique q obj)
	|[] -> []

let rec print_lo lo =
	match lo with
	|[] -> Printf.printf "\n\n"; flush stdout
	|(t,(dis,(x,y)))::q -> Printf.printf "((%f, %f), (%f, %f)) %f (%f, %f);\n" (fst (fst t)) (snd (fst t)) (fst (snd t)) (snd (snd t)) dis x y; print_lo q

let rec print_lop (lo : (float * (float * float)) list) =
	match lo with
	|[] -> Printf.printf "\n\n"; flush stdout
	|(dis,(x,y))::q -> Printf.printf "%f (%f, %f);\n" dis x y; print_lop q

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
	aux a;
	Printf.printf "\n"

let rec print_obj l =
	set_color (rgb 255 0 0);
	match l with
	|[] -> ()
	|o::q ->
		let x = min (fst (fst o)) (fst (snd o)) in 
		let y = min (snd (fst o)) (snd (snd o)) in 
		let w = max (fst (fst o)) (fst (snd o)) -. x in 
		let h = max (snd (fst o)) (snd (snd o)) -. y in 
		fill_rect (int_of_float (x *. prop)) (int_of_float (y *. prop)) (int_of_float (w *. prop)) (int_of_float (h *. prop));
		(*fill_circle (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (fst o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (snd o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (snd o)) *. prop)) (int_of_float ((snd (fst o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (snd o)) *. prop)) (int_of_float ((snd (snd o)) *. prop)) 5;*)
		print_obj q

let test_prog () = 
	Random.self_init ();
	let start_time = Sys.time () in
	open_graph "1000x1000";
	set_window_title "Deplacement test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	set_color (rgb 255 255 255);

(*	let test = people_init 74.5 0.4 (0.50, 0.50) 0.5 0.0 1 in*)
	let ltest = [people_init 0.579 0.33 70. (1.399393, 0.498728) 0.499227 0.88 1;
					people_init 0.579 0.33 70. (4., 4.) 0.5 4. 1] in
	print_donne_people ltest;

	let obj = [((2., 2.), (3., 3.));((3.5,0.75),(4.23,1.48))] in
	let people_block = ref [] in 
	let bvh = construit_bvh 1 obj in
	let compte = ref 0 in 
	let continue = ref true in 

	while Sys.time () < start_time +. 30.0 && !compte < 1000 && !continue do 
		let time = Sys.time() in
		if key_pressed () then continue := false;
		set_color (rgb 0 0 0);	
		fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
		set_color (rgb 255 0 40);
	   print_donne_people ltest;
		people_block := set_pos ltest minimal_frame_time bvh;
		(*set_dir test (test.direction +. (Random.float 0.5) -. 0.25);*)
	   print_people ltest !people_block;
		print_obj (obj @ !people_block);
		set_color (rgb 255 255 255);
		let lo = detection_pt_physique ltest (obj @ !people_block) in
		changement_direction_allure ltest lo;
		(*let _ = wait_next_event [Button_down]in compte := !compte +1 *)
		while Sys.time () < time +. (minimal_frame_time *. 1.) do if key_pressed () then continue := false done;
    done

let test_people_obj () = 
	open_graph "1000x1000";
	set_window_title "test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	set_color (rgb 255 255 255);

	let test = people_init 0.579 0.33 70. (0.5, 0.5) 0. 0. 1 in
	print_donne_people [test];

	let obj = [] in 
	let bvh = construit_bvh 1 obj in
	let people_block = ref (set_pos [test] 0. bvh) in
	set_allure test 0.;
	moveto 0 0;
	while true do 
		let start_time = Sys.time () in 
		set_color (rgb 0 0 0);	
		fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
		set_color (rgb 255 0 40);
		set_dir test (test.direction +. 0.005);
		people_block := set_pos [test] 0.1 bvh;
		set_allure test 0.;
	   print_people [test] (!people_block);
		moveto 0 0;
		draw_string (Printf.sprintf "orientation %f" test.direction);
		while Sys.time () < start_time +. 0.01 do () done
   done

let _ = test_prog ()
(*dune build --watch --terminal-persistence=clear-on-rebuild*)
(*OCMALRUNPARAM=b ./_build/default/deplacement.exe*)
(*pos 1.399393 0.498728
	all = 0.499227
	dir = 6.279973
	sens = 1*)