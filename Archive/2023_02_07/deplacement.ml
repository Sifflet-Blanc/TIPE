open Graphics
open Bvh_tree
(*open Raytracer*)

let minimal_frame_time = 1.0 /. 30.0(*flux d'information visuel le plus rapide*)
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
let prop : float = (float_of_int prop_i) 

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
	while !temp < 0.000001 do
		temp := Random.float f
	done;
	!temp

let set_pos lp t _ = 
	let rec aux lp t (acc : ((float * float) * (float * float)) list) : ((float * float) * (float * float)) list =
		match lp with 
		|[] -> List.rev acc
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
			aux q t (((newx, newy +. (sin (float_mod (p.direction -. a2))) *. p.rayon) , (newx +. (cos (float_mod (p.direction -. a2))) *. p.rayon, newy))::
					   ((newx, newy -. (sin (float_mod (p.direction -. a1))) *. p.rayon) , (newx -. (cos (float_mod (p.direction -. a1))) *. p.rayon, newy))::
					   ((newx -. (cos (float_mod (p.direction +. a2))) *. p.rayon, newy) , (newx, newy -. (sin (float_mod (p.direction +. a2))) *. p.rayon))::
					   ((newx +. (cos (float_mod (p.direction +. a1))) *. p.rayon, newy) , (newx, newy +. (sin (float_mod (p.direction +. a1))) *. p.rayon))::acc) in
	aux lp t []

let raytracing_bvh orient pos bvh = 
    let x = ref (fst pos) in
    let y = ref (snd pos) in
    let rep = ref None in
    while !rep == None do
        x := !x +. (cos orient)*. (1. /. prop);
        y := !y +. (sin orient)*. (1. /. prop);
        if 0. < !x && !x < maxX && 0. < !y && !y < maxY
        then begin
            plot (int_of_float (!x *. prop)) (int_of_float (!y *. prop));
            rep := intersection !x !y bvh
        end
        else 
            rep := Some ((0.,0.),(0.,0.))
    done;
    !rep,(!x,!y)

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
		|[] -> (vx, vy)
		|(a,b)::l ->(*a - ((a|po) / (po|po)) po avec a le vecteur acceleration et po le vecteur de la personne a l'objet*)
					let z = sqrt (a *. a +. b *. b) in
					let vx' = vx -. vx *. a *. a /. (z *. z) in
					let vy' = vy -. vy *. b *. b /. (z *. z) in
					if vx' <> vx' || vy' <> vy' then Printf.printf "obj_pers vect : (%f %f) vitesse : %f %f\n" a b vx vy;
					traitement_contactv l vx' vy' in
	(*calcul du pfd par itération sur tout les points vu*)
	let rec pfd (l : (float * (float * float)) list) (acceleration : float * float) (x : float) (y : float) (contact : (float * float) list) p =
		match l with
		| [] -> let rep_contact = List.map (fun (a,b) -> a -. x, b -. y) contact in
				let ax, ay = traitement_contacta rep_contact (fst acceleration) (snd acceleration) in
				((ax *. p.masse, ay *. p.masse), rep_contact)
		| (d,(a,b))::l' -> 	if p.rayon +. 1. >= d 
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
				Printf.printf "new_vy = 0\n";
				if new_vx > 0.
				then begin set_dir p 0. end
				else set_dir p pi 
			end
			else begin
				if new_vx <= min_float && 0. -. min_float <= new_vx
				then begin 
					Printf.printf "new_vx = 0\n";
					set_dir p (copysign (pi /. 2.) new_vy) end
				else set_dir p (tan (new_vy /. new_vx));
			end;
			if p.direction <> p.direction then Printf.printf "changement_allure dir : %f new_vy : %f new_vx : %f new_all : %f\nax : %f ay : %f vx : %f vy : %f\n"
							 p.direction new_vy new_vx new_allure ax ay vx vy;
			(*Printf.printf "après %f\n" p.direction*) 
			aux qp ql 
		|_,_ -> () in 
	aux lp ll

let detection_objet orient pos bvh =
	(*orient : direction de l'humain en radian
		pos : position de l'humain
		bvh : tableau contenant les objet qu'il i a sur le terrain*)
	let orient_ray = ref 0. in
	let obj_vu = ref [] in
	for _ = 0 to 250 do
		moveto (int_of_float(fst pos *. prop)) (int_of_float(snd pos *. prop));
		let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
		let r2 = Random.float 1. in
		let z = (sqrt(-2. *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) /. 1.348172 in
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
	done;
	!obj_vu

let rec detection_pt_physique lpeople (obj : ((float*float)*(float*float)) list) =
	(*lpeople : liste des humains present
		obj : tableau contenant les objet qu'il i a sur le terrain*)
	match lpeople with
	|p::q ->
		let orient_ray = ref 0. in
		let pt_vu = ref [] in
		moveto (int_of_float(fst p.position *. prop)) (int_of_float(snd p.position *. prop));
		for _ = 0 to 255 do
			let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
			let r2 = Random.float 1. in
			let z = (sqrt((-2.) *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) /. 1.348172 in
			orient_ray := asin (z);
			(match raytracing_vecteur (p.direction +. !orient_ray) (fst p.position) (snd p.position) p.rayon obj with
			|Some _,(x,y) ->
							lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float ((fst p.position) *. prop)) (int_of_float ((snd p.position) *. prop));
							let dist = dist_euclidienne (x,y) p.position in 
							if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
							then 
								pt_vu := (dist,(x,y)) :: !pt_vu
							else
								pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
			|None,_ -> ());
			set_color (rgb 255 255 255);
			(match raytracing_vecteur (p.direction -. !orient_ray) (fst p.position) (snd p.position) p.rayon obj with
			|Some _,(x,y) ->
							lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float ((fst p.position) *. prop)) (int_of_float ((snd p.position) *. prop));
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

let rec detection_pt_physique_bvh lpeople (bvh : bvh) =
	(*lpeople : liste des humains present
		obj : tableau contenant les objet qu'il i a sur le terrain*)
	match lpeople with
	|p::q ->
		let orient_ray = ref 0. in
		let pt_vu = ref [] in
		moveto (int_of_float(fst p.position *. prop)) (int_of_float(snd p.position *. prop));
		for _ = 0 to 255 do
			let r1 = random_diff0 1. in(*float aleatoire entre 0.000001 et 1.*)
			let r2 = Random.float 1. in
			let z = (sqrt((-2.) *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) /. 1.348172 in
			orient_ray := asin (z);
			(match raytracing_vecteur_bvh (p.direction +. !orient_ray) (fst p.position) (snd p.position) p.rayon bvh with
			|Some _,(x,y) ->
							lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float ((fst p.position) *. prop)) (int_of_float ((snd p.position) *. prop));
							let dist = dist_euclidienne (x,y) p.position in 
							if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
							then 
								pt_vu := (dist,(x,y)) :: !pt_vu
							else
								pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
			|None,_ -> ());
			set_color (rgb 255 255 255);
			(match raytracing_vecteur_bvh (p.direction -. !orient_ray) (fst p.position) (snd p.position) p.rayon bvh with
			|Some _,(x,y) ->
							lineto (int_of_float (x *. prop)) (int_of_float (y *. prop));
							moveto (int_of_float ((fst p.position) *. prop)) (int_of_float ((snd p.position) *. prop));
													let dist = dist_euclidienne (x,y) p.position in 
							if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
							then 
								pt_vu := (dist,(x,y)) :: !pt_vu
							else
								pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
			|None,_ -> ());
			set_color (rgb 255 255 255);
		done;
		!pt_vu :: (detection_pt_physique_bvh q bvh)
	|[] -> []

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
		set_color (rgb 255 0 0);
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

let test_prog () = 
	let start_time = Sys.time () in
   open_graph (Printf.sprintf " %dx%d" (prop_i*maxX_i) (prop_i*maxY_i));
	set_window_title "Deplacement test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	set_color (rgb 255 255 255);

(*	let test = people_init 74.5 0.4 (1.399393, 0.498728) 0.5 0.0 1 in*)
	let ltest = [people_init 0.579 0.33 70. (8. , 2.) 0.499227 0.88 1(*;
					people_init 0.579 0.33 70. (4., 4.) 0.5 4. 1*)] in
	print_donne_people ltest;

	let obj = [(*((-1., Float.neg_infinity), (0., Float.infinity));
		  ((maxX, Float.neg_infinity), (maxX+.1., Float.infinity));
		  ((Float.neg_infinity, -1.), (Float.infinity, 0.));
		  ((Float.neg_infinity, maxY), (Float.infinity, maxY+.1.));*)
		  ((0.,0.),(maxX, maxY));
		  ((2., 2.), (3., 3.));((3.5,0.75),(4.23,1.48))] in
	let people_block = ref [] in 
	let bvh = construit_bvh 1 obj in
	print_bvh bvh;
	let continue = ref true in 
	while Sys.time () < start_time +. 50.0 do 
		let time = Sys.time() in
		if key_pressed () then continue := false;
		set_color (rgb 0 0 0);	
		fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
		set_color (rgb 255 0 40);
	   	print_donne_people ltest;
		people_block := set_pos ltest minimal_frame_time bvh;
	   	print_people ltest !people_block;
		print_obj obj;
		set_color (rgb 255 255 255);
		let lo = detection_pt_physique ltest (obj @ !people_block) in
		(*let lo = detection_pt_physique_bvh ltest (construit_bvh 1 (obj @ !people_block)) in*)
		changement_direction_allure ltest lo;
		while Sys.time () < time +. (minimal_frame_time *. 1.) do if key_pressed () then continue := false done;
   done;
   close_graph ()

let test_people_obj () = 
	open_graph " 1000x1000";
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

let test_z () = 
	let rep = ref 0. in 
	let max = ref 0. in 
	let min = ref 0. in 
	let n = 100000000 in 
	for _ = 0 to n do 
		let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
		let r2 = Random.float 1. in
		let z = (sqrt((-2.) *. log(r1)) /. 3.899) *. cos(2. *. pi *. r2) /. 1.348172 in
		if z > !max then max := z;
		if z < !min then min := z;
		rep := !rep +. z
	done;
	Printf.printf "z_min = %f; z_moy = %f; z_max = %f\n" !min (!rep /. (float_of_int n)) !max;
	Printf.printf "asinz_min = %f; asinz_moy = %f; asinz_max = %f\n" (asin !min) (asin (!rep /. 10000.)) (asin !max)

let _ =
	Random.self_init ();
	test_prog ()
(*dune build --watch --terminal-persistence=clear-on-rebuild*)
(*OCAMLRUNPARAM=b ./_build/default/deplacement.exe*)
(*pos 1.399393 0.498728
	all = 0.499227
	dir = 6.279973
	sens = 1*)