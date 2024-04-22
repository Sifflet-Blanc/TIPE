open Graphics
open Bvh_tree

let minimal_frame_time = 1.0 /. 5.0(*flux d'information visuel le plus rapide*)
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
	rayon : float; (*en metre represente la personne comme un disque de rayon r dans l'espace 2D*)
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

let people_init m r pos allure dir sens : people = 
	{masse = m; rayon = r; position = pos; allure = allure; direction = dir; sens = sens}

let set_allure (p : people) a = if a < 15. then p.allure <- a

let rec set_dir (p : people) d = 
	if d > 2.0 *. pi then set_dir p (d -. 2.0 *. pi)
	else if d < 0.0 then set_dir p (d +. 2.0 *. pi)
	else p.direction <- d

let set_sens (p : people) s = p.sens <- s

let print (p : people) = match p.position with 
	|(x , y) -> Printf.printf "(%f, %f) - %f - %f - %d\n" x y p.allure p.direction p.sens

let print_people (p : people) =
	let x, y = p.position in
	fill_circle (int_of_float (x *. prop)) (int_of_float (y *. prop)) (int_of_float(p.rayon *. prop))

let print_donne_people (p: people) =
	Printf.printf "position = %f; %f\nallure = %f\ndirection = %f\nsens = %d\n\n" (fst p.position) (snd p.position) p.allure p.direction p.sens; flush stdout

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

let set_pos p t _ = 
	let dis_p = p.allure *. t in (*norme de la distance parcourue*)
	let x, y = p.position in 
	(*if (intersection (x +. (cos p.direction)*.dis_p, y +. (sin p.direction)*.dis_p) bvh) != None
	then*) 
		p.position <- (x +. (cos p.direction)*.dis_p, y +. (sin p.direction)*.dis_p)

let changement_direction_allure (p : people) (l: (float * (float * float)) list) =
	(*calculer les forces de coulomb entre p et tout les pt dans l pour obtenir l'acceleration v(t1) = a*(t1-t0) + v(t0)*)
	let q = 1. in
	let q' = 1. in
	Printf.printf "%f %f\n" p.allure p.direction;
	let rec traitement_contact contact (ax : float) (ay : float) vx vy =
		match contact with
		|[] -> Printf.printf "\n";(ax, ay)
		|(a,b)::l ->(*a - ((a|po) / (po|po)) po avec a le vecteur acceleration et po le vecteur de la personne a l'objet*)
					let pa = (((a -. ax) *. ax) +. ((b -. ay) *. ay)) /. ((a -. ax) *. (a -. ax) +. (b -. ay) *. (b -. ay)) in
					let pv = (((a -. vx) *. vx) +. ((b -. vy) *. vy)) /. ((a -. vx) *. (a -. vx) +. (b -. vy) *. (b -. vy)) in
					let ax' = ax -. pa *. (a -. ax) -. pv *. (a -. vx) in
					let ay' = ay -. pa *. (b -. ay) -. pv *. (b -. vy) in
					Printf.printf "obj : (%f %f) new_acceleration : %f %f\n" a b ax' ay';
					traitement_contact l ax' ay' vx vy in
	let rec aux (l : (float * (float * float)) list) (acceleration : float * float) (x : float) (y : float) (contact : (float * float) list) p : float * float =
		match l with
		|[] -> 
				let (vx,vy) = p.allure *. (cos p.direction), p.allure *. (sin p.direction) in
				let ax, ay = traitement_contact contact (fst acceleration) (snd acceleration) vx vy in
				(ax *. p.masse, ay *. p.masse)
		|(d,(a,b))::l' -> 	if p.rayon >= d 
							then begin 
								aux l' acceleration x y ((a,b)::contact) p
							end
							else begin
								let angle = atan ((a -. x) /. (b -. y)) in
								let f_repultion = ref ((q *. q') /. (4. *. pi *. epsilon0 *. d *. d)) in
								if !f_repultion > 0.04
								then f_repultion := 0.04;
								aux l' ((fst acceleration) -. !f_repultion *. (cos angle), 
									(snd acceleration) -. !f_repultion *. (sin angle)) x y contact p
							end in
	let (ax,ay) = aux l (0., 0.) (fst p.position) (snd p.position) [] p in
	let (vx,vy) = p.allure *. (cos p.direction), p.allure *. (sin p.direction) in
	let (new_vx, new_vy) = (ax *. minimal_frame_time +. vx), (ay *. minimal_frame_time +. vy) in
	let new_allure = sqrt (new_vx *. new_vx +. new_vy *. new_vy) in
	set_allure p (new_allure);
	Printf.printf "la !! dir : %f vy : %f vx : %f vy/vx : %f tan : %f ax : %f ay : %f\n" p.direction new_vy new_vx (new_vy /. new_vx) (tan (new_vy /. new_vx)) ax ay;
	if new_vy = 0. 
	then begin set_dir p (tan (0. /. new_vx)) end
	else begin
		if new_vx = 0.
		then begin set_dir p (((Float.abs new_vy) /. new_vy) *. (pi /. 2.)) end
		else set_dir p (tan (new_vy /. new_vx));
	end;
	Printf.printf "après %f\n" p.direction

let raytracing_bvh orient pos bvh = 
	let x = ref (fst pos) in
	let y = ref (snd pos) in
	let rep = ref None in
	(*Printf.printf "__%f %f %f \n" orient (cos orient) (sin orient);*)
	while !rep == None do
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

let raytracing_vecteur a b obj =
	(*let x = ref (fst pos) in
	let y = ref (snd pos) in*)
	let rep = ref None in
	for i = 0 to Array.length obj -1 do
		if fst (fst obj.(i)) <= ( (snd (fst obj.(i))) -. b) /. a && ( (snd (fst obj.(i))) -. b) /. a <= fst (snd obj.(i))
			|| fst (fst obj.(i)) <= ( (snd (snd obj.(i))) -. b) /. a && ( (snd (snd obj.(i))) -. b) /. a <= fst (snd obj.(i)) 
		then rep := Some obj.(i)
	done;
	!rep

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
		moveto (int_of_float(fst pos)) (int_of_float(snd pos));
		let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
		let r2 = random_diff0 1. in
		let z = sqrt(-2. *. log(r1)) *. cos(2. *. pi *. r2) /. 3. in
		orient_ray := asin (z*.z);
		(match raytracing_bvh (orient +. !orient_ray) pos bvh with
		|Some o,(x,y) ->
						let dist = sqrt ((Float.abs (x -. fst pos)) *. (Float.abs (x -. fst pos)) +. (Float.abs (y -. snd pos)) *. (Float.abs (y -. snd pos))) in 
						if not (List.exists (fun (v,_) -> o=v) !obj_vu) 
						then 
							obj_vu := (o,(dist,(x,y))) :: !obj_vu
						else
							obj_vu := List.map (fun (v,(d,point)) -> if o=v && d>dist then (v,(dist,(x,y))) else (v,(d,point))) !obj_vu
		|None,_ -> ());
		set_color (rgb 255 255 255);
		(match raytracing_bvh (orient -. !orient_ray) pos bvh with
		|Some o,(x,y) ->
						let dist = sqrt ((Float.abs (x -. fst pos)) *. (Float.abs (x -. fst pos)) +. (Float.abs (y -. snd pos)) *. (Float.abs (y -. snd pos))) in 
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

let detection_pt_physique orient pos bvh =
	(*orient : direction de l'humain en radian
		pos : position de l'humain
		bvh : tableau contenant les objet qu'il i a sur le terrain*)
	let orient_ray = ref alpha in
	let pt_vu = ref [] in
	for _ = 0 to 250 do
		moveto (int_of_float(fst pos)) (int_of_float(snd pos));
		let r1 = random_diff0 1. in(*float aleatoire entre 0. et 1. different de .0*)
		let r2 = random_diff0 1. in
		let z = sqrt(-2. *. log(r1)) *. cos(2. *. pi *. r2) /. 3. in
		orient_ray := asin (z*.z);
		(match raytracing_bvh (orient +. !orient_ray) pos bvh with
		|Some _,(x,y) ->
						let dist = sqrt ((Float.abs (x -. fst pos)) *. (Float.abs (x -. fst pos)) +. (Float.abs (y -. snd pos)) *. (Float.abs (y -. snd pos))) in 
						if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
						then 
							pt_vu := (dist,(x,y)) :: !pt_vu
						else
							pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
		|None,_ -> ());
		set_color (rgb 255 255 255);
		(match raytracing_bvh (orient -. !orient_ray) pos bvh with
		|Some _,(x,y) ->
						let dist = sqrt ((Float.abs (x -. fst pos)) *. (Float.abs (x -. fst pos)) +. (Float.abs (y -. snd pos)) *. (Float.abs (y -. snd pos))) in 
						if not (List.exists (fun (_,(a,b)) -> a=x && b=y) !pt_vu) 
						then 
							pt_vu := (dist,(x,y)) :: !pt_vu
						else
							pt_vu := List.map (fun (d,point) -> if (x,y) = point && d>dist then (dist,point) else (d,point)) !pt_vu
		|None,_ -> ());
		set_color (rgb 255 255 255);
	done;
	!pt_vu

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

let print_obj tab =
	set_color (rgb 255 255 255);
	for i = 0 to Array.length tab -1 do
		let o = tab.(i) in
		(*fill_rect ((int_of_float (fst (fst o)))*prop) ((int_of_float (snd (fst o)))*prop) ((int_of_float(fst (snd o)))*prop) ((int_of_float(snd (snd o)))*prop)*)
		fill_circle (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (fst o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (fst o)) *. prop)) (int_of_float ((snd (snd o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (snd o)) *. prop)) (int_of_float ((snd (fst o)) *. prop)) 5;
		fill_circle (int_of_float ((fst (snd o)) *. prop)) (int_of_float ((snd (snd o)) *. prop)) 5;
	done

let _ = 
	Random.self_init ();

	let start_time = Sys.time () in
    open_graph "1000x1000";
	set_window_title "Deplacement test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
	set_color (rgb 255 255 255);

	let test = people_init 74.5 0.4 (0.50, 0.50) 0.5 0.0 1 in
	print test;

	let obj = [|((2., 2.), (3., 3.));((3.5,0.75),(4.23,1.48))|] in
	let bvh = construit_bvh 1 obj in
	let _ = detection_pt_physique test.direction test.position bvh in
	
	while Sys.time () < start_time +. 3.0 do 
		let time = Sys.time() in
		set_color (rgb 0 0 0);	
		fill_rect 0 0 (maxX_i*prop_i) (maxY_i*prop_i);
		set_color (rgb 255 0 40);
	   	print_donne_people test;
		set_pos test minimal_frame_time bvh;
		(*set_dir test (test.direction +. (Random.float 0.5) -. 0.25);*)
		print_bvh_tree bvh;
	   	print_people test;
		set_color (rgb 255 255 255);
		let lo = detection_pt_physique test.direction test.position bvh in
		changement_direction_allure test lo;
		while Sys.time () < time +. (minimal_frame_time *. 5.) do
			()
		done;
    done

(*dune build --watch --terminal-persistence=clear-on-rebuild*)