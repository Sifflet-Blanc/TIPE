let somme_des_m_premier_nb l m =
	let rec aux l m (rep : float) =
		match l with
		|[] -> if m = 0 then rep else 0.
		|(t,_,_)::q -> if m = 0 then rep else aux q (m-1) (rep +. t) in 
	aux l m 0.

let calcul_orient l m = 
	let rec aux l acc m = 
		match l with 
		|[] -> acc
		|(_,_,t)::q -> if m = 0 then t else aux q t (m-1)
	in 
	aux l 0. (m/2)

let rec list_affiche l =
	match l with
	| [] -> Printf.printf "\n\n"
	| (t1,_,t2)::q -> Printf.printf "(%f,%f); " t1 t2; list_affiche q

let trouve_plus_grosse_somme_de_taille_m (l : ('a * 'b * float) list) m pas =
	let rec listtl l pas =
		match l with 
		| [] -> []
		| t::q -> if pas = 0 then
					t::q 
				else 
					listtl q (pas -1) in
	let rec aux l m max l_max =
		if List.length l >= pas && m >= 1 
		then begin
			let tmp_max = somme_des_m_premier_nb l pas in
			(*Printf.printf "(%f, %f) " tmp_max (calcul_orient l pas);*)
			if tmp_max >= max then
				aux (listtl l pas) m tmp_max l
			else aux (listtl l pas) m max l_max
		end
		else begin 
			let tmp = calcul_orient l_max pas in 
			Printf.printf "%f\n" tmp;
			tmp 
		end in  
	let new_l = List.sort (fun (_,_,a) (_,_,b) -> int_of_float ((a -. b) *. 1000.)) l in
	(*list_affiche new_l;*)
	aux new_l m 0. new_l