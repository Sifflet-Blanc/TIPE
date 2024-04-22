let somme_des_m_premier_nb l m =
	let rec aux l m rep =
		match l with
		|[] -> if m = 0 then rep else 0
		|(t,_,_)::q -> if m = 0 then rep else aux q (m-1) (rep +. t) in 
	aux l m 0.

let calcul_orient l m = 
	let rec aux l acc m= 
		match l with 
		|[] -> acc
		|(_,_,t)::q -> if m = 0 then t else aux q t (m-1)
	in 
	aux l 0. (m/2)

let trouve_plus_grosse_somme_de_taille_m l m =
	let aux l m max orient =
		if List.length l >= m then begin
			let tmp_max = somme_des_m_premier_nb l m in
			if tmp_max >= max then
				aux (List.tl l) m tmp_max (calcul_orient l m)
			else aux (List.tl l) m max oriant
		else orient in 
	aux l m 0. 0.