let new_orient (l : ('a * 'b * float) list) (dmax : float) (alpha0 : float)=
	let rec aux l min acc = 
		match l with
		| [] -> acc
		| (dist, _, alpha)::q ->
			let tmp = dmax *. dmax +. dist *. dist 
				-. 2. *. dmax *. dist *. cos (alpha0 -. alpha)
				+. dmax *. Float.abs (tan ((alpha0 -. alpha) /. 2.)) in
			if tmp < min
			then aux q tmp alpha
			else aux q min acc in
	aux l Float.infinity 0.