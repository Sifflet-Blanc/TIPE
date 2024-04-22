(**open Graphics*)

let new_orient (l : ('a * 'b * float) list) (dmax : float) (alpha0 : float) (*prop xp yp op *)=
																		  (*_____________*)
																			 (*en plus*)
	(*set_color (rgb 100 0 0);*)
	let rec aux l min acc = 
		match l with
		| [] -> acc
		| (dist, _, alpha)::q ->
			let tmp = dmax *. dmax +. dist *. dist 
				-. 2. *. dmax *. dist *. cos (alpha0 -. alpha)
				+. dmax *. Float.abs (tan ((alpha0 -. alpha) /. 2.)) in
			(*moveto (int_of_float (xp *. prop)) (int_of_float (yp *. prop));
			rlineto (int_of_float (prop *. tmp *. cos (op +. alpha) /. (dmax *. dmax))) (int_of_float (prop *. tmp *. sin (op +. alpha) /. (dmax *. dmax)));*)
			if tmp < min
			then aux q tmp alpha
			else aux q min acc in
	aux l Float.infinity 0.
