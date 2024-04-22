let pi : float = Float.pi

let rec float_mod orient = 
	if orient > 2. *. pi then float_mod (orient -. 2. *. pi)
	else if orient < 0. then float_mod (orient +. 2. *. pi)
	else orient

let dist_euclidienne (x,y) (x',y') =
   sqrt ((x-.x')*.(x-.x') +. (y-.y')*.(y-.y'))

let regarde_bonne_dir dx (dy : float) (o : float) : bool =
	((dx >= 0. && (o <= pi/. 2. || o >= 3. *. (pi /. 2.)))	|| (dx <= 0. && o >= pi/. 2. && o <= 3. *. (pi /. 2.))) && 
	((dy >= 0. && o <= pi) 									|| (dy <= 0. && o >= pi))

let intersection_droite_rect a b o x y rep xp yp rp obji =
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
	if (min d f) <= a *. c +. b && a *. c +. b <= (max d f) && regarde_bonne_dir (c-.xp) (a *. c +. b -. yp) o && dist_euclidienne (c,a*. c +. b) (xp, yp) > rp
	then begin 
		if (!x <> None && dist_euclidienne (c,a*. c +. b) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || !x = None
		then begin
			x := Some c;
			y := Some (a *. c +. b);
			rep := Some obji 
		end
	end;
	if (min d f) <= a *. e +. b && a *. e +. b <= (max d f) && regarde_bonne_dir (e -. xp) (a *. e +. b -. yp) o  && dist_euclidienne (e,a*. e +. b) (xp, yp) > rp
	then begin
		if (!x <> None && dist_euclidienne (e,a*. e +. b) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || !x = None
		then begin
			x := Some e;
			y := Some (a *. e +. b);
			rep := Some obji 
		end
	end;
	(*verification de l'intersection avec les segments horisontaux*)
	(*Printf.printf "(min %f %f) <= (%f -. %f) /. %f && (%f -. %f) /. %f <= (max %f %f)\n" c e d b a d b a c e;
	Printf.printf "%f <= %f && %f <= %f\n" (min c e) ((d -. b) /. a) ((d -. b) /. a) (max c e);
	Printf.printf "%f > %f\n" (dist_euclidienne ((d -. b) /. a,d) (xp, yp)) rp;*)
	if (min c e) <= (d -. b) /. a && (d -. b) /. a <= (max c e) && regarde_bonne_dir ((d -. b) /. a -. xp) (d -. yp) o  && dist_euclidienne ((d -. b) /. a,d) (xp, yp) > rp
	then begin
		if (!x <> None && dist_euclidienne ((d -. b) /. a,d) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || (!x = None && !y = None)
		then begin
			x := Some ((d -. b) /. a);
			y := Some d;
			rep := Some obji 
		end
	end;
	(*Printf.printf "(min %f %f) <= (%f -. %f) /. %f && (%f -. %f) /. %f <= (max %f %f)\n" c e f b a f b a c e;
	Printf.printf "%f <= %f && %f <= %f\n" (min c e) ((f -. b) /. a) ((f -. b) /. a) (max c e);
	Printf.printf "%f > %f\n" (dist_euclidienne ((f -. b) /. a,f) (xp, yp)) rp;*)
	if (min c e) <= (f -. b) /. a && (f -. b) /. a <= (max c e) && regarde_bonne_dir ((f -. b) /. a -. xp) (f -. yp) o && dist_euclidienne ((f -. b) /. a,f) (xp, yp) > rp
	then begin
		if (!x <> None && dist_euclidienne ((f -. b) /. a,f) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || (!x = None && !y = None)
		then begin
			x := Some ((f -. b) /. a);
			y := Some f;
			rep := Some obji 
		end
	end

let raytracing_vecteur (orient : float) (xp : float) (yp : float) (rp : float) (dmax : float) (obj : ((float * float) * (float * float)) list) =
	(*orient = angle du rayon sur le cercle trigo
		(xp, yp) = position a partir de la quelle le rayon est tire
		rp = rayon a partir du quel les colition sont possible
		obj = liste des objets (rectangle) sur le terrain *)
	let o = float_mod orient in
	let a = tan o in
	let b = yp -. a *. xp in
	let rep = ref (Some ((0., 0.), (0., 0.))) in
	let x = ref None in
	let y = ref None in
	(*Printf.printf "orient = %f;\ta = %f;\tb = %f" o a b;*)
	let rec aux obj a b =
		match obj with 
		|[] -> ()
		|obji::q ->
			intersection_droite_rect a b o x y rep xp yp rp obji;
			aux q a b
		in
	aux obj a b;
	if !x = None then begin
		x := Some 10.; 
		y := Some 10.;
		Printf.printf "xp %f; yp %f; x %f; y %f \t o %f; dist %f\n*----------------------------------------------------------------------------------------------------*\n" xp yp (Option.get !x) (Option.get !y) o (dist_euclidienne (Option.get !x, Option.get !y) (xp, yp));
		flush stdout;
		while true do () done
	end;
	!rep, (Option.get !x, Option.get !y)
