open Graphics

type objet = (float * float) * (float * float)
type bvh = 
	Nil 
	|Noeud of bvh * objet * bvh
	|Feuille of objet 

let fx a = (min (fst (fst a)) (fst (snd a)) +. max (fst (fst a)) (fst (snd a))) /. 2.0
let fy a = (min (snd (fst a)) (snd (snd a)) +. max (snd (fst a)) (snd (snd a))) /. 2.0

let union a b =
	let xmin = min (min (fst (fst a)) (fst (snd a))) (min (fst (fst b)) (fst (snd b))) in
	let xmax = max (max (fst (fst a)) (fst (snd a))) (max (fst (fst b)) (fst (snd b))) in
	let ymin = min (min (snd (fst a)) (snd (snd a))) (min (snd (fst b)) (snd (snd b))) in
	let ymax = max (max (snd (fst a)) (snd (snd a))) (max (snd (fst b)) (snd (snd b))) in
	((xmin, ymin), (xmax, ymax))

let cmpxy f a b =
    let af, bf = f a, f b in
    if af < bf
    then -1
    else if af > bf
        then 1
        else 0

let global_aabb (a : objet array) : objet =
    let b = ref a.(0) in
    for i = 1 to Array.length a - 1 do
        b := union !b a.(i)
    done;
    !b

let construit_bvh (comp : int) (a : objet list) : bvh=
    let rec aux (comp : int) (a : objet array) : bvh=
        if Array.length a = 0
        then Nil
        else if Array.length a = 1
        then Feuille a.(0)
        else let f = match comp with
                0 -> fx | _ -> fy  in
            let ncomp = (comp + 1) mod 2 in
            Array.sort (cmpxy f) a;
            let n1 = Array.length a / 2 in
            let n2 = Array.length a - n1 in
            let a1 = Array.sub a 0 n1 in
            let a2 = Array.sub a n1 n2 in
            let t1 = aux ncomp a1 in
            let t2 = aux ncomp a2 in
            Noeud (t1, global_aabb a, t2)
        in
    aux comp (Array.of_list a)

let print_bvh (t : bvh) =
	let rec aux t (c : string) =
		match t with
		|Feuille x -> Printf.printf "%s((%f, %f), (%f, %f))" c (fst (fst x)) (snd (fst x)) (fst (snd x)) (snd (snd x))
		|Noeud (g,x,d) -> aux g (c^"    ");
							Printf.printf "\n%s((%f, %f), (%f, %f))\n" c (fst (fst x)) (snd (fst x)) (fst (snd x)) (snd (snd x));
							aux d (c^"    ");
		|_ -> () in
	aux t "";
	Printf.printf "\n"

let rec intersection x y (bvh : bvh) : 'a option =
    match bvh with
    |Nil -> None 
    |Noeud (g, o, d) -> if fst (fst o) <= x && x <= fst (snd o) && snd (fst o) <= y && y <= snd (snd o)
                        then begin 
                            set_color (rgb 0 255 0);
                            let i = intersection x y g in
                            if i <> None
                            then i 
                            else intersection x y d
                        end
                        else None
    |Feuille o -> if fst (fst o) <= x && x <= fst (snd o) && snd (fst o) <= y && y <= snd (snd o)
                    then Some o
                else None


let pi : float = Float.pi

let rec float_mod orient = 
    if orient > pi then float_mod (orient -. 2. *. pi)
    else if orient < 0. -. pi then float_mod (orient +. 2. *. pi)
    else orient

let dist_euclidienne (x,y) (x',y') =
   sqrt ((x-.x')*.(x-.x') +. (y-.y')*.(y-.y'))

let regarde_bonne_dir dx (dy : float) (o : float) : bool =
    ((dx >= 0. && cos o >= 0.) || (dx <= 0. && cos o <= 0.)) && 
    ((dy >= 0. && sin o >= 0.) || (dy <= 0. && sin o <= 0.))

let intersection_droite_rect a b o x y rep xp yp rp obji (dmax : float) (change : bool) : bool =
    let c, d, e, f = (fst (fst obji)), (snd (fst obji)), (fst (snd obji)), (snd (snd obji)) in
    let tmp_xy = (!x, !y) in 
    (*Printf.printf "%f\n%f\n%f\t%f\n" f d c e;*)
    (*
    f   .-------*
        |       |
        |  obj  |
        |       |
    d   *-------.
    
        c       e
    *)
    (*verification de l'intersection avec les segments verticaux*)
    (*verif de l'intersection
        verif que l'intersection se face du bon cote (droite si l'humain regarde a droite et gauche si non)
        verif que l'intersection se face sur la bonne hauteur (haut si l'humain regarde en haut et bas si non) *)
    if (min d f) <= a *. c +. b && a *. c +. b <= (max d f) && regarde_bonne_dir (c-.xp) (a *. c +. b -. yp) o && dist_euclidienne (c,a*. c +. b) (xp, yp) > rp && dist_euclidienne (c,a*. c +. b) (xp, yp) < dmax
    then begin 
        if (!x <> None && dist_euclidienne (c,a*. c +. b) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || !x = None
        then begin
            x := Some c;
            y := Some (a *. c +. b);
            rep := Some obji 
        end
    end;
    if (min d f) <= a *. e +. b && a *. e +. b <= (max d f) && regarde_bonne_dir (e -. xp) (a *. e +. b -. yp) o  && dist_euclidienne (e,a*. e +. b) (xp, yp) > rp && dist_euclidienne (e,a*. e +. b) (xp, yp) < dmax
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
    if (min c e) <= (d -. b) /. a && (d -. b) /. a <= (max c e) && regarde_bonne_dir ((d -. b) /. a -. xp) (d -. yp) o  && dist_euclidienne ((d -. b) /. a,d) (xp, yp) > rp && dist_euclidienne ((d -. b) /. a,d) (xp, yp) < dmax
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
    if (min c e) <= (f -. b) /. a && (f -. b) /. a <= (max c e) && regarde_bonne_dir ((f -. b) /. a -. xp) (f -. yp) o && dist_euclidienne ((f -. b) /. a,f) (xp, yp) > rp && dist_euclidienne ((f -. b) /. a,f) (xp, yp) < dmax
    then begin
        if (!x <> None && dist_euclidienne ((f -. b) /. a,f) (xp, yp) <= dist_euclidienne (Option.get !x, Option.get !y) (xp, yp)) || (!x = None && !y = None)
        then begin
            x := Some ((f -. b) /. a);
            y := Some f;
            rep := Some obji 
        end
    end;
    let tmp = tmp_xy <> (!x, !y) in 
    if tmp && not change
    then begin 
        x := fst tmp_xy;
        y := snd tmp_xy
    end;
    tmp

let raytracing_vecteur (orient : float) (xp : float) (yp : float) (rp : float) (dmax : float) (obj : ((float * float) * (float * float)) list) =
    (*orient = angle du rayon sur le cercle trigo
        (xp, yp) = position a partir de la quelle le rayon est tire
        rp = rayon a partir du quel les colition sont possible
        obj = liste des objets (rectangle) sur le terrain *)
    let o = float_mod orient in
    let a = tan o in
    let b = yp -. a *. xp in
    let rep = ref (Some ((0., 0.), (0., 0.))) in
    let x = ref (Some (xp +. dmax *. cos o)) in
    let y = ref (Some (yp +. dmax *. sin o)) in
    (*Printf.printf "orient = %f;\ta = %f;\tb = %f" o a b;*)
    let rec aux obj a b =
        match obj with 
        |[] -> ()
        |obji::q ->
            ignore (intersection_droite_rect a b o x y rep xp yp rp obji dmax true);
            aux q a b
        in
    aux obj a b;
    if !x = None || Option.get !x = Float.infinity || Option.get !x = Float.neg_infinity then begin
        x := Some 10.; 
        y := Some 10.;
        Printf.printf "xp %f; yp %f; x %f; y %f \t o %f; dist %f\n*----------------------------------------------------------------------------------------------------*\n" xp yp (Option.get !x) (Option.get !y) o (dist_euclidienne (Option.get !x, Option.get !y) (xp, yp));
        flush stdout;
        failwith "raytracing error"
    end;
    !rep, (Option.get !x, Option.get !y)

let raytracing_vecteur_bvh (orient : float) (xp : float) (yp : float) (rp : float) (dmax : float) (bvh : bvh) =
    let o = float_mod orient in
    let a = tan o in
    let b = yp -. a *. xp in
    let rep = ref (Some ((0., 0.), (0., 0.))) in
    let x = ref None in
    let y = ref None in
    let rec aux bvh : unit=
        match bvh with
        |Nil -> () 
        |Noeud (g, c, d) -> if intersection_droite_rect a b o x y rep xp yp rp c dmax false
                            then begin 
                                aux g;
                                aux d;
                            end
        |Feuille c -> ignore (intersection_droite_rect a b o x y rep xp yp rp c dmax true)
        in
    aux bvh;
    if !x = None || Option.get !x = Float.infinity || Option.get !x = Float.neg_infinity then begin
        Printf.printf "xp %f; yp %f; x %f; y %f \t o %f; dist %f\n*----------------------------------------------------------------------------------------------------*\n" xp yp (Option.get !x) (Option.get !y) o (dist_euclidienne (Option.get !x, Option.get !y) (xp, yp));
        x := Some 10.; 
        y := Some 10.;
        flush stdout;
        while true do () done
    end;
    !rep, (Option.get !x, Option.get !y)