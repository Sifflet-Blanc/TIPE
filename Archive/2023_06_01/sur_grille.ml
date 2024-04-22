(*bonjour programme du plus court chemin sur un graphe en *)
open Graphics
open Couleur

open Labyrinte

type case = Labyrinte.case

type plateau = case array array

exception Solution

type etat = Traite | Traitement | Inconnu

let creer_pile () = ref []

let empile p x = p := x :: !p

let depile p = match !p with
    | [] -> failwith "pile vide"
    | t::q -> p := q; t

let creer_file () = (creer_pile (), creer_pile ())

let enfile (p1,_) x = empile p1 x

let defile ((p1,p2) : ('a list ref * 'a list ref))  : 'a =
	if !p2 = []
	then 
		while !p1 <> [] do 
			empile p2 (depile p1)
		done;
	depile p2

let est_vide (p1,p2) = !p1 = [] && !p2 = []


let rec asort f a =
	if Array.length a <= 1 then a
	else begin
		let a1 = asort f (Array.sub a 0 (Array.length a /2)) in
		let a2 = asort f (Array.sub a (Array.length a /2) (Array.length a - (Array.length a /2))) in
		let i1 = ref 0 in
		let i2 = ref 0 in
		for i = 0 to Array.length a -1 do
      if !i2 = Array.length a2 || (!i1 <= Array.length a1 -1 && f a1.(!i1) a2.(!i2) < 0.0) 
			then begin 
        a.(i) <- a1.(!i1);
				i1 := !i1 +1;
				end
			else begin
				a.(i) <- a2.(!i2);
				i2 := !i2 +1
			end
		done; a
		end

let sort f l = 
	Array.to_list (asort f (Array.of_list l))


let print_map (p : plateau) : unit =
	for j = 0 to Array.length p -1 do
		for i = 0 to Array.length p.(0) -1 do
			match p.(j).(i) with
			| Empty -> Printf.printf "|  "
			| Obstacle -> Printf.printf "|##"
			| Exit -> Printf.printf "|__"
			| People n -> Printf.printf "|%2d" n
		done;
		Printf.printf "|\n";
	done;
	Printf.printf "\n";
	flush stdout

let print_pere (p : (int * int) array array) : unit =
	for j = 0 to Array.length p -1 do
		for i = 0 to Array.length p.(0) -1 do
			match p.(j).(i) with
			| (x, y) when x = -1 && y = -1 -> Printf.printf "| "
			| (x, y) when x = i-1 && y = j -> Printf.printf "|<"
			| (x, y) when x = i+1 && y = j -> Printf.printf "|>"
			| (x, y) when x = i && y = j-1 -> Printf.printf "|^"
			| _  -> Printf.printf "|v"
		done;
		Printf.printf "|\n";
	done;
	Printf.printf "\n";
	flush stdout

let print_graph (p : plateau) c prop : unit =
	let n = Array.length p.(0) -1 in 
	for j = 0 to Array.length p -1 do
		for i = 0 to n do
			set_color (rgb 150 150 150);
			draw_rect  (i*prop) ((n-j-1)*prop) prop prop;
			(match p.(j).(i) with
			| Obstacle -> set_color (rgb 0 0 0)
			| People n -> set_color c.(n)
			| _ -> set_color (rgb 250 250 250));
			fill_rect (i*prop+1) ((n-j-1)*prop+1) (prop-2) (prop-2)
		done;
	done

let print_graph_chemin (p : plateau) c prop acoord achemin : unit =
	let n = Array.length p.(0) -1 in 
	for j = 0 to Array.length p -1 do
		for i = 0 to n do
			set_color (rgb 150 150 150);
			draw_rect  (i*prop) ((n-j-1)*prop) prop prop;
			(match p.(j).(i) with
			| Obstacle -> set_color (rgb 0 0 0)
			| People n -> set_color c.(n)
			| _ -> set_color (rgb 250 250 250));
			fill_rect (i*prop+1) ((n-j-1)*prop+1) (prop-2) (prop-2)
		done;
	done;
	set_color (rgb 0 0 0);
	let t1 = 10 in
	(*let t2 = 4 in*)
	for i = 0 to Array.length achemin -1 do
		let xt = ref (fst acoord.(i)) in 
		let yt = ref (snd acoord.(i)) in
		List.iter (fun (x, y) -> 
				set_color (rgb 0 0 0);
				fill_rect ((min !xt x) * prop + prop/2 - t1/2) ((n-1-(max !yt y)) * prop + prop/2 - t1/2)
						(t1 + (abs (!xt - x)) * prop) (t1 + (abs (!yt - y)) * prop);
				(*set_color c.(i);
				fill_rect ((min !xt x) * prop + prop/2 - t2/2) ((n-1-(max !yt y)) * prop + prop/2 - t2/2)
						(t2 + (abs (!xt - x)) * prop) (t2 + (abs (!yt - y)) * prop);*)
				xt := x;
				yt := y) achemin.(i)
	done


let valide (p : plateau) (x : int) (y : int) : bool =
	p.(y).(x) == Empty || p.(y).(x) == Exit

let mouvement (x : int) (y : int)  = 
	[(x+1,y); (x-1,y); (x,y+1); (x,y-1)]

let mouvement_valide (p : plateau) (pp : etat array array) (x : int) (y : int) : (int * int) list = 
	List.filter 
		(fun (x,y) -> 
			x >= 0 && y>=0 && y< Array.length p && x < Array.length p.(0) && (valide p x y) && pp.(y).(x) == Inconnu) 
		(mouvement x y)

let mouvement_prioritaire_valide (p : plateau) (pp : etat array array) (x : int) (y : int) 
								(ld: (int * int) list) : (int * int) list =
	sort (fun (a,b) (c,d) ->let minab = ref (Float.infinity) in 
							let mincd = ref (Float.infinity) in
							List.iter (fun (xd,yd) -> 
									let tmp1 = sqrt (float_of_int ((xd-a)*(xd-a) + (yd-b)*(yd-b))) in
									let tmp2 = sqrt (float_of_int ((xd-c)*(xd-c) + (yd-d)*(yd-d))) in 
									if tmp1 < !minab then begin minab := tmp1 end;
									if tmp2 < !mincd then mincd := tmp2) ld;
							!minab -. !mincd) (mouvement_valide p pp x y)


let trouve_chemin pere xe ye x y = 
	let chemin = ref [] in
	(try
		while (!xe,!ye) <> (x, y) do
			chemin := (!xe, !ye) :: !chemin;
				let x_tmp, y_tmp = pere.(!ye).(!xe) in
				xe := x_tmp;
				ye := y_tmp 
		done;
	with 
		|Invalid_argument _ -> chemin := List.init (Array.length pere * Array.length pere.(0)) (fun _ -> (0, 0)));
	!chemin

let resolution_flood_fill (p : plateau) (x : int) (y : int) (ld : (int * int) list) : (int* int) list=
	let pere = Array.make_matrix (Array.length p) (Array.length p.(0)) (-1,-1) in
	try 
		let map_deplacement = Array.make_matrix (Array.length p) (Array.length p.(0)) Inconnu in
		let f = creer_file () in
		let n = List.length ld in
		let m = ref 0 in
		enfile f (x,y);
		map_deplacement.(y).(x) <- Traitement;
		while not (est_vide f) do
			let i,j = defile f in
			if map_deplacement.(j).(i) == Traitement
			then begin
				if p.(j).(i) = Exit then begin 
					m := !m +1;
					if !m = n 
					then 
						raise Solution
				end;

				map_deplacement.(j).(i) <- Traite;
				List.iter (fun (ip, jp) -> 
						enfile f (ip, jp);
						if map_deplacement.(jp).(ip) == Inconnu
						then begin  
							pere.(jp).(ip) <- (i, j);
							map_deplacement.(jp).(ip) <- Traitement end)
							(mouvement_prioritaire_valide p map_deplacement i j ld)
			end
		done;
		if !m > 0 then raise Solution
		else begin 
			[]
		end
	with Solution -> 
		print_pere pere;
		let rep = ref (List.init (Array.length p * Array.length p) (fun _ -> (0, 0))) in 
		List.iter (fun (xd,yd) ->
			let xe, ye = ref xd, ref yd in
			let tmp = trouve_chemin pere xe ye x y in 
			if List.length tmp < List.length !rep then rep := tmp) ld;
		!rep

let rec applique_chemin (chemin : (int * int) list) (p : plateau) (n : int): plateau =
	match chemin with
	|[] -> p
	|(x, y)::q -> p.(y).(x) <- People n;
				applique_chemin q p (n-1)

let init_matrix (leny : int) (lenx : int) (f : (int * int) -> 'a) : 'a array array = 
	let rep = Array.make_matrix leny lenx (f (0, 0)) in
	for i = 0 to Array.length rep -1 do
		for j = 0 to Array.length rep.(0) -1 do
			rep.(i).(j) <- f (j,i)
		done
	done;
	rep

let trouve_sortie p = 
	let xd = ref 0 in
	let yd = ref 0 in
	for i = 0 to Array.length p -1 do
		for j = 0 to Array.length p.(0) -1 do
			if p.(i).(j) = Empty
			then begin xd := i; yd := j end
		done
	done;
	!xd, !yd

let randomarray n max =
	if max < n then failwith "max < n";
	let rep = Array.make n (-1) in
	for i = 0 to n-1 do
		let tmp = ref (Random.int max) in
		while Array.exists (fun i -> i = !tmp) rep do
			tmp := Random.int max
		done;
		rep.(i) <- !tmp
	done;
	rep

let _ = 
	Random.init 90;
	let my = 20 in 
	let mx = my +1 in
	let p = init_matrix my mx (fun (x,y) -> match x, y with
													|_, _ when x = mx-1 && y < my/2 -1 -> Obstacle
													|_, _ when x = mx-1 && y > my/2 +1 -> Obstacle
													|_, _ -> Empty) in
	let ld = [(mx-1, my/2); (mx-1, my/2 -1); (mx-1, my/2 +1)] in
	List.iter (fun (xd, yd) -> p.(yd).(xd) <- Exit) ld;
	let n = 90 in
	let tmp = randomarray n (my*my) in
	let acoord = Array.init n (fun i -> tmp.(i)/my, tmp.(i) mod my) in
	let apeople = Array.init n (fun i -> i) in
	let acouleur = Array.init n (fun i -> 
									let c = float_of_int (tmp.(i)) /. float_of_int (my*my) in 
									rgb (r c) (g c) (b c)) in
	let achemin = Array.init (Array.length acoord) 
							(fun i -> resolution_flood_fill p (fst acoord.(i)) (snd acoord.(i)) ld) in 
	let plateau = Array.init (Array.length p) (fun i -> Array.copy p.(i)) in
	for i = 0 to n-1 do 
		p.(snd acoord.(i)).(fst acoord.(i)) <- People apeople.(i)
	done;

	let prop_i = 50 in
	open_graph (Printf.sprintf " %dx%d" (prop_i*mx) (prop_i*my));
	set_window_title "test";
	print_graph_chemin p acouleur prop_i acoord achemin;
	let temp = ref 1000 in 
	while Array.fold_left (+) 0 apeople <> -n && !temp > 0 do
		temp := !temp -1;
		let start_time = Sys.time () in
		for i = 0 to n-1 do
			if apeople.(i) <> -1
			then begin  
				(match achemin.(i) with
				| _ when List.mem acoord.(i) ld -> 
					p.(snd acoord.(i)).(fst acoord.(i)) <- Exit;
					apeople.(i) <- -1
				| [] -> achemin.(i) <- resolution_flood_fill plateau (fst acoord.(i)) (snd acoord.(i)) ld;
					if achemin.(i) <> [] 
					then begin 
						let nx, ny = List.hd achemin.(i) in
						if valide p nx ny 
						then begin 
							p.(snd acoord.(i)).(fst acoord.(i)) <- Empty;
							p.(ny).(nx) <- People apeople.(i);
							achemin.(i) <- List.tl achemin.(i);
							acoord.(i) <- (nx,ny)
						end
					end
				| (x,y)::q when valide p x y -> 
					p.(snd acoord.(i)).(fst acoord.(i)) <- Empty;
					p.(y).(x) <- People apeople.(i);
					achemin.(i) <- q;
					acoord.(i) <- (x,y)
				| (x,y)::_ -> 
					plateau.(y).(x) <- Obstacle;
					achemin.(i) <- resolution_flood_fill plateau (fst acoord.(i)) (snd acoord.(i)) ld;
					plateau.(y).(x) <-Empty;
					if achemin.(i) <> [] 
					then 
						let nx, ny = List.hd achemin.(i) in
						if valide p nx ny 
						then begin 
							p.(snd acoord.(i)).(fst acoord.(i)) <- Empty;
							p.(ny).(nx) <- People apeople.(i);
							achemin.(i) <- List.tl achemin.(i);
							acoord.(i) <- (nx,ny)
						end
				(*| _ -> ()*))
			end
		done;
		while Sys.time () < start_time +. 1. do () done;
		print_graph p acouleur prop_i
	done;
	close_graph ()