(*bonjour programe du plus court chemi sur un graphe en floodfill*)

open Labyrinte

type case = Labyrinte.case

type plateau = case array array

exception Solution of plateau * (int * int) array array * int

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

(*let creer_lab (x: int) (y: int) : case array array=
	let lab = Labyrinte.labyrinthe x y in
	let rep = Array.make_matrix (Array.length lab) (Array.length lab.(0)) Empty in
	for j = 0 to Array.length lab -1  do 
		for i = 0 to Array.length lab.(0) -1 do 
			match lab.(j).(i) with 
			| 0 -> rep.(j).(i) <- Empty;
			| 1 -> rep.(j).(i) <- Obstacle;
			| _ -> rep.(j).(i) <- Exit;
		done
	done; rep*)




let valide (p : plateau) (x : int) (y : int) : bool =
	p.(x).(y) == Empty || p.(x).(y) == Exit

let mouvement (x : int) (y : int)  = 
	[(x+1,y); (x-1,y); (x,y+1); (x,y-1)]

let mouvement_valide (p : plateau) (pp : etat array array) (x : int) (y : int) : (int * int) list = 
	List.filter (fun (x,y) -> x >= 0 && y>=0 && y< Array.length p && x < Array.length p.(0) && (valide p x y) && pp.(x).(y) == Inconnu) (mouvement x y)

let mouvement_prioritaire_valide (p : plateau) (pp : etat array array) (x : int) (y : int) (xd : int) (yd : int) : (int * int) list =
	List.sort (fun (c,d) (a,b) -> int_of_float(sqrt (float_of_int ((xd-a)*(xd-a) + (yd-b)*(yd-b))) -. sqrt (float_of_int ((xd-c)*(xd-c) + (yd-d)*(yd-d))))) (mouvement_valide p pp x y)

let print_map (p : plateau) : unit =
	for i = 0 to Array.length p -1 do
		for j = 0 to Array.length p.(0) -1 do
			match p.(i).(j) with
			|Empty -> Printf.printf "|   "
			|Obstacle -> Printf.printf "|###"
			|Exit -> Printf.printf "|___"
			|People n -> Printf.printf "|%3d" n
		done;
		Printf.printf "|\n";
	done;
	Printf.printf "\n";
	flush stdout

let print_pere (p : (int * int) array array) : unit =
	for i = 0 to Array.length p -1 do
		for j = 0 to Array.length p.(0) -1 do
			match p.(i).(j) with
			|(x, y) when x = -1 && y = -1 -> Printf.printf "|   "
			|(x, y) when x = i-1 && y = j -> Printf.printf "| ^ "
			|(x, y) when x = i+1 && y = j -> Printf.printf "| v "
			|(x, y) when x = i && y = j-1 -> Printf.printf "| < "
			|_  -> Printf.printf "| > "
		done;
		Printf.printf "|\n";
	done;
	Printf.printf "\n";
	flush stdout

let trouve_chemin pere xe ye x y = 
	let chemin = ref [] in
	while (!xe,!ye) <> (x, y) do
		chemin := (!xe, !ye) :: !chemin;
		let x_tmp, y_tmp = pere.(!xe).(!ye) in
		xe := x_tmp;
		ye := y_tmp 
	done;
	!chemin

let resolution (p : plateau) (x : int) (y : int) (xd : int) (yd : int) : (int* int) list=
	p.(xd).(yd) <- Exit;
	try 
		let map_deplacement = Array.make_matrix (Array.length p) (Array.length p.(0)) Inconnu in
		let pere = Array.make_matrix (Array.length p) (Array.length p.(0)) (-1,-1) in
		let f = creer_file () in
		let n = ref 0 in
		enfile f (x,y);
		map_deplacement.(x).(y) <- Traitement;
		(*map_deplacement.(x).(y) <- true;
		List.iter (enfile f) (mouvement_valide p map_deplacement x y);
		p.(x).(y) <- People !n;*)
		while not (est_vide f) do
			let i,j = defile f in
			if map_deplacement.(i).(j) == Traitement
			then begin
				n := !n +1;
				if p.(i).(j) = Exit then begin 
					p.(i).(j) <- People !n;
					raise (Solution (p, pere, !n))
				end;

				p.(i).(j) <- People !n;
				map_deplacement.(i).(j) <- Traite;
				List.iter (fun (ip, jp) -> 
						enfile f (ip, jp);
						if map_deplacement.(ip).(jp) == Inconnu
						then begin  
							pere.(ip).(jp) <- (i, j);
							map_deplacement.(ip).(jp) <- Traitement end)
					(mouvement_prioritaire_valide p map_deplacement i j xd yd)

			end
		done;
		raise Not_found
	with Solution (p, pere, n) -> 
		let xe = ref 0 in 
		let ye = ref 0 in 
		for i = 0 to Array.length p -1 do
			for j = 0 to Array.length p.(0) -1 do
				if p.(i).(j) = People n then begin 
					xe := i;
					ye := j
				end
			done;
		done;
		print_pere pere;
		trouve_chemin pere xe ye x y

let reparer_lab lab =
	for i = 0 to Array.length lab -1 do
		for j = 0 to Array.length lab.(0) -1 do
			match lab.(i).(j) with
			|Obstacle -> ()
			|Exit -> ()
			|_ -> lab.(i).(j) <- Empty
		done
	done


let rec applique_chemin (chemin : (int * int) list) (p : plateau) (n : int): plateau =
	match chemin with
	|[] -> p
	|(x, y)::q -> p.(x).(y) <- People n;
				applique_chemin q p (n-1)

let rec print_list c =
	match c with
	|[] -> Printf.printf "fin\n"
	|(x,y)::q -> Printf.printf "%d, %d\n" x y; print_list q

let init_matrix (leny : int) (lenx : int) (f : (int * int) -> 'a) : 'a array array = 
	let rep = Array.make_matrix leny lenx (f (0, 0)) in
	for i = 0 to Array.length rep -1 do
		for j = 0 to Array.length rep.(0) -1 do
			rep.(i).(j) <- f (j,i)
		done
	done;
	rep


let un_chemin : unit = 
	let p = (*creer_lab 20 20*) init_matrix 20 20 (fun _ -> Empty) in
	let chemin = resolution p 1 1 19 19 in
	print_map p;
	reparer_lab p;
	print_map (applique_chemin ((1, 1)::chemin) p (List.length chemin +1))

let _ =
	un_chemin;