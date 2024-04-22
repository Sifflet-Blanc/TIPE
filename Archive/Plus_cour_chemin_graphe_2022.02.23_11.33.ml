(*bonjour programe du plus court chemi sur un graphe en backtraking*)

let creer_pile () = ref []

let empile p x = p := x :: !p

let depile p = match !p with
    | [] -> failwith "pile vide"
    | t::q -> p := q; t

let creer_file = (creer_pile, creer_pile)

let enfile (p1,p2) x = empile p1

let defile (p1,p2) =
	if !p2 = []
	then 
		while !p1 <> [] do 
			empile p2 (depile p1)
		done;
	depile p2



type case = Obstacle | Exit | Empty | People of int

type plateau = case array array

exception Solution of int

let valide (p : plateau) (x : int) (y : int) : bool =
	p.(x).(y) == Empty || p.(x).(y) == Exit

let mouvement (x : int) (y : int)  = 
	[(x+1,y); (x-1,y); (x,y+1); (x,y-1)]

let mouvement_valide (p : plateau) (pp : bool array array) (x : int) (y : int) : (int * int) list = 
	List.filter (fun (x,y) -> (valide p x y) && not(pp.(x).(y))) (mouvement x y)

let print_map (p : plateau) : unit =
	for i = 0 to Array.length p -1 do
		for j = 0 to Array.length p.(0) -1 do
			match p.(i).(j) with
			|Empty -> Printf.printf "  "
			|Obstacle -> Printf.printf "##"
			|Exit -> Printf.printf "__"
			|People n -> Printf.printf "%2d" n
		done;
		Printf.printf "\n"
	done;
	Printf.printf "\n";
	flush stdout

let resolution (p : plateau) (x : int) (y : int) : (plateau * int) =
	try 
		let map_deplacement = Array.make_matrix (Array.length p) (Array.length p.(0)) false in
		let f = creer_file in
		let rec aux x y n = 
			if p.(x).(y) = Exit 
			then begin
				p.(x).(y) <- People n;
				raise (Solution n)
			end 
			else
				p.(x).(y) <- People n;
				List.iter (fun (x,y) -> map_deplacement.(x).(y) <- true;
							aux x y (n+1);
							map_deplacement.(x).(y) <- false;
							p.(x).(y) <- Empty;)
					(mouvement_valide p map_deplacement x y) in
		aux x y 0;
		raise Not_found
	with Solution n -> (p,n)

let un_chemin : unit = 
	let p = [|[|Obstacle; Obstacle; Obstacle; Obstacle; Obstacle|];
		 [|Obstacle; Empty; Empty; Empty; Obstacle|];
		 [|Obstacle; Empty; Empty; Empty; Exit|];
		 [|Obstacle; Empty; Empty; Empty; Obstacle|];
		 [|Obstacle; Obstacle; Obstacle; Obstacle; Obstacle|]|] in
	let (p,n) = resolution p 1 2 in 
	print_map p;
	Printf.printf "%d\n" n

let _ =
	un_chemin