type dir = N|S|E|O
type case = Obstacle | Exit | Empty | People of int
exception Fini

let rec is_in l x = 
	match l with 
	| [] -> false 
	| t::q -> if t = x then true else is_in q x


let rec element l x =
	match l with 
	| [] -> failwith "notfound but should not happend !!!!!!"
	| t::q -> if x = 0 then t else element q (x-1)

let labyrinthe (x: int) (y: int) : int array array=
	Random.self_init (); 
	let lab = Array.make_matrix x y 0 in 
	for i = 0 to x-1 do 
		for j = 0 to y -1 do 
			if i mod 2 = 0 || j mod 2 = 0 then 
			lab.(j).(i) <- 1
		done; done;
	let cases_pass = ref [] in let cases_blocs = ref [] in 
	let i,j = (ref 1),(ref 1) in
	try
	while(true) do 
		let potential = ref [] in let count = ref 0 in begin
		if !j -2 >= 0 && lab.(!j - 1).(!i) = 1 &&  not(is_in (!cases_blocs) ( !j - 2 , !i)) && not(is_in (!cases_pass) ( !j - 2 , !i)) then 
			(potential := N ::(!potential) ; count := !count +1);
		if !i -2 >= 0 && lab.(!j).(!i - 1) = 1 &&  not(is_in (!cases_blocs) ( !j , !i - 2)) && not(is_in (!cases_pass) ( !j , !i - 2)) then 
			(potential := O ::(!potential) ;count := !count +1);
		if !j +2 < y && lab.(!j + 1).(!i) = 1 &&  not(is_in (!cases_blocs) ( !j + 2 , !i)) && not(is_in (!cases_pass) ( !j + 2 , !i)) then 
			(potential := S ::(!potential) ;count := !count +1);
		if !i +2 < x && lab.(!j).(!i + 1) = 1 &&  not(is_in (!cases_blocs) ( !j , !i + 2)) && not(is_in (!cases_pass) ( !j , !i + 2)) then 
			(potential := E ::(!potential) ;count := !count +1);
	 	end;

		if !count <> 0 then let n = Random.int ( !count) in 
		let direction = element (!potential) n in match direction with 
		| N -> (lab.(!j - 1).(!i) <- 0 ;cases_pass :=  (!j,!i) :: !cases_pass ; j := !j - 2)
		| O -> (lab.(!j).(!i - 1) <- 0 ;cases_pass :=  (!j,!i) :: !cases_pass ; i := !i - 2)
		| S -> (lab.(!j + 1).(!i) <- 0 ;cases_pass :=  (!j,!i) :: !cases_pass ; j := !j + 2)
		| E -> (lab.(!j).(!i + 1) <- 0 ;cases_pass :=  (!j,!i) :: !cases_pass ; i := !i + 2)
		else (*count = 0*) match (!cases_pass) with 
						| [] -> raise Fini
						| t::q -> (cases_pass := q; cases_blocs :=  t:: !cases_blocs ; let g,h = t in (j := g; i := h))
		done; 
		lab
	with Fini -> lab 

let creer_lab (x: int) (y: int) : case array array=
	let lab = labyrinthe x y in
	let rep = Array.make_matrix (Array.length lab) (Array.length lab.(0)) Empty in
	for j = 0 to Array.length lab -1  do 
		for i = 0 to Array.length lab.(0) -1 do 
			match lab.(j).(i) with 
			| 0 -> rep.(j).(i) <- Empty;
			| 1 -> rep.(j).(i) <- Obstacle;
			| _ -> rep.(j).(i) <- Exit;
		done
	done; rep