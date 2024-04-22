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

let print_bvh t =
	let rec aux t (c : string) =
		match t with
		|Feuille x -> Printf.printf "%s((%f, %f), (%f, %f))" c (fst (fst x)) (snd (fst x)) (fst (snd x)) (snd (snd x))
		|Noeud (g,x,d) -> aux g (c^"    ");
							Printf.printf "\n%s((%f, %f), (%f, %f))\n" c (fst (fst x)) (snd (fst x)) (fst (snd x)) (snd (snd x));
							aux d (c^"    ");
		|_ -> () in
	aux t "";
	Printf.printf "\n"

let _ =
	let obj = [((200., 200.), (300., 300.));((350.,75.),(423.,148.))] in
	print_bvh (construit_bvh 0 obj)