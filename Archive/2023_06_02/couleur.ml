let pi = Float.pi

let r x = 
	let r = x *. pi *. 2. in 
	if r >= pi *. 4. /. 6. then int_of_float ((cos (r +. pi *. 2. /. 6.) +. 1.) *. 255. /. 2.)
	else begin 
		if r >= 3. *. pi /. 5.
		then 0
		else let tmp = cos (r *. r -. pi /. 6.) in int_of_float ((tmp +. 1.) *. 131. /. 2.)
	end

let g x = 
	let r = x *. pi *. 2. in 
	if r >= pi /. 3. then let tmp = sin (r -. 4. *. pi /. 6.) in int_of_float ((Float.pow tmp 1.2) *. 255.)
	else 0

let b x = 
	let r = x *. pi *. 2. in 
	if r >= pi +. pi /. 2. then 0
	else int_of_float ((sin r +. 1.) *. 255. /. 2.)
(*
open Graphics
let _ = 
	let n = 1000 in 
	open_graph (Printf.sprintf " %dx%d" n 600);
	for i = 0 to n-1 do 
		let a = ((float_of_int i) /. (float_of_int (n-1))) in 
		moveto i 600;
		set_color (rgb (r a) 0 0);
		lineto i 450;
		set_color (rgb 0 (g a) 0);
		lineto i 300;
		set_color (rgb 0 0 (b a));
		lineto i 150;
		set_color (rgb (r a) (g a) (b a));
		lineto i 0;
	done;
	while true do () done
*)