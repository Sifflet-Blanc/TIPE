open Graphics
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

let _ = 
	let n = 1000 in 
	open_graph (Printf.sprintf " %dx%d" n 1000);
	for i = 0 to n-1 do 
		let a = ((float_of_int i) /. (float_of_int (n-1))) in 
		set_color (rgb (r a) (g a) (b a));
		moveto i 0;
		lineto i 1000
	done;
	while true do () done
