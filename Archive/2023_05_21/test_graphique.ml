open Graphics

let _ = 
	open_graph " 1000x1000";
	set_window_title "Deplacement test";
	set_color (rgb 0 0 0);	
	fill_rect 0 0 1000 1000;
	set_color (rgb 255 255 255);
	moveto 0 985;
	let size = ref 1 in 
	while true do 
		if key_pressed () then begin 
			draw_char (read_key ());
			set_text_size !size;
			if current_x () >= 995 then moveto 0 (current_y () - 15)
		end
	done