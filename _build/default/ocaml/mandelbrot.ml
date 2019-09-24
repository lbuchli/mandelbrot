open Graphics;;


print_string "Width: ";;
let width  = read_int ();;
print_string "Height: ";;
let height = read_int ();;
print_string "Max Iterations: ";;
let max_iter = read_int ();;

open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height))

let rec calc_pixel x0 y0 xn yn iteration =
  if xn ** 2.0 +. yn ** 2.0 <= 2.0 *. 2.0 && iteration < max_iter then
    let newx = xn ** 2.0 -. yn ** 2.0 +. x0
    and newy = 2.0 *. xn *. yn +. y0 in
    calc_pixel x0 y0 newx newy (iteration + 1)
  else
    iteration
;;

let calc_color iterations =
  let normalized = (float_of_int iterations) /. (float_of_int max_iter) in
  let r = 1.0 -. normalized
  and g = Float.abs (0.5 -. normalized)
  and b = normalized in
  rgb
    (int_of_float (r *. 255.0))
    (int_of_float (g *. 255.0))
    (int_of_float (b *. 255.0));;

for x = 0 to width do
  for y = 0 to height do
    let x0 = ((float_of_int x) /. (float_of_int width)) *. 4.0 -. 2.0
    and y0 = ((float_of_int y) /. (float_of_int width)) *. 4.0 -. 2.0 in
    let iterations = calc_pixel x0 y0 0.0 0.0 0 in
    set_color (calc_color iterations);
    fill_rect x y 1 1
  done
done;;

wait_next_event [Key_pressed];;
