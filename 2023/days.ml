module type AoC = sig
  val solve : string -> int
end


module SUtil = struct
  let len = String.length
  let stoi s n = int_of_char s.[n] - 48
  let sub = String.sub
end

module Day1 : AoC = struct

  open SUtil

  let rec sum file acc =
    try
      let s = input_line file in
      let n = 10 * stoi s 0 + stoi s (len s - 1) in
      sum file (acc + n)
    with End_of_file -> acc

  let solve path =
    let f = open_in path in
    let res = sum f 0 in
    close_in f;
    res
end


module Day2 : AoC = struct
  open Types2

  let p =
    List.for_all @@
    List.for_all @@
    function
    | Red r -> r <= 12
    | Green g -> g <= 13
    | Blue b -> b <= 14

  let f g =
    List.fold_left
      (fun acc (id, l) ->
        if p l then acc + id else acc)
      0 g

  let solve path =
    let file = open_in path in
    let lb = Lexing.from_channel file in
    let g = Parser2.input Lexer2.token lb in
    f g

end

module Day2b : AoC = struct
  open Types2


  let min = 0,0,0

  let max (a1,b1,c1) (a2,b2,c2) = max a1 a2, max b1 b2, max c1 c2

  let mult (a,b,c) = a * b * c

  let ff (e : tirage list) =
    List.fold_left
      (fun ((r,g,b) as acc) e ->
         match e with
         | Blue b' -> if b' > b then (r,g,b') else acc
         | Red r' -> if r' > r then (r',g,b) else acc
         | Green g' -> if g' > g then (r,g',b) else acc
      )
      min e

  let f g =
    List.fold_left
      (fun acc (_, l) ->
         acc +
         mult (List.fold_left
                 (fun acc e -> max acc (ff e))
                 min l)
      )
      0 g

  let solve path =
    let file = open_in path in
    let lb = Lexing.from_channel file in
    let g = Parser2.input Lexer2.token lb in
    f g

end

(* let () = Format.print_int @@ AoC.solve "./inputs/input_1_s" *)
(* let () = Format.print_int @@ Day2.solve "./inputs/input2" (* part 1 *) *)
let () = Format.print_int @@ Day2b.solve "./inputs/input2" (* part 2 *)

