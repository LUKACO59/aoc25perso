let get2l nom =
  let rec aux ch l1 l2 =
    try
      let line = input_line ch in
      match String.split_on_char ' ' line |> List.filter (fun s -> s <> "")  with
      | [v1; v2] ->
         aux ch (int_of_string v1 :: l1) (int_of_string v2 :: l2)
      | _ -> (l1,l2)
    with End_of_file ->
      close_in ch;
      (l1, l2)
  in
  let ch = open_in nom in
  aux ch [] []


let split l =
  let rec asplit l a b =
    match l with
      |[] -> a,b
      |t::q -> asplit q (t::b) a
  in
  asplit l [] []

let rec fusion l1 l2 =
  match l1,l2 with
    |_,[] -> l1
    |[],_ -> l2
    |t::q, r::s when t<r -> t::(fusion q l2)
    |t::q, r::s when t>=r -> r::(fusion l1 s)

let rec tri_fusion l=
  match l with
    |[]|[_] -> l
    |_ -> let l1,l2=split l in
          let l1=tri_fusion l1 in
          let l2=tri_fusion l2 in
          fusion l1 l2
let abs a =
  if a<0 then -a
  else a
let rec getdist (l1: int list) (l2:int list) =
  assert(List.length l1 = List.length l2);
  match l1,l2 with
    |[],[] -> []
    |t::q,r::s -> let a = abs (t-r) in
                  a::(getdist q s)

let somme l=
  List.fold_left (fun acc x -> acc + x) 0  l

let l1,l2 = get2l "input1"
let l1 =tri_fusion l1
let l2 = tri_fusion l2
let d = getdist l1 l2
let vd =somme d

let nb_occ l x=
  let rec aux l x nb =
    match l with
    |[] -> nb
    |t::q -> if t=x then aux q x (nb+1)
             else aux q x nb
  in
  aux l x 0

let rec getsiml l1 l2=
  match l1 with
  |[] -> []
  |t::q -> let nb = nb_occ l2 t in
           (t*nb)::(getsiml q l2)

let siml = getsiml l1 l2
let vs =somme siml
