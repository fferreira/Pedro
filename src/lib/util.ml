(* some list utilities *)

let rec uniq = function
  | x :: xs -> if List.mem x xs then uniq xs else x :: uniq xs
  | [] -> []


let rec uniq_eq (eq : 'a -> 'a -> bool) =
  let mem_eq x = List.exists (fun p -> eq x p ) in
  function
  | x :: xs -> if mem_eq x xs then uniq_eq eq xs else x :: uniq_eq eq xs
  | [] -> []

(* remove the first element for which f returns true *)
let rec remove_one f = function
  | [] -> []
  | y :: ys when f y -> ys
  | y :: ys -> y :: remove_one f ys

(* returns an element that it found and a list without it, if the element was
   there *)
let rec find_and_remove_fst (f : 'a -> bool) :
    'a list -> ('a * 'a list) option = function
  | x :: xs when f x -> Some (x, xs)
  | x :: xs ->
      Option.bind (find_and_remove_fst f xs) (fun (y, ys) ->
          Some (y, x :: ys) )
  | [] -> None

let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1

(* true if l is included in l' *)
let included (l : 'a list) (l' : 'a list) : bool =
  List.for_all (fun x -> List.mem x l') l

(* true if a list is empty *)
let is_empty = function [] -> true | _ -> false

(* add x to l if x is not already in l *)
let uniq_cons x l = if List.mem x l then l else x :: l
