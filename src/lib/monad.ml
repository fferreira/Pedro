(* open Error *)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val fail : string (*Error.user_error*) -> 'a t

  val map : ('a -> 'b t) -> 'a list -> 'b list t
end

type 'a res = ('a, string) result

module type READER = sig
  include MONAD

  type r

  val run : 'a t -> r -> 'a res

  val local : (r -> r) -> 'a t -> 'a t

  val ask : r t
end

module Monad (* : MONAD *) = struct
  type 'a t = 'a res

  let return x = Ok x

  let bind = function
    | Ok x -> fun f -> f x
    | Error err -> fun _ -> Error err

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = Error err

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module Reader (Q : sig
  type r
end) =
(* : READER *)
struct
  type r = Q.r

  type 'a t = T of (r -> 'a res)

  let run = function T r -> r

  let return x = T (fun _ -> Ok x)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun env ->
        match run m env with
        | Ok m' -> run (f m') env
        | Error err -> Error err )

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun _ -> Error err)

  let local f m = T (fun env -> run m (f env))

  let ask = T (fun r -> Ok r)

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module State (Q : sig
  type s
end) =
struct
  type s = Q.s

  type 'a t = T of (s -> s * 'a res)

  let run = function T r -> r

  let return x = T (fun s -> (s, Ok x))

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun s ->
        match run m s with
        | s, Ok m' -> run (f m') s
        | s, Error err -> (s, Error err) )

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun s -> (s, Error err))

  let local f m =
    T
      (fun s ->
        let _, r = run m (f s) in
        (s, r) )

  let get = T (fun s -> (s, Ok s))

  let set s = T (fun _ -> (s, Ok ()))

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []
end

module ReaderState (Q : sig
  type st

  type rd
end) =
struct
  type s = Q.st (* state type *)

  type r = Q.rd (* reader type *)

  type 'a t = T of (s * r -> s * 'a res)

  let run = function T r -> r

  let return x = T (fun (s, _) -> (s, Ok x))

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    T
      (fun (s, r) ->
        match run m (s, r) with
        | s', Ok m' -> run (f m') (s', r)
        | s', Error err -> (s', Error err) )

  let ( >>= ) = bind

  let ( let* ) = bind

  let fail err = T (fun (s, _) -> (s, Error err))

  let local f m =
    T
      (fun (s, r) ->
        let s', r' = run m (s, f r) in
        (s', r') )

  let ask = T (fun (s, r) -> (s, Ok r))

  let get = T (fun (s, _) -> (s, Ok s))

  let set s = T (fun _ -> (s, Ok ()))

  let rec map (f : 'a -> 'b t) (l : 'a list) : 'b list t =
    match l with
    | x :: xs ->
        let* x' = f x in
        let* xs' = map f xs in
        return (x' :: xs')
    | [] -> return []

  let concat ll = List.concat ll |> return
end
