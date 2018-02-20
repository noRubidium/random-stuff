open Core

type 'a t [@@deriving sexp]

val empty : 'a t

val hd : 'a t -> 'a option

val tl : 'a t -> 'a t option

val cons : 'a -> 'a t -> 'a t

val nth : int -> 'a t -> 'a option
