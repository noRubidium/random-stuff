open Core

(* An interesting datastructor that encodes a sequence into an immutable data structure that
 * supports efficient indexing.
 * The most naive way to implement a sequence with efficient lookup will be a binary tree structure
 * Encoding the index with 0s and 1s.
 * creating a structure like:
 *              Root
 *             /   \
 *           / \  / \
 *          0  1 2  3
 * it has O(logn) lookup
 * But that is fairly inefficient in space utilization
 * Hence we are introducing an encoding using 1s and 2s:
 * ϵ is used to represent 0
 * a_n .. a_2 a_1 a_0 (a_0, a_1, a_2 .. a_n ∈ {1, 2}) represents: ∑(a_i * 2^i)
 * So we can see than 4 is represented as 12, 7 is represented as 111
 * TODO: Prove this is a bijection (very obvious)
 * so we can have something like:
 *                          0
 *                     _____|_____
 *                   1|           |2
 *                    1           2
 *                 ___|___     ___|___
 *               1|       |2 1|       |2
 *                3       4   5       6
 * But this is a bit sad :( since for a general sequence structure, we would like to still
 * have the ability to efficiently perform head/tail
 * hence we will tweak the implementation a bit:
 * Instead of having the a_n..a_0 sequence as the branch encoding, a_0 to a_n encoding will
 * be more useful
 *
 *                                            0
 *                      ______________________|_______________________
 *                    1|                                              |2
 *                     1                                              2
 *       ______________|______________                    ____________|_________________
 *     1|                             |2                1|                              |2
 *      3                             5                  4                              6
 *   ___|___                     _____|_____        _____|_____                 ________|______
 * 1|       |2                 1|           |2    1|           |2             1|               |2
 *  7      11                   9           13     8           12              10              14
 * It might be a bit weird at first glance, but it drasticly changed the property of the tree.
 * every single left/right branch of itself is a braun tree
 * left branch can have at most one more element than the right branch
 * So we can easily work out the insertion will actually flip the tree.
 *)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt [@@deriving sexp]
type 'a t = 'a bt [@@deriving sexp]

let empty = Empty

let hd = function
  | Empty            -> None
  | Node (x, _l, _r) -> Some x
;;

let rec cons x = function
  | Empty           -> Node (x, empty, empty)
  | Node (x', l, r) -> Node (x', cons x r, l)
;;

let rec uncons = function
  | Empty -> None
  | Node (x, l, r) ->
    match uncons l with
    | None -> Some (x, empty) (* implicitly r = Empty *)
    | Some (x', l') -> Some (x, Node (x', r, l'))
;;

let tl t =
  match uncons t with
  | None         -> None
  | Some (_x, t) -> Some t
;;

let rec nth n = function
  | Empty -> None
  | Node (x, l, r) ->
    if n = 0 then
      Some x
    else if n mod 2 = 0 then
      nth ((n - 1) / 2) r
    else
      nth ((n - 1) / 2) l
;;
