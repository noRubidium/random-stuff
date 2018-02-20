open Core
open Braun_tree

let () =
  let l =
    Braun_tree.empty
    |> cons 0
    |> cons 1
    |> cons 2
    |> cons 3
  in
  let l' = tl l |> Option.value_exn in
  printf
    !"Ha: %{sexp: int option}, %{sexp: int option}
    %{sexp: int Braun_tree.t}, %{sexp: int Braun_tree.t}
    \n%!"
    (nth 1 l)
    (nth 1 l')
    l l'
