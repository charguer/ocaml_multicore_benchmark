(**
   ocamlopt tree_grow_shrink.ml && time ./a.out 15 10 10 
   result 360448
   real	0m0.988s
 
  usage: the program expects 3 arguments on the command line:
  - depth of the tree
  - number of outer iterations
  - number of inner iterations

  It repeats nbOuterIterations times:
    - create a fresh tree of the specified depth 
    - repeat nbInnerIterations times:
      - replace each Leaf by Node(Leaf, Leaf)
      - replace every Node(Leaf, Leaf) by a Leaf
      [note: half of the leaves remain live w.r.t. GC]
    - compute the sum over the tree.

*)


let fork2 f1 f2 = (* to be made parallel *)
  let x1 = f1() in
  let x2 = f2() in
  (x1,x2)

type tree = Leaf of int | Node of tree * tree

let rec mk_tree depth =
  if depth = 0 then Leaf 1 else begin
    let (t1,t2) = fork2   
      (fun () -> mk_tree (depth-1))
      (fun () -> mk_tree (depth-1)) 
      in
    Node(t1,t2)
  end

let rec grow_tree = function
  | (Leaf n as t1) -> Node (t1, Leaf (n+1))
  | Node (t1,t2) ->
      let (u1,u2) = fork2 
        (fun () -> grow_tree t1)
        (fun () -> grow_tree t2)
        in
      Node (u1,u2)

let rec shrink_tree = function
  | Node ((Leaf n as t1), Leaf _) -> t1
  | Node (t1, t2) ->
      let (u1,u2) = fork2 
        (fun () -> shrink_tree t1)
        (fun () -> shrink_tree t2)
        in
      Node (u1,u2)
  | _ -> assert false

let rec sum_tree = function
  | Leaf n -> n
  | Node (t1, t2) ->
      let (n1,n2) = fork2 
        (fun () -> sum_tree t1)
        (fun () -> sum_tree t2)
        in
      n1 + n2

let _ =
  let depth = int_of_string Sys.argv.(1) in
  let outer_repeat = int_of_string Sys.argv.(2) in
  let inner_repeat = int_of_string Sys.argv.(3) in
  let result = ref 0 in
  for i = 1 to outer_repeat do
    let t0 = mk_tree depth in
    let n0 = inner_repeat in
    let rec aux n t =
      if n = 0 then t else begin
        let t = grow_tree t in
        let t = shrink_tree t in
        aux (n-1) t
      end 
      in
    let t = aux n0 t0 in
    result := sum_tree t;
  done;
  Printf.printf "result %d\n" !result
