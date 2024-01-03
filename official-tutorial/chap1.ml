(*
sort
insert 
deriv
*)

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst = 
  match lst with
  [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: (insert elt tail)

let deriv f dx = fun x -> (f (x +. dx) -. (f x)) /. dx

(*same as List.map*)
let rec map f l = 
  match l with
  [] -> []
  | hd :: tl -> f hd :: map f tl;;


(*records*)

(*attributes is called field*)

(* def of fraction by record*)
type ratio = {num: int; denom: int}
let n2d3 = {num=2;denom=3}
let add_ratio r1 r2 =
  {num = r1.num * r2.denom + r2.num * r1.denom;
  denom = r1.denom * r2.denom}
  
(*match with can also apply to record*)
(*1分岐だけのmatch withもできるらしい。*)
let integer_part r =
  match r with
  {num = b; denom=a} -> b / a
  
(*In func def, fields of record can be specified directory*)
(*unneeded fields can be omitted*)
let get_denom {denom=denom} = denom
let get_num {num=num; _} = num

(*substitute fieldname*)
let integer_part {num; denom} = num / denom
let get_num {num} = num;;

(*with, which makes copy of a record*)
let n2d5 = {n2d3 with denom=5}
let n3d5 = {{n2d3 with num=3} with denom=5}

(*variant*)
(*case name (capitalized) is called constructor*)

(*確かに、コンストラクタの名前を型名と同様にintなどにしたら、関数定義において返り値の型推論ができなくなりそう*)
type number = Int of int | Float of float | Error;;
type sign = Positive | Negative
let sign_int n = if n >= 0 then Positive else Negative

let add_num n1 n2 = 
  match (n1, n2) with
  (Int i1, Int i2) -> 
    (*2つの引数の符号が等しくて、因子と計算結果の符号が異なるということはoverflowが起きているということ*)
    if sign_int i1 = sign_int i2 && sign_int (i1 + i2) <> sign_int i1 (*<> は != に相当する*)
    then Float(float i1 +. float i2)
    else Int(i1 + i2)
  | (Int i1, Float f2) -> Float(float i1 +. f2)
  | (Float f1, Int i2) -> Float(f1 +. float i2)
  | (Float f1, Float f2) -> Float(f1 +. f2)
  | (Error, _) -> Error
  | (_ , Error) -> Error
  
let n2 = Int(2)
let f3 = Float(3.)

(*option type can represent by using type 'a*)
(*'a option type means either a value of type 'a or an absence of value*)
type 'a option = Some of 'a | None
let safe_square_root x = if x >= 0. then Some(sqrt x) else None;;

(*binary tree*)

(*Emptyは、親の無いノードや子の無いノードのためにあるようだ。*)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let node_b = 1 , Empty , Empty;;
(*val b : int * 'a btree * 'b btree = (1, Empty, Empty)*)
let node_c = 3, Empty, Empty;;
(*val c : int * 'a btree * 'b btree = (3, Empty, Empty)*)
let node_a = 2, node_b, node_c;;
(*val a : int * (int * 'a btree * 'b btree) * (int * 'c btree * 'd btree) =
  (2, (1, Empty, Empty), (3, Empty, Empty))*)

let rec member x btree = 
  match btree with
  Empty -> false
  | Node(y, left, right) ->
    if x = y then true else
    if x < y then member x left else member x right

    
let rec insert x btree = 
  match btree with
  Empty -> Node(x, Empty, Empty)
  | Node(y, left, right) ->
    if x <= y then Node( y, insert x left, right)
      else Node(y, left, insert x right)
      
(*imperative features*)

(*I learned
min
Array.length
for to do done
Array.make
<-
res.(i)
; res
[| ; ; |]
ref
:=
*)

let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res

let v1 = [| 1.0; 2.0|]
  
(*mutable recort*)
type mutable_point = {mutable x: float; mutable y: float}
(*change the value of p (mutable)*)
let translate p dx dy = 
  p.x <- p.x +. dx; 
  p.y <- p.y +. dy

(*jは変数ではなく、値を指す識別子。
   refで定義するとjの識別先は変更可能になる。
   !jでjの識別している値を返す*)

let insertion_sort a = 
  for i = 1 to Array.length a - 1 do
    let val_i = a.(i) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      j := !j - 1
    done;
    a.(!j) <- val_i
  done;

(* references = single-field mutable record
the codes below implememt ref, ! and := but make conflict against insert_sort

type 'a ref = {mutable contents: 'a}
let ( ! ) r = r.contents
let ( := ) r newval = r.contents <- newval
*)

(*porimorphic function*)

type expression = 
  Const of float
  | Var of string
  | Sum of expression * expression (*e1 + e2*)
  | Diff of expression * expression (*e1 - e2*)
  | Prod of expression * expression (*e1 * e2*)
  | Quot of expression * expression (*e1 / e2*)

exception Unbound_variable of string;;

let rec eval env exp = 
  match exp with
    Const c -> c
    | Var v -> (try List.assoc v env with Not_found -> raise (Unbound_variable v))
    | Sum(f, g) -> eval env f +. eval env g
    | Diff(f, g) -> eval env f -. eval env g
    | Prod(f, g) -> eval env f *. eval env g
    | Quot(f, g) -> eval env f /. eval env g
    
let env1 = [("pi", 3.1); ("n", 5.0)]
let form = Prod(Prod(Const 2.0, Var "pi"), Sum(Var "n", Const 3.0))

(*exceptions*)

exception Empty_list

let head l = 
  match l with
  [] -> raise Empty_list
  | hd :: tl -> hd

(*List.assoc*)
List.assoc 1 [(0, "zero"), (1, "one")]
List.assoc 2 [(0, "zero"), (1, "one")]

let name_of_binary_digit digit = 
  try
    List.assoc digit [0, "zero"; 1, "one"]
  with Not_found ->
    "not a binary digit"

let rec first_named_value values names =
  try
    List.assoc (head values) names
  with
  | Empty_list -> "no named value"
  | Not_found -> first_named_value (List.tl values) names

(*finalization: i could not catch this.*)
let temporarily_set_reference ref newval funct = 
  let oldval = !ref in
  try
    ref := newval;
    let res = funct () in
    ref := oldval;
    res
  with x ->
    ref := oldval;
    raise x

let assoc_may_map f x l =
  match List.assoc x l with
  | exception Not_found -> None
  | y -> f y;;

(* Some _ as v -> v can be replaced with v -> v*)
let flat_assoc_opt x l =
  match List.assoc x l with
  | None | exception Not_found -> None
  | Some _ as v -> v 
  

(*try with for imperative control (while loop)*)
let fixpoint f x = 
  let exception Done in 
  let x = ref x in
  try while true do
    let y = f !x in
    if abs_float (!x -. y) < 0.01  then raise Done else x := y
    done; assert false
  with Done -> !x
  
(*pretty-printing*)
let print_expr exp = 
  let open_paren prec op_prec = 
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  let rec print prec exp = 
    match exp with
    Const c -> print_float c
    | Var v -> print_string v
    | Sum(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + " ;print 0 g;
        close_paren prec 0
    | Diff(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " - " ;print 1 g;
        close_paren prec 0
    | Prod(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " * " ;print 2 g;
        close_paren prec 2
    | Quot(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " / " ;print 3 g;
        close_paren prec 2
  in print 0 exp
  
(*print formats*)

(*%a is useful to make custom printer and compose complex printers*)
(* I wonder why there is an sytax error in the code below*)
(*
let pp_int ppf n = Printf.fprintf ppf "%d" n
let demo_print = Printf.printf "Outputting an integer using a custom printer: %a\n" pp_int 42;;
*)

(*standard ml programs*)

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

let main () =
  let a = int_of_string Sys.argv.(1) in
  let b = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (gcd a b);
  exit 0;;
main ();;