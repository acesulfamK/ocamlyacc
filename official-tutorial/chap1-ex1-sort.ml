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