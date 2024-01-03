module Fifo =
	struct
		type 'a queue = {front: 'a list; rear: 'a list}
		let make front rear = 
			match front with
			| [] -> { front = List.rev rear; rear = [] }
			| _ -> {front; rear}
		let empty = {front = []; rear = [] }
		let is_empty = function { front = []; _} -> true | _ -> false
		let add x q = make q.front (x :: q.rear)
		exception Empty
		let top = function
		| { front = []; _ } -> raise Empty
		| { front = x :: _; _ } -> x
		let pop = function
		| { front = []; _ } -> raise Empty
		| { front = _ :: f; rear = r} -> make f r
	end;;
		
	
let open Fifo in
add "hello" empty;;

open Fifo;;
let st1 = add "bye" empty;;
let st1 = add "taro" st1;;
let st1 = add "sato" st1;;

let st2 = add "bye" empty;;

(*second form*)
Fifo.(add "hello" empty);;

(*omit parentheses*)
Fifo.[empty] = Fifo.([empty]);;
Fifo.[|empty|] = Fifo.([|empty|]);;
Fifo.{contents = empty} = Fifo.({contents = empty});;

(*second form also works for patterns*)
let at_most_one_element x = match x with
|Fifo.{front = ([] | [_]); rear = []} -> true
| _ -> false;;

module FifoOpt = 
struct
  include Fifo
  let top_opt q = if is_empty q then None else Some(top q)
  let pop_opt q = if is_empty q then None else Some(pop q)
end;;
	
module type FIFO = 
	sig 
		type 'a queue
		val empty : 'a queue
		val add : 'a -> 'a queue -> 'a queue
		val top : 'a queue -> 'a
		val pop : 'a queue -> 'a queue
		exception Empty
	end;;
		
(*FifoにFIFOというシグネチャを与え、公開範囲を制限する*)
module AbstructQueue = (Fifo : FIFO);;