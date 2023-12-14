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
	end

		
