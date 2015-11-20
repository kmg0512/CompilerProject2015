(* TODO: Implement an optimizer *)

let rec optimize : T.program -> T.program
=fun t -> optimize4 (optimize2 (optimize1 t t(*(optimize3 t []) (optimize3 t [])*)))

(* DEAD CODE *)
and optimize1 : T.program -> T.linstr list -> T.program
=fun t t' ->
	match t' with
	| [] -> []
	| t1 :: t2 -> nonuse t1 t @ optimize1 t t2

and optimize2 : T.program -> T.program
=fun t ->
	match t with
	| [] -> []
	| t1 :: t2 ->
		match t1 with
		| (_,T.UJUMP _) -> gotoskip t1 t2 @ optimize2 t2
		| (_,T.CJUMP (_,_)) -> gotoskip t1 t2 @ optimize2 t2
		| (_,T.CJUMPF (_,_)) -> gotoskip t1 t2 @ optimize2 t2
		| _ -> [t1] @ optimize2 t2

(* COPY PROPAGATION *)
and optimize3 : T.program -> T.linstr list -> T.program
=fun t tt->
	match t with
	| [] -> tt
	| t1 :: t2 ->
	begin
		match t1 with
		| (_,T.SKIP) | (_,T.HALT) ->
			copypropagate (tt @ [t1]) @ optimize3 t2 []
		| _ -> optimize3 t2 (tt @ [t1])
	end

(* SKIP LABEL SHIFT *)
and optimize4 : T.program -> T.program
=fun t ->
	match t with
	| [] -> []
	| t1 :: t2 ->
	begin
		match t1 with
		| (l,T.SKIP) ->
		begin
			match t2 with
			| [] -> []
			| (l',T.SKIP) :: t3 -> (l,T.SKIP) :: optimize4 t2
			| (_,instr) :: t3 -> (l,instr) :: optimize4 t3
		end
		| _ -> t1 :: optimize4 t2
	end

and nonuse : T.linstr -> T.linstr list -> T.linstr list
=fun t1 t2 ->
	match t1 with
	| (l,T.SKIP) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.UJUMP l') :: t3 ->
			if l = l' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (_,l')) :: t3 ->
			if l = l' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (_,l')) :: t3 ->
			if l = l' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.ALLOC (x,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.LOAD (_,(a,_))) :: t3 ->
			if x = a then [t1] else nonuse t1 t3
		| (_,T.STORE ((a,_),_)) :: t3 ->
			if x = a then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.ASSIGNV (x,_,_,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.ASSIGNC (x,_,_,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.ASSIGNU (x,_,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.COPY (x,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.COPYC (x,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| (_,T.LOAD (x,_)) ->
	begin
		match t2 with
		| [] -> []
		| (_,T.ASSIGNV (_,_,y',z')) :: t3 ->
			if x = y' || x = z' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNC (_,_,y',_)) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.ASSIGNU (_,_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.COPY (_,y')) :: t3 ->
			if x = y' then [t1] else nonuse t1 t3
		| (_,T.CJUMP (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.CJUMPF (x',_)) :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.LOAD (_,(_,i'))) :: t3 ->
			if x = i' then [t1] else nonuse t1 t3
		| (_,T.STORE ((_,i'),x')) :: t3 ->
			if x = i' || x = x' then [t1] else nonuse t1 t3
		| (_,T.READ x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| (_,T.WRITE x') :: t3 ->
			if x = x' then [t1] else nonuse t1 t3
		| _ :: t3 -> nonuse t1 t3
	end
	| _ -> [t1]

and gotoskip : T.linstr -> T.linstr list -> T.linstr list
=fun t1 t2 ->
	match t2 with
	| (l,T.SKIP) :: t3 ->
	begin
		match t1 with
		| (_,T.UJUMP l') ->
			if l = l' then [] else gotoskip t1 t3
		| (_,T.CJUMP (_,l')) ->
			if l = l' then [] else gotoskip t1 t3
		| (_,T.CJUMPF (_,l')) ->
			if l = l' then [] else gotoskip t1 t3
		| _ -> raise (Failure "Optimizer.gotoskip : invalid argument t1")
	end
	| _ -> [t1] 	

and copypropagate : T.linstr list ->T.linstr list
=fun t ->
	match t with
	| [] -> []
	| t1 :: t2 ->
	begin
		match t1 with
		| (_,T.COPY (x,y)) -> convertvar x y t2
		| (_,T.COPYC (x,n)) -> convertconst x n t2
		| _ -> t1 :: copypropagate t2
	end

and convertvar : T.var -> T.var -> T.linstr list -> T.linstr list
=fun x y t ->
	match t with
	| [] -> []
	| t1 :: t2 ->
	begin
		match t1 with
		| (l,T.ASSIGNV (x',bop,x'',y')) when x'' = x ->
			(l,T.ASSIGNV (x',bop,y,y')) :: convertvar x y t2
		| (l,T.ASSIGNV (x',bop,y',x'')) when x'' = x ->
			(l,T.ASSIGNV (x',bop,y',y)) :: convertvar x y t2
		| (l,T.ASSIGNC (x',bop,x'',n)) when x'' = x ->
			(l,T.ASSIGNC (x',bop,y,n)) :: convertvar x y t2
		| (l,T.ASSIGNU (x',uop,x'')) when x'' = x ->
			(l,T.ASSIGNU (x',uop,y)) :: convertvar x y t2
		| (l,T.COPY (x',x'')) when x'' = x ->
			(l,T.COPY (x',y)) :: convertvar x y t2
		| (l,T.CJUMP (x'',l')) when x'' = x ->
			(l,T.CJUMP (y,l')) :: convertvar x y t2
		| (l,T.CJUMPF (x'',l')) when x'' = x ->
			(l,T.CJUMPF (y,l')) :: convertvar x y t2
		| (l,T.ASSIGNV (x'',_,_,_)) when x'' = x -> t
		| (l,T.ASSIGNC (x'',_,_,_)) when x'' = x -> t
		| (l,T.ASSIGNU (x'',_,_)) when x'' = x -> t
		| (l,T.COPY (x'',_)) when x'' = x -> t
		| (l,T.COPYC (x'',_)) when x'' = x -> t
		| (l,T.LOAD (x'',_)) when x'' = x -> t
		| _ -> t1 :: convertvar x y t2
	end

and convertconst : T.var -> int -> T.linstr list -> T.linstr list
=fun x n t ->
	match t with
	| [] -> []
	| t1 :: t2 ->
	begin
		match t1 with
		| (l,T.ASSIGNV (x',bop,x'',y)) when x'' = x ->
			(l,T.ASSIGNC (x',bop,y,n)) :: convertconst x n t2
		| (l,T.ASSIGNV (x',bop,y,x'')) when x'' = x ->
			(l,T.ASSIGNC (x',bop,y,n)) :: convertconst x n t2
		| (l,T.ASSIGNC (x',bop,x'',n')) when x'' = x ->
		begin
			match bop with
			| T.ADD -> (l,T.COPYC (x',n + n')) :: convertconst x n t2
			| T.SUB -> (l,T.COPYC (x',n - n')) :: convertconst x n t2
			| T.MUL -> (l,T.COPYC (x',n * n')) :: convertconst x n t2
			| T.DIV -> (l,T.COPYC (x',n / n')) :: convertconst x n t2
			| T.LT ->
				if n < n' then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.LE ->
				if n <= n' then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.GT ->
				if n > n' then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.GE ->
				if n >= n' then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.EQ ->
				if n = n' then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.AND ->
				if n != 0 && n' != 0 then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
			| T.OR ->
				if n != 0 || n' != 0 then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
		end
		| (l,T.ASSIGNU (x',uop,x'')) when x'' = x ->
		begin
			match uop with
			| T.MINUS -> (l,T.COPYC (x',-n)) :: convertconst x n t2
			| T.NOT ->
				if n = 0 then (l,T.COPYC (x',1)) :: convertconst x n t2
				else (l,T.COPYC (x',0)) :: convertconst x n t2
		end
		| (l,T.COPY (x',x'')) when x'' = x ->
			(l,T.COPYC (x',n)) :: convertconst x n t2
		| (l,T.ASSIGNV (x'',_,_,_)) when x'' = x -> t
		| (l,T.ASSIGNC (x'',_,_,_)) when x'' = x -> t
		| (l,T.ASSIGNU (x'',_,_)) when x'' = x -> t
		| (l,T.COPY (x'',_)) when x'' = x -> t
		| (l,T.COPYC (x'',_)) when x'' = x -> t
		| (l,T.LOAD (x'',_)) when x'' = x -> t
		| _ -> t1 :: convertconst x n t2
	end