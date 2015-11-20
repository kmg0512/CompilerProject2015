(* TODO: Implement an optimizer *)

let rec optimize : T.program -> T.program
=fun t -> optimize3 (optimize2 (optimize1 t t))

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

(* SKIP LABEL SHIFT - MAY BE OPTIMIZED IN LAST *)
and optimize3 : T.program -> T.program
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
			| (l',T.SKIP) :: t3 -> (l,T.SKIP) :: optimize3 t2
			| (_,instr) :: t3 -> (l,instr) :: optimize3 t3
		end
		| _ -> t1 :: optimize3 t2
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
