(* TODO: Write a translator below. *)

let label = ref 0
let var = ref 0

let next : int ref -> int
=fun n ->
	n := !n + 1;
	!n

let rec trans_e : S.exp -> T.var * T.linstr list
=fun e ->
	match e with
	| S.NUM n ->
		let t = "t" ^ string_of_int (next var) in
			(t,[(T.dummy_label,T.COPYC (t,n))])
	| S.LV lv ->
	begin
		match lv with
		| S.ID x ->
			let t = "t" ^ string_of_int (next var) in
				(t,[(T.dummy_label,T.COPY (t,x))])
		| S.ARR (x,e) ->
			let (t1,code) = trans_e e in
				let t2 = "t" ^ string_of_int (next var) in
					(t2,code @ [(T.dummy_label,T.LOAD (t2,(x,t1)))])
	end
	| S.ADD (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.ADD,t1,t2))])
	| S.MINUS e ->
		let (t1,code1) = trans_e e in
		let t2 = "t" ^ string_of_int (next var) in
			(t2,code1 @ [(T.dummy_label,T.ASSIGNU (t2,T.MINUS,t1))])
	| S.SUB (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.SUB,t1,t2))])
	| S.MUL (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.MUL,t1,t2))])
	| S.DIV (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.DIV,t1,t2))])
	| S.NOT e ->
		let (t1,code1) = trans_e e in
		let t2 = "t" ^ string_of_int (next var) in
			(t2,code1 @ [(T.dummy_label,T.ASSIGNU (t2,T.NOT,t1))])
	| S.LT (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.LT,t1,t2))])
	| S.LE (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.LE,t1,t2))])
	| S.GT (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.GT,t1,t2))])
	| S.GE (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.GE,t1,t2))])
	| S.EQ (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.EQ,t1,t2))])
	| S.AND (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.AND,t1,t2))])
	| S.OR (e1,e2) ->
		let (t1,code1) = trans_e e1 in
		let (t2,code2) = trans_e e2 in
			let t3 = "t" ^ string_of_int (next var) in
				(t3,code1 @ code2 @ [(T.dummy_label,T.ASSIGNV (t3,T.OR,t1,t2))])

let rec trans_s : S.stmt -> T.linstr list
=fun s ->
	match s with
	| S.ASSIGN (lv,e) ->
	begin
		match lv with
		| S.ID x ->
			let (t1,code1) = trans_e e in 
				code1 @ [(T.dummy_label,T.COPY (x,t1))]
		|S.ARR (x,e1) ->
			let (t1,code1) = trans_e e1 in
			let (t2,code2) = trans_e e in
				code1 @ code2 @ [(T.dummy_label,T.STORE ((x,t1),t2))]
	end
	| S.READ x ->
		[(T.dummy_label,T.READ x)]
	| S.PRINT e ->
		let (t1,code1) = trans_e e in
			code1 @ [(T.dummy_label,T.WRITE t1)]
	| S.IF (e,stmt1,stmt2) ->
		let (t1,code1) = trans_e e in
		let codet = trans_s stmt1 in
		let codef = trans_s stmt2 in
		let lt = next label in
		let lf = next label in
		let lx = next label in
			code1 @
			[(T.dummy_label,T.CJUMP (t1,lt))] @
			[(T.dummy_label,T.UJUMP lf)] @
			[(lt,T.SKIP)] @
			codet @
			[(T.dummy_label,T.UJUMP lx)] @
			[(lf,T.SKIP)] @
			codef @
			[(T.dummy_label,T.UJUMP lx)] @
			[(lx,T.SKIP)]
	| S.WHILE (e,stmt) ->
		let (t1,code1) = trans_e e in
		let codeb = trans_s stmt in
		let le = next label in
		let lx = next label in
			[(le, T.SKIP)] @
			code1 @
			[(T.dummy_label,T.CJUMPF (t1,lx))] @
			codeb @
			[(T.dummy_label,T.UJUMP le)] @
			[(lx,T.SKIP)]
	| S.DOWHILE (stmt,e) ->
		let codeb = trans_s stmt in
		let (t1,code1) = trans_e e in
		let le = next label in
			[(le, T.SKIP)] @
			codeb @
			code1 @
			[(T.dummy_label,T.CJUMP (t1,le))]
	| S.BLOCK block ->
		trans_b block

let rec trans_ss : S.stmts -> T.linstr list
=fun ss ->
	match ss with
	| [] -> []
	| s1::s2 ->
		(trans_s s1) @ (trans_ss s2)

let trans_d : S.decl -> T.linstr
=fun d ->
	match d with
	| (typ,x) ->
		match typ with
		| S.TINT ->
			(T.dummy_label,T.COPYC (x,0))
		| S.TARR n ->
			(T.dummy_label,T.ALLOC (x,n))

let rec trans_ds : S.decls -> T.linstr list
=fun ds ->
	match ds with
	| [] -> []
	| d1::d2 ->
		(trans_d d1) :: (trans_ds d2)

let trans_b : S.block -> T.linstr list
=fun b ->
	match b with
	| (decls,stmts) ->
		(trans_ds decls) @ (trans_ss stmts)

let translate : S.program -> T.program
=fun s ->
	match s with
	| block ->
		trans_b block @ [(T.dummy_label,T.HALT)]