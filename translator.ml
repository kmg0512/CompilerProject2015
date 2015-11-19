(* TODO: Write a translator below. *)

let translate : S.program -> T.program
=fun s -> raise (Failure "translator: Not implemented")

let trans_e : e -> LabeledInstruction*
=fun e ->
match e with
| NUM n ->
(t, COPYC (t,n))
| LV ID x ->
(t,COPY(t,x))
| LV ARR (x,e) ->
let (t1,code) = trans_e(e)
in (t2, code@COPY(t2,x[t1]))
| ADD (e1,e2) ->
let 