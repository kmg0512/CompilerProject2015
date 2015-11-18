ocamlc -c enum.ml
ocamlc -c pMap.ml
ocamlc -c s.ml
ocamlc -c t.ml
ocamlc -c translator.ml
ocamlc -c ssa.ml
ocamlc -c optimizer.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o run.exe enum.cmo pMap.cmo lexer.cmo parser.cmo s.cmo t.cmo translator.cmo ssa.cmo optimizer.cmo main.cmo
