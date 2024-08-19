SOURCE=pfatt
TARGET=pfatt

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.o

build:
	ocamlfind ocamlopt -linkpkg -package sqlite3 -package unix -package yojson -package ppx_string_interpolation -o ${TARGET} html.ml db.ml ${SOURCE}.ml
