MODULES=user restaurant groups
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind -package re2

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
