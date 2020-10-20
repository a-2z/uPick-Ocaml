MODULES=user restaurant groups
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
