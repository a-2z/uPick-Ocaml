MODULES=graphqltest
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind 

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)