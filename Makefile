MODULES=db dbquery main 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST= ./src/test.byte 
APP= ./src/main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) $(TEST) && ./$(TEST) 

app:
	$(OCAMLBUILD) $(APP) && ./$(APP)

zip:
	zip project.zip *.ml* .ocamlinit .merlin *.mli* dune dune-project *.txt* *.md* *.json _tags Makefile




