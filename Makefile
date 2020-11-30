MODULES=lib/db lib/dbquery lib/main lib/search test/unit_test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test/unit_test.byte 
APP=lib/main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) $(TEST) && ./$(TEST) 

# docs:
	
app:
	$(OCAMLBUILD) $(APP) && ./$(APP)

zip:
	zip project.zip *.ml* .ocamlinit .merlin *.mli* dune dune-project *.txt* *.md* *.json _tags Makefile




