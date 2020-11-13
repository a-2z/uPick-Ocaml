MODULES=db dbquery main script
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
APP=main.byte
SCRIPT=script.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

deps:
	opam install . --deps-only --locked --working-dir

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

app:
	$(OCAMLBUILD) $(APP) && ./$(APP)

script:
	$(OCAMLBUILD) $(SCRIPT) && ./$(SCRIPT)

zip:
	zip app_state.zip *.ml* *.json _tags Makefile




