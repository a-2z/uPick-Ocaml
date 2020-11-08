MODULES=script user restaurant groups test app_state db main 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
SCRIPT=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

script:
	$(OCAMLBUILD) $(SCRIPT) && ./$(SCRIPT)

zip:
	zip app_state.zip *.ml* *.json _tags Makefile




