MODULES=db main dbquery
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
APP=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

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

zip:
	zip app_state.zip *.ml* *.json _tags Makefile




