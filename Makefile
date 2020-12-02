MODULES=lib/db lib/dbquery main lib/search testing/unit_test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)

default: build
	utop

build:
	@dune build main.exe

script:
	@dune exec ./script.exe

test: 
	@dune runtest

clean:
	@dune clean
	rm -rf doc.public project.zip

docs: 
	mkdir -p doc.public

doc.public: build
	@dune build @doc

# docs: docs-public docs-private
	
# docs-public: build
# 	mkdir -p doc.public
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.public $(MLIS)
	
app:
	@dune exec ./main.exe

zip:
	zip project.zip *.ml* .ocamlinit .merlin *.mli* dune dune-project *.txt* *.md* *.json _tags Makefile




