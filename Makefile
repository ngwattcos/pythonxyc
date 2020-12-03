MODULES=ast grammar lexer transform main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
RUN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

all: build

clean:
	ocamlbuild -clean

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST)
	./test.byte

run:
	$(OCAMLBUILD) $(RUN)