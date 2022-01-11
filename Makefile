MODULES=ast grammar lexer transform main print
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
RUN=main.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

build: clean
	$(OCAMLBUILD) $(OBJECTS)

all: build

clean:
	ocamlbuild -clean

play: build
	utop

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST)
	./${TEST}

run:
	$(OCAMLBUILD) $(RUN)
