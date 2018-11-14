.PHONY: all clean

# Menhir can be told to produce a parser that explains what
# it is doing.
ifeq ($(DEBUGPARSING), yes)
  MENHIROPT=-yaccflag --explain -yaccflag --trace
else
  MENHIROPT=-yaccflag --explain
endif

OCAMLBUILD=ocamlbuild -quiet -use-ocamlfind $(MENHIROPT)

all:
	$(OCAMLBUILD) lang.cma

clean:
	@ $(OCAMLBUILD) -clean
	@ rm -f *~
