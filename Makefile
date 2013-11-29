CC=ocamlopt
CCDOC=ocamldoc
SRC=GXL.ml gxlDocument.ml
OBJ=GXL.cmx gxlDocument.cmx
F=-I `ocamlfind query xml-light`
DDIR=doc
DF=-html -d $(DDIR)

build: gxl-light.cmxa

install:
	ocamlfind install ocaml-gxl-light gxl-light.cmxa META gxlDocument.cmi GXL.cmi gxl-light.a

gxl-light.cmxa: o
	$(CC) $(F) -a $(OBJ) -o $@

o: $(SRC)
	$(CC) $(F) -c $(SRC)

doc: gxl-light.cmxa
	mkdir -p $(DDIR)
	$(CCDOC) $(F) $(DF) $(SRC)

clean:
	rm -f *.cm* *.o *.a

test:
