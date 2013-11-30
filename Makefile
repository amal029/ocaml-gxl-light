CC=ocamlc
CCOPT=ocamlopt
CCDOC=ocamldoc
SRC=GXL.ml gxlDocument.ml
OBJ=GXL.cmx gxlDocument.cmx
OBJC=GXL.cmo gxlDocument.cmo
F=-I `ocamlfind query xml-light`
DDIR=doc
DF=-html -d $(DDIR)

build: gxl-light.cmxa gxl-light.cma

install:
	ocamlfind install ocaml-gxl-light gxl-light.cmxa gxl-light.cma META gxlDocument.cmi GXL.cmi gxl-light.a

gxl-light.cmxa: opt
	$(CCOPT) $(F) -a $(OBJ) -o $@

gxl-light.cma: o
	$(CC) $(F) -a $(OBJC) -o $@

opt: $(SRC)
	$(CCOPT) $(F) -c $(SRC)

o: $(SRC)
	$(CC) $(F) -c $(SRC)

doc: gxl-light.cma
	mkdir -p $(DDIR)
	$(CCDOC) $(F) $(DF) $(SRC)

clean:
	rm -f *.cm* *.o *.a testx testo

clean-doc:
	rm -rf $(DDIR)


test: testx testo

testx:
	ocamlfind $(CCOPT) -o $@ -linkpkg -package xml-light gxl-light.cmxa test.ml

testo:
	ocamlfind $(CC) -o $@ -linkpkg -package xml-light gxl-light.cma test.ml
