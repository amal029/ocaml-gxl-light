CC=ocamlc
CCDOC=ocamldoc
SRC=GXL.ml gxlDocument.ml
OBJ=GXL.cmo gxlDocument.cmo
F=-I ../xml-light
DDIR=/tmp/GXL
DF=-html -d $(DDIR)

all: gxl-light.cma doc

gxl-light.cma: o
	$(CC) $(F) -a $(OBJ) -o $@

o: $(SRC)
	$(CC) $(F) -c $(SRC)

doc: gxl-light.cma
	mkdir -p $(DDIR)
	cp ../style.css $(DDIR)
	$(CCDOC) $(F) $(DF) $(SRC)

clean:
	rm -f *.cm* 
	rm -rf $(DDIR)

test:

