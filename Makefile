.PHONY: all clean

OCAMLC   = ocamlc
OCAMLOPT = ocamlopt
WARN     = -w A-4-33-40-41-42-43-34-44 -strict-sequence

all: server.byte

frame.cmi: frame.mli
	$(OCAMLC) $(WARN) -c $<

frame.cmo: frame.ml frame.cmi
	$(OCAMLC) $(WARN) -c $<

frame.byte: frame.ml frame.cmi
	$(OCAMLC) $(WARN) -o $@ $<

frame.cmx: frame.ml frame.cmi
	$(OCAMLOPT) $(WARN) -c $<

frame.native: frame.ml frame.cmi
	$(OCAMLOPT) $(WARN) -o $@ $<

misc.cmo: misc.ml
	$(OCAMLC) $(WARN) -c -thread unix.cma threads.cma $<

misc.cmx: misc.ml
	$(OCAMLOPT) $(WARN) -c -thread unix.cmxa threads.cmxa $<

server.byte: misc.cmo server.ml
	$(OCAMLC) $(WARN) -o $@ -g -thread unix.cma threads.cma $^

server.native: misc.cmx server.ml
	$(OCAMLOPT) $(WARN) -o $@ -g -thread unix.cmxa threads.cmxa $^

clean:
	rm -f *.byte *.native
	rm -f *.o *.cmx *.cmo *.cmt *.cmti *.cmi
