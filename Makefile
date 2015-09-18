.PHONY: all clean check doc

OCAMLC   = ocamlc
OCAMLOPT = ocamlopt
WARN     = -w A-4-33-40-41-42-43-34-44 -strict-sequence
SRC      = bytes_ext.mli bytes_ext.ml crypto.mli crypto.ml misc.ml frame.mli frame.ml handshake.ml app.mli app.ml server.ml
TEST     = bytes_ext.mli bytes_ext.ml crypto.mli crypto.ml misc.ml frame.mli frame.ml handshake.ml t/test_helper.ml t/bytes_ext_test.ml t/crypto_test.ml t/frame_test.ml t/handshake_test.ml t/test.ml
DOCDIR   = doc

all: server.byte

server.byte: $(SRC)
	ocamlfind $(OCAMLC) $(WARN) -o $@ -g \
		-thread unix.cma threads.cma \
		$^

server.native: $(SRC)
	ocamlfind $(OCAMLOPT) $(WARN) -o $@ -g \
		-thread unix.cmxa threads.cmxa \
		$^

test.byte: $(TEST)
	ocamlfind $(OCAMLC) $(WARN) -o $@ -g \
		-I t \
		-thread unix.cma threads.cma \
		$^

check: test.byte
	./test.byte

clean:
	rm -f *.byte *.native
	rm -f *.o *.cmx *.cmo *.cmt *.cmti *.cmi
	rm -f t/*.o t/*.cmx t/*.cmo t/*.cmt t/*.cmti t/*.cmi

doc: server.byte
	mkdir -p $(DOCDIR)
	ocamldoc frame.mli app.mli -html -d $(DOCDIR)
