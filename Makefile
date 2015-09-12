.PHONY: all clean check

OCAMLC   = ocamlc
OCAMLOPT = ocamlopt
WARN     = -w A-4-33-40-41-42-43-34-44 -strict-sequence
SRC      = misc.ml frame.mli frame.ml handshake.ml echo.ml server.ml
TEST     = misc.ml frame.mli frame.ml handshake.ml echo.ml t/test_helper.ml t/frame_test.ml t/websocket_test.ml t/test.ml

all: server.byte

server.byte: $(SRC)
	ocamlfind $(OCAMLC) $(WARN) -o $@ -g \
		-thread unix.cma threads.cma \
		-package sha sha.cma \
		-package base64 base64.cma \
		$^

server.native: $(SRC)
	ocamlfind $(OCAMLOPT) $(WARN) -o $@ -g \
		-thread unix.cmxa threads.cmxa \
		-package sha sha.cmxa \
		-package base64 base64.cmxa \
		$^

test.byte: $(TEST)
	ocamlfind $(OCAMLC) $(WARN) -o $@ -g \
		-I t \
		-thread unix.cma threads.cma \
		-package sha sha.cma \
		-package base64 base64.cma \
		$^

check: test.byte
	./test.byte

clean:
	rm -f *.byte *.native
	rm -f *.o *.cmx *.cmo *.cmt *.cmti *.cmi
