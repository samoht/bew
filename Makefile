PACKAGES=ocaml-vdom,jsont.jsoo

.PHONY: all client server

all: server client
	@

server:
	jbuilder server.install

client: client/client.byte
	jbuilder client.install
	jbuilder client/client.js
	browse index.html

clean:
	rm -rf _build
	rm -f client/client.byte

client/client.byte: client/client.ml
	ocamlfind ocamlc -package ${PACKAGES} -no-check-prims \
	  -linkpkg -o client/client.byte client/client.ml
