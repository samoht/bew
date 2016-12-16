PACKAGES=ocaml-vdom,jsont.jsoo

.PHONY: all client server

all: server client
	@

server:
	jbuilder server.install

client:
	jbuilder client.install
	jbuilder client/client.js
	browse docs/index.html

clean:
	rm -rf _build
	rm -f client/client.byte

#client/client.byte: client/client.ml
#	jbuilder client.install
#	ocamlfind ocamlc -package ${PACKAGES} -no-check-prims \
#	  -linkpkg -o client/client.byte client/client.ml
