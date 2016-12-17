CLIENT=irmin
PACKAGES=ocaml-vdom,jsont.jsoo

.PHONY: all client server

all: server client
	@

server:
	jbuilder server.install

client:
	jbuilder ${CLIENT}.install
	jbuilder ${CLIENT}/${CLIENT}.js
	cp _build/default/${CLIENT}/${CLIENT}.js docs/client.js
	browse docs/index.html

clean:
	rm -rf _build
	rm -f docs/client.js

#client/client.byte: client/client.ml
#	jbuilder client.install
#	ocamlfind ocamlc -package ${PACKAGES} -no-check-prims \
#	  -linkpkg -o client/client.byte client/client.ml
