CLIENT=react
PACKAGES=ocaml-vdom,jsont.jsoo

.PHONY: all client server irmin react

all: server $(CLIENT)
	@

server:
	jbuilder server.install

client:
	jbuilder client.install
	jbuilder client/client.js
	cp _build/default/client/client.js docs/client.js
	browse docs/index.html

irmin:
	jbuilder irmin.install
	jbuilder irmin/irmin.js
	cp _build/default/irmin/irmin.js docs/client.js
	browse docs/index.html

react:
	jbuilder react.install
	jbuilder react/react.js
	cp _build/default/react/react.js docs/client.js
	browse docs/index.html


clean:
	rm -rf _build
	rm -f docs/client.js

#client/client.byte: client/client.ml
#	jbuilder client.install
#	ocamlfind ocamlc -package ${PACKAGES} -no-check-prims \
#	  -linkpkg -o client/client.byte client/client.ml
