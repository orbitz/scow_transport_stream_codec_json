CAMLP4=

OCAMLDEP_OPTS=-package core,scow,scow_transport_stream,json-wheel
OCAMLC_OPTS=$(OCAMLDEP_OPTS) -w '@f@p@u@s@40'
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=scow_transport_stream_codec_json.ml
NON_LIB_MODULES=

BYTE_TARGETS=scow_transport_stream_codec_json.cma
NATIVE_TARGETS=scow_transport_stream_codec_json.cmxa

.PHONY: all test install

all: native-code byte-code

install: all
	ocamlfind install scow_transport_stream_codec_json \
	META \
	$(BYTE_TARGETS) \
	$(NATIVE_TARGETS) \
	scow_transport_stream_codec_json.a \
	*.cmx \
	*.cmi \
	*.mli

test:

# Dependencies
include ../Ocamlrules.mk.in
