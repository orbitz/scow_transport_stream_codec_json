CAMLP4=

OCAMLDEP_OPTS=-package core,scow,scow_transport_stream,json-wheel
OCAMLC_OPTS=$(OCAMLDEP_OPTS) -w '@f@p@u@s@40'
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=scow_transport_stream_codec_json.ml
NON_LIB_MODULES=

BYTE_TARGETS=scow_transport_stream_codec_json.cma
NATIVE_TARGETS=scow_transport_stream_codec_json.cmxa

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
