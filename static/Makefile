
TRANSLATE = translate.cmo
RUN = run.cmo

.PHONY : clean run test
.SILENT: clean run test

$(TRANSLATE): translate.ml Interpreter.ml Parser.ml AbstractSyntaxPrinter.ml 
	ocamlc -c -I +camlp4 -pp camlp4of.opt translate.ml

$(RUN): run.ml Interpreter.ml Parser.ml AbstractSyntaxPrinter.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt run.ml

translate: $(TRANSLATE)
	camlp4 $(TRANSLATE) < test/source

run: $(RUN)
	camlp4 $(RUN) < test/source

clean:
	rm *.cmo *.cmi
