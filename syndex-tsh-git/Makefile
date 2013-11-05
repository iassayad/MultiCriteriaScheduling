### Operating System
ifeq ($(OSTYPE),)
  SYSTEM=windows
else
  SYSTEM=$(OSTYPE)
endif

SYNDEX_VERSION = $(shell sed -f Makefile.sed_script version.ml)

#include Makefile.config.$(SYSTEM)

include Makefile.config.linux

OCAMLC=$(OCAML_BIN_DIR)/ocamlc -g -dtypes -custom unix.cma
OCAMLOPT=$(OCAML_BIN_DIR)/ocamlopt

#INCLUDES=-I $(CAMLTK_DIR)/camltk -I +threads -I ./camltk
INCLUDES=-I $(CAMLTK_DIR) -I +threads -I ./camltk

COMPFLAGS=$(INCLUDES)
LINKFLAGS=-ccopt "-L $(CAMLTK_DIR)/support/" $(EXTRALINKFLAG)

OCAMLYACC=$(OCAML_BIN_DIR)/ocamlyacc
YACCFLAGS=-v
OCAMLLEX=$(OCAML_BIN_DIR)/ocamllex
OCAMLDEP=$(OCAML_BIN_DIR)/ocamldep
DEPFLAGS=$(INCLUDES)
OCAMLMKTOP=$(OCAML_BIN_DIR)/ocamlmktop
OCAMLDOC=$(OCAML_BIN_DIR)/ocamldoc
DOCFLAGS=-t "SynDEx $(SYNDEX_VERSION) Documentation" -v



COMMON_OBJS= version.cmo symbolic.cmo coord.cmo port.cmo types.cmo \
  application.cmo algorithm.cmo architecture.cmo adequationtypes.cmo\
  parserexpressioninit.cmo lexerexpression.cmo parserexpression.cmo \
  sdx_lexer.cmo sdx_parser.cmo write.cmo \
  transformation.cmo \
  adequation_core.cmo read.cmo genexec.cmo  camltk/progress_box.cmo dogan_adequation.cmo aaatp_adequation_reliability.cmo   fault_tolerance_adequation.cmo latency_adequation.cmo \
  reliability_adequation.cmo tsh_adequation.cmo \
  reliability_adequation_no_routing.cmo  \

#transformation.cmo camltk/progress_box.cmo \

GUI_OBJS=ihmcommon_ctk.cmo dot_ctk.cmo camltk/scroll.cmo camltk/graph.cmo	\
  graph_ctk.cmo xsc_ctk.cmo algorithm_ctk.cmo architecture_ctk.cmo		\
  camltk/ruler.cmo schedule_ctk.cmo ihm_ctk.cmo

BENCHS_OBJS=ihmcommon_ctk.cmo dot_ctk.cmo camltk/scroll.cmo camltk/graph.cmo	\
  graph_ctk.cmo xsc_ctk.cmo algorithm_ctk.cmo architecture_ctk.cmo		\
  camltk/ruler.cmo schedule_ctk.cmo ihm_ctk_benchs.cmo


TUI_OBJS=tui.cmo

SDX_GUI_OBJS=$(COMMON_OBJS) $(GUI_OBJS)

SDX_BENCHS_OBJS=$(COMMON_OBJS) $(BENCHS_OBJS)

SDX_TUI_OBJS=$(COMMON_OBJS) $(TUI_OBJS)

.PHONY: clean all opt tui tuiopt toplevel depend distrib doc benchs

all: syndex.bin
opt: syndex.opt
tui: syndex-tui.bin
benchs: syndex-benchs.bin
tuiopt: syndex-tui.opt

# The parser

sdx_parser.mli sdx_parser.ml: sdx_parser.mly
	$(OCAMLYACC) $(YACCFLAGS) sdx_parser.mly

parserexpression.mli parserexpression.ml: parserexpression.mly
	$(OCAMLYACC) $(YACCFLAGS) parserexpression.mly

partialclean::
	rm -f sdx_parser.ml sdx_parser.mli sdx_parser.output
	rm -f parserexpression.ml parserexpression.mli parserexpression.output

beforedepend:: sdx_parser.mli sdx_parser.ml
beforedepend:: parserexpression.mli parserexpression.ml

# The lexer

sdx_lexer.ml: sdx_lexer.mll
	$(OCAMLLEX) sdx_lexer.mll

lexerexpression.ml: lexerexpression.mll
	$(OCAMLLEX) lexerexpression.mll

partialclean::
	rm -f sdx_lexer.ml
	rm -f lexerexpression.ml

beforedepend:: sdx_lexer.ml
beforedepend:: lexerexpression.ml

#

syndex.bin: $(SDX_GUI_OBJS)
	$(OCAMLC) $(COMPFLAGS) $(LINKFLAGS) -o syndex.bin \
	-I $(CAMLTK_DIR)/lib -I ./camltk \
	unix.cma str.cma labltk.cma $(SDX_GUI_OBJS)

syndex-benchs.bin: $(SDX_BENCHS_OBJS)
	$(OCAMLC) $(COMPFLAGS) $(LINKFLAGS) -o syndex-benchs.bin \
	-I $(CAMLTK_DIR)/lib -I ./camltk \
	unix.cma str.cma labltk.cma $(SDX_BENCHS_OBJS)

partialclean::
	rm -f syndex.bin

syndex.opt: $(SDX_GUI_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LINKFLAGS) -o syndex.opt \
	-I $(CAMLTK_DIR)/wm.cmi -I ./camltk \
	unix.cmxa str.cmxa labltk.cmxa $(SDX_GUI_OBJS:.cmo=.cmx)

partialclean::
	rm -f syndex.opt

syndex-tui.bin: $(SDX_TUI_OBJS)
	$(OCAMLC) $(COMPFLAGS) $(LINKFLAGS) -o syndex-tui.bin \
	-I $(CAMLTK_DIR) -I ./camltk \
	unix.cma str.cma labltk.cma $(SDX_TUI_OBJS)

partialclean::
	rm -f syndex-tui.bin

partialclean::
	rm -f syndex-benchs.bin

syndex-tui.opt: $(SDX_TUI_OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(LINKFLAGS) -o syndex-tui.opt \
	-I $(CAMLTK_DIR)/lib -I ./camltk \
	unix.cmxa str.cmxa labltk.cmxa $(SDX_TUI_OBJS:.cmo=.cmx)

partialclean::
	rm -f syndex-tui.opt

syndex.top: $(SDX_GUI_OBJS)
	$(OCAMLMKTOP) $(LINKFLAGS) -o syndex.top \
	-I $(CAMLTK_DIR)/wm.cmi -I +threads \
	unix.cma str.cma labltk.cma threads.cma $(SDX_GUI_OBJS)

partialclean::
	rm -f syndex.top

toplevel: syndex.top init_toplevel.cmo

install: syndex.bin syndex-tui.bin installproc
	cp syndex.bin $(INSTALL_BIN_DIR)/syndex$(EXECUTABLE_EXT)
	cp syndex-tui.bin $(INSTALL_BIN_DIR)/syndex-tui$(EXECUTABLE_EXT)

installopt: syndex.opt installproc
	cp syndex.opt $(INSTALL_BIN_DIR)/syndex$(EXECUTABLE_EXT)
	#cp syndex-tui.opt $(INSTALL_BIN_DIR)/syndex-tui$(EXECUTABLE_EXT)

installproc:
	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)
	mkdir -p $(INSTALL_BIN_DIR)
	cd $(EXTERNALS_DIR) && cd $(SYSTEM) && cp syndex $(INSTALL_DIR)
	-cd $(EXTERNALS_COMMON_DIR) && cp * $(INSTALL_DIR)
	cd $(INSTALL_DIR) && gzip -d manual_html.tgz
	cd $(INSTALL_DIR) && $(GTAR) -xf manual_html.tar
	cd $(INSTALL_DIR) && rm -f manual_html.tar
	cd $(INSTALL_DIR) && gzip -d tutorial_html.tgz
	cd $(INSTALL_DIR) && $(GTAR) -xf tutorial_html.tar
	cd $(INSTALL_DIR) && rm -f tutorial_html.tar
	if [ "$(SYSTEM)" = "windows" ]; then cd $(INSTALL_DIR) && unix2dos README.txt HELP.txt COPYRIGHT.txt; fi
	cp -r $(LIBS_DIR) $(INSTALL_DIR)
	cp -r $(MACROS_DIR) $(INSTALL_DIR)
	cp -r $(EXAMPLES_DIR) $(INSTALL_DIR)
	rm -r $(INSTALL_DIR)/examples/fault_tolerance
	-find $(INSTALL_DIR) -name CVS -exec rm -rf {} \;
	cd $(INSTALL_DIR) && chmod +x syndex && mv syndex syndex-$(SYNDEX_VERSION)$(SCRIPT_EXT)

distrib:
	mkdir -p $(DISTRIB_DIR)
	cd $(INSTALL_DIR)/.. && \
	$(COMPRESS) syndex-$(SYNDEX_VERSION)-$(SYSTEM)$(COMPRESS_EXT) $(INSTALL_DIR_NAME) && \
	mv syndex-$(SYNDEX_VERSION)-$(SYSTEM)$(COMPRESS_EXT) $(DISTRIB_DIR)

htmldoc: syndex.bin
	$(OCAMLDOC) -html $(DOCFLAGS) -colorize-code $(INCLUDES) -d $(DOC_DIRECTORY) *.mli *.ml camltk/*.mli camltk/*.ml

latexdoc: syndex.bin
	$(OCAMLDOC) -latex $(DOCFLAGS) $(INCLUDES) -d $(DOC_DIRECTORY) *.mli *.ml camltk/*.mli camltk/*.ml

config:
	@echo Syndex Version... $(SYNDEX_VERSION)
	@echo System........... $(SYSTEM)
	@echo ---
	@echo OCaml
	@echo Binaries......... $(OCAML_BIN_DIR)
	@echo Libraries........ $(OCAML_LIB_DIR)
	@echo Camltk........... $(CAMLTK_DIR)
	@echo ---
	@echo Installation..... $(INSTALL_DIR)
	@echo ---
	@echo Distribution..... $(DISTRIB_DIR)
	@echo Archive.......... $(COMPRESS)

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<

depend: beforedepend
	$(OCAMLDEP) $(DEPFLAGS) *.mli *.ml camltk/*.mli camltk/*.ml > .depend

clean: partialclean
	rm -f *.cmi *.cmo *.cmx *.o *.annot *~
	rm -f camltk/*.cmi camltk/*.cmo camltk/*.cmx camltk/*.o camltk/*.annot camltk/*~

include .depend
