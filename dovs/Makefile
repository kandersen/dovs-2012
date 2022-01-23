############################################################
## Makefile for the dOvs CamlJoos compiler
## ---------------------------------------------------------
##
## Usage:
##   make [target]
##
## Targets:
##   joos.byte     -- build an ocaml byte code joos compiler (default target)
##   joos.native   -- build an native code joos compiler
##   joos.top      -- build an interactive joos compiler toplevel
##   joos.debug    -- build a joos compiler with debugging symbols
##   joos.profile  -- build a joos compiler with profiling instrumentation
##   htdoc         -- generate documentation (in doc/index.html)
##   libs          -- build and install ocaml library dependencies
##   clean         -- clean up all build related files
##
############################################################

# (just ignore this thing...)
.PHONY: default all \
        joos.byte joos.native joos.top joos.debug joos.profile \
        htdoc clean prepare findlib javalib libs

# Default build target (defined below)
# should probably be either 'joos.native' or 'joos.byte'
joos: joos.byte

# Default target for Eclipse
all: joos

# Non-source directories
# Add directories that do not contain ml source files.
# Fx directories with test cases (will speed up compilation)
EXCL_DIRS := lib,tests,doc

# dOvs modules for generating a toplevel and documentation
# (ie, with source code in src/<module>.ml)
MODS := utils globals error \
       stringMap canonicalName \
       types \
       ast \
       parser lexer \
       weedingast weeding \
       classfileparser classenvironment \
       nameresolvingast nameresolving \
       environment \
       hierarchy \
       disambiguationast disambiguation \
       typecheckingast typechecking \
       constantfolding reachability definiteassignment \
       resourceast resource \
       instruction \
       codegenerationast codegeneration \
       peephole optimization \
       limitsast limits \
       codeemission \
       main \
       typespp astpp

############################################################
## YOU SHOULD NOT NEED TO EDIT ANYTHING BELOW THIS LINE
############################################################

# Directories
SRC_DIR := src
DOC_DIR := doc
LIB_DIR := lib
BLD_DIR := _build

# Special check for the DAIMI installation of ocamlfind
DAIMIFIND := /users/dovs/bin/ocamlfind
OCAMLFIND := $(shell if [ -x $(DAIMIFIND) ]; then echo $(DAIMIFIND); else echo ocamlfind; fi)

# Special check for the DAIMI installation of menhir
DAIMIMENHIR := /users/dovs/bin/menhir
OCAMLMENHIR := $(shell if [ -x $(DAIMIMENHIR) ]; then echo $(DAIMIMENHIR); else echo menhir; fi)

# Locations of needed libraries
STDLIB  := $(shell $(OCAMLFIND) query stdlib  2>/dev/null || echo NO)
JAVALIB := $(shell $(OCAMLFIND) query javalib 2>/dev/null || echo NO)
PTREES  := $(shell $(OCAMLFIND) query ptrees  2>/dev/null || echo NO)
CAMLZIP := $(shell $(OCAMLFIND) query camlzip 2>/dev/null || echo NO)
EXTLIB  := $(shell $(OCAMLFIND) query extlib  2>/dev/null || echo NO)

# Base invokation of ocamlbuild
OCAMLBUILD := ocamlbuild -no-links -use-menhir -menhir "${OCAMLMENHIR} -v" -Xs $(EXCL_DIRS)

# Include, linking and library flags
LIBS   := str,unix,ptrees,zip,extLib,javalib
FLAGS  := -I,$(PTREES),-I,$(JAVALIB),-I,$(CAMLZIP),-I,$(EXTLIB),-annot
CFLAGS := $(FLAGS)
LFLAGS := $(FLAGS),-dllpath,$(PTREES),-dllpath,$(JAVALIB),-dllpath,$(CAMLZIP),-dllpath,$(EXTLIB)

joos.byte: prepare
	@echo "*** Building joos.byte"
	@$(OCAMLBUILD) -cflags $(CFLAGS) -lflags $(LFLAGS) -libs $(LIBS) \
	  $(SRC_DIR)/main.byte
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.byte joos.byte
	@ln -sf joos.byte joos

joos.native: prepare
	@echo "*** Building joos.native"
	@$(OCAMLBUILD) -cflags $(CFLAGS) -lflags $(FLAGS) -libs $(LIBS) \
	  $(SRC_DIR)/main.native
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.native joos.native
	@ln -sf joos.native joos

joos.debug: prepare .ocamldebug
	@echo "*** Building joos.debug"
	@$(OCAMLBUILD) -cflags $(CFLAGS) -lflags $(LFLAGS) -libs $(LIBS) \
	  $(SRC_DIR)/main.d.byte
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.d.byte joos.debug
	@$(MAKE) $(BLD_DIR)/$(SRC_DIR)/printers.cmo

joos.profile: prepare
	@echo "*** Building joos.profile"
	@$(OCAMLBUILD) -cflags $(CFLAGS) -lflags $(FLAGS) -libs $(LIBS) \
	  $(SRC_DIR)/main.p.native
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.p.native joos.profile

joos.top: prepare .ocamlinit
	@echo "*** Building joos.top"
	@echo $(MODS:%=$(SRC_DIR)/%) > $(SRC_DIR)/main.mltop
	@$(OCAMLBUILD) -cflags $(CFLAGS) -lflags $(LFLAGS) -libs $(LIBS) $(SRC_DIR)/main.top
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@rm -f $(SRC_DIR)/main.mltop
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.top joos.top
	@$(MAKE) $(BLD_DIR)/$(SRC_DIR)/printers.cmo

htdoc: joos.byte
	@echo "*** Building $(DOC_DIR)"
	@mkdir -p $(DOC_DIR)
	@ocamldoc -t "API documentation for the dOvs CamlJoos Skeleton Compiler" \
	  -d $(DOC_DIR) -all-params -keep-code \
	  -I $(STDLIB) -I $(CAMLZIP) -I $(EXTLIB) -I $(JAVALIB) -I $(BLD_DIR)/$(SRC_DIR) \
	  -html $(MODS:%=$(BLD_DIR)/$(SRC_DIR)/%.ml) $(SRC_DIR)/canonicalName.mli
	@echo "Open $(DOC_DIR)/index.html to browse the documentation."

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(DOC_DIR)
	rm -f joos joos.byte joos.native joos.top joos.debug joos.profile
	rm -f $(SRC_DIR)/main.mltop
	rm -f .ocamlinit .ocamldebug
	rm -f parser.automaton
	rm -f parser.conflicts

## Some specially generated files
$(BLD_DIR)/$(SRC_DIR)/printers.cmo: $(SRC_DIR)/printers.ml
	@cp $(SRC_DIR)/printers.ml $(BLD_DIR)/$(SRC_DIR)/printers.ml
	@ocamlc -I $(BLD_DIR)/$(SRC_DIR) -c $(BLD_DIR)/$(SRC_DIR)/printers.ml

.ocamlinit:
	@echo "#directory \"$(BLD_DIR)/$(SRC_DIR)\";;" > .ocamlinit
	@echo "#directory \"$(EXTLIB)\";;" >> .ocamlinit
	@echo "#load \"printers.cmo\";;" >> .ocamlinit
	@echo "#install_printer Printers.pp_canonical_name;;" >> .ocamlinit

.ocamldebug:
	@echo "directory $(BLD_DIR)/$(SRC_DIR)" > .ocamldebug
	@echo "load \"canonicalName.d.cmo\"" >> .ocamldebug
	@echo "load_printer \"printers.cmo\"" >> .ocamldebug
	@echo "install_printer Printers.pp_canonical_name" >> .ocamldebug

parser.automaton:
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.automaton parser.automaton


############################################################
## Setup to build and install needed libraries
############################################################

prepare: parser.automaton
ifeq ($(STDLIB), NO)
	@echo "Missing prerequisite: findlib"
	@echo "To install dependencies run: make libs"
	@exit 1
endif
ifeq ($(JAVALIB), NO)
	@echo "Missing prerequisite: javalib"
	@echo "To install dependencies run: make libs"
	@exit 1
endif
ifeq ($(EXTLIB), NO)
	@echo "Missing prerequisite: extlib"
	@echo "To install dependencies run: make libs"
	@exit 1
endif
ifeq ($(PTREES), NO)
	@echo "Missing prerequisite: ptree"
	@echo "To install dependencies run: make libs"
	@exit 1
endif
ifeq ($(CAMLZIP), NO)
	@echo "Missing prerequisite: camlzip"
	@echo "To install dependencies run: make libs"
	@exit 1
endif

# check if 'sudo' is installed, if not assume we have install privileges
IS_SUDO := $(shell sudo -V 2>/dev/null || echo NO)
ifeq ($(IS_SUDO), NO)
  SUDO_MAKE := make
else
  SUDO_MAKE := sudo make
endif

FINDLIB_DIR := findlib-1.2.7
JAVALIB_DIR := javalib-2.2.1

findlib:
	@echo "--------------------------------------------------------------"
ifeq ($(STDLIB), NO)
	@echo "Installing $(FINDLIB_DIR) (you may be asked for a sudo password)"
	@echo "Required cygwin libraries: m4, libncurses-devel"
	@echo "Required debian libraries: m4, libncurses5-dev"
	@echo "... this may take some time"
	@sleep 4
	@rm -rf $(LIB_DIR)/$(FINDLIB_DIR)
	@cd $(LIB_DIR) && tar xzf $(FINDLIB_DIR).tar.gz
	cd $(LIB_DIR)/$(FINDLIB_DIR) && ./configure && $(MAKE) && $(SUDO_MAKE) install
else
	@echo "findlib is already installed."
endif

javalib:
	@echo "--------------------------------------------------------------"
ifeq ($(JAVALIB), NO)
	@echo "Installing $(JAVALIB_DIR) (you may be asked for a sudo password)"
	@echo "Required cygwin libraries: zlib-devel"
	@echo "Required debian libraries: zlib1g-dev"
	@echo "... this may take some time"
	@sleep 4
	@rm -rf $(LIB_DIR)/$(JAVALIB_DIR)
	@cd $(LIB_DIR) && tar xjf $(JAVALIB_DIR).tar.bz2
ifeq ($(EXTLIB), NO)
	@echo "Installing extlib"
	cd $(LIB_DIR)/$(JAVALIB_DIR) && ./configure.sh
	cd $(LIB_DIR)/$(JAVALIB_DIR) && $(MAKE) extlib && $(SUDO_MAKE) installextlib
else
	@echo "extlib is already installed in $(EXTLIB)"
endif
ifeq ($(PTREES), NO)
	@echo "Installing ptrees"
	cd $(LIB_DIR)/$(JAVALIB_DIR) && ./configure.sh
	cd $(LIB_DIR)/$(JAVALIB_DIR) && $(MAKE) ptrees && $(SUDO_MAKE) installptrees
else
	@echo "ptrees is already installed in $(PTREES)"
endif
ifeq ($(CAMLZIP), NO)
	@echo "Installing camlzip"
	cd $(LIB_DIR)/$(JAVALIB_DIR) && ./configure.sh
	cd $(LIB_DIR)/$(JAVALIB_DIR) && $(MAKE) camlzip && $(SUDO_MAKE) installcamlzip
else
	@echo "camlzip is already installed in $(CAMLZIP)"
endif
	cd $(LIB_DIR)/$(JAVALIB_DIR) && ./configure.sh && $(MAKE) && $(SUDO_MAKE) install
else
	@echo "javalib is already installed in $(JAVALIB)"
endif
	@echo "--------------------------------------------------------------"

libs: findlib javalib
	@echo "The dOvs ocaml libraries are installed."
	@echo "To build your compiler run:"
	@echo "  make "
	@echo "To build documentation for your compiler run:"
	@echo "  make htdoc"
	@echo "Happy hacking!"
