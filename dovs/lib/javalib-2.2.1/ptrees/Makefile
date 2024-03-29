# This file is part of JavaLib
# Copyright (c)2008, 2009 Laurent Hubert (CNRS)
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA

-include ../Makefile.config

MODULES=ptmap.ml ptset.ml
MODULE_INTERFACES=ptmap.mli ptset.mli

.PHONY : all clean cleanall cleandoc doc install remove
.DELETE_ON_ERROR:.depend

all:ptrees.cma ptrees.cmxa

install: META ptrees.cma ptrees.cmxa $(MODULE_INTERFACES) $(MODULE_INTERFACES:.mli=.cmi)
	$(INSTALL) ptrees $^ ptrees.a
remove:
	$(FINDER) remove ptrees
ptrees.cma:$(MODULES:.ml=.cmo)
	$(OCAMLC) -o $@ -a $^

ptrees.cmxa:$(MODULES:.ml=.cmx)
	$(OCAMLOPT) -o $@ -a $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cma .cmxa

.ml.cmo:.depend Makefile
	$(OCAMLC) -c $< 
.mli.cmi:.depend Makefile
	$(OCAMLC) -c $<
.ml.cmx:.depend Makefile
	$(OCAMLOPT) -c $< 

# Clean up
clean:
	$(RM) *.o *.a *.cm[iox] *~ *.annot .depend
cleanall:clean
	$(RM) *.cma *.cmxa

# Dependencies
.depend: ../Makefile.config $(MODULE_INTERFACES) $(MODULES)
	$(OCAMLDEP) $(MODULE_INTERFACES) $(MODULES) > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
-include .depend
endif
endif
