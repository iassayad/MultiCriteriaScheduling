# Magic Makefile - useful makefile for latex formatting
# Copyright (C) 2001  Christophe Macabiau, Antoine Viel
# Modified by Julien Forget 03/03/2003
#
# This makefile is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This makefile is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this makefile; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

include ../Makefile.config
include Makefile.doc

needed=$(DOCUMENT).tex $(EXTRANEEDED)
ifeq ($(BIBLIOGRAPHY),yes)
  needed+=$(DOCUMENT).bbl
else
ifeq ($(TABLEOFCONTENT),yes)
  needed+=$(DOCUMENT).toc
endif
endif

.PHONY : config help clean cleanall updateconfig
.PRECIOUS : %.eps

%.aux : %.tex
	$(LATEX) $<

%.bbl : %.aux $(BIBLIOGRAPHYFILE)
	$(BIBTEX) $(basename $<)
	$(LATEX) $(basename $<)

%.eps : %.fig
	$(FIG2EPS) $< >$@

%.toc : %.tex
	$(LATEX) $<

%.dvi : %.tex

%.dvi : %.tex $(needed)
	$(LATEX) $<

%.ps : %.dvi
	$(DVIPS) -o $@ $<

%.pdf : %.ps
	$(PS2PDF) $<

dvi : $(DOCUMENT).dvi

pdf : $(DOCUMENT).pdf

all : ps html

ps : $(DOCUMENT).ps

html: $(DOCUMENT).dvi
	$(LATEX2HTML) -mkdir -dir html $(DOCUMENT)
	cd html;$(RM) index.html;cd ..

htmlnosplit: $(DOCUMENT).tex
	$(LATEX2HTML) -mkdir -dir htmlnosplit -split 0 -show_section_numbers -no_navigation $<
	cd htmlnosplit;$(RM) index.html;cd ..

vdvi : $(DOCUMENT).dvi
	$(XDVI) $(DOCUMENT).dvi &

vps : $(DOCUMENT).ps
	$(GV) $(DOCUMENT).ps

config :
	$(MORE) Makefile.doc

installtgz : html
	$(RM) -r $(DOCUMENT)
	$(MV) html $(DOCUMENT) && $(GTAR) cvfz $(DOCUMENT)_html.tgz $(DOCUMENT) && $(MV) $(DOCUMENT)_html.tgz $(OUTPUT_TGZ)

# For manual only
fullinstall : installtgz htmlnosplit
	$(HTML2TXT) -nobs htmlnosplit/$(DOCUMENT).html > htmlnosplit/$(DOCUMENT).txt && $(MV) htmlnosplit/$(DOCUMENT).txt $(OUTPUT_TXT)


help :
	$(ECHO) "make dvi         : produce the dvi file"
	$(ECHO) "make ps          : produce the ps file"
	$(ECHO) "make pdf         : produce the pdf file"
	$(ECHO) "make html        : produce the html file"
	$(ECHO) "make htmlnosplit : produce the no splitted html file"
	$(ECHO) "make vdvi        : view the dvi file"
	$(ECHO) "make vps         : view the ps file"
	$(ECHO) "make config      : view the configuration"
	$(ECHO) "make installtgz      : produce the tgz file of the html file and puts it into the output dir"
	$(ECHO) "make fullinstall      : performs installtgz, plus adds a txt version to the output dir"
	$(ECHO) "make doc=x doc       : x becomes the current file"
	$(ECHO) "make biblio=x biblio : files containing the bibliographic database"
	$(ECHO) "make extra=x extra   : sets files needed by document (eps,...)"
	$(ECHO) "make extadd=x extadd : add files needed by document (eps,...)"
	$(ECHO) "make toc      : generate table of document"
	$(ECHO) "make notoc    : no generate table of document"
	$(ECHO) "make bib      : generate bibliography"
	$(ECHO) "make nobib    : no generate bibliography"
	$(ECHO) "make clean    : clean temporary files of the current file"
	$(ECHO) "make cleanall : clean all the temporary files"

clean :
	$(RM) $(DOCUMENT).aux $(DOCUMENT).log
	$(RM) $(DOCUMENT).toc $(DOCUMENT).bbl $(DOCUMENT).blg
	$(RM) $(DOCUMENT).lof $(DOCUMENT).haux $(DOCUMENT).image.tex
	$(RM) *~ *.*~ $(DOCUMENT).*#
	$(RM) $(DOCUMENT).dvi $(DOCUMENT).ps $(DOCUMENT).html $(DOCUMENT).pdf
	$(RM) -r html htmlnosplit

cleanall :
	$(RM) *.aux *.log *.toc
	$(RM) *.bbl *.blg
	$(RM) *.lof *.haux *.image.tex *.eps
	$(RM) *~ *.*~ *#
	$(RM) *.dvi *.ps *.html *.pdf
	$(RM) -r html htmlnosplit

updateconfig :
	$(ECHO) DOCUMENT = $(DOCUMENT) > Makefile.doc
	$(ECHO) TABLEOFCONTENT = $(TABLEOFCONTENT) >> Makefile.doc
	$(ECHO) BIBLIOGRAPHY = $(BIBLIOGRAPHY) >> Makefile.doc
	$(ECHO) BIBLIOGRAPHYFILE = $(BIBLIOGRAPHYFILE) >> Makefile.doc
	$(ECHO) EXTRANEEDED = $(EXTRANEEDED) >>Makefile.doc
	$(MORE) Makefile.doc

ifneq (,$(findstring doc,$(MAKECMDGOALS)))
DOCUMENT=$(doc)
endif
ifneq (,$(findstring biblio,$(MAKECMDGOALS)))
BIBLIOGRAPHYFILE=$(biblio)
endif
ifneq (,$(findstring toc,$(MAKECMDGOALS)))
TABLEOFCONTENT=yes
endif
ifneq (,$(findstring notoc,$(MAKECMDGOALS)))
TABLEOFCONTENT=no
endif
ifneq (,$(findstring bib,$(MAKECMDGOALS)))
BIBLIOGRAPHY=yes
endif
ifneq (,$(findstring nobib,$(MAKECMDGOALS)))
BIBLIOGRAPHY=no
endif
ifneq (,$(findstring extra,$(MAKECMDGOALS)))
EXTRANEEDED=$(extra)
endif
ifneq (,$(findstring extadd,$(MAKECMDGOALS)))
EXTRANEEDED+=$(extadd)
endif

doc biblio toc notoc bib nobib extra extadd : updateconfig
