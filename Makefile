NAME=thesis

.PHONY: $(NAME).pdf all clean cleanall

TEX:=latexmk
TEXOPTIONS := -pdf -f

all: $(NAME).pdf

FRONTFILES := $(shell find frontmatter -name '*.Rnw')
CHFILES := $(shell find chapters -name '*.Rnw')
ENDFILES := $(shell find endmatter -name '*.Rnw')
INCLUDEFILES := $(shell find include -name '*.tex')

FILES = $(FRONTFILES) $(CHFILES) $(ENDFILES) $(INCLUDEFILES)

# convert *.Rnw -> *.tex
$(NAME).tex: $(NAME).Rnw $(FILES)
	@echo " * knitting $@"
	@R --slave -e "library(knitr);knit(input='$<', output='$@')" > /dev/null

# create PDF
$(NAME).pdf: $(NAME).tex
	@echo " * binding thesis"
	@$(TEX) $(TEXOPTIONS) $<

clean: $(NAME).tex
	@-$(TEX) -pdf -c -f $(NAME).tex
	@-rm -f $(NAME).tex
	@-rm -f *.acr *.acn *.alg *.bbl *-blx.bib *.glg *.glo *.gls *.ist *.run.xml

allrefs.bib:
	ln -s ~/Dropbox/PhD/readings/reflist.bib allrefs.bib

reflist.bib: $(NAME).tex allrefs.bib
	@cp allrefs.bib $@
	@$(TEX) $(TEXOPTIONS)
	@echo " * creating reflist.bib"
	@bibexport -o $@ $(NAME).aux
	@rm reflist.bib-save*
	@# remove pesky @{bibtex-control}
	@R --slave -f clean_reflist.R
