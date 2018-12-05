NAME=thesis

.PHONY: $(NAME).pdf all clean cleanall

TEX:=latexmk
TEXOPTIONS := -pdf -pdflatex="pdflatex -interaction=nonstop" -f

all: $(NAME).pdf

FRONTFILES := $(shell find frontmatter -name '*.Rnw')
CHFILES := $(shell find chapters -name '*.Rnw')
ENDFILES := $(shell find endmatter -name '*.Rnw')

FILES = $(FRONTFILES) $(CHFILES) $(ENDFILES)

# convert *.Rnw -> *.tex
$(NAME).tex: $(NAME).Rnw $(FILES)
	@echo " * knitting $@"
	@R --slave -e "library(knitr);knit(input='$<', output='$@')" > /dev/null

# create PDF
$(NAME).pdf: $(NAME).tex
	@echo " * binding thesis"
	@$(TEX) $(TEXOPTIONS) $<

clean:
	@-$(TEX) -pdf -c $(NAME).tex
	@-rm $(NAME).tex

allrefs.bib:
	ln -s ~/Dropbox/PhD/readings/reflist.bib allrefs.bib

reflist.bib: $(NAME).tex allrefs.bib
	@cp allrefs.bib $@
	@$(TEX) $(TEXOPTIONS)
	@echo " * creating reflist.bib"
	@bibexport -o $@ $(NAME).aux
	@rm reflist.bib-save*
