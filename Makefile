NAME=thesis

.PHONY: $(NAME).pdf all clean cleanall

TEX:=latexmk
TEXOPTIONS := -pdf -f

default: all
all: $(NAME).pdf

FRONTFILES := $(shell find frontmatter -name '*.Rnw')
FRONTTEX = $(patsubst %.Rnw, %.tex, $(FRONTFILES))
CHFILES := $(shell find chapters -name '*.Rnw')
CHTEX = $(patsubst %.Rnw, %.tex, $(CHFILES))
ENDFILES := $(shell find endmatter -name '*.Rnw')
ENDTEX = $(patsubst %.Rnw, %.tex, $(ENDFILES))
INCLUDEFILES := $(shell find include -name '*.tex')

FILES = $(FRONTTEX) $(CHTEX) $(ENDTEX) $(INCLUDEFILES)

# turn Rnw -> tex

test:
	@echo $(dirname chapters/chapter01/00_main.Rnw)

%.tex: %.Rnw
	@echo " * knitting $@"
	@sed -i'.original' -e '/set_parent/d' $<
	@sed -i'.original' -e "s/\\Sexpr{knit_child('\(.*\).Rnw')}/\\input{DIRNAME\/\1.tex}/g" $<
	@rm $<.original
	@R --slave -e "library(knitr); knit(input='$<', output='$@')" > /dev/null
	@R --slave -e "f <- commandArgs(TRUE)[1]; x <- gsub('DIRNAME', dirname(f), readLines(f)); writeLines(x, f)" --args $@

# $(NAME).tex: $(NAME).Rnw
# 	@echo " * knitting $@"
# 	@sed -i "s/\\Sexpr{knit_child('\(.*\).Rnw')}/\\input{\1.tex}/g" $<
# 	@R --slave -e "library(knitr);knit(input='$<', output='$@')" > /dev/null

# create PDF
$(NAME).pdf: $(NAME).tex $(FILES)
	@echo " * binding thesis"
	@$(TEX) $(TEXOPTIONS) $<

clean: #$(NAME).tex
	@-$(TEX) -pdf -c -f $(NAME).tex
	@-rm -f $(NAME).tex
	@-rm -f *.acr *.acn *.alg *.bbl *-blx.bib *.glg *.glo *.gls *.ist *.run.xml
	@-rm -f $(FRONTTEX) $(CHTEX) $(ENDTEX)

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
