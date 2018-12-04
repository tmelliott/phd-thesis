NAME=thesis

.PHONY: $(NAME).pdf all clean cleanall

TEX:=latexmk
TEXOPTIONS := -pdf -pdflatex="pdflatex -interaction=nonstop" -use-make

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
