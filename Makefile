default: thesis.pdf

KNITRFILES := $(wildcard *.Rnw)
FILES := thesis


# convert *.Rnw -> *.tex
$(FILES).tex: $(FILES).Rnw
	@R -e "knitr::knit('$^')"

# create PDF
thesis.pdf: thesis.tex | $(FILES).tex
	@latexmk -pdf $^

clean:
	@latexmk -c thesis.tex
	@rm $(FILES).tex

.PHONY: clean