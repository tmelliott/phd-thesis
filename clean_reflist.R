x <- readLines('reflist.bib')
w <- which(grepl('biblatex-control', x))
if (length(w) == 1) {
    x <- x[-c(w, w+1)]
    writeLines(x, 'reflist.bib')
}
