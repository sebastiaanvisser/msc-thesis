all: paper.pdf
	open $<

paper.pdf: paper.tex intro.tex

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex
	latexmk -pdf $<

# paper.bbl

#paper.bbl: paper.bib
#	bibtex paper

.PHONY: clean

clean: .
	rm -f *.aux *.bbl *.blg *.dvi *.fdb_latexmk *.log *.pdf *.ptb *.tex

