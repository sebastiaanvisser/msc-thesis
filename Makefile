all: paper.pdf
	open $<

paper.pdf: cont.tex more.tex goal.tex paper.tex pers.tex seri.tex stor.tex intro.tex draft.sty

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex %.bib
	latexmk -pdf $<

#paper.bbl: paper.bib
#	bibtex paper

.PHONY: clean

clean: .
	rm -f *.aux *.bbl *.blg *.dvi *.fdb_latexmk *.log *.pdf *.ptb *.tex

