all: pre-proposal.pdf
	open $<

pre-proposal.pdf: cont.tex more.tex goal.tex pre-proposal.tex pers.tex seri.tex stor.tex intro.tex draft.sty

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex %.bib
	latexmk -pdf $<

#pre-proposal.bbl: pre-proposal.bib
#	bibtex pre-proposal

.PHONY: clean

clean: .
	rm -f *.aux *.bbl *.blg *.dvi *.fdb_latexmk *.log *.pdf *.ptb *.tex

