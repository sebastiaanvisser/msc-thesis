all: proposal.pdf
	open $<

proposal.pdf: goal.tex motivation.tex related-work.tex proposal.tex

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex %.bib
	latexmk -pdf $<

#pre-proposal.bbl: pre-proposal.bib
#	bibtex pre-proposal

.PHONY: clean

clean: .
	rm -f *.aux *.bbl *.blg *.dvi *.fdb_latexmk *.log *.pdf *.ptb *.tex

