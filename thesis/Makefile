DOC=Thesis

all: ${DOC}.pdf

${DOC}.pdf: \
	abstract.tex \
	Morphisms.tex \
	Example.tex \
	Storage.tex \
	${DOC}.tex

%.tex: %.lhs
	lhs2TeX -o $@ $<

%.pdf: %.tex
	latexmk -pdf $<

#${DOC}.bbl: ${DOC}.bib
#	bibtex ${DOC}

.PHONY: clean

clean: .
	rm -f \
	*.aux \
	*.aux.bak \
	*.bbl \
	*.blg \
	*.dvi \
	*.fdb_latexmk \
	*.log \
	*.pdf \
	*.ptb \
	*.tex \
	*.toc

