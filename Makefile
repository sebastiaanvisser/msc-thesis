DOC=Thesis

all: ${DOC}.pdf
	open *.pdf

${DOC}.pdf: \
	Abstract.tex \
	Motivation.tex \
	Introduction.tex \
	Fixpoints.tex \
	Morphisms.tex \
	Example.tex \
	Heap.tex \
	Storage.tex \
	RelatedWork.tex \
	FutureWork.tex \
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

