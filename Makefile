DOC=Thesis

all: ${DOC}.pdf thesis.fmt haskell.fmt higherorder.fmt
	open *.pdf

${DOC}.pdf: \
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
	*.out \
	*.hers \
	*.toc

new: clean all

#	Abstract.tex \
#	Motivation.tex \
#	Overview.tex \
#	Introduction.tex \
#	Fixpoints.tex \
#	Morphisms.tex \
#	Example.tex \
#	Heap.tex \
#	Storage.tex \
#  HigherOrder.tex \
#  FingerTree.tex \
#	RelatedWork.tex \
#	Conclusion.tex \
#	FutureWork.tex \

