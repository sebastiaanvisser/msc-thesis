DOC=Thesis

all: ${DOC}.pdf thesis.fmt haskell.fmt higherorder.fmt
	open *.pdf

${DOC}.pdf: \
	${DOC}.tex

${DOC}.tex: \
	${DOC}.lhs \
	Abstract.lhs \
	Motivation.lhs \
	Overview.lhs \
	Introduction.lhs \
	Fixpoints.lhs \
	Morphisms.lhs \
	Heap.lhs \
	Storage.lhs \
	HigherOrder.lhs \
	FingerTree.lhs \
	RelatedWork.lhs \
	Conclusion.lhs \
	FutureWork.lhs
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

