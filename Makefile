DOC=Thesis

all: ${DOC}.pdf thesis.fmt haskell.fmt higherorder.fmt
	open ${DOC}.pdf

${DOC}.pdf: \
	${DOC}.tex

${DOC}.tex: \
	${DOC}.lhs \
	Titlepage.lhs \
	Abstract.lhs \
	Introduction.lhs \
	Fixpoints.lhs \
	Morphisms.lhs \
	Heap.lhs \
	Storage.lhs \
	HigherOrder.lhs \
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
	Thesis.pdf \
	*.aux \
	*.aux.bak \
	*.bbl \
	*.blg \
	*.dvi \
	*.fdb_latexmk \
	*.log \
	*.ptb \
	*.tex \
	*.out \
	*.hers \
	*.toc

new: clean all

