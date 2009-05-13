DOC=proposal

all: ${DOC}.pdf
	open $<

${DOC}.pdf: \
	intro.tex \
	abstract.tex \
	framework.tex \
	motivation.tex \
	goal.tex \
	planning.tex \
	extras.tex \
	related-work.tex \
	${DOC}.tex

%.tex: %.lhs
	lhs2Tex -o $@ $<

%.pdf: %.tex %.bib
	latexmk -pdf $<

#${DOC}.bbl: ${DOC}.bib
#	bibtex ${DOC}

.PHONY: clean

clean: .
	rm -f \
	*.aux \
	*.bbl \
	*.blg \
	*.dvi \
	*.fdb_latexmk \
	*.log \
	*.pdf \
	*.ptb \
	*.tex

