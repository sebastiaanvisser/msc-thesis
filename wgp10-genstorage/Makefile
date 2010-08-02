DOC=Paper

all: ${DOC}.pdf
	open *.pdf

submission: ${DOC}.pdf
	cp ${DOC}.pdf wgp25k-visser.pdf
	acroread -toPostScript wgp25k-visser.pdf
	sed -e '/bibliography{/r${DOC}.bbl' -e '/bibliography{/D' ${DOC}.tex > wgp25k-visser.tex
	pdftotext wgp25k-visser.pdf -enc UTF-8 -f 12 -eol unix bibliography.txt
	sed -i -e '0,/References/D' bibliography.txt
	sed -i -e "s/\[/\\`echo -e '\n\r'`[/g" bibliography.txt

${DOC}.tex: \
	*.lhs \
	*.fmt

#	${DOC}.tex

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
	*.toc

