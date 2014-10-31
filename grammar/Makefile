
PDFS=grammar.pdf

all: $(PDFS)

grammar.pdf: deckbuild-grammar.tex
	pdflatex deckbuild-grammar.tex
	pdflatex deckbuild-grammar.tex
	mv deckbuild-grammar.pdf grammar.pdf

clean:
	rm -f *.aux *.log $(PDFS)

