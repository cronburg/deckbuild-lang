
PDFS=deckBuilding.pdf

all: $(PDFS)

deckBuilding.pdf: deckBuilding.tex
	pdflatex deckBuilding.tex
	pdflatex deckBuilding.tex

clean:
	rm -f *.aux *.log $(PDFS)

