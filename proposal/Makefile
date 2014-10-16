
PDFS=proposal.pdf deckBuilding.pdf Proposal.pdf

all: $(PDFS)

proposal.pdf: proposal.tex
	pdflatex proposal.tex
	pdflatex proposal.tex

deckBuilding.pdf: deckBuilding.tex
	pdflatex deckBuilding.tex
	pdflatex deckBuilding.tex

Proposal.pdf: proposal.pdf deckBuilding.pdf
	pdftk proposal.pdf deckBuilding.pdf output Proposal.pdf

clean:
	rm -f *.aux *.log $(PDFS)

