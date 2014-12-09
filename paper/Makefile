
all: paper

tex=pdflatex -shell-escape

paper: *.tex *.bib *.cls
	$(tex) paper.tex
	bibtex paper
	$(tex) paper.tex
	$(tex) paper.tex
	(cd ../grammar; make)
	pdftk paper.pdf ../grammar/grammar.pdf cat output paper2.pdf
	mv paper2.pdf paper.pdf

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.pyg
