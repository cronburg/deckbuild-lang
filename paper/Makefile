
all: paper

paper: *.tex *.bib *.cls
#	pdflatex paper.tex
#	bibtex paper
	bibtex paper
	pdflatex -shell-escape paper.tex
	pdflatex -shell-escape paper.tex
	(cd ../grammar; make)
	pdftk paper.pdf ../grammar/grammar.pdf cat output paper2.pdf
	mv paper2.pdf paper.pdf

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.pyg
