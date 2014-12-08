
all: paper

paper: *.tex *.bib *.cls
#	pdflatex paper.tex
#	bibtex paper
	pdflatex -shell-escape paper.tex
	pdflatex -shell-escape paper.tex
	cp paper.pdf ~/public_html/

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.pyg

