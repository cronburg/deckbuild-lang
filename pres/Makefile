
all: pres.pdf
TEX_FILES := $(wildcard *.tex)
FN=pres

$(FN).pdf: $(TEX_FILES)
	pdflatex -shell-escape $(FN).tex
	pdflatex -shell-escape $(FN).tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc $(FN).pdf

