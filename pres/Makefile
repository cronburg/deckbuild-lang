
all: pres.pdf
TEX_FILES := $(wildcard *.tex)
FN=pres

$(FN).pdf: $(TEX_FILES)
	pdflatex $(FN).tex
	pdflatex $(FN).tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc $(FN).pdf

