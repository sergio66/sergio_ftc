
all: rtpspec.pdf rtpspec.html

xdvi: rtpspec.dvi
	xdvi rtpspec

rtpspec.dvi: rtpspec.tex
	latex rtpspec.tex

rtpspec.html: rtpspec.tex
	tth -e2 -a -i < rtpspec.tex > rtpspec.html

rtpspec.pdf: rtpspec.tex
	pdflatex rtpspec.tex

clean:
	rm *.log *.aux *.dvi 2> /dev/null || true

