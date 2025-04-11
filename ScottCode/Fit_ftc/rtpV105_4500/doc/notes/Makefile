
all: rtpnotes.pdf rtpnotes.html

xdvi: rtpnotes.dvi
	xdvi rtpnotes

rtpnotes.dvi: rtpnotes.tex
	latex rtpnotes.tex

rtpnotes.html: rtpnotes.tex
	tth -e2 -a -i < rtpnotes.tex > rtpnotes.html

rtpnotes.pdf: rtpnotes.tex
	pdflatex rtpnotes.tex

clean:
	rm *.log *.aux *.dvi 2> /dev/null || true

