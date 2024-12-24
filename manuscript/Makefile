source := $(wildcard *.tex)
# out := main.pdf
out := $(patsubst %.tex,%.pdf,$(source))
rev := 56a50ce
latest := $(shell git log --format="%h" | head -n 1)
bib := $(wildcard *.bib)
img_dir="../img"

full: manuscript.tex $(bib)
	latexmk -pdf manuscript

blind: manuscript.tex $(bib)
	sed 's|newcommand{\\blind}{0}|newcommand{\\blind}{1}|' manuscript.tex > manuscript-blind.tex
	latexmk -pdf manuscript-blind

supplement: supplement.tex $(bib)
	@latexmk -pdf -dvi- $<
## @latexmk -c

# pre_arxiv: 
# 	@mkdir -p arxiv/img
# 	@perl -ne \
# 		'print "$$1\n" while m/\\includegraphics(?:\[.*?\])?\{(.*?)\}/g' \
# 		main.tex | \
# 		sed 's/^/\.\.\/img\//' | \
# 		xargs -I {} cp {} arxiv/img
# 	@perl -ne \
# 		'print "$$1\n" while m/\\includegraphics(?:\[.*?\])?\{(.*?)\}/g' \
# 		supplement.tex | \
# 		sed 's/^/\.\.\/img\//' | \
# 		xargs -I {} cp {} arxiv/img
# 	@cp main-full.bbl arxiv/ms.bbl
# 	@cp supplement.bbl arxiv
# 	@sed 's/\.\.\/img\//img\//g' supplement.tex | \
# 		sed 's/{main-full}/{ms}/g' | \
# 		sed '/^%/d' > arxiv/supplement.tex
# 	@sed 's/\.\.\/img\//img\//g' main-full.tex | \
# 		sed '/^%/d' > arxiv/ms.tex
# 	@cd arxiv && tar -czf submission.tar.gz ./*

pre_arxiv: 
	@mkdir -p arxiv/img
	@perl -ne \
		'print "$$1\n" while m/\\includegraphics(?:\[.*?\])?\{(.*?)\}/g' \
		main.tex | \
		sed 's/^/\.\.\/img\//' | \
		xargs -I {} cp {} arxiv/img
	@perl -ne \
		'print "$$1\n" while m/\\includegraphics(?:\[.*?\])?\{(.*?)\}/g' \
		supplement.tex | \
		sed 's/^/\.\.\/img\//' | \
		xargs -I {} cp {} arxiv/img
	@cp references.bib arxiv
	@cp asa.bst arxiv
	@sed -n '110,658p' supplement.tex | \
		sed 's/{sec:/{sec:sm:/g' | \
		sed 's/{tab:/{tab:sm:/g' | \
		sed 's/{fig:/{fig:sm:/g' | \
		sed 's/{eq:/{eq:sm:/g' | \
		sed 's/{main:sec:/{sec:/g' | \
		sed 's/{main:tab:/{tab:/g' | \
		sed 's/{main:fig:/{fig:/g' | \
		sed 's/{main:eq:/{eq:/g' | \
		sed '/^%/d' | \
		awk 'BEGIN {print "\\appendix"} 1' | \
		awk 'BEGIN {print "\\setcounter{subsection}{0}"} 1' | \
		awk 'BEGIN {print "\\setcounter{section}{0}"} 1' | \
		awk 'BEGIN {print "\\renewcommand{\\thesection}{\\Alph{section}}"} 1' | \
		awk 'BEGIN {print "\\renewcommand{\\thesection}{\\Alph{section}}"} 1' | \
		awk 'BEGIN {print "\\renewcommand{\\thesubsection}{\\Alph{section}.\\arabic{subsection}}"} 1' | \
		awk 'BEGIN {print "\\renewcommand\\thetable{A.\\arabic{table}}"} 1' | \
		awk 'BEGIN {print "\\renewcommand\\theequation{A.\\arabic{equation}}"} 1' | \
		awk 'BEGIN {print "\\renewcommand\\thefigure{A.\\arabic{figure}}"} 1' > arxiv/appendix.tex
	@sed 's/\.\.\/img\//img\//g' main-full.tex | \
		sed '1273s/^/\\input{appendix}/' | \
		sed '38,56d' | \
		sed '33d' | \
		sed '19d'| \
		sed '15d'| \
		sed 's/{sm:sec:/{sec:sm:/g' | \
		sed 's/{sm:tab:/{tab:sm:/g' | \
		sed 's/{sm:fig:/{fig:sm:/g' | \
		sed 's/{sm:eq:/{eq:sm:/g' | \
		sed 's/material//g' | \
		sed 's/supplementary/appendix/g' | \
		sed -E 's/ ([.\,;!?)])/\1/g' | \
		sed '/^%/d' > arxiv/ms.tex
	@cd arxiv && latexmk -pdf ms.tex && \
		latexmk -c && \
		echo "appendix.tex ignore" > 00README.XXX && \
    tar -czf submission.tar.gz img/ ms.tex ms.bbl appendix.tex 00README.XXX

.PHONY: arxiv
arxiv: all pre_arxiv

.PHONY: all
all: full blind supplement

%.pdf: %.tex $(bib)
	@latexmk -pdf -dvi- $<
##	@latexmk -c

## compare modification with the latest version checked into git
## for comparison of specific commits:
##	latexdiff-vc --git -r old_githash -r new_githash --pdf source.tex
diff:
	latexdiff-vc --git --pdf --force $(source) -r $(rev)

log:
	@git log --pretty=format:"%h by %an at %ar: %s" $(source) | head -n 15

# bibformat: supplement.bib
# 	bibtool

grammar: $(source)
	@pandoc $< -o $(patsubst %.tex,%.rtf,$<)

.PHONY: bibclean

bibclean:
	bibtool -r std_bib.rsc -f "%-1n(author)%d(year)%-T(title)" -i $(bib) -o $(bib)

.PHONY: clean
clean:
	rm -rf $(out)\
	  *~ .*~ .\#* .Rhistory *.aux *.bbl *.blg *.out *.log *.toc\
	  *.fff *.fdb_latexmk *.fls *.ttt *diff* *oldtmp* \
	  main-full* main-blind*
