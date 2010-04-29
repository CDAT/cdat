######################################################################
## SpanLib, Raynaud 2006-2008
######################################################################

## For the distro
EXTRA_DIST = source/*.rst build source/_static

## For the installation
docdir = $(datadir)/@PACKAGE@
dist_doc_DATA = 

## Sphinx

# You can set these variables from the command line.
SPHINXOPTS    =
PAPER         = a4

# Internal variables.
ALLSPHINXOPTS   = -d build/doctrees -D latex_paper_size=$(PAPER) $(SPHINXOPTS) source


######################################################################
######################################################################


if HAS_DOC_SUPPORT

if HAS_LATEX_SUPPORT
all-local: html latex
else
all-local: html
endif



html-local:
	-mkdir -p build/html build/doctrees
	-$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) build/html
	@echo
	@echo "Build finished. The HTML pages are in build/html."

if HAS_LATEX_SUPPORT
latex:
	-mkdir -p build/latex build/doctrees
	-$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) build/latex
	-cd build/latex && make all-pdf
endif

changes:
	-mkdir -p build/changes build/doctrees
	-$(SPHINXBUILD) -b changes $(ALLSPHINXOPTS) build/changes
	@echo
	@echo "The overview file is in build/changes."

linkcheck:
	-mkdir -p build/linkcheck build/doctrees
	-$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) build/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in build/linkcheck/output.txt."

maintener-clean-local:
	-rm -rf build

endif

install-pdf-local:
	cp build/latex/SpanLib.pdf $(docdir)
	
install-html-local:
	cp -r build/html/* $(docdir)


