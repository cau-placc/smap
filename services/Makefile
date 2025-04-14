# The Curry system used to compile the web service script:
CURRYSYSTEM=/opt/pakcs/pakcs/bin/curry
#CURRYSYSTEM=/opt/kics2/kics2/bin/curry
CURRYOPTS=:set -time
# The main directory of the smap installation:
SMAPDIR=$(HOME)/public_html/smap
# The directory containing the executable service scripts:
CGIDIR=$(SMAPDIR)/exec
# Auxiliary Prolog program for sandbox execution:
SWILOAD=safeload.pl
# Directory containing standard libraries for PAKCS2
STDLIBDIR=curry_libs

.PHONY: install
install: Curry_System Haskell_GHC Prolog_SWI
	mkdir -p $(CGIDIR)
	chmod 755 $(CGIDIR)
	$(MAKE) uninstall
	mv Curry_System $(CGIDIR)/CURRY.cgi
	cp -a $(STDLIBDIR)* $(CGIDIR)/
	chmod -R go+rX $(CGIDIR)/$(STDLIBDIR)*
	mv Haskell_GHC $(CGIDIR)/GHC.cgi
	mv Prolog_SWI  $(CGIDIR)/SWI.cgi
	cp $(SWILOAD) $(CGIDIR)
	chmod 644 $(CGIDIR)/$(SWILOAD)
	chmod 755 $(CGIDIR)/*.cgi

.PHONY: uninstall
uninstall:
	cd $(CGIDIR) && rm -f PAKCS.cgi KiCS2.cgi GHC.cgi SWI.cgi $(SWILOAD)
	cd $(CGIDIR) && rm -rf $(STDLIBDIR)*

Curry_System: Curry_System.curry SimpleWebService.curry
	$(CURRYSYSTEM) $(CURRYOPTS) :load Curry_System.curry :save :quit

Haskell_GHC: Haskell_GHC.curry SimpleWebService.curry
	$(CURRYSYSTEM) $(CURRYOPTS) :load Haskell_GHC.curry :save :quit

Prolog_SWI: Prolog_SWI.curry SimpleWebService.curry
	$(CURRYSYSTEM) $(CURRYOPTS) :load Prolog_SWI.curry :save :quit

.PHONY: clean
clean: uninstall
	cleancurry
