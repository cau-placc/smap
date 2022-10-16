# Generic Makefile for Spicey applications

# Definition of the root of the Curry system to be used:
#CURRYHOME=$(HOME)/pakcs3
CURRYHOME=/opt/kics2/kics3

# Curry bin directory to be used:
export CURRYBIN=$(CURRYHOME)/bin

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored:
WEBSERVERDIR=$(HOME)/public_html/smap

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cypm

# Executable of the curry2cgi:
CURRY2CGI := $(shell which curry2cgi)

############################################################################

.PHONY: all
all:
	@echo "CURRYHOME: $(CURRYHOME)"
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) install

# check presence of tools required for deployment and install them:
.PHONY: checkdeploy
checkdeploy:
	@if [ ! -x "$(CURRY2CGI)" ] ; then \
	   echo "Installing required executable 'curry2cgi'..." ; \
           $(CPM) install html2 ; fi

# Compile the generated Spicey application:
.PHONY: compile
compile:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run:
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :eval main :q

# Deploy the generated Spicey application, i.e., install it in the
# web pages:
.PHONY: deploy
deploy: checkdeploy
	mkdir -p $(WEBSERVERDIR)
	$(MAKE) $(WEBSERVERDIR)/smap.cgi
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)
	mkdir -p $(WEBSERVERDIR)/data # create private data dir
	chmod 700 $(WEBSERVERDIR)/data
	cp -p data/htaccess $(WEBSERVERDIR)/data/.htaccess # and make it private
	mkdir -p $(WEBSERVERDIR)/sessiondata # create private data dir
	chmod 700 $(WEBSERVERDIR)/sessiondata
	cp -p data/htaccess $(WEBSERVERDIR)/sessiondata/.htaccess # and make it private

$(WEBSERVERDIR)/smap.cgi: src/*.curry src/*/*.curry
	$(CPM) exec $(CURRY2CGI) --cpmexec \"$(CPM) exec\" \
	  --system="$(CURRYHOME)" \
	  -i Controller.Admin \
	  -i Controller.AuthN \
	  -i Controller.Browser \
	  -i Controller.SmapIE \
	  -o $@ Main.curry

# clean up generated the package directory
.PHONY: clean
clean: 
	$(CPM) $(CPMOPTIONS) clean

# clean everything, including the deployed files (be sure to save the
# database files first!)
.PHONY: cleanall
cleanall: clean
	/bin/rm -f $(WEBSERVERDIR)/smap.cgi*
