# Generic Makefile for Spicey applications

# tar file to be generated with the complete web application
TARFILE := $(CURDIR)/SMAP.tgz

# Definition of the root of the Curry system used to compile the webapp:
#CURRYHOME=/opt/pakcs/pakcs
CURRYHOME=/opt/kics2/kics2

# Curry bin directory to be used:
export CURRYBIN=$(CURRYHOME)/bin

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored:
WEBDIR=$(HOME)/public_html/curry/smap

# Executable of the Curry Package Manager CPM to install the web application:
CPM := $(CURRYBIN)/cypm

# Executable of the curry2cgi:
CURRY2CGI := $(shell which curry2cgi)

# The main module of the webapp:
WEBAPPMAIN = Main

############################################################################

.PHONY: all
all:
	@echo "CURRYHOME: $(CURRYHOME)"
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) install

$(WEBDIR):
	mkdir -p $(WEBDIR)

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
deploy: checkdeploy | $(WEBDIR)
	$(MAKE) $(WEBDIR)/smap.cgi
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBDIR)
	chmod -R go+rX $(WEBDIR)
	mkdir -p $(WEBDIR)/data # create private data dir
	chmod 700 $(WEBDIR)/data
	cp -p data/htaccess $(WEBDIR)/data/.htaccess # and make it private
	mkdir -p $(WEBDIR)/sessiondata # create private data dir
	chmod 700 $(WEBDIR)/sessiondata
	cp -p data/htaccess $(WEBDIR)/sessiondata/.htaccess # and make it private

$(WEBDIR)/smap.cgi: src/*.curry src/*/*.curry
	$(CURRY2CGI) --cpm="$(CPM)" --system="$(CURRYHOME)" \
	  -i Controller.Admin \
	  -i Controller.AuthN \
	  -i Controller.Browser \
	  -i Controller.SmapIE \
	  -o $@ $(WEBAPPMAIN)

# create tar file with complete web app
.PHONY: tar
tar:
	/bin/rm -f $(TARFILE)
	$(MAKE) $(TARFILE)

$(TARFILE): $(WEBDIR)/smap.cgi
	cd $(WEBDIR) && tar czvf $(TARFILE) .
	chmod 644 $(TARFILE)
	@echo "tar file with web app generated:"
	@echo "$(TARFILE)"
	@echo "Copy and unpack it in the desired directory of the web server"

# clean up generated the package directory
.PHONY: clean
clean: 
	$(CPM) $(CPMOPTIONS) clean

# clean everything, including the deployed files (be sure to save the
# database files first!)
.PHONY: cleanall
cleanall: clean
	/bin/rm -f $(WEBDIR)/smap.cgi*
