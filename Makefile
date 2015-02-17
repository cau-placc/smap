# Generic Makefile for Spicey applications

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored:
WEBSERVERDIR=$(HOME)/public_html/smap

# Definition of the Curry installation bin directory to be used:
#export CURRYBIN=/opt/pakcs/bin
export CURRYBIN=/opt/kics2/bin

# The root directory of the sources of the Spicey application:
SRCDIR := $(CURDIR)
# The load path for the Spicey application:
export CURRYPATH := $(SRCDIR)/views:$(SRCDIR)/controllers:$(SRCDIR)/models:$(SRCDIR)/system:$(SRCDIR)/config:$(SRCDIR)/lib

.PHONY: all
all:
	@echo "make: deploy compile load run clean?"

# Deploy the generated Spicey application, i.e., install it in the
# web pages:
.PHONY: deploy
deploy:
	$(CURRYBIN)/makecurrycgi -standalone -m main -o $(WEBSERVERDIR)/spicey.cgi Main.curry
	# copy other files (style sheets, images,...)
	cp -r $(SRCDIR)/public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)

# Compile the generated Spicey application:
.PHONY: compile
compile:
	$(CURRYBIN)/curry :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	$(CURRYBIN)/curry :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run:
	$(CURRYBIN)/curry :load Main :eval main :quit

# clean up generated programs
.PHONY: clean
clean: 
	$(CURRYBIN)/cleancurry -r
