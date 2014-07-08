#!/bin/sh

# 1. configure the following two variables:
# target directory where the compiled cgi program should be stored:
WEBSERVERDIR=$HOME/public_html/smap
if [ $# = 1 ] ; then
  WEBSERVERDIR=$1
fi
# makecurrycgi script to deploy the system (included in the PAKCS distribution):
MAKECURRYPATH=/opt/kics2/bin/makecurrycgi
CGI=smap.cgi

# 2. delete the following line
# echo "Please configure the deploy.sh script first!"; exit 2

##########################################################################
# here starts the standard script:

# create web directory if necessary:
if [ ! -d $WEBSERVERDIR ] ; then
  echo "Creating web directory '$WEBSERVERDIR'..."
  mkdir $WEBSERVERDIR
  chmod 755 $WEBSERVERDIR
fi

ORIGDIR=`pwd`                          # original directory (builtin)
CODEPATH=$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config:$ORIGDIR/lib

CURRYPATH=$CODEPATH
export CURRYPATH

$MAKECURRYPATH -standalone -m main -o $WEBSERVERDIR/$CGI Main.curry

# copy other files (Stylesheets, Images...)
cp -r $ORIGDIR/public/* $WEBSERVERDIR
chmod -R go+rX $WEBSERVERDIR
