#!/bin/sh
# Update the installed version of smap on giscours: by
# compiling and deploying this version

# directory with web installation of smap:
WWWDIR=/srv/sites/informatik.uni-kiel.de/www-ps/htdocs/smap

if [ "`hostname -s`" != "giscours" ] ; then
  echo "Please generate executable on giscours!"
  exit 1
fi

echo "Current sessionAuthData file:"
ls -l $WWWDIR/data/sessionAuthData
cat $WWWDIR/data/sessionAuthData
echo "Proceed (y/n)?"
read ANSWER
if [ "$ANSWER" != "y" ] ; then
  echo "Aborted!"
  exit 1
fi

make install
# change the owner from www-data to myself:
sudo chown -R `id -u -n` $WWWDIR
sudo chgrp -R `id -g -n` $WWWDIR
# deploy current version:
export PATH=$HOME/.cpm/bin_kics2:$PATH && make deploy WEBSERVERDIR=$WWWDIR
# change the owner to www-data:
sudo chown -R www-data $WWWDIR
sudo chgrp -R www-data $WWWDIR
