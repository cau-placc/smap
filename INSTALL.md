Installation of Smap at
=======================

https://smap.curry-lang.org/
--------------------------------------

Installation directory: /var/www/vhosts/curry-lang.org/smap.curry-lang.org

Install new version:
--------------------

1. Login at sam-hanus.de

2. Create local installation for deploy:

   - either set CURRYHOME to kics2 in Makefile and run `make deploy`
   - or run `make CURRYHOME=/opt/kics2/kics2 deploy`

3. Create tar file `SMAP.tgz` pf the web installation:

       > make tar

4. Unpack and install tar file in installation directory.
   If necessary, add the following `.htaccess` file after unpacking:

       Options +ExecCGI
       AddHandler cgi-script .cgi


Install the execution services:
-------------------------------

Login as sam-hanus.de and

    > cd home/curry/applications/smap/services
    > make clean
    > make

Then copy the directory `~/public_html/smap/exec` as `exec` into
the installation directory.

### Curry systems

Curry systems should be installed in `/opt/pakcs/pakcs-<version>`
and `/opt/kics2/kics2-<version>`. Then it is sufficient to adapt
the execution URL defined in Smap to a new version.
In order to use the CLP libraries for PAKCS, one should also
install the libraries under `exec/curry_lib_pakcs_<version>`
in the installation directory of Smap.

The directories `/opt/pakcs/.cpm/bin` and `/opt/kics2/.cpm/bin`
should also contain the executable `currypp' in order to support
default rules in example programs.


Save current data to local files:
---------------------------------

Login on `sam-hanus.de` and execute

    > ~/home/curry/applications/smap/fetch-smapdb.sh

This copies the database into `~/public_html/smap/data/Smap.db`.

------------------------------------------------------------------------------
