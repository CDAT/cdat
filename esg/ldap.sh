#!/bin/sh
. ./prolog.sh
sitepackages=`${prefix}/bin/python ../find_site.py`
if (test "$#" -ne 2) then
    echo "Usage: $0 prefix ldapdir";
    exit 1
fi
(cd ldapmodule; \
./configure --prefix=${prefix} --with-ldap=$2; \
make; \
/bin/cp ldapmodule.so ${sitepackages} )
