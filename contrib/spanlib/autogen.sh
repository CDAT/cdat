#! /bin/sh

echo "###########################################"
(

echo "Generating m4 include file..."
rm -f acinclude.m4
for m4 in admin/m4/*.m4 ; do cat $m4 >> acinclude.m4 ; done

) && (

echo "Aclocal..."
aclocal

) && (

echo "Automake..."
automake --add-missing

) && (

echo "Autoconf..."
autoconf

) && (

echo "###########################################"
echo "Now you can use ./configure <your options>"
echo "###########################################"

)
