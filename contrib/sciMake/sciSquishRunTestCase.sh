#!/bin/bash

# $Id: sciSquishRunTestCase.sh 1235 2012-01-28 00:10:11Z jdelamere $

# This script launches a single Squish test, and screens the Squish output for failures.  

# Input:
# $1 - Squish server executable
# $2 - Squish runner executable
# $3 - Application Under Testing (AUT) name
# $4 - Path to AUT
# $5 - Location of Squish test suite
# $6 - Name of Squish test in test suite
# $7 - Name of Squish output log file

# Note: Script returns integer containing total number of failures.

#Create logfile name and move to backup if it already exists
logfile=$7
if test -f $7; then
    \mv $7 $7.bak
fi

# Start Squish server
$1 --verbose &

# Mapping application ($3) to path ($4)
$1 --config addAUT $3 $4
$1 --config setAUTTimeout 60
$1 --config setResponseTimeout 60 


# Call Squish runner to run test
$2 --testsuite $5 --testcase $6 --reportgen stdout,$7 --aut $3

# Stop Squish server
$1 --stop

# Initial method to sort Squish test results.  Returning an errtot greater than
# zero will result in an overall test failure.

if test -f $7; then
  errtxt=( "Number of Errors:" "Number of Fatals:" "Number of Fails:")
  errtot=0
  for ierr in "${errtxt[@]}"; do
    serr=`grep "${ierr}" "$7"`
    if [ $? -eq 0 ]; then
      errtotn=${serr/${ierr}/" "}
      let errtot=errtot+errtotn
    fi
  done
else 
  echo "Squish did not create logfile; assuming test failed."
  errtot=999
fi

exit $errtot

