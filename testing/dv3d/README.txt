Instructions for generating and running ctests.

1. After updating and building the UVCDAT source code the cmake ctest script should be exercised at least once by
   sourcing the 'build/install/bin/setup_runtime.sh' script and then executing  'ctest –R dv3d' in the build directory.
2. All ctests should be defined using the uvcdat sample data.  To find the sample data you can cd to the build/install
   directory and then execute 'find . -name clt.nc'.   If the issue or feature cannot be tested with any of the sample
   data files then a new data file will need to be added to the sample data collection (please consult with Tom on this).
3. Using a sample data file as input, create a UVCDAT plot that exercises the feature/bug fix.
4. Hit the 't' key which prints a test definition in the shell.
5. Copy the test definition and paste it into the file 'source/testing/dv3d/TestDefinitions.py'.
6. Edit the test name 'ctest' in the test definition, giving it a name that reflects the purpose of the test.
7. Execute the dv3d ctests by executing 'python ReviewTests.py' in the ''source/testing/dv3d/' directory. This will
   display the plot image for each ctest in turn.    Following the directions, type <Enter> to continue and create/update
   the reference image if the displayed image looks correct  ( type 'n' to skip the update if there is an error ).
8. When run in batch mode (i.e. ‘ctest’ is executed ) the generated plot image for each ctest will be compared against
   the corresponding reference image to determine if the ctest passes or fails.
9. To push the new ctest to the uvcdat repo, the command 'git push origin HEAD' must be executed in the 'source' and
   'build/uvcdat-testdata' directories.
