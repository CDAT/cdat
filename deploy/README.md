# Running climos test via girder

After running `vagrant up` in this directory, `vagrant ssh` into the machine,
then run the following commands:

    sudo girder-server --port 9080 &
    cd /opt/girder/plugins/romanesco
    . /opt/uvcdat_install/bin/setup_runtime.sh
    /usr/bin/python2.7 -m romanesco

On your machine, open a web browser and go to [the girder instance](http://localhost:9080).
Register a new user, then go to the admin console and create a filesystem assetstore with path `/opt/girder/assetstore`. Go to the plugins page of the admin console, then enable the
**Climos test** plugin and restart the server.

Create a folder in Girder and upload the set of netcdf input files you want to run
climos on into the folder. Create a different folder for the output of the execution
alongside the input folder (not underneath it).

Navigate to the [web API](http://localhost:9080/api/v1). You should see the top-level
`climos` resource with one endpoint available beneath it. Calling this endpoint with
the input and output folder ID's will start the climos job, which is returned. When
the job completes, the output file should appear in the output folder.
