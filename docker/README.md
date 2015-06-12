UV-CDAT Docker images
=====================

Docker images of UV-CDAT are available at [docker hub](https://registry.hub.docker.com/u/uvcdat/uvcdat/).  These
images are built with offscreen rendering and no GUI support due to limitations in docker; however, it is possible
to render images from UV-CDAT scripts or directly using the commandline interface.  The primary motivation for
the creation of these images is for easy deployment of visualization servers for CDATWeb, but users may find
them useful for quick image generation or data exploration without needing to build and configure the UV-CDAT
environment.

To try using the latest version of UV-CDAT, first [install](https://docs.docker.com/compose/install/) Docker
for your platform.  Note, Docker runs natively on Linux, but requires a lightweight virtual machine on Mac and
Windows.  The management of this virtual machine is handled through the `boot2docker` command.  Typically,
you will need to start up the virtual machine with `boot2docker start` and configure the environment with
`$(boot2docker shellinit)`.  See the docker installation guide for more information.

Running uvcdat once docker is set up is easy:
```
docker run -i -t uvcdat/uvcdat ipython
```
This will download the docker image, run it in a new container, and give an ipython shell inside the container.
From here, you can import all python modules that are shipped with the CLI version of UV-CDAT.

Building the docker images
--------------------------

The `Dockerfile` for building UV-CDAT is stored at the root level of this repository.  It is built on top of
a custom Ubuntu 14.04 install containing all necessary dependencies.  Generally it will not be necessary to
rebuild this base image, but if new packages are necessary from apt, they can be added to `docker/ubuntu/Dockerfile`
and the image rebuilt with `docker build -t uvcdat/ubuntu`.

When updating master, a new `uvcdat/uvcdat:latest` should be generated and pushed to docker hub.  The
build should be initiated from a clean checkout of the repository to avoid adding unnecessary files to the
image.  From the top level of the repository, issue the following command to build the image.
```
docker build -t uvcdat/uvcdat .
```
Once it is built successfully you can test the image with ctest and upload the results to
[CDash](https://open.cdash.org/index.php?project=UV-CDAT&display=project) with
```
docker run ctest -S /usr/src/uvcdat/CMake/dashboard/docker.cmake -VV
```
Finally, if you have push access to the docker hub account, you can push the image up to docker.
```
docker push uvcdat/uvcdat
```

Note that UV-CDAT probably won't build with the standard VM setup by `boot2docker`.  If you get build errors, you
may need to increase the memory and disk size when initializing.  For example, to create a VM with 4 GB of RAM and 
50 GB of disk space:
```
boot2docker --memory=4096 --disksize=50000
```
