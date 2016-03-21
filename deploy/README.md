UV-CDAT devops scripts
======================

This directory contains ansible, docker, and vagrant configurations
for repeatable builds and deployment of the UV-CDAT distribution.

Docker images
-------------

Running UV-CDAT inside docker is limited because docker does not support
GUI's or OpenGL rendering.  In addition, docker containers must run in
a virtual machine on unsupported operating systems (including Mac OS,
Windows, and older linux distributions).  This means that the containers
will not have direct access to the host filesystem which can make
operating on local files difficult without configuring shared folders
through in the virtual machine hosting the containers.

For further details see the documentation in [docker/README.md](docker/README.md).

Vagrant configuration
---------------------

To quickly start up a new UV-CDAT environment in a virtual machine, you can
use the [Vagrant](https://www.vagrantup.com/) configuration in the
included [Vagrantfile](Vagrantfile).  You must have Vagrant installed with a
supported virtual machine provider such as [VirtualBox](https://www.virtualbox.org/)
The default configuration will provision a headless configuration that
you can SSH into with
```
vagrant up
vagrant ssh
```
You can also configure the VM and build details inside of YAML file
called `config.yml`.  See [`config.example.yml`](config.example.yml)
for details on supported options.  When you make changes to the configuration,
you must reload the virtual machine by running
```
vagrant reload --provision
```

Ansible playbook
----------------

The playbook at [ansible/provision.yml](ansible/provision.yml) can be used as a
standalone playbook for custom deployments (currently only Ubuntu is supported).
You must provide the following variables to configure the build:

  * `gui`: To build with GUI support
  * `uvcmetrics`: To build with the uvcmetrics development branch and test data
