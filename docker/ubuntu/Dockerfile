FROM ubuntu:14.04
MAINTAINER UV-CDAT Developers <uvcdat-support@llnl.gov>

RUN apt-get update && apt-get install -y git gfortran g++ libffi-dev libsqlite-dev libssl-dev libbz2-dev libexpat-dev ncurses-dev curl make wget libjpeg-dev libpng-dev

RUN curl http://www.cmake.org/files/v3.2/cmake-3.2.0-rc2-Linux-x86_64.tar.gz | tar -v -C /opt -zx

ENV PATH /opt/cmake-3.2.0-rc2-Linux-x86_64/bin:$PATH
