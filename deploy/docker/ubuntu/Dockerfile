FROM ubuntu:15.04
MAINTAINER UV-CDAT Developers <uvcdat-support@llnl.gov>

RUN apt-get update && apt-get install -y git gfortran g++ libffi-dev libsqlite-dev libssl-dev libbz2-dev libexpat-dev ncurses-dev libcurl3-openssl-dev curl make wget libjpeg-dev libpng-dev m4

RUN curl https://cmake.org/files/v3.4/cmake-3.4.3-Linux-x86_64.tar.gz | tar -v -C /opt -zx

ENV PATH /opt/cmake-3.4.3-Linux-x86_64/bin:$PATH
