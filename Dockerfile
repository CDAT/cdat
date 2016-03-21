FROM uvcdat/ubuntu
MAINTAINER UV-CDAT Developers <uvcdat-support@llnl.gov>

RUN mkdir -p /tmp/uvcdat-build
ADD . /usr/src/uvcdat
RUN cd /tmp/uvcdat-build && cmake -DCDAT_DOWNLOAD_UVCMETRICS_TESTDATA=OFF -DCDAT_BUILD_WEB=ON -DCDAT_BUILD_GUI=OFF -DCDAT_BUILD_OSMESA=ON -DCDAT_BUILD_OFFSCREEN=ON -DCMAKE_INSTALL_PREFIX=/opt/uvcdat /usr/src/uvcdat && make && cd / && rm -fr /tmp/uvcdat-build

RUN useradd -d /data -m -U uvcdat

VOLUME /data
WORKDIR /data
USER uvcdat

ENV UVCDAT_SETUP_PATH /opt/uvcdat
ENV PATH $UVCDAT_SETUP_PATH/bin:$UVCDAT_SETUP_PATH/Externals/bin:$PATH
ENV PYTHONPATH $UVCDAT_SETUP_PATH/lib/python2.7/site-packages:$UVCDAT_SETUP_PATH/Externals/lib/python2.7/site-packages
ENV LD_LIBRARY_PATH $UVCDAT_SETUP_PATH/lib:$UVCDAT_SETUP_PATH/Externals/lib
ENV UVCDAT_ANONYMOUS_LOG no
