FROM 40ants/base-lisp-image:latest-sbcl as base

EXPOSE 80
EXPOSE 4005

# These a dev dependencies to simplify log reading and support
# file search from remote Emacs.
RUN set -x; \
    apt-get update && \
    apt-get install -y \
	    git \
	    gcc \
            curl \
            unzip \
            postgresql-client && \
    mkdir -p /tmp/s6 && cd /tmp/s6 && \
    git clone https://github.com/skarnet/skalibs && cd skalibs && \
    git checkout v2.10.0.2 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/execline && cd execline && \
    git checkout v2.8.0.0 && \
    ./configure && make install && cd /tmp/s6 && \
    git clone https://github.com/skarnet/s6 && cd s6 && \
    git checkout v2.10.0.2 && \
    ./configure --with-lib=/usr/lib/execline && make install && \
    cd / && rm -fr /tmp/s6
# && rm -rf /var/lib/apt/lists/*

# RUN set -x; \
#     curl https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-386.zip --output ngrok.zip && \
#     unzip ngrok.zip && \
#     chmod +x ngrok

ENV CC=gcc

COPY . /app
RUN qlot install
RUN qlot exec ros run --load load-all.lisp --quit

ENV CL_SOURCE_REGISTRY=/app/bundle//:/app/server/:/app/common/


FROM base as server
RUN qlot exec ros build server/roswell/server.ros
CMD /app/server/roswell/server
