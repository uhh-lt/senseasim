#
# Senseasim build
#
FROM r-base

RUN set -ex \
      && DEBIAN_FRONTEND=noninteractive \
      && apt-get update \
      && apt-get install -y --no-install-recommends apt-utils locales libcurl4-openssl-dev libssl-dev libssh2-1-dev curl python3 \
      && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
      && dpkg-reconfigure --frontend=noninteractive locales \
      && update-locale LANG=en_US.UTF-8 \
      && apt-get clean

ENV LANG en_US.UTF-8

RUN mkdir -p /opt/project

COPY . /opt/project

WORKDIR /opt/project

RUN set -ex \
      && Rscript -e 'install.packages("devtools")' \
      && Rscript -e 'devtools::install(".")'

EXPOSE 6348

# re-use r-base's entrypoint
