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
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*

ENV LANG en_US.UTF-8

RUN mkdir -p /opt/project && mkdir -p /data/temp

COPY . /opt/project

WORKDIR /opt/project

ENV DATA_HOME /data
ENV DATA_TEMP /data/temp

RUN set -ex \
      && alias sensevectors='Rscript --vanilla --default-packages=methods,utils,stats /opt/project/bin/sensevectors' \
      && alias sensasim='Rscript --vanilla --default-packages=methods,utils,stats /opt/project/bin/sensasim' \
      && alias rds='Rscript --vanilla --default-packages=methods,utils,stats /opt/project/bin/rds' \
      && alias tobigmatrix='Rscript --vanilla --default-packages=methods,utils,stats /opt/project/bin/toBigMatrix.R'

RUN set -ex \
      && Rscript -e 'install.packages("devtools")' \
      && Rscript -e 'devtools::install(".")' \
      && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

EXPOSE 6348

# re-use r-base's entrypoint which is CMD ["R"]
