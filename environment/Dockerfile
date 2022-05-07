FROM registry.codeocean.com/codeocean/r-studio:1.4.1106-r4.0.5-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt -y update \
    && apt -y install cmake \
    && apt -y install libopenmpi-dev \
    && apt -y install libxml2-dev \
    && apt -y install libnlopt-dev

RUN Rscript -e 'remotes::install_version("semPlot")' 
RUN Rscript -e 'remotes::install_version("dplyr")' 
RUN Rscript -e 'remotes::install_version("knitr")' 
RUN Rscript -e 'remotes::install_version("rmarkdown")' 
RUN Rscript -e 'remotes::install_version("lavaan")' 
RUN Rscript -e 'remotes::install_version("matrixStats")'
RUN Rscript -e 'remotes::install_version("mirt")'
RUN Rscript -e 'remotes::install_version("olsrr")'
RUN Rscript -e 'remotes::install_version("parameters")'
RUN Rscript -e 'remotes::install_version("psych")' 
RUN Rscript -e 'remotes::install_version("readr")' 
RUN Rscript -e 'remotes::install_version("semTools")'
