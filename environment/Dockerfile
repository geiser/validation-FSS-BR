FROM registry.codeocean.com/codeocean/r-studio:1.4.1106-r4.0.5-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("dplyr")' \
    && Rscript -e 'remotes::install_version("knitr")' \
    && Rscript -e 'remotes::install_version("lavaan")' \
    && Rscript -e 'remotes::install_version("matrixStats")' \
    && Rscript -e 'remotes::install_version("mirt")' \
    && Rscript -e 'remotes::install_version("olsrr")' \
    && Rscript -e 'remotes::install_version("parameters")' \
    && Rscript -e 'remotes::install_version("psych")' \
    && Rscript -e 'remotes::install_version("readr")' \
    && Rscript -e 'remotes::install_version("semPlot")' \
    && Rscript -e 'remotes::install_version("semTools")' \
    && Rscript -e 'remotes::install_version("stats")'