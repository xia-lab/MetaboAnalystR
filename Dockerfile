FROM rocker/r-ver:3.6.2

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libcairo2-dev \
        libnetcdf-dev \
        libxml2-dev \
        libxt-dev \
        libssl-dev \
        && rm -rf /var/lib/apt/lists/*

# Install R libraries
RUN R -e "install.packages('devtools', repos = c(CRAN = 'https://cran.rstudio.com/'))"
RUN R -e "install.packages('crmn')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('qs')"

RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('impute')"
RUN R -e "BiocManager::install('pcaMethods')"
RUN R -e "BiocManager::install('globaltest')"
RUN R -e "BiocManager::install('GlobalAncova')"
RUN R -e "BiocManager::install('Rgraphviz')"
RUN R -e "BiocManager::install('preprocessCore')"
RUN R -e "BiocManager::install('genefilter')"
RUN R -e "BiocManager::install('SSPA')"
RUN R -e "BiocManager::install('sva')"
RUN R -e "BiocManager::install('limma')"
RUN R -e "BiocManager::install('KEGGgraph')"
RUN R -e "BiocManager::install('siggenes')"
RUN R -e "BiocManager::install('BiocParallel')"
RUN R -e "BiocManager::install('MSnbase')"
RUN R -e "BiocManager::install('multtest')"
RUN R -e "BiocManager::install('RBGL')"
RUN R -e "BiocManager::install('edgeR')"
RUN R -e "BiocManager::install('fgsea')"
RUN R -e "BiocManager::install('ctc')"

RUN R -e "install.packages('glasso')"
RUN R -e "install.packages('ppcor')"
RUN R -e "install.packages('huge')"
RUN R -e "install.packages('crmn')"
RUN R -e "install.packages('plotly')"


RUN R -e "devtools::install_github('xia-lab/MetaboAnalystR', build = TRUE, build_vignettes = FALSE);"

# Create user
RUN useradd -ms /bin/bash docker
USER docker

# Start at prompt
CMD ["/bin/bash R"]

