FROM rocker/r-ver:4.5.1
LABEL author="yuqingwang1995@gmail.com" description="Shiny app skinOmicsExplorer. Direct run, no shiny server"

ENV LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
    DEBIAN_FRONTEND=noninteractive

# Install System libraries
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
     gcc g++ make locales apt-file \
  && apt-file update
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
     libglpk40 libgdal34t64 libproj25 libgeos-c1t64 libudunits2-0 libglu1-mesa \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e 'install.packages(c("remotes","BiocManager","shiny","bslib","shinyjs","Seurat","tidyr","ggplot2","plotly","dplyr","gganimate","ggforce","networkD3","rgl","transport","webshot","eulerr"))'
RUN R -q -e 'BiocManager::install("ComplexHeatmap")'
RUN R -q -e 'remotes::install_github("Yuqing66/AddOns")'
RUN R -q -e 'remotes::install_github("Yuqing66/scSpatial")'

COPY . /shinyApp

CMD ["R","-e","shiny::runApp('/shinyApp')"]