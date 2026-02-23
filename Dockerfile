FROM rocker/shiny-verse:4.4.1

RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    golem config bslib bsicons \
    leaflet sf plotly \
    DT shinyWidgets \
    rnaturalearth rnaturalearthdata \
    htmltools eurostat giscoR renv

COPY . /srv/shiny-server/NatureJust
WORKDIR /srv/shiny-server/NatureJust

RUN R -e "renv::restore()"
RUN R CMD INSTALL .

EXPOSE 3838

CMD ["R", "-e", "NatureJust::run_app(host='0.0.0.0', port=3838)"]
