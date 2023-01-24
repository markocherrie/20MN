# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libudunits2-dev \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    #libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
#COPY /BuiltEnvironmentChange /
#COPY /AlcoholTobacco /
#COPY /SubsetShiny /
##COPY ./shiny-app/ /srv/shiny-server/
##COPY ./index.html /srv/shiny-server/index.html

# install renv & restore packages
RUN Rscript -e 'install.packages("shiny", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyjs", dependencies = TRUE)'
RUN Rscript -e 'install.packages("leaflet", dependencies = TRUE)'
RUN Rscript -e 'install.packages("rgdal",dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyBS", dependencies = TRUE)'
RUN Rscript -e 'install.packages("gtools", dependencies = TRUE)'
RUN Rscript -e 'install.packages("ggplot2", dependencies = TRUE)'
RUN Rscript -e 'install.packages("RCurl", dependencies = TRUE)'
RUN Rscript -e 'install.packages("RJSONIO", dependencies = TRUE)'
RUN Rscript -e 'install.packages("plyr", dependencies = TRUE)'
RUN Rscript -e 'install.packages("rgeos", dependencies = TRUE)'
RUN Rscript -e 'install.packages("dplyr", dependencies = TRUE)'
RUN Rscript -e 'install.packages("jsonlite", dependencies = TRUE)'
RUN Rscript -e 'install.packages("mongolite", dependencies = TRUE)'
RUN Rscript -e 'install.packages("DT", dependencies = TRUE)'
RUN Rscript -e 'install.packages("leaflet.extras", dependencies = TRUE)'
RUN Rscript -e 'install.packages("BAMMtools", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyWidgets", dependencies = TRUE)'
RUN Rscript -e 'devtools::install_github("hrbrmstr/ipapi")'
RUN Rscript -e 'install.packages("lubridate", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyTime", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinycssloaders", dependencies = TRUE)'
RUN Rscript -e 'devtools::install_version("flexpolyline", version = "0.2.6", repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'devtools::install_version("hereR", version = "0.9.0", repos = "http://cran.us.r-project.org")'
#RUN Rscript -e 'devtools::install_github("munterfi/hereR")'
RUN Rscript -e 'install.packages("geomtextpath", dependencies = TRUE)'
RUN Rscript -e 'install.packages("htmlwidgets", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shinyMobile", dependencies = TRUE)'
RUN Rscript -e 'install.packages("ggpubr", dependencies = TRUE)'

ENV HEREAPIKEY='-V2lLMbX0T77qvxSEhVt5oZ9Czu2MxHzM2SsUIIcKDs'
ENV BINGKEY='AhN-K4JcOT-6mVUnuQcGI3np04FL7uA-acM8q2Ms99Qy-oZCCJgVdqUHX41IzUBs'

# expose port
#EXPOSE 3838
# copy thlse app to the image
RUN mkdir /root/mn
COPY app /root/mn
COPY app/Rprofile.site /usr/local/lib/R/etc/

# expose port
EXPOSE 3838

# run app
CMD ["R", "-q", "-e", "shiny::runApp('/root/mn')"]
# run app on container start
# CMD ["R", "-e", "shiny::runApp('/SubsetShiny', host = '0.0.0.0', port = 3838)"]
#CMD ["R", "-e", "shiny::runApp('/ATapp', host = '0.0.0.0', port = 3838)"]
