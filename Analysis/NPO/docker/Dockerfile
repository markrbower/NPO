FROM rocker/r-ver:4.0.2

RUN mkdir /home/analysis

RUN Rscript -e "install.packages('NPO')"

COPY myscript.R /home/analysis/myscript.R
COPY install_packages.R /home/analysis/install_packages.R

RUN Rscript /home/analysis/install_packages.R

CMD R -e "source('/home/analysis/myscript.R')"

