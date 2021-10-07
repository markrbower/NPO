FROM rocker/r-ver:4.0.2

RUN mkdir ./docker_analysis

RUN R -e "options(repos = \
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-01-06/')); \
  install.packages('markrbower/NPO')"

