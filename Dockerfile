FROM rocker/r-ver:4.1.2
LABEL maintainer="ciranka"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y make \
	pandoc
RUN ["install2.r", "batchtools", "brms", "debugme", "dplyr", "forcats", "future", "future.batchtools", "ggplot2", "listenv", "parallelly", "purrr", "Rcpp", "readr", "rstan", "StanHeaders", "stringr", "tibble", "tidyr", "tidyverse"]
WORKDIR /payload/
CMD ["R"]
