#!/bin/bash 

CONFIG_PATH=${RSERVE_HOME}/etc/Rserve.conf

R \
-e 'library(mgcv)' \
-e 'library(purrr)' \
-e 'library(data.table)' \
-e 'library(Rserve)' \
-e 'source("functions.R")' \
-e 'run.Rserve(debug = TRUE, config.file = "'${CONFIG_PATH}'")'
