#!/bin/bash 

CONFIG_PATH=${RSERVE_HOME}/etc/Rserve.conf

R \
-e 'library(epitools)' \
-e 'library(mgcv)' \
-e 'library(purrr)' \
-e 'library(data.table)' \
-e 'library(Rserve)' \
-e 'source("lib/functions.R")' \
-e 'run.Rserve(debug = TRUE, config.file = "'${CONFIG_PATH}'")'
