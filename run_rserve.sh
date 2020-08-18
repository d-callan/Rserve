#!/bin/bash 

CONFIG_PATH=${RSERVE_HOME}/etc/Rserve.conf

R \
-e 'library(plot.data)' \
-e 'library(data.table)' \
-e 'library(Rserve)' \
-e 'source("lib/functions.R")' \
-e 'run.Rserve(debug = TRUE, config.file = "'${CONFIG_PATH}'")'
