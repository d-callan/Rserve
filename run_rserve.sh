#!/bin/bash 

CONFIG_PATH=/etc/Rserv.conf

R \
-e 'library(plot.data)' \
-e 'library(data.table)' \
-e 'library(Rserve)' \
-e 'source("'${RSERVE_HOME}'/lib/functions.R")' \
-e 'run.Rserve(config.file = "'${CONFIG_PATH}'")'
