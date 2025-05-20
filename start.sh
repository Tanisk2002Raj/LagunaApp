#!/bin/bash
R -e "shiny::runApp('app.R', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
