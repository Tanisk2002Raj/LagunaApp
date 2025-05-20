# Use Rocker image with Shiny
FROM rocker/shiny:latest

# Install R packages
COPY install.R /install.R
RUN Rscript /install.R

# Copy app to container
COPY . /app

# Set working directory
WORKDIR /app

# Expose default Shiny port
EXPOSE 3838

# Run the Shiny app directly (not with shiny-server)
CMD ["R", "-e", "shiny::runApp('/app', port=3838, host='0.0.0.0')"]
