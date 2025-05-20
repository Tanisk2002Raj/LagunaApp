FROM rocker/shiny:latest

# Install system dependencies (add more if needed)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy your Shiny app into the image
COPY . /srv/shiny-server/

# Allow permission
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the port the app runs on
EXPOSE 3838

# Run the application
CMD ["/usr/bin/shiny-server"]
