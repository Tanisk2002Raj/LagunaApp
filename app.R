library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(prophet)
library(DT)
library(tibble)
library(ggplot2)

# ---------------------- Load Data ----------------------
load_data <- function() {
  if (file.exists("MachineTransfer_Combined.csv")) {
    read_csv("MachineTransfer_Combined.csv", show_col_types = FALSE)
  } else if (file.exists("MachineTransfer_Combined.xlsx")) {
    read_excel("MachineTransfer_Combined.xlsx", sheet = 1)
  } else {
    stop("❌ No valid data file found.")
  }
}

data <- tryCatch({
  df <- load_data()
  df %>%
    rename(
      UploadedTime = `uploaded_time`,
      MachineID = `Machine ID`,
      FinalFactory = `Final Factory`,
      FinalSubFactory = `Final Sub Factory`,
      FinalNewLine = `Final New Line`,
      FinalSection = `Final Section`
    ) %>%
    filter(!is.na(MachineID), !is.na(UploadedTime)) %>%
    mutate(UploadedTime = as.Date(UploadedTime))
}, error = function(e) {
  message("Error loading data: ", e$message)
  NULL
})

# ---------------------- Prophet Forecast ----------------------
get_forecast_info <- function(dates) {
  dates <- sort(dates)
  n <- length(dates)
  
  if (n >= 3) {
    df <- tibble(ds = dates, y = rep(1, n))
    m <- prophet(df, yearly.seasonality = TRUE, weekly.seasonality = FALSE,
                 daily.seasonality = FALSE, n.changepoints = min(5, n - 1))
    future <- make_future_dataframe(m, periods = 60)
    forecast <- predict(m, future)
    next_date <- forecast$ds[which.max(forecast$yhat)]
    
    matched <- any(abs(as.numeric(difftime(next_date, dates, units = "days"))) <= 3)
    accuracy <- ifelse(matched, 100, 0)
    
    p <- plot(m, forecast) + ggtitle("Prophet Forecast") + theme_minimal()
    for (cp in m$changepoints) {
      p <- p + geom_vline(xintercept = as.numeric(cp), color = "red", linetype = "dashed")
    }
    
    return(list(predicted = as.Date(next_date), matched = matched, accuracy = accuracy, plot = p))
    
  } else if (n >= 1) {
    avg_interval <- if (n == 1) 30 else mean(diff(dates))
    next_date <- max(dates) + round(avg_interval)
    
    matched <- any(abs(as.numeric(difftime(next_date, dates, units = "days"))) <= 3)
    accuracy <- ifelse(matched, 100, 0)
    
    p <- ggplot() + 
      ggtitle("Average Interval Forecast") +
      annotate("text", x = 1, y = 1, label = paste("Predicted:", next_date), size = 6) +
      theme_void()
    
    return(list(predicted = as.Date(next_date), matched = matched, accuracy = accuracy, plot = p))
  }
  
  return(list(predicted = NA, matched = FALSE, accuracy = 0, plot = NULL))
}

# ---------------------- Maintenance Summary ----------------------
maintenance_summary <- if (!is.null(data)) {
  data %>%
    group_by(MachineID) %>%
    summarise(
      MaintenanceDates = list(UploadedTime),
      FinalFactory = last(FinalFactory),
      FinalSubFactory = last(FinalSubFactory),
      FinalNewLine = last(FinalNewLine),
      FinalSection = last(FinalSection),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      result = list(get_forecast_info(MaintenanceDates)),
      PredictedDate = result$predicted,
      MaintenanceMatched = result$matched,
      Accuracy = result$accuracy,
      ProphetPlot = list(result$plot)
    ) %>%
    ungroup()
} else {
  NULL
}

# ---------------------- UI ----------------------
ui <- fluidPage(
  titlePanel("🔧 Smart Predictive Maintenance (Prophet AI)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("machine_id", "Select Machine:", 
                  choices = if (!is.null(maintenance_summary)) maintenance_summary$MachineID else ""),
      helpText("Prophet AI predicts next maintenance date.")
    ),
    mainPanel(
      h4("Forecast Summary"),
      DTOutput("summaryTable"),
      h4("Forecast Plot"),
      plotOutput("forecastPlot", height = "500px")
    )
  )
)

# ---------------------- Server ----------------------
server <- function(input, output) {
  output$summaryTable <- renderDT({
    if (!is.null(maintenance_summary)) {
      maintenance_summary %>%
        mutate(
          Status = ifelse(MaintenanceMatched, "✅ Matched", "❌ Not Matched"),
          Accuracy = paste0(Accuracy, "%")
        ) %>%
        select(MachineID, FinalFactory, FinalSubFactory, FinalNewLine, FinalSection,
               PredictedDate, Status, Accuracy)
    }
  })
  
  output$forecastPlot <- renderPlot({
    if (!is.null(maintenance_summary)) {
      selected <- maintenance_summary %>% filter(MachineID == input$machine_id)
      if (!is.null(selected$ProphetPlot[[1]])) {
        print(selected$ProphetPlot[[1]])
      } else {
        plot.new()
        text(0.5, 0.5, "No forecast available", cex = 1.5)
      }
    }
  })
}

# ---------------------- Launch ----------------------
shinyApp(ui, server)

