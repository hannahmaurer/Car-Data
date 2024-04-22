library(ggplot2)
library(shiny)
library(DT)
library(readxl)
library(dplyr)

setwd('C:/Users/hanna/Documents/DATA 332/CarData')

# ABBA Data
abba_data <- read_excel('MergedCarData.xlsx', .name_repair = 'universal')

#All group data
gr1 <- read_excel('Car Data Excel.xlsx', .name_repair = 'universal')
gr2 <- read_excel('Car.xlsx', .name_repair = 'universal')  
gr3 <- read_excel('CarData2.xlsx', .name_repair = 'universal')
gr4 <- read_excel('counting_cars.xlsx', .name_repair = 'universal')
gr5 <- read_excel('Speed analyst 332 Car Data.xlsx', .name_repair = 'universal')
gr6 <- read.csv("IRL_Car_Data.csv")
gr7 <- read.csv("UpdatedCarTracking.csv")

#Clean data so they have same columns
gr1_clean <- subset(gr1, select = -c(Color, License.plate.state, Date, Weather))
gr2_clean <- subset(gr2, select = -c(Flashing.Light, Manufacturer, Vehicle.Color, Vehicle.Type, Day, Weather, Collector.Name))
gr3_clean <- subset(gr3, select = -c(Color, License.plate.state, Date, Weather, Name))
gr4_clean <- subset(gr4, select = -c(...6, ...7, ...8, ...9, ...10, ...11, Date, Weather))
gr5_clean <- subset(gr5, select = -c(Type.of.se, Orange.Light, Student, Date, Weather))
gr6_clean <- subset(gr6, select = -c(Wheater, Week.Day, Collector))
gr7_clean <- subset(gr7, select = -c(Car.Number, Type.of.Car, Name))
abba_data_clean <- subset(abba_data, select = -c(Orange.Light, State, Date, Weather, Name))

#Change Column names
names(abba_data_clean)[names(abba_data_clean) == "Speed"] <- "MPH"
names(gr1_clean)[names(gr1_clean) == "Speed"] <- "MPH"
names(gr2_clean)[names(gr2_clean) == "Speed.MPH"] <- "MPH"
names(gr3_clean)[names(gr3_clean) == "Speed"] <- "MPH"
names(gr4_clean)[names(gr4_clean) == "Temp"] <- "Temperature"                
names(gr5_clean)[names(gr5_clean) == "Time.of.Day"] <- "Time"
names(gr6_clean)[names(gr6_clean) == "Time.of.Day"] <- "Time"
names(gr7_clean)[names(gr7_clean) == "Time.of.Day"] <- "Time"
names(gr7_clean)[names(gr7_clean) == "Speed..mph."] <- "MPH"
names(gr7_clean)[names(gr7_clean) == "Weather"] <- "Temperature"

#Add group # column 
abba_data_clean$Group <- 8
gr1_clean$Group <- 1
gr2_clean$Group <- 2
gr3_clean$Group <- 3
gr4_clean$Group <- 4
gr5_clean$Group <- 5
gr6_clean$Group <- 6
gr7_clean$Group <- 7



#Combine df into list
dfs <- list(abba_data_clean, gr1_clean,gr2_clean,gr3_clean,gr4_clean,gr5_clean,gr6_clean,gr7_clean)

# Function to reorder columns and ensure consistent data type for the "Time" column
reorder_and_convert_time_column <- function(df) {
  df$Time <- as.character(df$Time)  # Convert "Time" column to character
    df <- df[, c("Time", "MPH", "Temperature", "Group")]  # Reorder columns
  return(df)
}

# Apply the reorder_and_convert_time_column function to each data frame in the list
dfs <- lapply(dfs, reorder_and_convert_time_column)

# Combine data frames into a single data frame
combined_df <- bind_rows(dfs)

# Reset row names
rownames(combined_df) <- NULL






# Get column names for dropdown menus
column_names <- colnames(combined_df)

ui <- fluidPage(
  
  # Application title
  titlePanel("Speed Analysis"),
  
  fluidRow(
    column(2,
           selectInput("X", "Choose X", column_names, selected = column_names[1]),
           selectInput("Y", "Choose Y", column_names, selected = column_names[3]),
           selectInput("Splitby", "Split By", column_names, selected = column_names[3])),
    column(4, plotOutput("plot_01")),
    column(6, DT::dataTableOutput("table_01", width = "100%"))
  ),
  
  # Output: Results displayed in main panel
  mainPanel(
    h3("Results:"),
    verbatimTextOutput("min"),
    verbatimTextOutput("max"),
    verbatimTextOutput("median"),
    verbatimTextOutput("mean"),
    plotOutput("histogram")
  )
)

# Define server logic
server <- function(input, output) {
  
  calculate_stats <- function(data) {
    speed <- combined_df$MPH
    min_val <- min(speed)
    max_val <- max(speed)
    median_val <- median(speed)
    mean_val <- round(mean(speed), digits = 0)  # Round mean to whole number
    return(list(min = min_val, max = max_val, median = median_val, mean = mean_val, speeds = speed))
  }
  
  # Render the ggplot plot
  output$plot_01 <- renderPlot({
    ggplot(combined_df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point()
  })
  
  # Calculate statistics and render outputs
  output$min <- renderPrint({ paste("Minimum Speed:", calculate_stats(combined_df)$min) })
  output$max <- renderPrint({ paste("Maximum Speed:", calculate_stats(combined_df)$max) })
  output$median <- renderPrint({ paste("Median Speed:", calculate_stats(combined_df)$median) })
  output$mean <- renderPrint({ paste("Mean Speed:", calculate_stats(combined_df)$mean) })
  output$histogram <- renderPlot({
    speeds <- calculate_stats(combined_df)$speeds
    hist(speeds, main = "Distribution of Speeds", xlab = "Speed", ylab = "Frequency", col = "skyblue")
  })
  output$table_01 <- DT::renderDataTable(combined_df[, c(input$X, input$Y, input$Splitby)], 
                                         options = list(pageLength = 4))
}

# Run the application
shinyApp(ui, server)
