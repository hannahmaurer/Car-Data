library(ggplot2)
library(shiny)
library(DT)
library(readxl)
library(dplyr)

#setwd('C:/Users/stick/Documents/GitHub/Car-Data')
dir.create('data')

capitalize_words <- function(word_list) {
  sapply(word_list, function(word) {
    paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "")
  })
}

combine_dataframes <- function(df1, df2) {
  # Identify the new columns in df2
  new_columns <- setdiff(names(df2), names(df1))
  
  # Rename columns of df2 to match df1
  colnames(df2) <- names(df1)
  
  # Add the new columns to df1
  for (col in new_columns) {
    df1[[col]] <- NA
  }
  
  # Append the rows of df2 to df1
  combined_df <- bind_rows(df1, df2)
  
  return(combined_df)
}

# This horror of a creature converts all time stuff to stuff we can actually use.
# This can convert percentages of a day (like .27)
# Or if it's 9 PM, turn it into 24 hours
convert_percentage_to_time <- function(time_percentage) {
  if (is.null(time_percentage)) return(time_percentage)
  if (!grepl(":", time_percentage)) {
    time_percentage <- as.numeric(time_percentage)
    hour <- floor(time_percentage * 24)
    minute <- round((time_percentage * 24 - hour) * 60)
    if (hour < 8) {
      hour <- hour + 12
    }
    time <- sprintf("%02d:%02d:00", hour, minute)
  } else {
    if (grepl("PM", time_percentage, fixed = TRUE)) {
      time <- gsub(" PM", "", time_percentage)
      if (time != "12:00") {
        hour <- as.numeric(strsplit(time, ":")[[1]][1]) + 12
        if (hour == 24) hour <- 0
        time <- sprintf("%02d:%s:00", hour, strsplit(time, ":")[[1]][2])
      }
    } else if (grepl("AM", time_percentage, fixed = TRUE)) {
      time <- gsub(" AM", "", time_percentage)
      if (time == "12:00") {
        time <- "00:00:00"
      } else if (as.numeric(strsplit(time, ":")[[1]][1]) < 8) {
        hour <- as.numeric(strsplit(time, ":")[[1]][1]) + 12
        time <- sprintf("%02d:%s:00", hour, strsplit(time, ":")[[1]][2])
      }
    } else {
      time <- time_percentage
      hour <- as.numeric(strsplit(time, ":")[[1]][1])
      if (hour < 8) {
        hour <- hour + 12
      }
      time <- sprintf("%02d:%s:00", hour, strsplit(time, ":")[[1]][2])
    }
  }
  return(time)
}

# Get group data (From the excel and csv, this is a cluster and a half right here)
group_1 <- read_excel('data/Car_Data.xlsx', .name_repair = 'universal', col_types = c("text"))
group_2 <- read_excel('data/Car.xlsx', .name_repair = 'universal', skip = 1, col_types = c("text"), col_names = c("Speed", "Orange.Light", "Color", "Manufacturer", "Type", "Day", "Time", "Weather", "Temperature", "Name"))
group_3 <- read_excel('data/counting_cars.xlsx', .name_repair = 'universal', col_types = c("text"), skip = 1, col_names = c("Date", "Speed", "Time", "Temperature", "Weather", "V1", "V2", "V3", "V4", "V5", "V6"))
group_6 <- read_excel('data/Speed analyst 332 Car Data.xlsx', .name_repair = 'universal', skip = 1, col_types = c("text"), col_names = c("Name", "Date", "Speed", "Time", "Type", "Orange.Light", "Temperature", "Weather"))
group_5 <- read_excel('data/CarData  (2).xlsx', .name_repair = 'universal', col_types = c("text"), skip = 1, col_names = c("Date", "Temperature", "Weather", "Time", "Speed", "Color", "State", "Name"))

group_4 <- read.csv('data/IRL_Car_Data.csv', colClasses = "character", col.names = c("Speed", "Time", "Temperature", "Weather", "Day", "Name"))
abba_data <- read.csv('data/MergedCarData.csv', stringsAsFactors = FALSE, colClasses = "character")
group_7 <- read.csv('data/UpdatedCarTracking.csv', stringsAsFactors = FALSE, colClasses = "character", col.names = c("CarNumber", "Time", "Temperature", "Type", "Speed", "Name"))

# Add all the group data into it
abba_data$Group <- 8
group_1$Group <- 1
group_2$Group <- 2
group_3$Group <- 3
group_4$Group <- 4
group_5$Group <- 5
group_6$Group <- 6
group_7$Group <- 7

# New empty frame
df <- data.frame()
# ABBA Data
df <- bind_rows(df, abba_data)
# Append all the data
df <- bind_rows(df, group_1)
df <- bind_rows(df, group_2)
df <- bind_rows(df, group_3)
df <- bind_rows(df, group_4)
df <- bind_rows(df, group_5)
df <- bind_rows(df, group_6)
df <- bind_rows(df, group_7)

# Drop any useless to us columns. These are rows that are NA all around or that are useless to any combined research. 
columns_to_remove <- c("CarNumber", "V1", "V2", "V3", "V4", "V5", "V6")
df <- df[, !names(df) %in% columns_to_remove]

# Refactor all the time to be 24 hours
df$Time <- sapply(df$Time, convert_percentage_to_time)

# Assign Values
df$Date <- as.Date(df$Date)
df$Speed <- as.numeric(df$Speed)
df$Orange.Light <- as.logical(df$Orange.Light)
df$Temperature <- as.numeric(df$Temperature)

# Clean all the data
df$State <- replace(df$State, df$State == 'IO', 'IA') # Fix IA being IO
df$Weather <- replace(df$Weather, df$Weather %in% c('Clear skies, sundown', 'Sunny, clear skies'), 'Sunny') # Keep it consistent 
df$Weather <- capitalize_words(df$Weather) # Make first letter capitalize
df$Color <- capitalize_words(df$Color)
df$Type <- capitalize_words(df$Type)
df$Manufacturer <- capitalize_words(df$Manufacturer)
df$Day <- capitalize_words(df$Day)

# Get column names for dropdown menus
column_names <- colnames(df)

ui <- fluidPage(
  
  # Application title
  titlePanel("Speed Analysis"),
  tabsetPanel(
    tabPanel("Sandbox", 
             fluidPage(
               titlePanel("The Data Sandbox"), 
               mainPanel(
                fluidRow(
                  column(2,
                      selectInput("X", "Choose X", column_names, column_names[1]),
                      selectInput("Y", "Choose Y", column_names, column_names[3]),
                      selectInput("Splitby", "Split By", column_names, column_names[3])),
                  column(4, plotOutput("plot_01")),
                  column(6, DT::dataTableOutput("table_01", width = "100%"))
                ),
               )
             )
    ),
    tabPanel("Important Findings", 
             fluidPage(
               titlePanel("Important Findings"), 
               mainPanel(
                 h3("Results:"),
                 verbatimTextOutput("min"),
                 verbatimTextOutput("max"),
                 verbatimTextOutput("median"),
                 verbatimTextOutput("mean"),
                 plotOutput("histogram")
               )
             )
    ),
    tabPanel("Essay", 
             fluidPage(
               titlePanel("Essay"), 
               mainPanel(
                 p("This paper is also available in Google Doc format: "),
                 p("https://docs.google.com/document/d/15i1i3AIdO7DNEd4U7Oc_HgTqgV2t2j6VjaFmQWMsqlQ/edit?usp=sharing"),
                 h4("Introduction"),
                 p("Risk Management for road accidents is important 
                      and placing a speed regulation is the easiest way to control drivers. 
                      However, these regulations should be placed with careful 
                      consideration putting to mind factors such as road conditions, 
                      traffic patterns, and the surrounding environment. 
                      Previous research conducted in South Carolina has found 
                      that speed radar signs have shown that they may decrease 
                      speeds in the range between 3 to 10 miles per hour (Sorrell et al). 
                      We did this analysis to see if drivers followed these regulations for their safety. 
                      We recorded data on cars near 30th St and 24th Avenue in Rock Island IL. 
                      Collecting variables such as speed, flashing orange light, state plate, 
                      weather, time, and date in addition to the cars passing the speed limit. 
                      We conducted this study to evaluate safety issues and identify drivers 
                      responses to acknowledging their speed in a speed limited zone."),
                 h4("Method"),
                 p("At the point of collection, we used Google Sheets to record our data. We each had a sheet to record our data and names. We sat in front of the sign, in our cars, at different times throughout the week and recorded the first 50 cars that passed by. We recorded the date, time, speed, if the orange light went off, the state-issued car plate going by the state’s initials, the weather, and then the temperature outside. Afterward, we gathered all of our data into one CSV file for easy access and uploaded it to GitHub so that it could be downloaded at any time. We then assign values to each of the columns, to ensure that they have been inputted correctly and so R will format them correctly. Finally, we cleaned the data. In some of our data for license plates, there was IO instead of IA, so we ran a simple replace function to find IO and replace it with IA. Then the weather wasn’t always capitalized, so we used a custom capitalize words function to uppercase the first letter. And finally, we formatted the time to just show the hours, minutes, and seconds. 
Once all of the data has been cleaned, we created a server and ui function to calculate the minimum, maximum, median, and mean that the inputted data has. The shiny app generated the mean median mode and average for us.  Additionally, we utilized dropdown menus to allow users to choose the variables they want to analyze. The app generates our plots and table based on the selected variable by the user.  We used the calculate_stats function which helped calculate a summary stats for our datasets while the renderPlot and renderTAble functions generated plots and tables based on the input of the user. This allows users to understand and analyze the data easily. There is also a histogram to show the distribution of the data (to help determine if it’s normal or skewed). "),
                 h4("Results"),
                 p("The speed limit on the road was 35, with the lowest speed recorded being 12 mph and the fastest being 50. The median and mean are both 33, below the speed limit. The tendency for most drivers to operate slightly under the speed limit can contribute positively to overall road safety. This cautious approach allows for better reaction times, reduces the likelihood of accidents, and fosters a smoother flow of traffic, enhancing the safety of all road users. We hope while putting together the collected data from our class, the days are different.  This is important to us as if we have a lot of similar day and time data collection, we will just end up with duplicates. Other than that we can make a good analysis to figure out the trends with our variables."),
                 h4("Citations"),
                 p("Speed study data collection. Speed Study Data Collection | FHWA. (n.d.). https://highways.dot.gov/safety/speed-management/methods-and-practices-setting-speed-limits-informational-report/speed-study 
Sorrell, Mark, et al. Use of Radar Equipped Portable Changeable Message Sign to Reduce Vehicle Speed in South Carolina Work Zones. 2006.")
               )
             )
    )
  )
  

  
  # Output: Results displayed in main panel

)

# Define server logic
server <- function(input, output) {
  
  calculate_stats <- function(data) {
    speed <- data$Speed
    min_val <- min(speed)
    max_val <- max(speed)
    median_val <- median(speed)
    mean_val <- round(mean(speed), digits = 0)  # Round mean to whole number
    return(list(min = min_val, max = max_val, median = median_val, mean = mean_val, speeds = speed))
  }
  
  # Render the ggplot plot
  output$plot_01 <- renderPlot({
    ggplot(df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point(na.rm = FALSE)
  })
  
  # Calculate statistics and render outputs
  output$min <- renderPrint({ paste("Minimum Speed:", calculate_stats(df)$min) })
  output$max <- renderPrint({ paste("Maximum Speed:", calculate_stats(df)$max) })
  output$median <- renderPrint({ paste("Median Speed:", calculate_stats(df)$median) })
  output$mean <- renderPrint({ paste("Mean Speed:", calculate_stats(df)$mean) })
  output$histogram <- renderPlot({
    speeds <- calculate_stats(df)$speeds
    hist(speeds, main = "Distribution of Speeds", xlab = "Speed", ylab = "Frequency", col = "skyblue")
  })
  output$table_01 <- DT::renderDataTable(df[, c(input$X, input$Y, input$Splitby)], 
                                         options = list(pageLength = 25))
}

# Run the application
shinyApp(ui, server)