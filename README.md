# Car-Data
## Contributors
Hannah Maurer, Logan Schulz, Tsion Nigate, and Reid McNeill
## Introduction
For this project, we sat at the radar detector and recorded the date, time, speed, orange light, license plate by state, weather, and temperature of the different cars passing through. 
## Dictionary
- Radar detector located near 30th St and 24th Avenue in Rock Island IL
- Collected at least 50 cars per person(totaling more than 200)
## Data Organization
1. We started by merging our data into one Excel file. Then we exported it to a csv file before uploading it to our GitHub. 

2. We then download the file to our local machine. 

```
download <- function(name) {
  url <- "https://github.com/hannahmaurer/Car-Data/raw/main/"
  download.file(paste0(url, name), paste0("data/", name), quiet = TRUE)
}

download("MergedCarData.csv")
```
  
2. Once we got the data, we then pull it into R. 

```
df <- data.frame() # Created earlier
abba_data <- read.csv('data/MergedCarData.csv')
df <- rbind(df, abba_data) # Allows us to insert more data in the future
```

3. We then ensure the data is all the correct values.

```
df$Date <- as.Date(df$Date)
df$Time <- as.Date(df$Date)
df$Speed <- as.numeric(df$Speed)
df$Orange.Light <- as.logical(df$Orange.Light)
df$Temperature <- as.numeric(df$Temperature)
```

3. We then clean the data by fixing misinputs and formatting the time. 

```
df$State <- replace(df$State, df$State == 'IO', 'IA') # Fix IA being IO
df$Weather <- capitalize_words(df$Weather) # Make first letter capitalize
df$Time <- format(df$Time, "%H:%M:%S")
```

4. We then began creating the ui for the shiny app. Writing code for the application title and the output for results displayed in the main panel.

```
ui <- fluidPage(
  titlePanel("Speed Analysis"),
  mainPanel(
    h3("Results:"),
    verbatimTextOutput("min"),
    verbatimTextOutput("max"),
    verbatimTextOutput("median"),
    verbatimTextOutput("mean"),
    plotOutput("histogram")
  )
)
```

5. Then We created the server for the shiny app. We did this by creating a function to calculate min, max, median, and mean from an Excel sheet. Then make a path to the Excel file so it can be extracted from the speed column. Then finally put the calculations statistics and render outputs so it can all be shown in a histogram. 

```
server <- function(input, output) {
  calculate_stats <- function(file_path) {
    data <- read_excel(file_path)
    speed <- data$Speed
    min_val <- min(speed)
    max_val <- max(speed)
    median_val <- median(speed)
    mean_val <- round(mean(speed), digits = 0)  # Round mean to whole number
    return(list(min = min_val, max = max_val, median = median_val, mean = mean_val, speeds = speed))
  }
  df <- "MergedCarData.xlsx"
  output$min <- renderPrint({ paste("Minimum Speed:", calculate_stats(df)$min) })
  output$max <- renderPrint({ paste("Maximum Speed:", calculate_stats(df)$max) })
  output$median <- renderPrint({ paste("Median Speed:", calculate_stats(df)$median) })
  output$mean <- renderPrint({ paste("Mean Speed:", calculate_stats(df)$mean) })
  output$histogram <- renderPlot({
    speeds <- calculate_stats(df)$speeds
    hist(speeds, main = "Distribution of Speeds", xlab = "Speed", ylab = "Frequency", col = "skyblue")
  })
}
```

6. Finally we end our code with running the application.

`shinyApp(ui = ui, server = server)`

## Shiny Results with Chart
<img width="431" alt="image" src="https://github.com/hannahmaurer/Car-Data/assets/159860800/3f82d14a-b2eb-47ba-9ecf-d47e65f876b1">

1. In the first image shows the calculations of minimum, maximum, median, and mean of the cars speed.

<img width="452" alt="image" src="https://github.com/hannahmaurer/Car-Data/assets/159860800/0ddfa977-676a-4858-abcb-543d5b2f6445">

2. In the second image shows a histogram of the frequency of speed for the cars.

# Combined Data 
## Data Organization
1. First we began by capitalizing the words.

```
capitalize_words <- function(word_list) {
  sapply(word_list, function(word) {
    paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "")
  })
}
```

2. We then added and renamed new columns to the data frames.

```
combine_dataframes <- function(df1, df2) {
  new_columns <- setdiff(names(df2), names(df1))
  
   colnames(df2) <- names(df1)
  
    for (col in new_columns) {
    df1[[col]] <- NA
  }
  
    combined_df <- bind_rows(df1, df2)
  
  return(combined_df)
}
```

3. We then had to create a code that converts the time so they are all the same format to use.

```
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
```

4. We then had to add a column to each set of data stating what data came from which group.

```
abba_data$Group <- 8
group_1$Group <- 1
group_2$Group <- 2
group_3$Group <- 3
group_4$Group <- 4
group_5$Group <- 5
group_6$Group <- 6
group_7$Group <- 7
```

5. We then began to bind all the data together. 

```
df <- data.frame()
df <- bind_rows(df, abba_data)
df <- bind_rows(df, group_1)
df <- bind_rows(df, group_2)
df <- bind_rows(df, group_3)
df <- bind_rows(df, group_4)
df <- bind_rows(df, group_5)
df <- bind_rows(df, group_6)
df <- bind_rows(df, group_7)
```

6. We then removed any extra rows that were irrelevant.

```
columns_to_remove <- c("CarNumber", "V1", "V2", "V3", "V4", "V5", "V6")
df <- df[, !names(df) %in% columns_to_remove]
```

7. Refactored all the time to be 24 hours.

```
df$Time <- sapply(df$Time, convert_percentage_to_time)
```

8. Then assigned values and cleaned the data.

```
df$Date <- as.Date(df$Date)
df$Speed <- as.numeric(df$Speed)
df$Orange.Light <- as.logical(df$Orange.Light)
df$Temperature <- as.numeric(df$Temperature)
df$State <- replace(df$State, df$State == 'IO', 'IA') # Fix IA being IO
df$Weather <- replace(df$Weather, df$Weather %in% c('Clear skies, sundown', 'Sunny, clear skies'), 'Sunny') # Keep it consistent 
df$Weather <- capitalize_words(df$Weather) # Make first letter capitalize
df$Color <- capitalize_words(df$Color)
df$Type <- capitalize_words(df$Type)
df$Manufacturer <- capitalize_words(df$Manufacturer)
df$Day <- capitalize_words(df$Day)
```

9. We then got the column names for the dropdown menu.

```
column_names <- colnames(df)
```

10. We then edited our ui to work with all data.

```
ui <- fluidPage(
  
  # Application title
  titlePanel("Speed Analysis"),
  
  fluidRow(
    column(2,
           selectInput("X", "Choose X", column_names, column_names[1]),
           selectInput("Y", "Choose Y", column_names, column_names[3]),
           selectInput("Splitby", "Split By", column_names, column_names[3])),
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
```

11. We then updated the server to work with the new data.

```
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
                                         options = list(pageLength = 4))
}
```

12. Finally we finish off by running the application. 

```
shinyApp(ui, server)
```

## Conclusion
We were initially concerned about whether we had gathered sufficient data to detect any significant speed variations among drivers, which could potentially indicate a safety issue. However, after collecting data over a span of four days with our group, we observed that the speed distribution among cars was fairly consistent. Once we compiled all the Excel data collected by our group across various days of the week, we confirmed our hypothesis that the speed distribution follows a relatively normal pattern.
