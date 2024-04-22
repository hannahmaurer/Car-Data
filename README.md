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
In the ALLCARDATA.R, all data files are read into separate data frames (abba_data, gr1, ..., gr7). 
Each data frame is then cleaned individually before being combined directly into a single data frame (combined_df).
```
dfs <- list(abba_data_clean, gr1_clean, gr2_clean, gr3_clean, gr4_clean, gr5_clean, gr6_clean, gr7_clean)
combined_df <- bind_rows(dfs)
```
Our new code snippet directly read each data file into a separate data frame, cleaned it individually, and then merged the cleaned data frames directly into a single data frame (combined_df). 


##Conclusionn 
We were initially concerned about whether we had gathered sufficient data to detect any significant speed variations among drivers, which could potentially indicate a safety issue. However, after collecting data over a span of four days with our group, we observed that the speed distribution among cars was fairly consistent. Once we compiled all the Excel data collected by our group across various days of the week, we confirmed our hypothesis that the speed distribution follows a relatively normal pattern.
