#  LOG: Living Optimally with Gains

# What is needed to make this a repo?
# input and output folder?
# data (old_data) not there it should be generated, same with log
# maybe a config with paths? how do one deal with different computers?

library(tidyverse)
library(glue)

source("config.R")

## exercises
exercises <- c("bench_120kg",
               "dips_40kg",
               "biceps_curls_15kg",
               "pullups_10kg",
               "squats_120kg",
               "straight_wheel")

## checks if all the necessary files exists
# if not thay are created

if(!file.exists(log_path)){
    write(file = log_path, 
          glue("exercise:reps\n{paste(exercises, collapse = ':\n')}:\n")) 
}

if(!file.exists(data_path)){
    write.table(x = data.frame(date = NA, exercise = NA, reps = NA),
                file=data_path, 
                sep = ";")
}

## check if there is a new log
# If there is a new log stuff will happen
# Otherwise do nothing
log <- paste0(readLines(log_path, encoding = "UTF-8"), collapse = "")
new_log <- log != glue("exercise:reps{paste(exercises, collapse = ':')}:")

if(new_log){
    ## get new data
    new_data <- read.table(log_path, sep = ":", header = TRUE)

    new_data_clean <- new_data %>% 
        filter(!is.na(reps))
    ## read and extract new data

    ## get todays date
    today <- Sys.Date()
    formatted_date <- format(today, "%d-%m-%Y")
    new_data$date <- formatted_date
    
    ## add lines to existing dataset
    old_data <- read.table(data_path, sep = ";", header = TRUE)
    data <- rbind(old_data, new_data) %>% 
        filter(!is.na(reps))
    write.table(x = data, file=data_path, sep = ";")

    ## plot
    # all excersises
    fig <- data %>% 
        ggplot(aes(x = as.Date(date, "%d-%m-%Y"), y = reps, color = exercise)) + 
        geom_point(na.rm=TRUE) + 
        geom_line(aes(x = as.Date(date, "%d-%m-%Y"), y = reps), size = 2) +
        geom_text(aes(label = reps), vjust = -1) +
        scale_y_continuous(limits = c(1, 20)) +
        xlab("Date") +
        theme_dark()
    ggsave(output_path, plot = fig, dpi = 300) # saveing the plot
    
    ## overwrite data with new draft
    write(file = log_path, 
          glue("exercise:reps\n{paste(exercises, collapse = ':\n')}:\n"))
}
