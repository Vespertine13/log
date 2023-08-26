library(tidyverse)
library(glue)

source("config.R")
setwd("../mega")

## checks if all the necessary files exist
# if not they are created

if(!file.exists(log_path)){
    write(file = log_path, 
          glue("exercise:repsx3\n{paste(exercises, collapse = ':\n')}:\n")) 
}

if(!file.exists(data_path)){
    write.table(x = data.frame(date = NA, exercise = NA, repsx3 = NA),
                file=data_path, 
                sep = ";")
}

log <- paste0(readLines(log_path, encoding = "UTF-8"), collapse = "")
new_log <- log != glue("exercise:reps{paste(exercises, collapse = ':')}:")


## check if there is a new log
# If there is a new log it will be added to the data
# Otherwise do nothing
if(new_log){
    ## get new data
    new_data <- read.table(log_path, sep = ":", header = TRUE)

    new_data_clean <- new_data %>% 
        filter(!is.na(repsx3)) %>% 
        filter(repsx3 != "")

    for(i in 1:nrow(new_data_clean)){
        new_data_clean$repsx3[i] <- str_split(new_data_clean$repsx3[i], pattern = ",") %>% 
            unlist() %>% 
            as.numeric() %>% 
            sum()
    }
    
    ## get todays date
    today <- Sys.Date()
    formatted_date <- format(today, "%d-%m-%Y")
    new_data_clean$date <- formatted_date
    
    ## add lines to existing dataset
    old_data <- read.table(data_path, sep = ";", header = TRUE)
    data <- rbind(old_data, new_data_clean) %>% 
        filter(!is.na(repsx3))
    write.table(x = data, file=data_path, sep = ";")

    ## plot
    data$repsx3 <- as.numeric(data$repsx3)
    fig <- data %>% 
        ggplot(aes(x = as.Date(date, "%d-%m-%Y"), y = repsx3, color = exercise)) + 
        geom_point() + 
        geom_line(aes(x = as.Date(date, "%d-%m-%Y"), y = repsx3), size = 2, alpha = 0.5) +
        geom_text(aes(label = repsx3), vjust = -1) +
        scale_y_continuous(limits = c(1, 40)) +
        xlab("Date") +
        theme_dark()
    ggsave(output_path, plot = fig, dpi = 300) # saving the plot
    
    ## overwrite data with new draft
    write(file = log_path, 
          glue("exercise:repsx3\n{paste(exercises, collapse = ':\n')}:\n"))
}
