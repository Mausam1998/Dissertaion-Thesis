library(readxl)
library(tidyverse)

# Load the dataset
df <- read_excel("d:/settings/mktmg/Downloads/Traveler Booking Preferences Study_August 22, 2024_07.00.xlsx")

# Initialize a new data frame with the specified columns
new.df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(new.df) <- c("respondent id", "set number/question",  "card", "price", "rating", 
                      "eco-friendly", "online review", "online presence", "choice", "age")

# Number of respondents
n_respondents <- nrow(df)

for (i in 2:n_respondents) {
  
  # Check for Q21
  if (!is.na(df[i,"Q21"])) {
    if (df[i,"Q21"] == "1.0") {choice <- c(1,0)} else {choice <- c(0,1)}
    
    new.df <- rbind(new.df, data.frame(`respondent id` = df[i,'ResponseId'], `set number/question` = 21, card = 1, 
      price = '£150', 
      rating = '3.5 to 4.5 stars', 
      `eco-friendly` = 'Basic(Energy Efficient Lighting)', 
      `online review` = 'Avg: Mixed Reviews', 
      `online presence` = 'low', 
      choice = choice[1], 
      age = df[i,'Q1...19']
    ))
    
    new.df <- rbind(new.df, data.frame(
      `respondent id` = df[i,'ResponseId'], 
      `set number/question` = 21, 
      card = 2, 
      price = '£150', 
      rating = '3.5 or lower stars', 
      `eco-friendly` = 'High(Solar power and Water conservation)', 
      `online review` = 'poor', 
      `online presence` = 'High', 
      choice = choice[2], 
      age = df[i,'Q1...19']
    ))
  }
  
}


# View the new dataframe
head(new.df)
View(new.df)
