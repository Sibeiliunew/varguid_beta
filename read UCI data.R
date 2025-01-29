# src: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/

library(readxl)
library(openxlsx)
library(mlbench)
library(tidyverse)
auto_mpg_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"

cols <- c("mpg",
          "cylinders",
          "displacement",
          "horsepower",
          "weight",
          "acceleration",
          "model year",
          "origin",
          "car name")

auto_mpg <- read.table(auto_mpg_url,
                       header = FALSE,
                       col.names = cols,
                       na.strings = "?",
                       stringsAsFactors = FALSE)

auto_mpg <- tibble::as_tibble(auto_mpg)
auto_mpg <- janitor::clean_names(auto_mpg, "snake")

usethis::use_data(auto_mpg, overwrite = TRUE)


final=auto_mpg %>% relocate(mpg, .after = last_col())

#write.xlsx(final,"./20240424/chem.xlsx")

data <-read_excel("./20240424/chem.xlsx") 
colnames(data)="Long_Position"
data=data %>%  separate(Long_Position,into=c("Long_Position","Prismatic coefficient","length-displacement ratio",
                                                                  "bean-draught ratio","length-bean ratio","froude number","residuary resistance"),sep = " ") 


colnames(dat2)=c("Long Position","Prismatic coefficient","length-displacement ratio",
                 "bean-draught ratio","length-bean ratio","froude number","residuary resistance")
data2=apply(data,2, as.numeric) %>% as.data.frame()

write.xlsx(data2,"./20240424/chem.xlsx")

c1=read_excel("./20240424/c1.xlsx")
c2=read_excel("./20240424/c2.xlsx")
c=cbind(c1,c2) %>% janitor::clean_names()

write.xlsx(c,"./20240424/for.xlsx")

f=read_excel("./20240424/for.xlsx")
