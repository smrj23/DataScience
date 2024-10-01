#####
# Leer la base  

install.packages("readxl")
library("readxl")

my_data <- read_excel(file.choose())
my_data2 <- read_excel(file.choose())
my_data3 <- read_excel(file.choose())

install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)

#####
# Unir tablas

completo <- my_data3 %>% 
  left_join(my_data, by = c('product.id'='bike.id'))

completo <- completo %>% 
  left_join(my_data2, by = c('order.line'='bikeshop.id'))

completo$categoria <- word(completo$description,1)

completo$Revenue <- completo$quantity * completo$price 

Supersix <- completo %>%
   filter(str_detect(model,'Supersix'))

Supersix$States <- str_sub(Supersix$location, -2)

Supersix_FL <- Supersix %>%
  filter(str_detect(States,'FL'))

quantile(Supersix_FL$Revenue)
RQ <- numeric(19)

for (i in 1:19){
   if(Supersix_FL$Revenue[i] > 5865){
     RQ[i] <- 'High'
   }else
     if((Supersix_FL$Revenue[i] <= 5865) & (Supersix_FL$Revenue[i] >= 3200)){
     RQ[i] <- 'Medium'
   }else 
     if(Supersix_FL$Revenue[i] < 3200){
     RQ[i] <- 'Low'
   }
}

Supersix_FL$RQ <-RQ