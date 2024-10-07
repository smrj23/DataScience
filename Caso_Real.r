#####
# Leer la base
getwd()
setwd('C:/Users/xxx/Documents/data_science/xxxx')

install.packages("readxl")
library(readxl)

file.list <- list.files(pattern = '*.xlsx')
df.list <- lapply(file.list, read_excel)

#####
# Unir tablas

library(tidyverse)
library(stringr)
library(dplyr)

completo <- df.list[[3]] %>% 
  left_join(df.list[[1]], by = c('product.id'='bike.id'))

completo <- completo %>%
  left_join(df.list[[2]], by = c('order.line'='bikeshop.id'))

####
# Modificar tabla, crear columnas

completo$categoria <- word(completo$description,1)

completo$Revenue <- completo$quantity * completo$price 

#####
# Subset nueva variable y agregar estados

Supersix <- completo %>%
  filter(str_detect(model,'Supersix'))

Supersix$States <- str_sub(Supersix$location, -2)

#####
# Filtrar y crear funcion de low medium high por cuantile

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
