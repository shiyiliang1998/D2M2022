
library(tidyverse)
library(readxl)

data.path <- "datasets/"

data1 <- read.csv(paste0(raw.data.path,"MM Data.csv"))

data1 <- data1 %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange","Yellow", "Brown"),
               names_to = "Color", values_to = "Number") 
data1 %>%
ggplot(aes(Color, Number))+
  scale_color_manual(values = c("blue", "brown", "green","orange", "red", "yellow"))+ 
  geom_violin(aes(color=Color))+
  geom_jitter(color= color)


