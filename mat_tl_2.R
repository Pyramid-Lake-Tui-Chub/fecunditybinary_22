#load packages!
library(reshape2)
library(ggplot2)
library(dplyr)
library(chron)
library(lubridate)
library(plotrix)
library(RColorBrewer)
library(ggrepel)
library(directlabels)
library(tidyverse)

# change expressing column from y/n to 1/0
mat_tl_2 <- mat_tl_2 %>% mutate(exp_num = ifelse(expressing == "Y", 1, 0))

#no mature individuals in August so I eliminated it
mat_tl_2 <- subset(mat_tl_2, month != 8)

#plot maturity as 0 AND 1 by length, wrap by month
mon_names<- c( "5"="May", "6"="June", "7"="July", "8"="August")
mat_tl_2_plot <- ggplot(data= mat_tl_2, aes(x=length, y=exp_num)) +
  facet_wrap(~month, labeller=as_labeller(mon_names), ncol=1) +
  geom_point(alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x= element_text(size=10, angle=45, hjust=1))+
  scale_x_continuous(breaks = seq(0, 430, 10)) +
  scale_y_continuous(breaks = c(0,1), labels = c("Not Fecund", "Fecund")) +
  labs( x= "Total Length (mm)", y= "") 
mat_tl_2_plot

#export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")
png(filename = "Maturity_numeric.png", units = "in", width = 8, height = 6, res=300)
mat_tl_2_plot
dev.off()