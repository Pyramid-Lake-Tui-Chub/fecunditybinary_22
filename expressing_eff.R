#load packages!
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(cowplot)
library(data.table)

#make columns for percentages by maturity code
total <- length(expressing_eff$tl)
by_matcode <- aggregate(tl~sex, data=expressing_eff, length)
names(by_matcode)[2] <- c("count_group")
expressing_eff <- merge(x=expressing_eff, y=by_matcode, by = c('sex'))
by_matcode <- by_matcode %>% mutate(perc=count_group/188)

#barplot
exp_eff_plot <- ggplot(data=by_matcode, aes(x= sex, y= perc)) + 
  geom_bar(position="stack", stat="identity", alpha=0.7, fill = "brown2")+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="% of Total Sacrificed Not Expressing", 
       title = "Percent of Sacrificed Not Expressing Tui Chub by Maturity Class")
exp_eff_plot

# convert matcodes into 0/1 for analysis
expressing_eff <- expressing_eff %>% mutate(tl_code = ifelse(sex == "F" | sex == "M", 1, 0))

#summer month names
mon_names<- c( "5"="May", "6"="June", "7"="July", "8"="August")

#length 0/1 maturity graph but only with fish that are not expressing that I know sex information for
exp_tl_plot <- ggplot(data= expressing_eff, aes(x=tl, y=tl_code)) +
  facet_wrap(~month, labeller=as_labeller(mon_names), ncol=1) +
  geom_point(alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x= element_text(size=10, angle=45, hjust=1))+
  scale_x_continuous(breaks = seq(0, 430, 10)) +
  scale_y_continuous(breaks = c(0,1), labels = c("Immature or Unknown", "Male or Female")) +
  labs( x= "Total Length (mm)", y= "") 
exp_tl_plot

#dont break out by month
exp_tl_2_plot <- ggplot(data= expressing_eff, aes(x=tl, y=tl_code)) +
  geom_point(alpha=0.5) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x= element_text(size=10, angle=45, hjust=1))+
  scale_x_continuous(breaks = seq(0, 430, 10)) +
  scale_y_continuous(breaks = c(0,1), labels = c("Immature or Unknown", "Male or Female")) +
  labs( x= "Total Length (mm)", y= "") 
exp_tl_2_plot