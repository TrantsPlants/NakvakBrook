# Analysis of shrub establishment and growth data - Supplementary Figure S5a
# Emma Davis - emmalaureldavis@gmail.com

# Load packages
library(readr)
library(dplR)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(dplyr)

# Load up data
rm(list=ls())

nb1<-read.rwl("AllSamplesNB1.csv")
nb2<-read.rwl("AllSamplesNB2.csv")
nb3<-read.rwl("AllSamplesNB3.csv")
ind.samp <- read.csv("AllIndSamples.csv")

#Bind data from 3 transects together into site-level
all <- rbind(nb1, nb2, nb3)

#Establishment plot - Histogram with climate overlay
pal <- c('#4A6990FF', '#8F7700FF', '#CD534CFF')  

ggplot(ind.samp, aes(x = MeanEst, fill = SPP))+
  geom_histogram(binwidth = 0.8, color = 'black', size = 0.2)+
  #ggtitle("Shrub establishment dates by species")+
  scale_fill_npg(alpha = 0.8)+
  xlab('Establishment year')+
  ylab('No. ramets established')+
  theme_pubr(base_size = 10, legend = 'bottom')+
  labs(fill = "Species")

clim <- read_csv("~/NakvakBrook/ShrubRings/Nakvak_Interp_MoMean.csv")[,-1]

ave.clim <- clim %>%
  filter(., Year > 1996) %>%
  mutate(Tave_07_scale = Tave_07-9.071991) %>%
  
mean(ave.clim$Tave_07)

ggplot()+
  geom_histogram(data = ind.samp, aes(x = MeanEst, fill = SPP), binwidth = 1, color = 'black', size = 0.2)+
  #ggtitle("Shrub establishment dates by species")+
  scale_fill_npg(alpha = 0.7)+
  xlab('Establishment year')+
  ylab('No. ramets established')+
  labs(fill = "Species")+
  geom_line(data = filter(clim, Year > 1996), aes(x = Year, y = (Tave_07-5)))+
  xlim(1996, 2015)+
  scale_y_continuous(sec.axis = sec_axis(~.+5, name = "August temperature (°C)"))+
  theme_pubr(base_size = 10, legend = 'bottom')

#5 x 5 is good

#write.csv(all, 'AllSamples.csv')


