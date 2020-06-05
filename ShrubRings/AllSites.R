# Analysis of shrub establishment and growth data
# Emma Davis - emmalaureldavis@gmail.com

# Load packages
library(readr)
library(dplR)
library(ggplot2)
library(ggsci)
library(ggpubr)

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
  scale_fill_jco(alpha = 0.8)+
  xlab('Establishment year')+
  ylab('No. ramets established')+
  theme_pubr(base_size = 10, legend = 'bottom')+
  labs(fill = "Species")

clim <- read_csv("Nakvak_Interp_MoMean.csv")

ave.clim <- clim %>%
  filter(., Year > 1996) %>%
  mutate(Tave_07_scale = Tave_07-9.071991)
mean(ave.clim$Tave_07)

ggplot()+
  geom_histogram(data = ind.samp, aes(x = MeanEst, fill = SPP), binwidth = 1, color = 'black', size = 0.2)+
  #ggtitle("Shrub establishment dates by species")+
  scale_fill_jco(alpha = 0.8)+
  xlab('Establishment year')+
  ylab('No. ramets established')+
  labs(fill = "Species")+
  geom_line(data = filter(clim, Year > 1996), aes(x = Year, y = (Tave_07-5)))+
  xlim(1996, 2015)+
  scale_y_continuous(sec.axis = sec_axis(~.+5, name = "August temperature (°C)"))+
  theme_pubr(base_size = 10, legend = 'bottom')

#5 x 5 is good

ind.sum <- ind.samp %>%
  group_by(MeanEst) %>%
  summarize(N = n())

names(ind.sum) <- c('Year', 'n')

test <- merge(ind.sum, clim, by = 'Year', all.y = TRUE)


###
ggplot(ind.samp, aes(LocationX, LocationY))+
  geom_point(aes(size = Area, color = Species), alpha = .8)+
  scale_size_area()+
  scale_color_jco()+
  facet_wrap(~Site)+
  ylim(-60, 70)+
  xlim(-60, 70)

#write.csv(all, 'AllSamples.csv')

ggplot(filter(all, Height == 0), aes(Sample, MeanEst))+
  geom_point(aes(color = Site))+
  ylab("Est. year")+
  xlab("Sample")+
  ggtitle("NB Cores")

ggplot(filter(all, Height == 0), aes(MeanEst))+
  geom_histogram(binwidth = 1)+
  ylab("Est. year")+
  xlab("Sample")+
  ggtitle("NB Cores")

ggplot(filter(ind.samp, Species == 'BEGL'), aes(x = (2019-MeanEst), Height.2))+
  geom_smooth(method = lm, color = 'black')+
  geom_point(aes(color = Species),size = 2)+
  xlim(8, 18)+
  scale_color_jco()+
  xlab('Establishment year')+
  ylab('Height (m)')

#Shrub size
size <- ind.samp %>%
  mutate(Volume = Height.2*AreaL*AreaW) %>%
  filter(., Species == 'BEGL') %>%
  filter(!ID %in% c('NB2-4', 'NB3-9'))

ggplot(size, aes(x = MeanEst, Height.2))+
  geom_smooth(method = lm, color = 'black')+
  geom_point(aes(color = Species),size = 2)+
  scale_color_jco()+
  xlab('Establishment year')+
  ylab('Shrub volume')

summary(lm(data = size, Area ~ MeanEst))


