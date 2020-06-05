# Creating BEGL site chronology and climate analysis
# Emma Davis - emmalaureldavis@gmail.com

# Load packages
library(readr)
library(dplR)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(dplyr)
library(GGally)
library(psych)

#Clear workspace and load in data
rm(list=ls())
setwd("~/NakvakBrook/ShrubRings")

#NB1
nb1.1<-read.rwl("NB1_1.raw")
nb1.2<-read.rwl("NB1_2.raw")
nb1.3<-read.rwl("NB1_3.raw")
nb1.4<-read.rwl("NB1_4.raw")
nb1.5<-read.rwl("NB1_5.raw")
nb1.6<-read.rwl("NB1_6.raw")
nb1.7<-read.rwl("NB1_7.raw")
nb1.8<-read.rwl("NB1_8.raw")
nb1.9<-read.rwl("NB1_9.raw")
nb1.10<-read.rwl("NB1_10.raw")

nb2<- read.rwl("NB2.raw")

nb3<- read.rwl("NB3.raw")

#Combine individual shrub series together 
BEGL.1 <- combine.rwl(nb1.1, nb1.2) %>%
  combine.rwl(., nb1.3) %>%
  combine.rwl(., nb1.4) %>%
  combine.rwl(., nb1.8) %>%
  combine.rwl(., nb1.9) %>%
  combine.rwl(., nb1.10)

#Some summary statistics 
interseries.cor(BEGL.1) 
spag.plot(BEGL.1, zfac = 3)

#Subset BEGL series and combine multiple series per sample using rowMeans
BEGL.1ave <-BEGL.1 %>%
  mutate(NB1100 = rowMeans(.[,1:2], na.rm = TRUE),
         NB1200 = rowMeans(.[,9:10]),
         NB1300 = rowMeans(.[,23:24]),
         NB1400 = rowMeans(.[,31:32]),
         NB1800 = rowMeans(.[,39:40]),
         NB1900 = rowMeans(.[,45:46]),
         NB11000 = rowMeans(.[,53:54])) %>%
  select(., NB1100:NB11000) 

#Set row names to correct years
rownames(BEGL.1ave)<- c(seq(1997,2017)) 

#Repeat for 2 and 3
BEGL.2ave <-nb2 %>%
  mutate(NB20 = rowMeans(.[,1:2], na.rm = TRUE),
         NB22 = rowMeans(.[,3:4]),
         NB23 = rowMeans(.[,5:6]),
         NB24 = rowMeans(.[,7:8])) %>%
  select(., NB20:NB24) 
rownames(BEGL.2ave)<- c(seq(2004,2017))

BEGL.3ave <-nb3 %>%
  mutate(NB303 = rowMeans(.[,5:6], na.rm = TRUE),
         NB305 = rowMeans(.[,9:10]),
         NB307 = rowMeans(.[,13:14]),
         NB309 = rowMeans(.[,17:18]),
         NB310 = rowMeans(.[,19:20]),
         NB311 = rowMeans(.[,21:22]),
         NB312 = rowMeans(.[,23:24])) %>%
  select(., NB303:NB312) 
rownames(BEGL.3ave)<- c(seq(1997,2017))

#Combine BEGL samples from Transects 1, 2, and 3
BEGL <- combine.rwl(BEGL.1ave, BEGL.2ave) %>%
  combine.rwl(., BEGL.3ave)
interseries.cor(BEGL)   #Interseries correlations

#Drop series that have been remeasured but don't cross-date well
BEGL2 <- BEGL[,c(5:6, 8:18)]

#Detrending using 'mean' - no age-related growth trend is apparent 
BEGL.det <- detrend(BEGL2, method = 'Mean')
NB1.BEGL <- chron(BEGL.det, prefix = 'NB1', biweight = TRUE, prewhiten = TRUE)
plot(NB1.BEGL[1:20,])

#Summary statistics for chronology
rwl.report(BEGL2)
NB1.rw <- NB1.BEGL %>%
  mutate(Year = as.numeric(rownames(.)))

#write.csv(NB1.rw, 'NB1_BEGL.csv') #Chronology to CSV

#RING-WIDTH PLOT
plot(NB1.BEGL)

ggplot(NB1.rw, aes(x = Year, y = NB1res))+
  geom_area(aes(x = Year, y = samp.depth/10), fill = 'light grey') +
  geom_line(stat = "identity", color = '#0073C2FF')+
  theme_pubr()+
  ylab('Residual shrub ring width')+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Sample depth"))

#Save dimensions: 5 x 3 inches 

##########################################
#EXTRA - Local interpolated climate data 
clim <- read_csv("Nakvak_Interp_MoMean.csv")[,-1]
rw.temp <- merge(clim, NB1.rw, 'Year')

#Pair-wise correlations for exploratory purposes
ave.pair <- ggcorr(rw.temp, palette = "RdBu", label = TRUE)
ave.pair

corr.test(rw.temp[,'Tave_08'], rw.temp[,'NB1res'], method = "spearman")

ggplot(rw.temp)+
  geom_line(aes(x = Year, y = scale(NB1res)), stat = "identity", color = '#0073C2FF', size = 0.8)+
  geom_line(aes(x = Year, y = scale(Tave_08)), stat = "identity", color = '#EFC000FF', size = 0.8)+
  scale_color_jco()+
  ylab('Standardized august temperatures and ring widths')

ccf(rw.temp[2:21,'Tave_08'], rw.temp[2:21,'NB1res'], lag.max =  2) #Not autocorrelated
