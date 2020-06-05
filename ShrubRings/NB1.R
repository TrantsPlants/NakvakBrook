# Visualization of NB1 and analysis of stem elongation data (Figure 4)
# Emma Davis - emmalaureldavis@gmail.com

# Load packages
library(readr)
library(dplR)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(dplyr)

#Clear workspace and load in data 
rm(list=ls())
setwd("~/NakvakBrook/ShrubRings")

#NB1-1
nb1.1<-read.rwl("NB1_1.raw")
#spag.plot(nb1.1, zfac =2)
#interseries.cor(nb1.1)

nb1.1sum<-summary.rwl(nb1.1) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.1plot <- nb1.1sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))

#NB1-2
nb1.2<-read.rwl("NB1_2.raw")
#spag.plot(nb1.2, zfac =2)
#interseries.cor(nb1.2)

nb1.2sum<-summary.rwl(nb1.2) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.2plot <- nb1.2sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
       Sample = substr(SampleID, 4,4))


#NB1-3
nb1.3<-read.rwl("NB1_3.raw")
#spag.plot(nb1.3, zfac =2)
#interseries.cor(nb1.3)

nb1.3sum<-summary.rwl(nb1.3) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.3plot <- nb1.3sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))


#NB1-4
nb1.4<-read.rwl("NB1_4.raw")
#spag.plot(nb1.4b, zfac =2)
#interseries.cor(nb1.4)

nb1.4sum<-summary.rwl(nb1.4) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.4plot <- nb1.4sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))

#NB1-5
nb1.5<-read.rwl("NB1_5.raw")
#spag.plot(nb1.5, zfac =2)
#interseries.cor(nb1.5)

nb1.5sum<-summary.rwl(nb1.5) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.5plot <- nb1.5sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))

#NB1-6
nb1.6<-read.rwl("NB1_6.raw")
#spag.plot(nb1.6, zfac =2)
#interseries.cor(nb1.6)

nb1.6sum<-summary.rwl(nb1.6) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.6plot <- nb1.6sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))

#NB1-7
nb1.7<-read.rwl("NB1_7.raw")
#spag.plot(nb1.7, zfac =2)
#interseries.cor(nb1.7)

nb1.7sum<-summary.rwl(nb1.7) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.7plot <- nb1.7sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))


#NB1-8
nb1.8<-read.rwl("NB1_8.raw")
#spag.plot(nb1.8, zfac =2)
#interseries.cor(nb1.8)

nb1.8sum<-summary.rwl(nb1.8) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.8plot <- nb1.8sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))


#NB1-9
nb1.9<-read.rwl("NB1_9.raw")
#spag.plot(nb1.9, zfac =2)
#interseries.cor(nb1.9)

nb1.9sum<-summary.rwl(nb1.9) %>%
  mutate(Path = substr(series, 7,7),
         SampleID = substr(series, 1,6),
         Height = substr(series, 5,6))

nb1.9plot <- nb1.9sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))


#NB1-10
nb1.10<-read.rwl("NB1_10.raw")
#spag.plot(nb1.10, zfac =2)
#interseries.cor(nb1.10)

nb1.10sum<-summary.rwl(nb1.10) %>%
  mutate(Path = substr(series, 8,8),
         SampleID = substr(series, 1,7),
         Height = substr(series, 6,7))

nb1.10plot <- nb1.10sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,5))

#Combine all yer formatted samples together into a dataframe
full <- rbind(nb1.1plot, nb1.2plot, nb1.3plot, 
              nb1.4plot, nb1.5plot, nb1.6plot, 
              nb1.7plot, nb1.8plot, nb1.9plot,
              nb1.10plot)
full$Height <- as.numeric(full$Height)
full$Sample <- factor(full$Sample, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

#Model best fit assessment - linear vs. exponential growth models

#MODEL to find better model fit
linearMod <- lm(Height ~ MeanEst, data= full)
summary(linearMod)

linearModExp <- lm(log(as.numeric(Height)+0.1) ~ MeanEst, data= full)
summary(linearModExp) #Best fitting model was exponential; R2 = 0.64

#Testing model syntax and estimating values
test.eq <- (exp(0.49*(full$MeanEst-991))) #Yup the EQ works

lm2.preds <- as.data.frame(exp(predict(linearModExp, interval = c("confidence"), level = 0.95)))

full.predict <- cbind(x = full, LMpreds = (predict(linearModExp)), Fit = lm2.preds$fit, Upper = lm2.preds$upr, Lower = lm2.preds$lwr) %>%
  mutate(Sample = factor(Sample, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>%
  mutate(Species = if_else(Sample == 5, c('SAAL'), (if_else(Sample == 6, c('SAPE'), (if_else(Sample == 7, c('SAPE'), c('BEGL')))))))
                      
ggplot(full.predict)+
  geom_line(aes(MeanEst, Fit), size = 0.8) +
  geom_point(aes(MeanEst,as.numeric(Height),fill = Species), shape = 21, size = 5) +
  xlab('Year')+
  ylab('Stem length (cm)')+
  #ggtitle('Rate of stem elongation - Transect NB1')+
  theme_pubr(base_size = 10, legend = 'bottom')+
  scale_fill_npg(alpha = 0.6, labels = c("B. glandulosa", "Salix sp.", "S. pedicellaris"))+
  ylim(0, 100)
  #annotate("text", x = 2008, y = 80, label = "log(Length+0.1) = (Year*0.49)-974.73", size = 3)

#Save dimensions: 3.25 x 3.25 inches

#write.csv(full, 'AllSamplesNB1.csv')