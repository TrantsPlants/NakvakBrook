rm(list=ls())
setwd("~/Desktop/Labrador Project/Nakvak Shrubs/NB2")
#NB2
nb2<-read.rwl("NB2.raw")
spag.plot(nb2, zfac =2)
interseries.cor(nb2)

nb2.sum<-summary.rwl(nb2) %>%
  mutate(Path = substr(series, 5,5),
         SampleID = substr(series, 1,4),
         Height = 0)
         
nb2.plot <- nb2.sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,4))

ggplot(nb2.plot, aes(Sample, MeanEst))+
  geom_point()+
  ylab("Est. year")+
  xlab("Sample")+
  ggtitle("NB2")

#write.csv(nb2.plot, 'AllSamplesNB2.csv')
