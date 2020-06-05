rm(list=ls())
setwd("~/Desktop/Nakvak Shrubs/NB3/")
#NB3
nb3<-read.rwl("NB3.raw")
spag.plot(nb3, zfac =2)
interseries.cor(nb3)


nb3.sum<-summary.rwl(nb3) %>%
  mutate(Path = substr(series, 6,6),
         SampleID = substr(series, 1,5),
         Height = 0)


nb3.plot <- nb3.sum %>%
  group_by(SampleID, Height) %>%
  summarize(MeanEst = mean(first)) %>%
  mutate(Site = substr(SampleID, 1,3),
         Sample = substr(SampleID, 4,5))

ggplot(nb3.plot, aes(Sample, MeanEst))+
  geom_point()+
  ylab("Est. year")+
  xlab("Sample")+
  ggtitle("NB3")

#write.csv(nb3.plot, 'AllSamplesNB3.csv')
