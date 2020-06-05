######################################################
### Creating BEGL chronology, clim correlati       ###
# FEB 26, 2020        ------------------------------ #
######################################################

rm(list=ls())
setwd("~/Desktop/Labrador Project/Nakvak Shrubs/NB1")

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

setwd("~/Desktop/Labrador Project/Nakvak Shrubs/NB2")
nb2<- read.rwl("NB2.raw")

setwd("~/Desktop/Labrador Project/Nakvak Shrubs/NB3")
nb3<- read.rwl("NB3.raw")

BEGL.1 <- combine.rwl(nb1.1, nb1.2) %>%
  combine.rwl(., nb1.3) %>%
  combine.rwl(., nb1.4) %>%
  combine.rwl(., nb1.8) %>%
  combine.rwl(., nb1.9) %>%
  combine.rwl(., nb1.10)
interseries.cor(BEGL.1) 
spag.plot(BEGL.1, zfac = 3)

BEGL.1ave <-BEGL.1 %>%
  mutate(NB1100 = rowMeans(.[,1:2], na.rm = TRUE),
         NB1200 = rowMeans(.[,9:10]),
         NB1300 = rowMeans(.[,23:24]),
         NB1400 = rowMeans(.[,31:32]),
         NB1800 = rowMeans(.[,39:40]),
         NB1900 = rowMeans(.[,45:46]),
         NB11000 = rowMeans(.[,53:54])) %>%
  select(., NB1100:NB11000) 

rownames(BEGL.1ave)<- c(seq(1997,2017)) 

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

BEGL <- combine.rwl(BEGL.1ave, BEGL.2ave) %>%
  combine.rwl(., BEGL.3ave)
interseries.cor(BEGL)   

#Drop series that don't cross-date well
BEGL2 <- BEGL[,c(5:6, 8:18)]

BEGL.det <- detrend(BEGL2, method = 'Mean')
NB1.BEGL <- chron(BEGL.det, prefix = 'NB1', biweight = TRUE, prewhiten = TRUE)
plot(NB1.BEGL[1:20,])
rwl.report(BEGL2)
NB1.rw <- NB1.BEGL %>%
  mutate(Year = as.numeric(rownames(.)))

#write.csv(NB1.rw, 'NB1_BEGL_JAN2020.csv') #Chronology to CSV

#RING-WIDTH PLOT
plot(NB1.BEGL)

ggplot(NB1.rw, aes(x = Year, y = NB1res))+
  geom_area(aes(x = Year, y = samp.depth/10), fill = 'light grey') +
  geom_line(stat = "identity", color = '#0073C2FF')+
  theme_bw()+
  ylab('Residual shrub ring width')

#Nakvak Interpolated climate
clim <- read_csv("~/Desktop/Labrador Project/Climate Data/Nakvak Local/Nakvak_Interp_MoMean.csv")
rw.temp <- merge(clim[,2:13], NB1.rw, 'Year')

ave.pair <- ggcorr(rw.temp, palette = "RdBu", label = TRUE)
ave.pair

corr.test(rw.temp[,'Tave_08'], rw.temp[,'NB1res'], method = "spearman")

ggplot(rw.temp)+
  geom_line(aes(x = Year, y = scale(NB1res)), stat = "identity", color = '#0073C2FF', size = 0.8)+
  geom_line(aes(x = Year, y = scale(Tave_08)), stat = "identity", color = '#EFC000FF', size = 0.8)+
  scale_color_jco()+
  ylab('Standardized august temperatures and ring widths')

ccf(rw.temp[2:21,'Tave_08'], rw.temp[2:21,'NB1res'], lag.max =  2) #Not autocorrelated



#CLIM NA ANALYSIS - Could use other clim data as needed
nak <- read_csv("~/Desktop/Labrador Project/Climate Data/Climate NA/Nakvak_1901-2018M_CLIMNA.csv")
rw.temp <- merge(nak, NB1.rw, 'Year') %>%
  mutate(TaveSM = rowMeans(cbind(Tave07, Tave08)))

variable.names(rw.temp)

tmin <- rw.temp[,c(1, 19:30, 176)]
tave <- rw.temp[,c(1, 31:42, 178, 176)]
#pas <- rw.temp[,c(1, 127:138, 178)]
#ppt <- rw.temp[,c(1, 43:54, 176)]
#rad <- rw.temp[,c(1, 55:66, 176)]
#tmax <- rw.temp[,c(1, 7:18, 176)]

ave.pair <- ggcorr(tave, palette = "RdBu", label = TRUE)
ave.pair
#max.pair <- ggcorr(tmax, palette = "RdBu", label = TRUE)
#max.pair
#min.pair <- ggcorr(tmin, palette = "RdBu", label = TRUE)
#min.pair
#pas.pair <- ggcorr(pas, method = c("pairwise", "spearman"), palette = "RdBu", label = TRUE)
#pas.pair
#ppt.pair <- ggcorr(ppt, palette = "RdBu", label = TRUE)
#ppt.pair

corr.test(tave[2:21, 'TaveSM'], tave[2:21, 'NB1res'], method = "spearman")

ggplot(tave)+
  geom_line(aes(x = Year, y = scale(NB1res)), stat = "identity", color = '#0073C2FF', size = 0.8)+
  geom_line(aes(x = Year, y = scale(TaveSM)), stat = "identity", color = '#EFC000FF', size = 0.8)+
  scale_color_jco()+
  ylab('Standardized summer temperatures and ring widths')

ccf(tave[2:21, 'TaveSM'], tave[2:21, 'NB1res'], lag.max =  2) #Not autocorrelated

ja.change <- as.data.frame(cbind(Year = nak$Year, TaveSM = rowMeans(cbind(nak$Tave07, nak$Tave08))))
ja.short <- filter(ja.change, Year > 1988) #From 1989; climate normal period
summary(lm(TaveSM ~ Year, data = ja.short))
plot(ja.short$Year, ja.short$TaveSM, type = 'l')
0.05267*30

#FIN!

###EXTRA CODE
#NACHVAK DATA 
setwd("~/Desktop/Labrador Project/Nakvak Shrubs/Clim Dat")
temp <- read.csv("NachvakTemp.csv")

temp.sum <- temp %>%
  mutate(Year = substring(Date, 1,4),
         Month = substring(Date, 6,7),
         Day = substring(Date, 9,10)) %>%
  # filter(Month %in% c('06', '07', '08', '01')) %>%
  group_by(Year, Month) %>%
  summarize(AveTemp = mean(Temp))

# temp.sum <- temp %>%
#   mutate(Year = substring(Date, 1,4),
#          Month = substring(Date, 6,7),
#          Day = substring(Date, 9,10)) %>%
#   filter(Month %in% c('07', '08')) %>%
#   group_by(Year, Month) %>%
#   summarize(AveTemp = mean(Temp))
# write.csv(temp.sum, 'JulAug_Temp.csv')

ggplot(rw.aug)+
  geom_line(aes(x = Year, y = MeanTemp), stat = "identity", col = 'blue', size = 0.8)+
  geom_line(aes(x = Year, y = Tave08), stat = "identity", col = 'red', size = 0.8)+
  theme_bw()

ggplot(pas)+
  geom_line(aes(x = Year, y = NB1res*14), stat = "identity", col = '#6C945E', size = 0.8)+
  geom_line(aes(x = Year, y = PAS06), stat = "identity", col = '#63A5D2', size = 0.8)+
  theme_bw()


ggplot(temp.sum, aes(x = as.numeric(Year), y = AveTemp, group = Month, color = Month)) +
  geom_line(stat = "identity")+
  scale_colour_jco()+
  xlim(1990, 2017)

temp.sum2 <- temp.sum %>%
  ungroup() %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year > 1997) %>%
  filter(Month == '08') 

rw.sum <- merge(temp.sum2, rw.aug, 'Year')

corr.test(rw.sum$AveTemp, rw.sum$Tave08)

ggplot(rw.temp, aes(x = as.numeric(Year), y = AveTemp, group = Month, color = Month)) +
  geom_line(stat = "identity")+
  geom_line(aes(x = Year, y = NB1std*4), stat = "identity", col = 'red')+
  #scale_colour_jco()+
  xlim(1990, 2017)

#aug
aug <- filter(rw.temp, Month == '08') %>%
  mutate(Scaled = scale(AveTemp, center = TRUE))

corr.test(aug$AveTemp, aug$NB1res, method = 'spearman')

ggplot(jan)+
  geom_line(aes(x = Year, y = NB1std*8), stat = "identity", col = 'darkgreen')+
  geom_line(aes(x = Year, y = Scaled), stat = "identity", col = 'black')+
  theme_few()

#jan
jan <- filter(rw.temp, Month == '01') %>%
  mutate(Scaled = scale(AveTemp, center = TRUE))

corr.test(jan$Scaled, jan$NB1res, method = 'spearman')

ccf(as.numeric(jan$Scaled), jan$NB1res, lag.max =  2)


#Summer temperature change 
sm.change <- temp.sum %>%
  filter(Month %in% c('06', '07', '08', '09')) %>%
  group_by(Year) %>%
  summarize(MeanSm = mean(AveTemp)) %>%
  filter(Year > 1989)
plot(sm.change$Year, sm.change$MeanSm, type = 'l')

change <- temp.sum %>%
  filter(Month %in% c('07', '08')) %>%
  group_by(Year) %>%
  summarize(MeanSm = mean(AveTemp)) %>%
  filter(Year > 1989)
plot(change$Year, change$MeanSm, type = 'l')
summary(lm(MeanSm ~ as.numeric(Year), data = sm.change))
