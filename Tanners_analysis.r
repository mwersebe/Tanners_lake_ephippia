############# Wersebe, Matthew 6/16/2020############################################
####################################################################################
############# Tanners Lake Long Term WQ data analysis###############################
####################################################################################
setwd("C:/Users/mwers/Dropbox/Matthew Wersebe- OU work/Projects/Tanners Lake Data")
library("tidyverse")
#Read in the datasets
LT_Data <- read_csv("Tanners_conducivity_longterm.csv", col_names = TRUE, na = c("", "na"))
LT_cond_chl <- read_csv("Tanners_conductivity_chloride_longterm.csv", col_names = TRUE, na = c("", "na"))

########################################
#GOAL: Combine all data by date and location

LT_Data.1 <- separate(LT_Data, ActivityStartDate, into = c("Month", "Day", "Year"), sep = "/")
LT_Data.2 <- LT_Data.1 %>% arrange(Year, Month, Day, `ActivityDepthHeightMeasure/MeasureValue`)

LT_Data.2 <- LT_Data.2 %>% pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue)


LT_cond_chl <- separate(LT_cond_chl, ActivityStartDate, into = c("Month", "Day", "Year"), sep = "/")
LT_cond_chl <- LT_cond_chl %>% arrange(Year, Month, Day, `ActivityDepthHeightMeasure/MeasureValue`)

LT_cond_chl <- LT_cond_chl %>% pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue)

plot(LT_cond_chl$`Specific conductance`~LT_cond_chl$ChlorideConcentration)

plot(LT_cond_chl$ChlorideConcentration~LT_cond_chl$Year, col = LT_cond_chl$`ActivityDepthHeightMeasure/MeasureValue`)

chl.14 <- LT_cond_chl %>% filter(`ActivityDepthHeightMeasure/MeasureValue`== 14)
chl.0 <- LT_cond_chl %>% filter(`ActivityDepthHeightMeasure/MeasureValue` == 0)
chl.7 <- LT_cond_chl %>% filter(`ActivityDepthHeightMeasure/MeasureValue`== 7)


year1996 <- LT_cond_chl %>% filter(Year == 1996)

plot(chl.0$ChlorideConcentration~chl.0$Year, col = "black", pch =1, cex = 0.8)
  points(chl.14$ChlorideConcentration~chl.14$Year, col = "blue", pch = 2, cex = 0.8)
  #points(chl.7$ChlorideConcentration~chl.7$Year, col = "blue", pch = 0, cex = 0.8)
  legend(1955, 200, legend = c("0m", "14m"), col = c("black", "blue"), cex = 0.8, pch = c(1,2)) 
  
year2016 <- LT_cond_chl %>% filter(Year == 2016, `ActivityDepthHeightMeasure/MeasureValue`== 0.1)   
 mean(year2016$ChlorideConcentration)
 
 year2014 <- LT_cond_chl %>% filter(Year == 2014, `ActivityDepthHeightMeasure/MeasureValue`== 0.1)   
 mean(year2014$ChlorideConcentration)
 
 year2012 <- LT_cond_chl %>% filter(Year == 2012, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year2012$ChlorideConcentration)
 
 year2010 <- LT_cond_chl %>% filter(Year == 2010, `ActivityDepthHeightMeasure/MeasureValue`== 0.1)   
 mean(year2010$ChlorideConcentration)
 
 year2008 <- LT_cond_chl %>% filter(Year == 2008, `ActivityDepthHeightMeasure/MeasureValue`== 0.1)   
 mean(year2008$ChlorideConcentration)
 
 year2006 <- LT_cond_chl %>% filter(Year == 2006, `ActivityDepthHeightMeasure/MeasureValue`== 0.1)   
 mean(year2006$ChlorideConcentration)
 
 year2003 <- LT_cond_chl %>% filter(Year == 2003, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year2003$ChlorideConcentration)
 
 year2000 <- LT_cond_chl %>% filter(Year == 2000, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year2000$ChlorideConcentration)
 
 year1998 <- LT_cond_chl %>% filter(Year == 1998, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1998$ChlorideConcentration)
 
 year1996 <- LT_cond_chl %>% filter(Year == 1996, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1996$ChlorideConcentration)
 
 year1994 <- LT_cond_chl %>% filter(Year == 1994, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1994$ChlorideConcentration)
 
 year1990 <- LT_cond_chl %>% filter(Year == 1990, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1990$ChlorideConcentration)
 
 year1986 <- LT_cond_chl %>% filter(Year == 1986, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1986$ChlorideConcentration)
 
 year1982 <- LT_cond_chl %>% filter(Year == 1982, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1982$ChlorideConcentration)
 
 year1977 <- LT_cond_chl %>% filter(Year == 1977, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1977$ChlorideConcentration)
 
 year1972 <- LT_cond_chl %>% filter(Year == 1972, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1972$ChlorideConcentration)
 
 year1968 <- LT_cond_chl %>% filter(Year == 1968, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1968$ChlorideConcentration)
 
 year1967 <- LT_cond_chl %>% filter(Year == 1967, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1967$ChlorideConcentration)
 
 year1965<- LT_cond_chl %>% filter(Year == 1965, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1965$ChlorideConcentration)
 
 year1964<- LT_cond_chl %>% filter(Year == 1964, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1964$ChlorideConcentration)
 
 year1961<- LT_cond_chl %>% filter(Year == 1961, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1961$ChlorideConcentration)
 
 year1958<- LT_cond_chl %>% filter(Year == 1958, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1958$ChlorideConcentration)
 
 year1955<- LT_cond_chl %>% filter(Year == 1955, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1955$ChlorideConcentration)
 
 year1952<- LT_cond_chl %>% filter(Year == 1952, `ActivityDepthHeightMeasure/MeasureValue`== 0)   
 mean(year1952$ChlorideConcentration)



##################################################################################
#Ephippia Size linear model
library(tidyverse)
Ephippia.size <- read_csv("Tanners_EphippiaSize_mw.csv", col_names = T, na = "NA")

Ephippia.size$Depth_top_of_interval <-as.factor(Ephippia.size$Depth_top_of_interval)

dev.new()
vio <- ggplot(Ephippia.size, aes(x=Depth_top_of_interval, y=Ephippia_dorsal_length)) + geom_violin()
vio <- vio + stat_summary(fun.y=mean, geom = "point", shape=23, size= 2)
vio

Ephippia.size <- read_csv("Tanners_EphippiaSize_mw.csv", col_names = T, na = "NA")

Size <- lm(Ephippia.size$Ephippia_dorsal_length~Ephippia.size$Depth_top_of_interval)
summary(Size)

modelSummary <- summary(Size)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
modelCoeffs
beta.estimate <- modelCoeffs["Ephippia.size$Depth_top_of_interval", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Ephippia.size$Depth_top_of_interval", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(Ephippia.size)-ncol(Ephippia.size))  # calc p Value
f_statistic <- Size$fstatistic[1]  # fstatistic
f <- summary(Size)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

t_value
p_value
f_statistic
f
model_p
#####################################################################################
###################Generalized-Additive Mixed Models#################################
#####################################################################################
library(mgcv)
library(gratia)
GAMData<- read_csv("Tanners_ephippia_gmm.csv", col_names = TRUE, na = c("", "na"))
head(GAMData)


scatter.smooth(x= GAMData$Year, y= GAMData$Ephip.Flux, main = "Total Ephippial Flux", ylab = "Ephippial Flux (ephippia/g sediment/year)", xlab = "Year (approximate)")

#scatter.smooth(x= GAMData$Year, y = log(GAMData$Ephip.Flux))

scatter.smooth(x = GAMData$Year, y= GAMData$Puli.Flux, main = "D. pulicaria Ephippial Flux", ylab = "Ephippial Flux (ephippia/g sediment/year)", xlab = "Year (approximate)")

scatter.smooth(x=GAMData$Year, y = GAMData$Mend.Flux, main = "D. mendotae Ephippial Flux", ylab = "Ephippial Flux (ephippia/g sediment/year)", xlab = "Year (approximate)")

scatter.smooth(x = GAMData$Year, y = GAMData$Small.Flux, main = " D. retrocurva and D. Parvula ('Small') Ephippial Flux", ylab = "Ephippial Flux (ephippia/g sediment/year)", xlab = "Year (approximate)")

scatter.smooth(x= GAMData$Year, y = GAMData$Puli.Other_ratio)
####################################################################################################################################
#Correlations
head(GAMData)
pairs(GAMData[,c(2:3,8:12)])
pairs(GAMData[,4:7])

cor.test(GAMData$POP_density, GAMData$Year, method = "pearson")
#correlated
cor.test(GAMData$POP_density, GAMData$Chloride_top, method = "pearson", use = "complete.obs")
#correlated
cor.test(GAMData$POP_density, GAMData$X._Organic, method = "pearson")
#uncorrelated
cor.test(GAMData$POP_density, GAMData$OC_flux, method = "pearson")
#uncorrelated
cor.test(GAMData$X._Organic, GAMData$X._Inorg., method = "pearson")
#correlated
cor.test(GAMData$X._Organic, GAMData$X._CaCO3, method = "pearson")
#uncorrelated

##########################################################################################################
#Pulicaria

Puli.gamm1 <- gamm(Pulicaria ~ s(Year) + s(Chloride_top) + s(CaCO3), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))
                   
summary(Puli.gamm1$gam)
draw(Puli.gamm1$gam)
appraise(Puli.gamm1$gam)

plot(Puli.gamm1$gam, residuals = T, pch =1, pages = 1, all.terms = T, shade = T)
par(mfrow = c(2,2))
gam.check(Puli.gamm1$gam)


plot(Puli.gamm1$gam, seWithMean = T, shift = coef(Puli.gamm1$gam)[1], shade = T, pages = 1)

#####################################################################################################
#Mendotae

Mend.gamm1 <- gamm(Mendotae ~ s(Year) + s(Chloride_top, k= 9)  + s(CaCO3) + s(Organic), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))

summary(Mend.gamm1$gam)

draw(Mend.gamm1$gam)
appraise(Mend.gamm1$gam)

Mend.gamm1 <- gamm(Mendotae ~ s(CaCO3) + s(Chloride_top), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))


plot(Mend.gamm1$gam, residuals = T, pch =1, pages = 1, all.terms = T, shade = T)
par(mfrow = c(2,2))
gam.check(Mend.gamm1$gam)


plot(Mend.gamm1$gam, seWithMean = T, shift = coef(Mend.gamm1$gam)[1], shade = T, pages = 1)

########################################################################################################
#Small Ephippia

Small.gamm1 <- gamm(Small ~ s(Year) + s(Chloride_top) + s(Organic)  + s(CaCO3), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))

summary(Small.gamm1$gam)
draw(Small.gamm1$gam)
appraise(Small.gamm1$gam)


Small.gamm1 <- gamm(Small ~ s(Chloride_top), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))


plot(Small.gamm1$gam, residuals = T, pch =1, pages = 1, all.terms = T, shade = T)
par(mfrow = c(2,2))
gam.check(Small.gamm1$gam)

Small.gamm2 <- gamm(Small.Flux ~ s(Chloride_top) + s(Year), data = GAMData, method = "REML", correlation = corCAR1(form = ~ 1|Year), family = poisson(link = log))

summary(Small.gamm2$gam)

plot(Small.gamm2$gam, residuals = T, pch =1, pages = 1, all.terms = T, shade = T)
par(mfrow = c(2,2))
gam.check(Small.gamm2$gam)

plot(Small.gamm2$gam, seWithMean = T, shift = coef(Small.gamm2$gam)[1], shade = T)


