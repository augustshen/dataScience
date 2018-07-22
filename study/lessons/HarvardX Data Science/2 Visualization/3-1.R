#summary data with dplyr
library(dslabs)
library(tidyverse)
data(heights)

s <- heights %>% filter(sex=="Male") %>% summarize(average=mean(height), standard_deviation=sd(height))
s
s$average

heights %>% filter(sex=="Male") %>% summarize(median=mean(height), min=min(height), max=max(height))

#can only use the function return a single value, below will cause error
#heights %>% filter(sex=="Male") %>% summarize(range=quantile(height, c(0, 0.5, 1))) 

#3.1.2 the dot placeholder
data(murders)
murders <- murders %>% mutate(murder_rate=total/population*10^5)
summarize(murders, mean(murder_rate))

umr <- murders %>% summarize(rate=sum(total)/sum(population)*10^5)
umr
class(umr)
umr %>% .$rate
class(umr)
class(umr$rate)

umr2 <- murders %>% summarize(rate=sum(total)/sum(population)*10^5) %>% .$rate
umr2
class(umr2)

#3.1.3  group then summarize
heights %>% group_by(sex) %>% summarize(avg=mean(height), sd = sd(height))

murders %>% group_by(region) %>% summarize(media_rate=median(murder_rate))

#3.1.4 sorting data tables
murders %>% arrange(population) %>% head()
murders %>% arrange(murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()
murders %>% arrange(region, murder_rate) %>% head()
murders %>% top_n(10, murder_rate)
murders %>% arrange(desc(murder_rate)) %>% top_n(10)
murders %>% arrange(murder_rate) %>% top_n(10)

#exercises
library(NHANES)
data(NHANES)
names(NHANES)
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

tab <- NHANES %>% filter(AgeDecade==" 20-29", Gender=="female") 

ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm= TRUE))

NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(min=min(BPSysAve, na.rm=TRUE), max=max(BPSysAve, na.rm=TRUE))

NHANES %>%
  filter(Gender == "female") %>% group_by(AgeDecade) %>% summarize(average=mean(BPSysAve, na.rm= TRUE), standard_deviation=sd(BPSysAve, na.rm= TRUE))

NHANES %>% group_by(AgeDecade, Gender) %>% summarize(average=mean(BPSysAve, na.rm= TRUE), standard_deviation=sd(BPSysAve, na.rm= TRUE))

NHANES %>% filter(AgeDecade == " 40-49"  & Gender == "male") %>% group_by(Race1) %>% summarize(average=mean(BPSysAve, na.rm= TRUE), standard_deviation=sd(BPSysAve, na.rm= TRUE)) %>% arrange(average)