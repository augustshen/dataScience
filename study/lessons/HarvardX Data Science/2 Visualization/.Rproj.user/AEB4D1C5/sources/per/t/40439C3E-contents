#5.1.1 Visualization Principles

#5.1.2 encoding data using visual cues

#5.1.3 know when to include zero

#5.1.4 do not distort quantities

#5.1.5 order by a meaningful valuse

#ex4
library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state
rate
state <- reorder(state, rate, mean)
levels(state)
print(state)

#ex5
library(dplyr)
library(dslabs)
library(gridExtra)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
dat <- dat %>% mutate(state=reorder(state, rate, mean))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#5.2.1 show the data
data("heights")
heights %>% ggplot(aes(sex, height)) + geom_point()

#5.2.2 ease comparisons: use common axes

#5.2.3 consider transformations

#5.2.4 ease comparisons: compared visual cues should be adjacents
color_blind_friendly_cols <- c("#999999", "#e69f00")
p1 <- data.frame()

#ex5.2.2
library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total/population*100000)  %>% mutate(region=reorder(region, rate, median)) %>% ggplot(aes(region, rate)) + geom_boxplot() + geom_point()

#5.3.1 slope charts
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>% filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>% mutate(location=ifelse(year==2010, 1, 2), location=ifelse(year==2015 & country %in% c("United Kingdom", "Portugal"), location + 0.22, location), hjust=ifelse(year==2010, 1, 0)) %>% mutate(year=as.factor(year)) %>% ggplot(aes(year, life_expectancy, group=country)) + geom_line(aes(color=country), show.legend = FALSE) + geom_text(aes(x=location, label=country, hjust=hjust), show.legend = FALSE) + xlab("") + ylab("Life Expectancy")

#5.3.2 encoding a third variable

#5.3.3 case study:Vaccines
data(us_contagious_diseases)
str(us_contagious_diseases)

the_disease <- "Measles"
dat <- us_contagious_diseases %>% filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>% mutate(rate=count/population * 10000) %>% mutate(state=reorder(state, rate))
dat %>% filter(state == "California") %>% ggplot(aes(year, rate)) + geom_line() + ylab("Cases per 10,000") + geom_vline(xintercept = 1963, col="blue")

library(RColorBrewer)
display.brewer.all(type="seq")

display.brewer.all(type="div")

dat %>% ggplot(aes(year, state, fill= rate)) + geom_tile(color="grey50") + scale_x_continuous(expand=c(0,0)) + scale_fill_gradientn(colors=brewer.pal(9, "Reds"), trans="sqrt") + geom_vline(xintercept = 1963, col="blue") + theme_minimal() + theme(panel.grid = element_blank()) + ggtitle(the_disease) + ylab("") + xlab("")

avg <- us_contagious_diseases %>% filter(disease==the_disease) %>% group_by(year) %>% summarize(us_rate=sum(count, na.rm=TRUE)/sum(population, na.rm = TRUE) * 10000)
avg

#5.3.4 avoid Pseudo and gratuitous 3D Plots

#5.3.5 avoid too many significant digits

#5.3.1 ex
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Measles"
the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

#5.3.2 ex
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Measles"
the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#5.3 ex3
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California" & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()

#5.3 ex4
us_contagious_diseases %>% filter(!is.na(population)) %>% group_by(year, disease) %>% summarize(rate = sum(count)/sum(population)*10000) %>% ggplot(aes(year, rate, color=disease)) + geom_line()

