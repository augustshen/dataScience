library(dslabs)
library(tidyverse)
data(murders)

murders %>% ggplot() + geom_point(aes(x= population/10^6, y = total))

p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total), size=3) + geom_text(aes(population/10^6, total, label= abb), nudge_x = 1)


p <- ggplot(data = murders, aes(population/10^6, total, label=abb))
p + geom_point(size=3) + geom_text(nudge_x = 1.5) #+ scale_x_continuous(trans ="log10") + scale_y_continuous(trans="log10")

#p + geom_point(size=3) + geom_text(aes(x=10, y=800, label= "hello there"))

p + geom_point(size=3) + geom_text(nudge_x = 0.075) + scale_x_continuous(trans ="log10") + scale_y_continuous(trans="log10")

p + geom_point(size=3) + geom_text(nudge_x = 0.075) + scale_x_log10() + scale_y_log10() + xlab("Populations in millions(log scale)") + ylab("Total number of murders(log scale)") + ggtitle("US Gun Murders in US 2010")

p <- p + geom_text(nudge_x = 0.075) + scale_x_log10() + scale_y_log10() + xlab("Populations in millions(log scale)") + ylab("Total number of murders(log scale)") + ggtitle("US Gun Murders in US 2010")

p + geom_point(size=3, color="blue")

p + geom_point(aes(col = region), size=3)

r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate

p + geom_point(aes(col = region), size=3) + geom_abline(intercept = log10(r))

p <- p + geom_abline(intercept=log10(r), lty=2, color="darkgrey") + geom_point(aes(col = region), size=3)

p <- p+ scale_color_discrete(name="Region")
p

#addon packages
library(ggthemes)
library(ggrepel)   #arrange the label position 

p + theme_economist()
p + theme_fivethirtyeight()

p
ds_theme_set()


# start from begin again
library(ggthemes)
library(ggrepel)   #arrange the label position 

r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate

p <- ggplot(data = murders, aes(population/10^6, total, label=abb))

p <- p + geom_abline(intercept=log10(r), lty=2, color="darkgrey") + geom_point(aes(col = region), size=3)

p <- p + geom_text_repel()  + scale_x_log10() + scale_y_log10()

p <- p + xlab("Populations in millions(log scale)") + ylab("Total number of murders(log scale)") + ggtitle("US Gun Murders in US 2010")

p <- p + scale_color_discrete(name="Region") + theme_economist()

p

murders %>% ggplot(aes(population, total, label = abb)) + geom_label(aes(col=region))

#other example
p1 <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 + geom_histogram()
p1 + geom_histogram(binwidth = 1, fill="blue", col="black") + xlab("Male heights in inches") + ggtitle("Histogram")

p1 + geom_density(fill="blue")

p2 <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
p2 + geom_qq()

params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd=sd(height))
p2 + geom_qq(dparams = params) + geom_abline()


library(gridExtra)
p3 <- p1 + geom_histogram(binwidth = 1, fill="blue", col="black")
p4 <- p1 + geom_histogram(binwidth = 2, fill="blue", col="black")
p5 <- p1 + geom_histogram(binwidth = 3, fill="blue", col="black")
grid.arrange(p3,p4,p5,ncol=3)
