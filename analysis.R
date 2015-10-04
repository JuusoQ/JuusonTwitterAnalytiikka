library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(ggthemes)
# analysis for the tweet archive
setwd("~/Documents/Twiitit2015/")
all_tweets <- read.csv("tweets.csv")

all_tweets$user <- str_extract(all_tweets$text, "@[A-Za-z0-9^_]*")
all_tweets$date <- substring(all_tweets$time, 0,10)
all_tweets$date <- as.Date(all_tweets$date)

all_tweets <- tbl_df(all_tweets)
jarjetys <- all_tweets %>% group_by(user) %>% filter(!is.na(user)) %>% summarise(count=length(user)) %>% select(user, count) %>% arrange(desc(count))

pvm <- all_tweets %>% filter(!is.na(user)) %>% group_by(user, date) %>% summarise(count=length(user))

all_time_top <- head(jarjetys, 16)
top <- subset(pvm, user %in% top16$user)

#barchart all time

ggplot(data=top, aes(x=date, y=count, fill=user)) + geom_bar(stat="identity") + facet_wrap(~user) + xlab("Päivämäärä") + ylab("Tweettejä") + scale_x_date(breaks = "2 month") + theme(legend.position="none")

ggplot(data=all_time_top, aes(user,count)) + geom_bar(stat="identity") + coord_flip() + theme_wsj()  + scale_fill_wsj() + xlab("Twiipsi") + ylab("Mainintoja")


# all time top by date
all_time_wsj <- 
  ggplot(data=top, aes(x=date, y=count, fill=user)) + geom_line(stat="identity") + facet_wrap(~user) + xlab("Päivämäärä") + ylab("Tweettejä") + scale_x_date(breaks = "1 year") + theme_wsj()+ scale_fill_wsj()+ theme(legend.position="none")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

## time series plot by date
twiitit.ts <- ggplot(data=by_date2, aes(x=date, y=cumulative)) + geom_line(stat="identity") + geom_smooth(model=glm, family="poisson") + theme_wsj() + xlab("Päivämäärä") + scale_x_date(breaks="6 months")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##
ggplot(data=jarjetys, aes(x=count)) + geom_boxplot()
