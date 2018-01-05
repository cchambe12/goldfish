## 5 January 2017 - Cat
## Working on Goldfish demographics to determine strongest factors in determining successful
# goldfish locations -- just cleaning!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(ggplot2)
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/goldfish/analyses/")

## Load the data
d<-read.csv("input/Demos.July2017.csv", header=TRUE)
df<-read.csv("input/Scorecard_7.26.17.csv", header=TRUE)

## Lots of cleaning... start with 'd'
d<-d[-c(1:4), ]
colnames(d) = d[1, ] # the first row will be the header
d = d[-1, ] 
bad<-c("", "DMA")
d$DMA<-ifelse(d$DMA%in%bad, NA, d$DMA)
d<-d[!is.na(d$DMA),]
d$`Existing/Planned U.S. Locations`<-ifelse(d$`Existing/Planned U.S. Locations`=="", NA,
                                            d$`Existing/Planned U.S. Locations`)
d<-d[!is.na(d$`Existing/Planned U.S. Locations`),]
d$`Existing/Planned U.S. Locations` <- gsub(",.*","", d$`Existing/Planned U.S. Locations`)
demo<-d%>%rename(Store=`Existing/Planned U.S. Locations`)

## More cleaning... now with 'df'
for(i in c(1:nrow(df))){
  df$Store[i]<-ifelse(df$Store[i]=="", df$stores[i], df$Store[i])
}
df <- subset(df, select = -c(X.1,X.2, stores) )
dx<-df%>%dplyr::select(Store, age, X7.26.17) %>% rename(enrollment=X7.26.17)
dx<-dx[(dx$Store!=""),]

## Combine them up!
demos<-inner_join(dx, demo)
demos[] <- lapply(demos, gsub, pattern=",", replacement="")
miles<-demos%>%rename(one.100=`<1 Mile Kids > $100K`)%>%rename(one=`<1 Mile All Kids`)%>%
  rename(two.100=`<2 Miles Kids > $100K`)%>%rename(two=`<2 Miles All Kids`)%>%
  rename(three.100=`<3 Miles Kids > $100K`)%>%rename(three=`<3 Miles All Kids`)%>%
  rename(four.100=`<4 Miles Kids > $100K`)%>%rename(four=`<4 Miles All Kids`)%>%
  rename(five.100=`<5 Miles Kids > $100K`)%>%rename(five=`<5 Miles All Kids`)%>%
  rename(six.100=`<6 Miles Kids > $100K`)%>%rename(six=`<6 Miles All Kids`)%>%
  rename(seven.100=`<7 Miles Kids > $100K`)%>%rename(seven=`<7 Miles All Kids`)%>%
  rename(eight.100=`<8 Miles Kids > $100K`)%>%rename(eight=`<8 Miles All Kids`)%>%
  rename(nine.100=`<9 Miles Kids > $100K`)%>%rename(nine=`<9 Miles All Kids`) %>%
  rename(ten.100=`<10 Miles Kids > $100K`)%>%rename(ten=`<10 Miles All Kids`) 
miles<-dplyr::select(miles, Store, age, enrollment, DMA, one.100, one, two.100, two, 
                     three.100, three, four.100, four, five.100, five, six.100, six, 
                     seven.100, seven, eight.100, eight, nine.100, nine, ten.100, ten)

#write.csv(miles, file="~/Documents/git/goldfish/analyses/output/miles.csv", row.names = FALSE)

mins<-demos%>%rename(fift.100=`15 Minutes Kids > $100K`)%>%rename(fift=`15 Minutes All Kids`)%>%
  rename(twen.100=`20 Minutes Kids > $100K`)%>%rename(twen=`20 Minutes All Kids`)%>%
  rename(tfiv.100=`25 Minutes Kids > $100K`)%>%rename(tfiv=`25 Minutes All Kids`)
mins<-dplyr::select(mins, Store, age, enrollment,DMA, fift.100, fift, twen.100, twen, tfiv.100, tfiv)

#write.csv(mins, file="~/Documents/git/goldfish/analyses/output/minutes.csv", row.names = FALSE)

