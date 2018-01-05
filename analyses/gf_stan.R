## 5 January 2017 - Cat
## Working on Goldfish demographics to determine strongest factors in determining successful
# goldfish locations -- initial raw results and stan models

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(dplyr)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/goldfish/analyses/")
#source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Load the data
miles<-read.csv("output/miles.csv", header=TRUE)
mins<-read.csv("output/minutes.csv", header=TRUE)

### Some plots!
ggplot(mins, aes(x=age, y=enrollment)) +geom_point()

## Models! - minutes!
mins$DMA<-as.numeric(as.factor(mins$DMA))
mins$age<-as.numeric(mins$age)
mins$enrollment<-as.numeric(mins$enrollment)
mins$fift.100<-as.numeric(mins$fift.100)
mins$fift<-as.numeric(mins$fift)
mins$twen.100<-as.numeric(mins$twen.100)
mins$twen<-as.numeric(mins$twen)
mins$tfiv.100<-as.numeric(mins$tfiv.100)
mins$tfiv<-as.numeric(mins$tfiv)

mins$enrollment<-scale(mins$enrollment, center=TRUE, scale=FALSE)
mins$fift.100<-scale(mins$fift.100, center=TRUE, scale=FALSE)
mins$fift<-scale(mins$fift, center=TRUE, scale=FALSE)
mins$twen.100<-scale(mins$twen.100, center=TRUE, scale=FALSE)
mins$twen<-scale(mins$twen, center=TRUE, scale=FALSE)
mins$tfiv.100<-scale(mins$tfiv.100, center=TRUE, scale=FALSE)
mins$tfiv<-scale(mins$tfiv, center=TRUE, scale=FALSE)

mod.mins<-stan_glmer(enrollment~age+fift.100+fift+fift.100:fift+twen.100+twen+twen.100:twen+
                       tfiv.100+tfiv + tfiv.100:tfiv + (1|DMA), data=mins)

mod.mins
plot(mod.mins, pars="beta")

### Models - miles!
miles$DMA<-as.numeric(as.factor(miles$DMA))
miles$age<-as.numeric(miles$age)
miles$enrollment<-as.numeric(miles$enrollment)
miles$one.100<-as.numeric(miles$one.100)
miles$one<-as.numeric(miles$one)
miles$two.100<-as.numeric(miles$two.100)
miles$two<-as.numeric(miles$two)
miles$three.100<-as.numeric(miles$three.100)
miles$three<-as.numeric(miles$three)
miles$four.100<-as.numeric(miles$four.100)
miles$four<-as.numeric(miles$four)
miles$five.100<-as.numeric(miles$five.100)
miles$five<-as.numeric(miles$five)
miles$six.100<-as.numeric(miles$six.100)
miles$six<-as.numeric(miles$six)
miles$seven.100<-as.numeric(miles$seven.100)
miles$seven<-as.numeric(miles$seven)
miles$eight.100<-as.numeric(miles$eight.100)
miles$eight<-as.numeric(miles$eight)
miles$nine.100<-as.numeric(miles$nine.100)
miles$nine<-as.numeric(miles$nine)
miles$ten.100<-as.numeric(miles$ten.100)
miles$ten<-as.numeric(miles$ten)

miles$enrollment<-scale(miles$enrollment, center=TRUE, scale=FALSE)
miles$one.100<-scale(miles$one.100, center=TRUE, scale=FALSE)
miles$one<-scale(miles$one, center=TRUE, scale=FALSE)
miles$two.100<-scale(miles$two.100, center=TRUE, scale=FALSE)
miles$two<-scale(miles$two, center=TRUE, scale=FALSE)
miles$three.100<-scale(miles$three.100, center=TRUE, scale=FALSE)
miles$three<-scale(miles$three, center=TRUE, scale=FALSE)
miles$four.100<-scale(miles$four.100, center=TRUE, scale=FALSE)
miles$four<-scale(miles$four, center=TRUE, scale=FALSE)
miles$five.100<-scale(miles$five.100, center=TRUE, scale=FALSE)
miles$five<-scale(miles$five, center=TRUE, scale=FALSE)
miles$six.100<-scale(miles$six.100, center=TRUE, scale=FALSE)
miles$six<-scale(miles$six, center=TRUE, scale=FALSE)
miles$seven.100<-scale(miles$seven.100, center=TRUE, scale=FALSE)
miles$seven<-scale(miles$seven, center=TRUE, scale=FALSE)
miles$eight.100<-scale(miles$eight.100, center=TRUE, scale=FALSE)
miles$eight<-scale(miles$eight, center=TRUE, scale=FALSE)
miles$nine.100<-scale(miles$nine.100, center=TRUE, scale=FALSE)
miles$nine<-scale(miles$nine, center=TRUE, scale=FALSE)
miles$ten.100<-scale(miles$ten.100, center=TRUE, scale=FALSE)
miles$ten<-scale(miles$ten, center=TRUE, scale=FALSE)

mod.miles<-stan_glmer(enrollment~age+one.100+one+one.100:one+two.100+two+two.100:two+
                        three.100+three+three.100:three +four.100+four+four.100:four+
                        five.100+five+five.100:five +six.100+six+six.100:six +
                        +seven.100+seven+seven.100:seven + 
                        +eight.100+eight+eight.100:eight + 
                        +nine.100+nine+nine.100:nine +
                        +ten.100+ten+ten.100:ten +(1|DMA), data=miles)

mod.miles
plot(mod.miles, pars="beta")
