#Q3
data_got=read.csv("character-deaths.csv")

#Q4
colnames(data_got)

#Q5
summary(data_got)

#random comments on descriptive statistics: 
#All characters die between the years 297 and 299, 
#the median death year is 299 and the mean 299.2
#Out of 917 characters 612 are still alive, that's only about 2/3!
#Considering there mediocre importance in the books (at least compared to
#Starks and Lannisters) there are surprisingly many Greyjoys out there (51)!
# The mean book of death is 2.928, so this seems quite nicely distributed.
#The mean gender is 0.8288, so there are clearly too many men in GoT!

#Q6
data_got[410,]
data_got2[,"Death.Year"]
#I assume that charactter who are still alive at the year 300 
#live for 25 years more
data_got2=read.csv("character-deaths.csv")
colnames(data_got2)
data_got2[is.na(data_got2)]=325
require(data.table)
data_got3=data.table(data_got2,key = "Allegiances")
data_got3[,Death.Year.by.Allegiances := mean(Death.Year), by=Allegiances] 
data_got3

library(devtools)
library(roxygen2)

devtools::create("GameofCodes")

nchaptersb1 <- 73
nchaptersb2 <- sum(nchaptersb1+70)
nchaptersb3 <- sum(nchaptersb2+82)
nchaptersb4 <- sum(nchaptersb3+46)
nchaptersb5 <- sum(nchaptersb4+73)

data_got4[, "Book.of.Death"][is.na(data_got4[, "Book.of.Death"])] <- nchaptersb5

data_got4=read.csv("character-deaths.csv")
data_got4["Cumulative.Death.Chapter"] <- NA
data_got4$Cumulative.Death.Chapter <- data_got4$Gender + data_got4$Nobility

data_got4=read.csv("character-deaths.csv")
data_got4["Cumulative.Death.Chapter"] <- NA
data_got4[, "Book.of.Death"][is.na(data_got4[, "Book.of.Death"])] <- 0

data_got4=read.csv("character-deaths.csv")
data_got4["Cumulative.Death.Chapter"] <- NA
#maybe exclude below
data_got4$Allegiances <- lapply(data_got4$Allegiances,gsub,pattern="House ",replacement="")
#until here
data_got4$Death.Chapter <- with(data_got4, ifelse(!is.na(Book.of.Death) & is.na(Death.Chapter),0, Death.Chapter))
data_got4$Cumulative.Death.Chapter <- with(data_got4, ifelse(Book.of.Death == 1, Death.Chapter,
                                                             ifelse(Book.of.Death == 2, Death.Chapter+nchaptersb1,
                                                                    ifelse(Book.of.Death == 3, Death.Chapter+nchaptersb2,
                                                                           ifelse(Book.of.Death == 4, Death.Chapter+nchaptersb3,
                                                                                  ifelse(Book.of.Death == 5, Death.Chapter+nchaptersb4,
                                                                      NA))))))
data_got4$Cumulative.Death.Chapter<- with(data_got4, ifelse(is.na(Cumulative.Death.Chapter),nchaptersb5+50, Cumulative.Death.Chapter))

data_got4


#right-censored data - alternative approach
require(data.table)
data_got5=data.table(data_got4,key = "Allegiances")
data_got5[,Death.Year.by.Allegiances := mean(Cumulative.Death.Chapter), by=Allegiances] 
#######

#Creating a function for life expectancy conditional on allegiance
#' Life expectancy by allegiances
#' @param x House of Allegiance
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.allegiance <- function(x){
  mean(data_got4[data_got4$Allegiances==x, "Cumulative.Death.Chapter"])
}
setwd("./GameOfCodes")

devtools::document()

 lexpectancy.allegiance("Stark")

#Creating a function for life expectancy conditional on gender (0 for not noble and 1 for noble).
#' Life expectancy by gender
#' @param x Dummy: 0 for not noble and 1 for noble
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.gender <- function(x){
  mean(data_got4[data_got4$Gender==x, "Cumulative.Death.Chapter"])
}
lexpectancy.gender("1")


#Creating a function for life expectancy conditional on nobility (0 for not noble and 1 for noble).
#' Life expectancy by nobility
#' @param x Dummy: 0 for not noble and 1 for noble
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.nobility <- function(x){
  return(mean(data_got4[data_got4$Nobility==x, "Cumulative.Death.Chapter"]))
}
lexpectancy.nobility("0")

data_got4$Allegiances <- lapply(data_got4$Allegiances,gsub,pattern="House ",replacement="")

