library(devtools)
library(roxygen2)

#Fixing the data

data_got4=read.csv("character-deaths.csv")
data_got4["Cumulative.Death.Chapter"] <- NA
data_got4$Allegiances <- lapply(data_got4$Allegiances,gsub,pattern="House ",replacement="")
data_got4$Death.Chapter <- with(data_got4, ifelse(!is.na(Book.of.Death) & is.na(Death.Chapter),0, Death.Chapter))
data_got4$Cumulative.Death.Chapter <- with(data_got4, ifelse(Book.of.Death == 1, Death.Chapter,
                                                             ifelse(Book.of.Death == 2, Death.Chapter+nchaptersb1,
                                                                    ifelse(Book.of.Death == 3, Death.Chapter+nchaptersb2,
                                                                           ifelse(Book.of.Death == 4, Death.Chapter+nchaptersb3,
                                                                                  ifelse(Book.of.Death == 5, Death.Chapter+nchaptersb4,
                                                                                         NA))))))
data_got4$Cumulative.Death.Chapter<- with(data_got4, ifelse(is.na(Cumulative.Death.Chapter),nchaptersb5+50, Cumulative.Death.Chapter))

data_got4

#Creating a function for life expectancy conditional on allegiance
#' Life expectancy by allegiances
#' @param x House of Allegiance
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.allegiance <- function(x){
  mean(data_got4[data_got4$Allegiances==x, "Cumulative.Death.Chapter"])
}
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

#Functions

#Creating a function for life expectancy conditional on nobility (0 for not noble and 1 for noble).
#' Life expectancy by nobility
#' @param x Dummy: 0 for not noble and 1 for noble
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.nobility <- function(x){
  return(mean(data_got4[data_got4$Nobility==x, "Cumulative.Death.Chapter"]))
}
lexpectancy.nobility("0")

setwd("./GameOfCodes/R")
