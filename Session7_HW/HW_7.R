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
data_got[839,]
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
