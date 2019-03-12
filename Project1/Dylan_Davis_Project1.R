#1. Use install.packages and library functions to install and load the package “mosaicData”
install.packages("mosaicData")
library("mosaicData")

#2. Store the dataset Births in your own variable myDATA 
myDATA = Births

#3a. What data type is myDATA? -> "data.frame"
class(myDATA)

#3b.  What are the dimensions of myDATA? -> 7305 by 8
dim(myDATA)

#3c. What data type is each column in myDATA? -> 
# $ date        : Date
# $ births      : int  
# $ wday        : Ord.factor 
# $ year        : int  
# $ month       : int 
# $ day_of_year : int  
# $ day_of_month: int  
# $ day_of_week : int
str(myDATA)

#3d. What is the mean of each column in myDATA? 
mean(myDATA[,"date"]) # -> "1979-01-01"
mean(myDATA[,"births"]) # -> 9648.94
mean(myDATA[,"wday"]) # -> NA
mean(myDATA[,"year"]) # -> 1978.501
mean(myDATA[,"month"]) # -> 6.52293
mean(myDATA[,"day_of_year"]) # -> 183.7536
mean(myDATA[,"day_of_month"]) # -> 15.72964
mean(myDATA[,"day_of_week"]) # -> 4.000274

#4a. How many unique values are there in the column year from myDATA?
length(unique(myDATA[,"year"])) # -> 20

#4b. Does myDATA include leap year dates? What are a couple of ways to figure this out?
# Stores a subdata frame of myDATA in febDATA where the month == 2 or February
febDATA = myDATA[myDATA$month==2,]
# Prints a subdata frame of febDATA where the day of the month is greater than 28, which would be a leap year
febDATA[febDATA$day_of_month>28,] # -> 5

#4c. Select of subset of rows from myDATA that do not contain the date Feb 29th
# This function adds the rows of the two subdata frames together
rbind(myDATA[myDATA$month!=2,],febDATA[febDATA$day_of_month<=28,])

#5. Return a data.frame containing the top 10 births in order from highest to lowest, and the date that corresponds with them
# The inner function 'order' orders the output births from highest to lowest by setting decreasing to true
# The c(1,2) combines the first two columns, which is just the date and births
# The outer function head outputs the firs nth outputs, which in this case is 10
myDATA[head(order(myDATA$births, decreasing = TRUE),10),c(1,2)]
# or
myDATA[order(myDATA$births, decreasing = TRUE)[1:10],c(1,2)]

#6a. Use the “cut” function to create a new variable factor variable, birth_rate, with 3 categories (“low”, “med”, “high”) (“low” has the range (0,8000], “med” range is (8000,10000], “high” range is 10000+)
# This function cuts catagorizes the birth column into 1 of 3 categories, which are defined by the respective breaks
cut(myDATA$births,breaks=c(0,8000,10000,Inf), labels=c("low","med","high"),include.lowest=TRUE)

#6b. Add this new variable as a column to myDATA that corresponds to each row
myDATA["birth_rate"] = birth_rate

#7. How is the pace of the class so far? Do you feel it is too fast, too slow, or just right?
# The pace of the class is a little slow for me. I really like the content and the first assignment, but I am ready for more.