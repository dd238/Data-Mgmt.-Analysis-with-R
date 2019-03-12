# 1. Compare and contrast: lapply, sapply, and apply (in your own words, no directly copy and pasting from stackoverflow, one sentence or so is fine):

# 1a. Explain what each of these functions do.
#lappy returns a list of list, sapply returns a simplified vector if it can be flattened (the lists being flattened must be of the same dimension)
#apply allow you to apply a function to a specific subset of a matrix using the MARGIN parameter

# 1b. Include details like what the main input of the function is (the variable you are iterating over), and what the output of the function is. 
#lappy takes a vector/list and function as parameters, outputs a list of lists
#sapply takes a vector/list and function as parametes, outputs a vector if it can simplify a list of lists
#apply takes a matrix/array, a MARGIN (rows/columns to iterate over), and function to be applied 

# 1c. Highlight the main differences between each of these functions. 
#lapply and sapply similar parameter yet return different objects list vs vector
#apply takes a matrix and allows you to interate of specific subsets of the matrix

# 1d. When might you use one of these functions over another?
#use lapply for lists of different lengths
#use sapply for lists of equal length
#use apply for matrices in which you want to apply a function to a subset

# 1e. When should you use an apply statement vs a loop? (there are many reasons why you should use one vs the other)
#if you don't know how many values are in the vector or matrix you are interating over an apply would be more useful
#a loop allows a little more control if you need to handle things like NA values

#2a. Use lapply, and iterate over the vector c(23,41,17,5,81) with the “collatz” function, save this in the variable myLIST
collatz = function(x){
  v = c()
  steps = 0
  while(x != 1){
    if(x%%2 == 1){
      x = (x*3)+1
    }else{
      x = x/2
    }
    steps=steps+1
    v[steps] = x
  }
  return(v)
}

v = c(23,41,17,5,81)
myLIST = lapply(v, function(v) collatz(v))

#2b. Now use sapply and iterate over your new list, myLIST, with the function “max”, save this in the variable myVEC. 
#Which number in the initial vector c(23,41,17,5,81) produces the largest number along its collatz process, and what is that number?
myVEC = sapply(myLIST, function(myLIST) max(myLIST))

#41 produces the largest number -> 9232

#2c. For the two previous problems a and b, does switching lapply and sapply in each problem change the format of the output. If so, why? If not, why?
myLIST2 = sapply(v, function(v) collatz(v)) 
#myLIST and myLIST are the same as sapply cannot flatten a list of lists that are unequal in size 
myVEC2 = lapply(myLIST2, function(myLIST2) max(myLIST2))
#myVEC is a vector and myVEC2 is a list of lists so these aren't the same either because sapply is flattening myVEC to a vector
#where as lapply returns a list

#3. Load the dataset Births from the mosaicData package and store it in the variable myDATA
install.packages("mosaicData")
library("mosaicData")
myDATA = Births

#3a. Use sapply to iterate over myDATA (which iterates over the columns), and take the mean of each column)
sapply(myDATA, function(x) mean(x))

#3b. Use lapply with the function top_DF to produce a list of data.frames, where k is varying from c(10,20,30,40,50) 
#(hint: the variable that is varying is the variable you iterate over ) and the DF parameter of top_DF is fixed to myDATA, save this in the variable top_list
top_k = function(DF, sortcol, k=10, othercols= c("date")){
  top_k_col_order = order(DF[[sortcol]],decreasing=T)[1:k]
  vector_of_column_names = c("date",sortcol)
  top_k_DF = DF[top_k_col_order,vector_of_column_names]
  return(top_k_DF)
}
 
top_list = lapply(c(10,20,30,40,50), function(x) top_k(DF=myDATA,sortcol="births",k=x))

#3c. Run the lines :
v = names(myDATA)
ind = sapply(myDATA,function(x) class(x)[1] ) == "integer"
int_names = v[ind]
# produces a character vector, int_names, which contain the names of integer cols

#Iterate over int_names with lapply with the function, top_DF, where each of values of int_names are being passed into the sortcol input in the lapply funciton
lapply(int_names, function(x) top_k(DF=myDATA,sortcol=x))

#3d. Use lapply with the function DF_bydate to create a list of data.frames, where each data.frame contains all the entries from myDATA for a given month 
#that also have day_of_month equal to 13 (you output list should have 12 data.frames in it, one for each month)
DF_bydate = function(DF,the_month,the_day){
  logicaldate = (DF[, "month"]== the_month) & (DF[, "day_of_month"]==the_day) 
  newDF = DF[logicaldate,]
  return(newDF)
}

lapply(1:12, function(x) DF_bydate(DF=myDATA,the_month=x,the_day = 13))

#4. Create a 4 (rows) by 3 (cols) matrix, m, that contains the numbers 1 through 12 in order
m = matrix(1:12, nrow = 4)

#4a. Use apply and iterate over the rows of m, and calculate the product of each row (use the function prod, which is the multiplication version of sum). 
#What is that product?
apply(m, 1, prod) # -> [1] 45 120 231 384  

#4b. Do the same but iterate over the columns instead
apply(m, 2, prod) # -> [1]  24  1680 11880


