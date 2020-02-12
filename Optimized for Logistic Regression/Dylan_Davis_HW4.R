rm(list = ls())

setwd("/Users/dylandavis/Documents/Math 370/CDC_10_Years_of_Data")
library(plyr)

E = lapply(list.files(),function(x) read.csv(x,header = T))
e = E

w = list(
  lapply(E,function(x) names(x)[grep("DRUGID", names(x))]),
  lapply(E,function(x) names(x)[grep("CONTSUB", names(x))]),
  lapply(1:10,function(x) if(x<=8){c("DIAG1","DIAG2","DIAG3")}else{c("DIAG1","DIAG2","DIAG3","DIAG4","DIAG5")} ),
  lapply(1:10,function(x) if(x<=8){c("RFV1","RFV2","RFV3")}else{c("RFV1","RFV2","RFV3","RFV4","RFV5")} )
)

combine_columns = function(df_L,col_L){
  sub_df_L = mapply(function(x,y) x[,y] ,df_L,col_L ) 
  new_df = do.call(rbind.fill,sub_df_L)
  return(new_df)
}

newDF = do.call(cbind, lapply(w,function(x) combine_columns(e,x) ))

whitespace2NA = function(vec){
  myclass = class(vec)
  vec =  as.character(vec)
  vec[trimws(vec)==""] = NA
  func = get( paste0("as.",myclass) )
  vec = func(vec)
  return(vec)
}

newDF = as.data.frame(lapply(newDF,function(x)   whitespace2NA(x)))

#creats a vector with #'s 1-70
v = 1:length(newDF)
#interates over in value in the 70 columns and replaces -9 with NA
for(i in v){
  (newDF[[i]][newDF[[i]] == -9] = NA)
  (newDF[[i]][newDF[[i]] == "-9"] = NA)
}
contains_dash = list()
#creates a list of lists for each column in the data.frame
#where the index of a value containing a "-" is stored
for(i in v){
  contains_dash[i] = list(grep("-", newDF[[i]]))
}
contains_dash_ind = which(sapply(contains_dash, function(x) length(x) ) != 0)

#replace all "-" with 0
for(i in contains_dash_ind){
  newDF[[i]] = as.factor(as.character(gsub("-","",newDF[[i]])))
}

setwd("/Users/dylandavis/Documents/Math 370/Assigment 4 Resources")

op_codes = as.vector(read.csv("OpioidCodesOnly.csv",header=F)[,1])

#eliminates duplicate values from op_codes
op_codes = unique(op_codes)

#gets rid of all the values in the vector interated over from op_codes
for(i in c("d04766","a11242","d03826","n09045","n11008")){op_codes=op_codes[-grep(i,op_codes)]}

load(file = "ICD9_codes.rda")
load(file = "RFV_codes.rda")

practice_newDF = newDF[c(1,2),]

#list of all icd9 codes
ICD9_list = c(Alcohol_ICD9,Diabetes_ICD9,Mental_ICD9,Pain_ICD9)
op_list = list(OP = op_codes)
schedule_list = list(S2=2,S3=3,S4=4,S5=5,S6=6)

bicols = function(myDF, myList){
  bicol = function(myDF, myVector){return(apply(myDF, 1, function(x) any(x %in% myVector)))}
  biDF = as.data.frame(lapply(myList, function(x) bicol(myDF,x)))*1
  return(biDF)
}

diags = bicols(newDF[names(newDF)[grep("DIAG", names(newDF))]],ICD9_list)
rfvs = bicols(newDF[names(newDF)[grep("RFV", names(newDF))]],codes_RFV)
ops = bicols(newDF[names(newDF)[grep("DRUGID", names(newDF))]],op_list)
constubs = bicols(newDF[names(newDF)[grep("CONTSUB", names(newDF))]],schedule_list)

biDF = cbind(diags,rfvs,ops,constubs)

setwd("/Users/dylandavis/Documents/Math 370")
write.csv(biDF, file = "biDF.csv", row.names = FALSE)

#=============================test_cases==========================================#

load("testdf.Rda")

test = bicols(df_char, list("1or2"=c(1,2),"3"=3))
testy = cbind(df_char,test)

test = bicols(df_fac, list("1or2"=c(1,2),"3"=3))
testy = cbind(df_fac,test)

test = bicols(df_num, list("1or2"=c("1","2"),"3"="3"))
testy = cbind(df_num,test)

test = bicols(df_int, list("1or2"=c("1","2"),"3"="3"))
testy = cbind(df_int,test)

