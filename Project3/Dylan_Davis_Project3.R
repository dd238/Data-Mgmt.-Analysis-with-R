rm(list = ls())

vars2grab = c("YEAR","PATWT","TOTPROC","AGE","PAINSCALE","LOV","SEX","ETHIM","ADMITHOS","RACER","REGION","PAYTYPER")

setwd("/Users/dylandavis/Documents/Math 370/CDC_10_Years_of_Data")
list.files()

file_names = list.files()

E = lapply(file_names,function(x)  read.csv(x,header=T) )
names(E) = file_names

(lapply(E,function(x) dim(x)))

########### checks for names not in each data.frame in list #######

checks4names = function(DF,vector2check){
  ind = sapply(vector2check, function(x) any(names(DF) == x ) )
  r = vector2check[ !ind ]
  return(r)
  }

bad_names =  lapply(E, function(x)  checks4names(DF = x,vector2check = vars2grab) )

E_some_bad_names_ind = which(sapply(bad_names, function(x) length(x) ) != 0)
 
name_patterns = c("PAIN","ETH","PAY")

check_fun =  function(patterns_vec,DF){
  mylist =  lapply(patterns_vec, function(y) names(DF)[which(grepl(y,names(DF)))] )
  return(mylist)
  }

lapply(E[E_some_bad_names_ind], function(x) check_fun(name_patterns,x) )

#determined:
# in 2006, (E[[1]]): "PAINSCALE" is "PAIN", "ETHIM" is "ETHNIC", "PAYTYPER" is "PAYTYPE"
# in 2007, (E[[2]]): "PAINSCALE" is "PAIN", "PAYTYPER" is "PAYTYPE"
# in 2008, (E[[3]]): "PAINSCALE" is "PAIN"

##<------------------------------- start new code after here

#loops through the indices of E_some_bad_names_ind 1:3
#subsets each names(E[[i]]) by checking to see if it contains the value in brackets
#if it does contain that column name it assigns the name outside the brackets to the column
for(i in E_some_bad_names_ind){
  (names(E[[i]])[ names(E[[i]]) == "ETHNIC" ] = "ETHIM")
  (names(E[[i]])[ names(E[[i]]) == "PAYTYPE" ] = "PAYTYPER")
  (names(E[[i]])[ names(E[[i]]) == "PAIN" ] = "PAINSCALE")
}

#2006 
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - 99
#"AGE" - no change
#"PAINSCALE" - 5 
#"LOV" - 9999
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - 0,8
NA_2006 = list(NA,NA,c(99),NA,c(0,5),c(9999),NA,NA,NA,NA,NA,c(0,8))

#2007
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2007 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2008
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2008 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2009
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2009 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2010
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2010 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2011
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2011 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2012
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2012 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2013
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2013 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2014
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2014 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#2015
#"YEAR" - no change
#"PATWT" - no change
#"TOTPROC" - -9
#"AGE" - no change
#"PAINSCALE" - -8,-9
#"LOV" - -9
#"SEX" - no change
#"ETHIM" - no change
#"ADMITHOS" - no change
#"RACER" - no change
#"REGION" - no change
#"PAYTYPER" - -8,-9
NA_2015 = list(NA,NA,c(-9),NA,c(-8,-9),c(-9),NA,NA,NA,NA,NA,c(-8,-9))

#each of the statements above creates a list with vectors that contain the values in each column
#that need to be mapped to NA based off the CDC pdf for that year

#creates a list of lists 
NA_YEARS = list(NA_2006,NA_2007,NA_2008,NA_2009,NA_2010,NA_2011,NA_2012,NA_2013,NA_2015,NA_2015)

values2NA = function(vec,na_vec){ 
  myclass = class(vec)
  vec = as.character(vec)
  vec[vec %in% na_vec] = NA
  func = get( paste0("as.",myclass) )
  vec = func(vec)
  return(vec)
}

#subset E by vars2grab, thus E with only 12 columns
E = lapply(E, function(x) x[,vars2grab])

f = function(x,y){as.data.frame(mapply(function(xx,yy) values2NA(xx,yy), x,y,SIMPLIFY = F))}
E2 = mapply(function(x,y) f(x,y),E,NA_YEARS,SIMPLIFY = F)

#scales PAINSCALE column values in the years 2006,2007,2008 in E2 from (1-4) to (0-3)
lapply(1:3, function(x) as.integer(as.character(cut(E2[[x]]$PAINSCALE, c(-1,1,2,3,4),labels = c(0,1,2,3)))))
#scales PAINSCALE column values in the years 2009,2010,2011,2012,2013,2014,2015 in E2 from (1-4) to (0-3)
lapply(4:10, function(x) as.integer(as.character(cut(E2[[x]]$PAINSCALE, c(-1,0,3,6,10),labels = c(0,1,2,3)))))

combined_E_subset = do.call(rbind, E2)

setwd("/Users/dylandavis/Documents/Math 370")
write.csv(combined_E_subset, file = "clean_subE.csv", row.names = FALSE)
