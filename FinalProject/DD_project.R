rm(list = ls())

library("rvest")
library("tidyverse")

get_episode_names = function(url){
  #get episode number
  #Specifying the url for desired website to be scraped
  web_url = url
  #Reading the HTML code from the website
  webpage = read_html(url)
  script <- webpage %>% html_node(".season-episodes")
  full_text <- html_text(script, trim = TRUE)
  head(full_text)
  
  split_on_period_space = unlist(full_text)
  spaces_removed = gsub(" ", "", split_on_period_space)
  spaces_removed = tolower(spaces_removed)
  split_on_period_space = strsplit(spaces_removed, split = "[0-9][.]")
  split_on_period_space = unlist(split_on_period_space)
  remove_punct = gsub("[[:punct:]]", "", split_on_period_space)
  remove_num = gsub("[0-9]+", "", remove_punct)
  clean_episode_names = remove_num[-which("season" %in% remove_num)]
  return (clean_episode_names)
}

#rick and morty episode names
rm_season_names = sapply(1:3, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=rick-and-morty-2013&season=",x)))

#sum(sapply(rm_season_names, function(x) length(x))) #check to make sure all episodes are accounted for
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt2861424"),]) #missing 0 episodes

#south park episode names
sp_all_season_names = sapply(1:22, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=south-park&season=",x)))

#sum(sapply(sp_all_season_names, function(x) length(x))) # missing 1 episode
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt0121955"),])

#family guy episode names
fg_all_season_names = sapply(1:17, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=family-guy&season=",x)))

#sum(sapply(fg_all_season_names, function(x) length(x))) #missing 3 episodes
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt0182576"),])

#simpsons episode names
simp_all_season_names = sapply(1:30, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=the-simpsons&season=",x)))

#sum(sapply(simp_all_season_names, function(x) length(x))) #missing 0 episodes
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt0096697"),])

#bob's burgers episode names
bb_all_season_names = sapply(1:9, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=bobs-burgers&season=",x)))

#sum(sapply(bb_all_season_names, function(x) length(x))) #missing 0 episodes 
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt1561755"),])

#futurama episode names
fut_all_season_names = sapply(1:7, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=futurama&season=",x)))

#sum(sapply(fut_all_season_names, function(x) length(x))) #missing 0 episodes,
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt0149460"),])
fut_all_season_names[[7]] = fut_all_season_names[[7]][-21] #remove empty value

#bojack horseman episode names
bh_all_season_names = sapply(1:5, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=bojack-horseman-2014&season=",x)))

#sum(sapply(bh_all_season_names, function(x) length(x))) #missing 0 episodes 
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt3398228"),])

#archer episode names
arch_all_season_names = sapply(1:9, function(x) get_episode_names(paste0("https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=archer&season=",x)))

#sum(sapply(arch_all_season_names, function(x) length(x))) #missing 1 episodes 
#dim(imdb_merged_data[which(imdb_merged_data$parentTconst == "tt1486217"),])

#combine all tv show episodes into one list
all_shows_episodes = c(rm_season_names,sp_all_season_names,fg_all_season_names,simp_all_season_names,bb_all_season_names,fut_all_season_names,bh_all_season_names,arch_all_season_names)

#flatten the list of lists to one list
all_shows_episodes_names = unlist(all_shows_episodes)

#check the length to make sure correct number of total episodes
length(all_shows_episodes_names)

setwd("/Users/dylandavis/Documents/Math 370/FinalProject")
save(all_shows_episodes_names, file = "all_shows_episodes.rda") #save episode names as an RDA
load(file = "all_shows_episodes.rda")


#=========Code above gets episode names================================================


setwd("/Users/dylandavis/Documents/Math 370/FinalProject")

imdb_files = list.files()[grep("tsv",list.files())]

imdb_data = lapply(imdb_files,function(x)  read.csv(x,header=T,sep="\t") )
names(imdb_data) = imdb_files

imdb_merged_data = merge(imdb_data[[1]], imdb_data[[2]], by.x = "tconst", by.y = "tconst")
imdb_merged_data = merge(imdb_merged_data, imdb_data[[3]], by.x = "tconst", by.y = "tconst")

original_title = gsub("[[:punct:]]", "", imdb_merged_data$originalTitle) #remove punctuation
original_title = gsub("[0-9]+", "", original_title) #remove numbers
original_title = gsub(" ", "", original_title) #remove spaces
original_title = tolower(original_title) #make everything lowercase
imdb_merged_data$originalTitle = original_title #replace originalTitle with modified originalTitle

#rick and morty parentTconst = tt2861424
#south park parentTconst = tt0121955
#family guy parentTconst = tt0182576
#simpsons parentTconst = tt0096697
#bob's burgers parentTconst = tt1561755
#futurama parentTconst = tt0149460
#bojack horseman parentTconst = tt3398228
#archer episode names parentTconst = tt1486217

#make sure length of this matches the number of episodes in
length(which(imdb_merged_data$parentTconst %in% c("tt2861424","tt0121955","tt0182576","tt0096697","tt1561755","tt0149460","tt3398228","tt1486217")))

#subset imdb_merged_data by only shows that are: rick and morty, south park, family guy, simpsons, bob's burgers, futurama, bojack horseman, archer episode names
imdb_merged_data = imdb_merged_data[which(imdb_merged_data$parentTconst %in% c("tt2861424","tt0121955","tt0182576","tt0096697","tt1561755","tt0149460","tt3398228","tt1486217")),]

original_title = gsub("[[:punct:]]", "", imdb_merged_data$originalTitle)
original_title = gsub("[0-9]+", "", original_title)
original_title = gsub(" ", "", original_title)
original_title = tolower(original_title)
imdb_merged_data$originalTitle = original_title

save(imdb_merged_data, file = "imdb_merged_data.rda") #save imdb_merged_data as an RDA
load(file = "imdb_merged_data.rda")

#=========Code above gets IMDB episode info and merges table together================================================

#install.packages("rvest")
library(rvest)
## Loading required package: xml2

# Which tv show, if you want another show, first check on the website which tv show url is used. 
tvshow <- "the-simpsons"

directory = paste("/Users/dylandavis/Documents/Math 370/FinalProject/DD_DATA", tvshow, sep="")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
setwd(directory)


# Setting base url and complte url
baseurl <- "http://www.springfieldspringfield.co.uk/"
url <- paste(baseurl,"episode_scripts.php?tv-show=", tvshow, sep="")


#the first href tag of s01e01 with the class class="season-episode-title". This class we need to select as our node.
# read the HTML page
scrape_url <- read_html(url)
# node selector
s_selector <- ".season-episode-title"

# scrape href nodes in .season-episode-title
all_urls_season <- html_nodes(scrape_url, s_selector) %>%
  html_attr("href")

str(all_urls_season)

head(all_urls_season)

tail(all_urls_season)

# Loop through all season urls 
for (i in all_urls_season) {
  uri <- read_html(paste(baseurl, i, sep="/"))
  # same thing here first check which node we need to select, so forst do a inspect of the site
  script_selector <- ".scrolling-script-container"
  # scrape all script text to a variable
  text <- html_nodes(uri, script_selector) %>% 
    html_text()
  
  # Get last five characters of all_urls_season as season for saving this to seperate text files
  substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
  }
  seasons <- substrRight(i, 5)
  # Write each script to a seperate text file
  write.csv(text, file = paste(directory, "/", tvshow, "_", seasons, ".txt", sep=""), row.names = FALSE)
}

#install.packages("tm")
library(tm)
## Loading required package: NLP
# set filepath to scripts
cname <- file.path(directory)
# see if the filepath contains our scripts
(docname <- dir(cname))

# Crete a Corpus of the text files so we can do some analysis
docs <- Corpus(DirSource(cname), readerControl = list(id=docname))
# Show summary of the Corpus, we have 40 document in our Corpus
summary(docs)

inspect(docs[1])

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removePunctuation)
#docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))

#install.packages("SnowballC")
library(SnowballC)
#docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)

inspect(docs[1])

#docs <- tm_map(docs, PlainTextDocument)

# Create a tdm
tdm <- TermDocumentMatrix(docs)
# Add readable columnnames, in our case the document filename
docname <- gsub("rick_morty_", "",docname)
docname <- gsub(".txt", "",docname)
docname <- paste("s",docname, sep="")
colnames(tdm) <- docname
# Show and inspect the tdm
tdm

adtm.m<-as.matrix(tdm)
adtm.df<-as.data.frame(adtm.m)

word_count_df = t(adtm.df)

dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- docname
dtm

dtm.m = as.matrix(dtm)
SIMPdf = as.data.frame(dtm.m) #MERGE THIS DATAFRAM WITH OTHER TV SHOWS

#===Did the process above for all TV shows===============================================================

setwd("/Users/dylandavis/Documents/Math 370/FinalProject")
save(SIMPdf, file = "SIMP_WC.rda") #save episode names as an RDA 

load(file = "RM_WC.rda")
load(file = "SP_WC.rda") 
load(file = "FG_WC.rda") 
load(file = "SIMP_WC.rda")
load(file = "BB_WC.rda")
load(file = "FUT_WC.rda")
load(file = "BH_WC.rda")
load(file = "ARCH_WC.rda")

lapply(RMdf, function(x) if(sum(x)<1){print(x)})

library(plyr)

all_shows_word_count = do.call(rbind.fill, list(RMdf,SPdf,FGdf,SIMPdf,BBdf,FUTdf,BHdf,ARCHdf)) 
save(all_shows_word_count, file = "all_shows_word_count.rda")

all_shows_word_count[is.na(all_shows_word_count)] = 0 #replace all NA's with 0's

save(all_shows_word_count, file = "all_shows_word_count.rda")
load(file = "all_shows_word_count.rda")

col_names = unlist(sapply(all_shows_word_count, function(x) which(mean(x)>6))) #NEXT GRAB THE MEAN WORD COUNT FOR EACH ROW
col_names = attributes(col_names)
col_names = unlist(col_names)
ind = which(names(all_shows_word_count) %in% col_names)
all_shows_word_count_sub = all_shows_word_count[,ind]

dim(all_shows_word_count)
dim(all_shows_word_count_sub)

save(all_shows_word_count_sub, file = "all_shows_word_count_sub.rda")

#add a column to these data sets with the episode names so it can be merged with imdb$originalTitle
all_shows_word_count_sub$originalTitle = all_shows_episodes_names

#======================================Code above get the word count for ALL SHOWS, BINDS THEM, THEN ELIMINATES COLUMNS BASED ON MEAN WORD COUNT ============================
setwd("/Users/dylandavis/Documents/Math 370/FinalProject")
load(file = "all_shows_word_count_sub.rda")
load(file = "imdb_merged_data.rda")

test_merge = merge(all_shows_word_count_sub, imdb_merged_data, by.x = "originalTitle", by.y = "originalTitle") #merges WORD COUNT DATA FRAME and IMDB INFO DATA FRAME

test_merge$tconst = NULL
test_merge$parentTconst = NULL
test_merge$primaryTitle = NULL
test_merge$isAdult = NULL
test_merge$endYear = NULL
test_merge$titleType = NULL
test_merge$genres = NULL 

ind_original_title_empty = which(test_merge$originalTitle == "")

test_merge = test_merge[-ind_original_title_empty,]

test_merge$originalTitle = NULL

test_merge$startYear = as.numeric(as.character(test_merge$startYear))
test_merge$runtimeMinutes = as.numeric(as.character(test_merge$runtimeMinutes))
test_merge$seasonNumber = as.numeric(as.character(test_merge$seasonNumber))
test_merge$episodeNumber = as.numeric(as.character(test_merge$episodeNumber))

final_merge = na.omit(test_merge)

setwd("/Users/dylandavis/Documents/Math 370/FinalProject")
save(final_merge, file = "final_merge.rda")
load(file = "final_merge.rda")


#==========================================================================================================
library(MASS)
library(ggplot2)

set.seed(2)

w      = sample(1:length(final_merge[,1]),round(.9*length(final_merge[,1]))) #formart as numerics
train  = final_merge[w,]
val    = final_merge[-w,]

train_plot = ggplot()+geom_point(data=train, aes(x=come, y=averageRating))+ggtitle("TRAINING SET")
train_plot
test_plot = ggplot()+geom_point(data=val, aes(x=come, y=averageRating))+ggtitle("TEST SET")
test_plot

train_plot = ggplot()+geom_point(data=train, aes(x=hey, y=averageRating))+ggtitle("TRAINING SET")
train_plot
test_plot = ggplot()+geom_point(data=val, aes(x=hey, y=averageRating))+ggtitle("TEST SET")
test_plot


model1 = glm(averageRating~.,family=gaussian,data=train)
summary(model1)

plot(model1)

predicted = predict(model1,newdata = val)

ind_of_actual = as.numeric(unlist(attributes(predicted)))

actual = final_merge$averageRating[ind_of_actual]

MAE = mean(abs((actual-predicted)/actual))

MAE

#MAE = 0.09855234


#=========TEST CODE FOR PREDICTIVE MODEL================================================