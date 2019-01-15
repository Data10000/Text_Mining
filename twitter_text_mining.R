#Twitter data mining and Visualization in Wordcloud

#Clear Workspace
rm(list=ls())

setwd(paste0("C:\\Users\\",Sys.info()["user"], "\\Documents\\Facebook and Twitter text Mining"))

#Check and download required packages

required_lib <- c("twitteR","ROAuth", "wordcloud2", "wordcloud", "httr", "tm", "RCurl", "stringr")

suppressMessages({suppressWarnings({
  for(ilib in required_lib){
    if(!require(ilib, character.only = TRUE)){
      install.packages(ilib)
    }
    library(ilib, character.only = TRUE)
  }
  
})})


#Navigate to (https://developer.twitter.com/) to set up an account and create an app.  You will allow you to create an api.
#Navigate to Keys and Tokens. We will use these will give us permission to your account.

consumer_key <- "my_consumer_key"
consumer_secret <-"my_consumer_secret"
access_token <- "my_access_token"
access_token_secret <- "my_access_secret"

#Create twitter Authorization

setup_twitter_oauth(consumer_key =  consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_token_secret)

#Accept authorization by typing yes on request

searched_twits <- searchTwitteR("RiversideAttack", lang = "en", n = 1400, resultType = "recent") # Maximum of 1500 tweets(n=1500))

#To vector
searched_twits_vec <- sapply(searched_twits, FUN = function(y) str_to_lower(y$getText()))
searched_twits_vec[1:5]



# SInce we have our vector we now convert it to corpus for more manipulation and cleaning
searched_twits_cop <- Corpus(VectorSource(searched_twits_vec))

#######Data Cleaning #######################################################################
suppressWarnings({
corp_RiversideAttack_rm <- tm_map(searched_twits_cop, removeWords, c("nairobi", "riverside", "riversideattack", "kenya", "rt", "can", "please", "put", "??????", "victorwanyama", "robertalai", "bonifacmwangi", "matiangi"))
corp_rm_other <- tm_map(corp_RiversideAttack_rm, removeNumbers)
corp_rm_other1<- tm_map(corp_rm_other, removePunctuation)
corp_rm_other2<- tm_map(corp_rm_other1, removeWords, stopwords(kind = "en"))
corp_rm_other3<- tm_map(corp_rm_other2, stripWhitespace)
})

wordcloud(corp_rm_other3, random.order = F, colors = rainbow(100), max.words = 1000)

