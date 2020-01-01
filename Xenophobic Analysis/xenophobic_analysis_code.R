#Twitter Mining
#Navigate to (https://developer.twitter.com/) to set up an account and create an app.  You will allow you to generate an api.
#Navigate to Keys and Tokens. We will use these will give us permission to your account.


#Load required libraries
rm(list = ls())


suppressPackageStartupMessages({
  libs = c("stringr", "tm", "wordcloud", "wordcloud2", "ggplot2", "tidytext", "dplyr", "textdata", "sentimentr", "plotly", "textstem","svDialogs", "twitteR")
  #Stringr for text manipulation, tm for text mining, wordcloud(2) for drawing wordclouds, ggplot2 and plotly for graphs, dplyr for 
  #general data manipulations , textdata for sentiments dictionaries, 
  #sentmentr for sentiments analysis textstem for lammetization
  
  for(ilib in libs){
    if(!ilib%in%installed.packages()){
      install.packages(ilib)
    }
    library(ilib, character.only = T)
  }
})



#Implement with interactive inputs
consumer_key <- dlg_input(message = "Enter Consumer Key:")$res
consumer_secret <-dlg_input(message = "Enter Consumer Secret:")$res
access_token <- dlg_input(message = "Enter Access Token:")$res
access_token_secret <- dlg_input(message = "Enter Access Token Secret:")$res

#Create twitter Authorization

setup_twitter_oauth(consumer_key =  consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_token_secret)

#Accept authorization by typing yes on request


############################Hashtags
#saynotoxenophobia, #xenophobia, #xenephobicattacks, #southafricaxenophobia,#xenophobiaattacks, #NoToXenophobia, #WeAreAfrica
#StopXenophobia, #XenophobiaInSouthAfrica

searched_twits <- list()

hashtags <- c("saynotoxenophobia", "southafricaxenophobia", "XenophobiaInSouthAfrica")

for(ihash in hashtags){
  searched_twits <- c(searched_twits, searchTwitteR(ihash, lang = "en", n = 1400, resultType = "recent")) # Maximum of 1500 tweets(n=1500)
}

#To vector
searched_twits_vec <- sapply(searched_twits, FUN = function(y) str_to_lower(y$getText()))
searched_twits_vec[1:5]


searched_twits_vec1 <- str_replace_all(searched_twits_vec, pattern = "havent|haven't|haven't", "have not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "hasnt|hasn't|hasn't", "has not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "cannt|can't|can't", "cannot")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "couldnt|couldn't|couldn't", "could not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "arent|aren't", "are not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "isnt|isn't|isn't", "is not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "wouldnt|wouldn't|wouldn't", "Would not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "werent|weren't|weren't", "were not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "the're|they're", "they are")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "doesnt|doesn't", "does not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "dont|don't|don’t", "do not")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "you’re|you’re", "you are")

searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "\\.\\.\\.", "\\.")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "…", "\\.")

searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "\\?\\?\\?", "\\?")
searched_twits_vec1 <- str_replace_all(searched_twits_vec1, pattern = "!!!", "!")



searched_twits_vec1 <-str_to_sentence(str_squish(str_trim(str_replace_all(searched_twits_vec, pattern = "rt\\s@.*?:|@\\S{1,50}|#\\S{1,50}|https\\S{1,50}", replacement=" "))))
searched_twits_vec1


searched_twits_vec2 <- lemmatize_strings(searched_twits_vec1)

tweets_cop <- Corpus(VectorSource(searched_twits_vec2))



suppressWarnings({
  corp_rm_other <- tm_map(tweets_cop, removeNumbers)
  corp_rm_other1<- tm_map(corp_rm_other, removePunctuation)
  corp_rm_other2<- tm_map(corp_rm_other1,  removeWords, stopwords("english"))
  corp_rm_other3<- tm_map(corp_rm_other2, stripWhitespace)
  
})


#Create Dataframe with words and their frequencies

doc_mat <- TermDocumentMatrix(corp_rm_other3)
text_matrix <- sort(rowSums(as.matrix(doc_mat)),decreasing=TRUE) 
text_df <- data.frame(word = names(text_matrix),freq=text_matrix)
View(text_df)


text_df_clean <- text_df

text_df_clean$word <- str_to_sentence(as.character(text_df_clean$word))


text_df_clean <- text_df_clean%>% group_by(word)%>%summarise(freq = sum(freq))%>%arrange(desc(freq))


rownames(text_df_clean)<- text_df_clean$word

View(text_df_clean)

#Draw wordcloud
set.seed(1300)
dev.off() 
wordcloud(words = text_df_clean$word, freq  = text_df_clean$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))


wordcloud2(text_df_clean, size = .5,fontFamily = "Segoe UI", fontWeight = "bold",
           color = "random-dark", backgroundColor = "white", shuffle = F,
           rotateRatio = 0.3, shape = "circle")

#############################Sentiments Analysis

tweets <- searched_twits_vec2


sentiments <- sentiment(searched_twits_vec2)#Sentiments per sentences. It may results to duplicate sentences in case of  multiple sentences

#Extract zero sentiments

appended_minutes_df <- data.frame(texts = searched_twits_vec2, element_id= unique(sentiments$element_id))
final_sentiments_1 <- left_join(appended_minutes_df, sentiments, by= "element_id")

sentiments_zero <- final_sentiments_1[final_sentiments_1$sentiment==0, ]
View(sentiments_zero)


#Distribution of sentiment scores

h =2*IQR(final_sentiments_1$sentiment)*length(final_sentiments_1$sentiment)**(-1/3) ;h

bins = round((max(final_sentiments_1$sentiment)- min(final_sentiments_1$sentiment))/h, 0);bins


Sentiments_scores_distributions<- ggplot(data=final_sentiments_1)+geom_histogram(fill="blue",color="black", aes(x= sentiment, y=..density..), bins = bins)+geom_density(aes(x= sentiment, y=..density..), color ="red", size=1.5)+theme_light()+xlab("Sentiments")+
  theme_minimal()+ggtitle("Sentiments Scores Distribution")+theme(plot.title = element_text(hjust=0.5,size = 22, face = "bold"), axis.title =  element_text(size = 18),  axis.text =  element_text(size = 15), axis.text.y =element_blank())


ggplotly(ggplot(data=final_sentiments_1)+geom_histogram(fill="blue",color="black", aes(x= sentiment, y=..density..), bins = bins)+geom_density(aes(x= sentiment, y=..density..), color ="red", size=1.5)+theme_light()+xlab("Sentiments")+
           theme_minimal()+ggtitle("Sentiments Scores Distribution")+theme(plot.title = element_text(hjust=0.5,size = 22, face = "bold"), axis.title =  element_text(size = 18),  axis.text =  element_text(size = 15)))


#Add cat column for splitting into neutral, negative, and positive

final_sentiments_1[, "sentiment_score"] <- ifelse(final_sentiments_1[, "sentiment"]<0, "Negative", ifelse(final_sentiments_1[, "sentiment"]==0, "Neutral", "Positive"))

#Plot positve, Neutral, and Negative

#Sentimate Perceptions Pie  and Donut Plots
scores_df1 <- as.data.frame(t(table(final_sentiments_1[, "sentiment_score"])))[2:3]
names(scores_df1) <- c("Scores", "Frequency")

score_df <- scores_df1%>%group_by(Scores)%>%summarise(score= sum(Frequency))

score_df<- score_df%>% mutate(prop = (score/sum(score))*100)

score_df<- score_df %>% arrange(desc(Scores)) %>%mutate(lab.ypos = cumsum(prop) - 0.6*prop)


ggplot(score_df, aes(x = 2, y = prop, fill = Scores)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(prop), "%")), color = "white")+
  #scale_fill_manual(values = mycols) +
  theme_void()+ggtitle("Sentiments Scores Proportions")+theme(plot.title = element_text(hjust=0.5,size = 22),axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                              legend.title=element_text(size=14))+ labs(color='Item Type')+
  scale_fill_manual(values=c("darkred","orange","darkgreen"))+ guides(fill=guide_legend(title="Score"))




sentiment_by = sentiment_by(searched_twits_vec2) #Sentiments per line/row. No duplicate rows in the ou

#Extract zero sentiments
appended_minutes_df1 <- data.frame(texts = searched_twits_vec2, element_id= sentiment_by$element_id)
final_sentiments_2 <- left_join(appended_minutes_df1, sentiment_by, by= "element_id")

sentiments_zero1 <- final_sentiments_2[final_sentiments_2$ave_sentiment==0, ]
View(sentiments_zero1)


#Distribution of sentiment scores
h1 =2*IQR(final_sentiments_2$ave_sentiment)*length(final_sentiments_2$ave_sentiment)**(-1/3) ;h1

bins1 = round((max(final_sentiments_2$ave_sentiment)- min(final_sentiments_2$ave_sentiment))/h, 0);bins1



ggplotly(ggplot(data=final_sentiments_2)+geom_histogram(fill="blue",color="black", aes(x= ave_sentiment, y=..density..), bins = bins1)+geom_density(aes(x= ave_sentiment, y=..density..), color ="red", size=1.5)+theme_light()+xlab("Sentiments")+
           theme_minimal()+ggtitle("Sentiments Scores Distribution")+theme(plot.title = element_text(hjust=0.5,size = 22, face = "bold"), axis.title =  element_text(size = 18),  axis.text =  element_text(size = 15)))


final_sentiments_2[, "sentiment_score"] <- ifelse(final_sentiments_2[, "ave_sentiment"]<0, "Negative", ifelse(final_sentiments_2[, "ave_sentiment"]==0, "Neutral", "Positive"))

#Plot positve, Neutral, and Negative

#Sentimate Perceptions Pie  and Donut Plots
scores_df1 <- as.data.frame(t(table(final_sentiments_2[, "sentiment_score"])))[2:3]
names(scores_df1) <- c("Scores", "Frequency")

score_df1 <- scores_df1%>%group_by(Scores)%>%summarise(score= sum(Frequency))

score_df1<- score_df1%>% mutate(prop = (score/sum(score))*100)

score_df1<- score_df1 %>% arrange(desc(Scores)) %>%mutate(lab.ypos = cumsum(prop) - 0.6*prop)


sentiment_graph <- ggplot(score_df1, aes(x = 2, y = prop, fill = Scores)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(prop), "%")), color = "white")+
  #scale_fill_manual(values = mycols) +
  theme_void()+ggtitle("Sentiments Scores Proportions")+theme(plot.title = element_text(hjust=0.5,size = 22),axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                              legend.title=element_text(size=14))+ labs(color='Item Type')+
  scale_fill_manual(values=c("darkred","orange","darkgreen"))+ guides(fill=guide_legend(title="Score"))


png("C:/Users/johnl/OneDrive/Documents/GIt Projects/Text_Mining/Xenophobic Analysis/Graphs/Sentiments_scores.png")
print(sentiment_graph)
dev.off() 


png("C:/Users/johnl/OneDrive/Documents/GIt Projects/Text_Mining/Xenophobic Analysis/Graphs/Sentiments_scores_distributions.png")
print(Sentiments_scores_distributions)
dev.off() 





