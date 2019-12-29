#Load required libraries
rm(list = ls())

lib_vec <- c("rvest","xml2", "stringr", "tm", "wordcloud", "wordcloud2", "ggplot2", "tidytext", "dplyr", "textdata")

for(lib in lib_vec){
  if(!require(lib, character.only = T)){
    install.packages(lib)
  }
  library(lib, character.only = T)
}


web_url <- "https://pmg.org.za/hansards/"
web_page <- read_html(web_url)

#Nodes in the hearder
head_nodes <- web_page %>% 
  html_node("head") %>% 
  html_children()


length(head_nodes)

#View Section of Header
head_nodes[20: length(head_nodes)]

#Extract title tag
title_tag <- head_nodes[str_detect(head_nodes, pattern = "<title>")]
title_tag

#Retrive Main Title of the page
page_title <- str_extract(title_tag, pattern = "(?<=<title>).*(?=</title>)")
page_title

#OR
html_text(title_tag)


#Extract Page Metadata tags in the header
meta_tags <- head_nodes[str_detect(head_nodes, pattern = "<meta name=")]
meta_tags


#Extract Meta contents
meta_content <- str_extract(meta_tags, pattern = "(?<=<meta name=).*(?=>\\n)")
meta_content



#Extract Minutes Dates
minutes_dates <- web_page %>% 
  html_node("li")%>%xml_find_all("//div[contains(@class, 'date')]")
  
dates_vec <- html_text(minutes_dates)


#Extract Minutes Titles
minutes_title <- web_page %>% 
  html_node("li")%>%xml_find_all("//div[contains(@class, 'hansard-stub')]")

minutes_titles_and_dates <- str_trim(str_replace_all(html_text(minutes_title), pattern = "\n", replacement = ""), side = "both")
minutes_titles_and_dates


#extract links to the minutes
#links <- web_page %>% html_nodes("h4")

base_url <- "https://pmg.org.za/hansard/"


dates_and_links <- web_page %>% 
  html_nodes(".hansard-stub")


minutes_links = c()
for(link in as.character(dates_and_links)){
  minutes_links <- c(minutes_links, paste0(base_url, unlist(str_extract_all(link, pattern = '(?<=<h4><a href=\"/hansard/)[0-9]{5,8}'))))
  #print(link)
}

minutes_links


#Dateframe of dates and links
dates_links_df <- data.frame(dates_vals= dates_vec, minutes_links = minutes_links, stringsAsFactors = F)
View(dates_links_df)

#Save as textfiles/Append textfile vectors

appended_minutes <- c()
for(ival in 1:nrow(dates_links_df)){
  write.table(read_html(dates_links_df[ival, 2])%>%html_nodes("p")%>%as.character(), file = paste0("C:/Users/johnl/Desktop/",dates_links_df[ival, 1], ".txt"))
  appended_minutes <-c(appended_minutes, read_html(dates_links_df[ival, 2])%>%html_nodes("p")%>%as.character())
}

appended_minutes1 <- appended_minutes[!str_detect(appended_minutes, pattern = "<p>\\s</p>|<p>____</p>|file:///|https://www|<p><em>No summary available|<p><strong>Page: 1</strong></p>|<p><strong>Page|<p><strong>UNREVISED HANSARD|<p>(Draft Resolution)</p>|[0-9]|<p><em>English|<p><em>Setswana</em>|<p>\\(Draft Resolution|<p><em>IsiXhosa</em>|<p><em>Declaration\\(s")]

appended_minutes1 <-str_to_sentence(str_replace_all(appended_minutes1, pattern = "<.*?>|\\.|,|\\?", replacement = ""))

#appended_minutes_df <- data.frame(appended_minutes1 = appended_minutes1)
#appended_minutes2 <- apply(appended_minutes_df, MARGIN = 2, FUN = paste, sep=" ")

#View(appended_minutes2)

minutes_cop <- Corpus(VectorSource(appended_minutes1))



suppressWarnings({
  corp_rm_other <- tm_map(minutes_cop, removeNumbers)
  corp_rm_other1<- tm_map(corp_rm_other, removePunctuation)
  corp_rm_other2<- tm_map(corp_rm_other1, removeWords)
  corp_rm_other3<- tm_map(corp_rm_other2, stripWhitespace)
})



#Create Dataframe with words and their frequencies

doc_mat <- TermDocumentMatrix(corp_rm_other3)
text_matrix <- sort(rowSums(as.matrix(doc_mat)),decreasing=TRUE) 
text_df <- data.frame(word = names(text_matrix),freq=text_matrix)
View(text_df)




text_df_clean_st <- text_df
text_df_clean <- text_df[!str_detect(text_df$word, pattern = "the|can|will|are|'s"), ]

text_df_clean$word <- as.character(text_df_clean$word)


text_df_clean <- text_df_clean%>% group_by(word)%>%summarise(freq = sum(freq))%>%arrange(desc(freq))


rownames(text_df_clean)<- text_df_clean$word

View(text_df_clean)
#Draw wordcloud
set.seed(1300)
wordcloud(words = text_df_clean$word, freq  = text_df_clean$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))


wordcloud2(text_df_clean[1:500, ], size = .5,fontFamily = "Segoe UI", fontWeight = "bold",
           color = "random-dark", backgroundColor = "white", shuffle = F,
           rotateRatio = 0.3, shape = "circle")





#Extract Most Appearing words
word_freq = 1000

ggplot(text_df_clean[text_df_clean$freq>word_freq, ], aes(x = reorder(word, -freq), y = freq, fill = word, label = freq)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Words with Frequency Greater Than 1000", x = "Words", y = "Word Frequency") +
  geom_label(aes(fill = word),colour = "white", fontface = "bold", show.legend = FALSE)+theme_classic()+theme(plot.title = element_text(hjust=0.5,size = 22))+scale_fill_manual(values = rep(c("blue"),39))


#Analyse Emotions

afinn_df <- get_sentiments("afinn")
bing_df <- get_sentiments("bing")

affin_sent <- inner_join(text_df_clean_st, afinn_df, by="word")
bing_sent <- inner_join(text_df_clean_st, bing_df, by="word")
affin_sent$Score <-  affin_sent$freq*affin_sent$value
affin_sent$color <- ifelse(affin_sent$Score<0, "darkred", "darkgreen")

#Sentiment Scores Bars
ggplot(affin_sent, aes(x = word, y = Score, fill = color, label = freq)) +
  geom_bar(stat="identity", show.legend = FALSE)+labs(title = "Sentiment Scores", x = "", y = "Score") +theme_classic()+theme(plot.title = element_text(hjust=0.5,size = 22), axis.text.y = element_blank(), axis.ticks = element_blank())+scale_fill_manual(values = c("darkgreen", "darkred"))+coord_flip()

#Sentimate Perceptions Pie  and Donut Plots

sent_df <- bing_sent%>%group_by(sentiment)%>%summarise(score= sum(freq))

sent_df<- sent_df%>% mutate(prop = (score/sum(score))*100)

sent_df<- sent_df %>% arrange(desc(sentiment)) %>%mutate(lab.ypos = cumsum(prop) - 0.6*prop)


####Donut Chart
ggplot(sent_df, aes(x = 2, y = prop, fill = sentiment)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(prop), "%")), color = "white")+
  #scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)+ggtitle("Sentiments Scores Proportions")+theme(plot.title = element_text(hjust=0.5,size = 22),axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                                legend.title=element_text(size=14))+ labs(color='Item Type')+
  scale_fill_manual(values=c("darkred","darkgreen"))+ guides(fill=guide_legend(title="Score"))


####Pie Plot

ggplot(sent_df, aes(x = 2, y = prop, fill = sentiment)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(prop), "%")), color = "white")+
  #scale_fill_manual(values = mycols) +
  theme_void()+ggtitle("Sentiments Scores Proportions")+theme(plot.title = element_text(hjust=0.5,size = 22),axis.text.y = element_blank(), axis.ticks = element_blank(),
                                                              legend.title=element_text(size=14))+ labs(color='Item Type')+
  scale_fill_manual(values=c("darkred","darkgreen"))+ guides(fill=guide_legend(title="Score"))






