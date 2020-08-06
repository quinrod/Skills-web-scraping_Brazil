rm(list=ls())
#Call the libraries
library(tm)
library(lexRankr)
library(dplyr)
library(tidytext)
library(R2HTML)
#Read data
data8483<-read.table("Data\\EmpregosScraping8483.txt",sep="@", header = TRUE, stringsAsFactors=FALSE)

clean.corpus <- function(string) {
  corpus <- Corpus(VectorSource(string))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese")[-which(stopwords("portuguese")=="são")]) 
  return(corpus)
}

################################################################################################
################################### CONFEITEIRO ################################################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="CF"),"Information"]
top_200 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 200 sentences to mimick /u/autotldr's output
                            n = 200,
                            continuous = TRUE, returnTies=FALSE)

#reorder the top 200 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_200$sentenceId)))
#extract sentences in order of appearance
ordered_top_200 <- top_200[order_of_appearance, "sentence"]
ordered_top_200<-unique(ordered_top_200)
ordered_top_30<-ordered_top_200[1:30]

#Frequency
textCF <- paste(data8483[which(data8483$Category=="CF"),"Information"],collapse = " ")
textCF<-gsub("formação formação","formação",textCF)
textCF<-gsub("formação formação","formação",textCF)
textCF<-gsub("descrição descrição","descrição",textCF)
textCF<-gsub("local trabalho local","local trabalho",textCF)
textCF<-gsub("trabalho local trabalho","trabalho local",textCF)
corpus.CF <- clean.corpus(textCF)
text_df <- tibble(line = 1, text = corpus.CF[[1]]$content)
textCF.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countCF<-textCF.ngram %>%  count(words, sort = TRUE)
countCF <- countCF[order(-countCF$n),] 

#More frequent word in phrase
data30<-data.frame(ordered_top_30)
data30$ordered_top_30<-as.character(data30$ordered_top_30)
data30$node<-""

for(j in 1:nrow(data30)){
  for(i in 1:nrow(countCF)){
    if(sapply(tolower(as.character(countCF[i,1])), grepl, tolower(data30[j,1]))){
      data30$node[j]<-countCF[i,1]
      break;
    }
  }
}
data30<-data30[,c("node","ordered_top_30")]
colnames(data30)<-c("Tema","Resumo")
HTML(data30,"Confeiteiro.html",row.names = FALSE)


################################################################################################
################################### MASSEIRO    ################################################
#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="MA"),"Information"]
top_200 <- lexRankr::lexRank(page_text,
                             #only 1 article; repeat same docid for all of input vector
                             docId = rep(1, length(page_text)),
                             #return 200 sentences to mimick /u/autotldr's output
                             n = 200,
                             continuous = TRUE, returnTies=FALSE)

#reorder the top 200 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_200$sentenceId)))
#extract sentences in order of appearance
ordered_top_200 <- top_200[order_of_appearance, "sentence"]
ordered_top_200<-unique(ordered_top_200)
ordered_top_30<-ordered_top_200[1:30]

#Frequency
textMA <- paste(data8483[which(data8483$Category=="MA"),"Information"],collapse = " ")
textMA<-gsub("formação formação","formação",textMA)
textMA<-gsub("formação formação","formação",textMA)
textMA<-gsub("descrição descrição","descrição",textMA)
textMA<-gsub("local trabalho local","local trabalho",textMA)
textMA<-gsub("trabalho local trabalho","trabalho local",textMA)
corpus.MA <- clean.corpus(textMA)
text_df <- tibble(line = 1, text = corpus.MA[[1]]$content)
textMA.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countMA<-textMA.ngram %>%  count(words, sort = TRUE)
countMA <- countMA[order(-countMA$n),] 

#More frequent word in phrase
data30<-data.frame(ordered_top_30)
data30$ordered_top_30<-as.character(data30$ordered_top_30)
data30$node<-""

for(j in 1:nrow(data30)){
  for(i in 1:nrow(countMA)){
    if(sapply(tolower(as.character(countMA[i,1])), grepl, tolower(data30[j,1]))){
      data30$node[j]<-countMA[i,1]
      break;
    }
  }
}
data30<-data30[,c("node","ordered_top_30")]
colnames(data30)<-c("Tema","Resumo")
HTML(data30,"Masseiro.html",row.names = FALSE)

################################################################################################
################################### PADEIRO     ################################################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="PD"),"Information"]
top_200 <- lexRankr::lexRank(page_text,
                             #only 1 article; repeat same docid for all of input vector
                             docId = rep(1, length(page_text)),
                             #return 200 sentences to mimick /u/autotldr's output
                             n = 200,
                             continuous = TRUE, returnTies=FALSE)

#reorder the top 200 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_200$sentenceId)))
#extract sentences in order of appearance
ordered_top_200 <- top_200[order_of_appearance, "sentence"]
ordered_top_200<-unique(ordered_top_200)
ordered_top_30<-ordered_top_200[1:30]

#Frequency
textPD <- paste(data8483[which(data8483$Category=="PD"),"Information"],collapse = " ")
textPD<-gsub("formação formação","formação",textPD)
textPD<-gsub("formação formação","formação",textPD)
textPD<-gsub("descrição descrição","descrição",textPD)
textPD<-gsub("local trabalho local","local trabalho",textPD)
textPD<-gsub("trabalho local trabalho","trabalho local",textPD)
corpus.PD <- clean.corpus(textPD)
text_df <- tibble(line = 1, text = corpus.PD[[1]]$content)
textPD.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countPD<-textPD.ngram %>%  count(words, sort = TRUE)
countPD <- countPD[order(-countPD$n),] 

#More frequent word in phrase
data30<-data.frame(ordered_top_30)
data30$ordered_top_30<-as.character(data30$ordered_top_30)
data30$node<-""

for(j in 1:nrow(data30)){
  for(i in 1:nrow(countPD)){
    if(sapply(tolower(as.character(countPD[i,1])), grepl, tolower(data30[j,1]))){
      data30$node[j]<-countPD[i,1]
      break;
    }
  }
}
data30<-data30[,c("node","ordered_top_30")]
colnames(data30)<-c("Tema","Resumo")
HTML(data30,"Padeiro.html",row.names = FALSE)



################################################################################################
################################### TRABALHADOR DE FABRICAÇÃO DE SORVETE  ######################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="TS"),"Information"]
top_200 <- lexRankr::lexRank(page_text,
                             #only 1 article; repeat same docid for all of input vector
                             docId = rep(1, length(page_text)),
                             #return 200 sentences to mimick /u/autotldr's output
                             n = 200,
                             continuous = TRUE, returnTies=FALSE)

#reorder the top 200 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_200$sentenceId)))
#extract sentences in order of appearance
ordered_top_200 <- top_200[order_of_appearance, "sentence"]
ordered_top_200<-unique(ordered_top_200)
ordered_top_30<-ordered_top_200[1:30]

#Frequency
textTS <- paste(data8483[which(data8483$Category=="TS"),"Information"],collapse = " ")
textTS<-gsub("formação formação","formação",textTS)
textTS<-gsub("formação formação","formação",textTS)
textTS<-gsub("descrição descrição","descrição",textTS)
textTS<-gsub("local trabalho local","local trabalho",textTS)
textTS<-gsub("trabalho local trabalho","trabalho local",textTS)
corpus.TS <- clean.corpus(textTS)
text_df <- tibble(line = 1, text = corpus.TS[[1]]$content)
textTS.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countTS<-textTS.ngram %>%  count(words, sort = TRUE)
countTS <- countTS[order(-countTS$n),] 

#More frequent word in phrase
data30<-data.frame(ordered_top_30)
data30$ordered_top_30<-as.character(data30$ordered_top_30)
data30$node<-""

for(j in 1:nrow(data30)){
  for(i in 1:nrow(countTS)){
    if(sapply(tolower(as.character(countTS[i,1])), grepl, tolower(data30[j,1]))){
      data30$node[j]<-countTS[i,1]
      break;
    }
  }
}
data30<-data30[,c("node","ordered_top_30")]
colnames(data30)<-c("Tema","Resumo")
HTML(data30,"Sorvete.html",row.names = FALSE)