rm(list=ls())
#Call the libraries
library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(visNetwork)
library(lexRankr)

#### Produto http://shiny.rstudio.com/gallery/word-cloud.html
#### https://kateto.net/network-visualization
#### https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/
#### https://cran.r-project.org/web/packages/textrank/vignettes/textrank.html#identify_relevant_sentences
#Read data
data8483<-read.table("Data\\EmpregosScraping8483.txt",sep="@", header = TRUE, stringsAsFactors=FALSE)
clean.corpus <- function(string) {
  corpus <- Corpus(VectorSource(string))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese")[-which(stopwords("portuguese")=="são")]) 
  return(corpus)
}

################################################################################################
################################### CONFEITEIRO ################################################

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
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countCF<-textCF.ngram %>%  count(words, sort = TRUE)
countCF <- countCF[order(-countCF$n),] 

#Wordcloud CF
pal <- brewer.pal(8,"Dark2")
png("wordcloudCF.png", width=1280,height=800)
wordcloud(countCF$words, countCF$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()

#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Confeiteiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
textCF.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countWords<-textCF.ngram %>%  count(words, sort = TRUE)
countWords <- countWords[order(-countWords$n),] 
countWords$nodeSize<-ifelse(countWords$n<10,10,ifelse(countWords$n<100,25,50))
countWords$textSize<-ifelse(countWords$n<100,0.5,1)
countWords$title<-paste0("<p>Comunidade ", "</p>")
words_counts$edgeSize<-ifelse(words_counts$n<10,5,ifelse(words_counts$n<100,25,50))

#Graph plot
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = countWords)
E(graph)$width <- words_counts$edgeSize
V(graph)$size<-V(graph)$nodeSize
V(graph)$label.cex <-V(graph)$textSize
#Community
wc <-  cluster_walktrap(graph)
V(graph)$color <- wc$membership + 1
V(graph)$title <-paste0("<p>",V(graph)$name,"</p><p> Comunidade ", wc$membership,"</p>")
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)
net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
#  visIgraphLayout(layout = "layout_with_dh") 
#  visIgraphLayout(layout = "layout_with_fr")
#  visIgraphLayout(layout = "layout_with_lgl")
#  visIgraphLayout(layout = "layout_with_kk")
#  visIgraphLayout(layout = "layout_with_graphopt") 
  visIgraphLayout(layout = "layout_on_sphere") 
#  visIgraphLayout(layout = "layout_on_grid") 
visSave(net, file = "networkCF.html")

#Select community
table(wc$membership)
Keep <- V(graph)[wc$membership == 16]
sub_graph  <- induced_subgraph(graph, Keep)
wc2 <- cluster_walktrap(sub_graph)
weights <- ifelse(crossing(wc2, sub_graph), 1, 100)
layout <- layout_with_kk(sub_graph, weights=weights)
net2 <- visIgraph(sub_graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_with_dh")
visSave(net2, file = "sub_networkCF.html")

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="CF"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15CF.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)

################################################################################################
################################### MASSEIRO    ################################################

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
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countMA<-textMA.ngram %>%  count(words, sort = TRUE)
countMA <- countMA[order(-countMA$n),] 

#Wordcloud MA
pal <- brewer.pal(8,"Dark2")
png("wordcloudMA.png", width=1280,height=800)
wordcloud(countMA$words, countMA$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Masseiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
textMA.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countWords<-textMA.ngram %>%  count(words, sort = TRUE)
countWords <- countWords[order(-countWords$n),] 
countWords$nodeSize<-ifelse(countWords$n<10,10,ifelse(countWords$n<100,25,50))
countWords$textSize<-ifelse(countWords$n<100,0.5,1)
countWords$title<-paste0("<p>Comunidade ", "</p>")
words_counts$edgeSize<-ifelse(words_counts$n<10,5,ifelse(words_counts$n<100,25,50))

#Graph plot
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = countWords)
E(graph)$width <- words_counts$edgeSize
V(graph)$size<-V(graph)$nodeSize
V(graph)$label.cex <-V(graph)$textSize
#Community
wc <-  cluster_walktrap(graph)
V(graph)$color <- wc$membership + 1
V(graph)$title <-paste0("<p>",V(graph)$name,"</p><p> Comunidade ", wc$membership,"</p>")
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)
net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_on_sphere") 
visSave(net, file = "networkMA.html")

#Select community
table(wc$membership)
Keep <- V(graph)[wc$membership == 16]
sub_graph  <- induced_subgraph(graph, Keep)
wc2 <- cluster_walktrap(sub_graph)
weights <- ifelse(crossing(wc2, sub_graph), 1, 100)
layout <- layout_with_kk(sub_graph, weights=weights)
net2 <- visIgraph(sub_graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_with_dh")
visSave(net2, file = "sub_networkMA.html")

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="MA"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15MA.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)


################################################################################################
################################### PADEIRO     ################################################

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
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countPD<-textPD.ngram %>%  count(words, sort = TRUE)
countPD <- countPD[order(-countPD$n),] 

#Wordcloud PD
pal <- brewer.pal(8,"Dark2")
png("wordcloudPD.png", width=1280,height=800)
wordcloud(countPD$words, countPD$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Padeiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
textPD.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countWords<-textPD.ngram %>%  count(words, sort = TRUE)
countWords <- countWords[order(-countWords$n),] 
countWords$nodeSize<-ifelse(countWords$n<10,10,ifelse(countWords$n<100,25,50))
countWords$textSize<-ifelse(countWords$n<100,0.5,1)
countWords$title<-paste0("<p>Comunidade ", "</p>")
words_counts$edgeSize<-ifelse(words_counts$n<10,5,ifelse(words_counts$n<100,25,50))

#Graph plot
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = countWords)
E(graph)$width <- words_counts$edgeSize
V(graph)$size<-V(graph)$nodeSize
V(graph)$label.cex <-V(graph)$textSize
#Community
wc <-  cluster_walktrap(graph)
V(graph)$color <- wc$membership + 1
V(graph)$title <-paste0("<p>",V(graph)$name,"</p><p> Comunidade ", wc$membership,"</p>")
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)
net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_on_sphere") 
visSave(net, file = "networkPD.html")

#Select community
table(wc$membership)
Keep <- V(graph)[wc$membership == 16]
sub_graph  <- induced_subgraph(graph, Keep)
wc2 <- cluster_walktrap(sub_graph)
weights <- ifelse(crossing(wc2, sub_graph), 1, 100)
layout <- layout_with_kk(sub_graph, weights=weights)
net2 <- visIgraph(sub_graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_with_dh")
visSave(net2, file = "sub_networkPD.html")

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="PD"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15PD.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)
################################################################################################
################################### TRABALHADOR DE FABRICAÇÃO DE SORVETE  ######################

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
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countTS<-textTS.ngram %>%  count(words, sort = TRUE)
countTS <- countTS[order(-countTS$n),] 

#Wordcloud TS
pal <- brewer.pal(8,"Dark2")
png("wordcloudTS.png", width=1280,height=800)
wordcloud(countTS$words, countTS$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)


#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Fabricação de sorvete",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
textTS.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 1, n_min = 1)
countWords<-textTS.ngram %>%  count(words, sort = TRUE)
countWords <- countWords[order(-countWords$n),] 
countWords$nodeSize<-ifelse(countWords$n<10,10,ifelse(countWords$n<100,25,50))
countWords$textSize<-ifelse(countWords$n<100,0.5,1)
countWords$title<-paste0("<p>Comunidade ", "</p>")
words_counts$edgeSize<-ifelse(words_counts$n<10,5,ifelse(words_counts$n<100,25,50))

#Graph plot
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = countWords)
E(graph)$width <- words_counts$edgeSize
V(graph)$size<-V(graph)$nodeSize
V(graph)$label.cex <-V(graph)$textSize
#Community
wc <-  cluster_walktrap(graph)
V(graph)$color <- wc$membership + 1
V(graph)$title <-paste0("<p>",V(graph)$name,"</p><p> Comunidade ", wc$membership,"</p>")
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)
net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_on_sphere") 
visSave(net, file = "networkTS.html")

#Select community
table(wc$membership)
Keep <- V(graph)[wc$membership == 16]
sub_graph  <- induced_subgraph(graph, Keep)
wc2 <- cluster_walktrap(sub_graph)
weights <- ifelse(crossing(wc2, sub_graph), 1, 100)
layout <- layout_with_kk(sub_graph, weights=weights)
net2 <- visIgraph(sub_graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)%>%
  visIgraphLayout(layout = "layout_with_dh")
visSave(net2, file = "sub_networkTS.html")

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="TS"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                           #only 1 article; repeat same docid for all of input vector
                           docId = rep(1, length(page_text)),
                           #return 15 sentences to mimick /u/autotldr's output
                           n = 15,
                           continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15TS.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)
