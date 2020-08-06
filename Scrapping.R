rm(list=ls())
#Call the libraries
library(rvest)
library(purrr)
library(tidyverse)
library(stringr)

#Define the link with all positions
linksPage<-c("https://www.empregos.com.br/vagas/padeiro","https://www.empregos.com.br/vagas/masseiro/","https://www.empregos.com.br/vagas/confeiteiro/","https://www.empregos.com.br/vagas/sorvete/")
category<-c("PD","MA","CF","TS")
for(k in 1:4){
  #Define the link with all positions
  link <- read_html(linksPage[k])
  
  #Pages
  pages <- link %>% 
    html_nodes(xpath='//*[@id="ctl00_ContentBody_pPaiResultadoTopo"]/strong[2]') %>% 
    html_text()
  
  #Find the last page
  posIni <- regexpr('de ', pages)+3
  subsPage<- substr(pages,posIni,nchar(pages))
  posEnd <- regexpr('\r',subsPage)-1
  last<- as.numeric(gsub("[.]","",substr(subsPage,1,posEnd)))
  
  
  pb <- txtProgressBar(min = 0, max = last)
  for(j in 1:last){
    #Define the link with all positions
    erro<-TRUE
    while(erro==T){
      link <-tryCatch(read_html(paste0(linksPage[k],"/p",j)), 
                      error = function(e){'empty page'})
      if(length(link)<2){
        erro<-TRUE
        Sys.sleep(5)
      } 
      else{
        erro<-FALSE
      }
    }
    jobLinks <- link %>% 
      #html_nodes(xpath='//*[@id="ctl00_ContentBody_divPaiMioloBusca"]/ul')%>% 
      html_nodes(xpath='//*[@id="ctl00_ContentBody_divPaiMioloBusca"]')%>% 
      html_nodes('a') %>% html_attr('href')
    #Filter the links
    links<-grepl("vagas", jobLinks)
    if(length(links)>0){
      jobLinks<-data.frame(j,jobLinks[links],category[k])
      write.table(jobLinks, file="Data\\EmpregosLinks8483.txt", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE) 
    }
    setTxtProgressBar(pb, j)
  }
  close(pb)
}

#Read the links
links<-read.table("Data\\EmpregosLinks8483.txt",sep=" ")
#links<-unique(links)
last<-nrow(links)
pb <- txtProgressBar(min = 0, max = last)
for(i in 1:last){
  tryCatch({
        #Define the link with all positions
        link <- read_html(as.character(links[i,2]))
        
        #Position
        position <- link %>% 
          html_nodes(xpath='//*[@id="ctl00_ContentBody_h1TituloVaga"]')%>% 
          html_text()
        if(identical(position, character(0))) position<-""
        #Firm
        firm <- link %>% 
          html_nodes(xpath='//*[@id="ctl00_ContentBody_hplEmpresa"]')%>% 
          html_text()
        if(identical(firm, character(0))) firm<-""
        #Information Job
        information <- link %>% 
          html_nodes(xpath='//*[@id="ctl00_ContentBody_divVaga"]/article/div[5]/div/table[1]')%>% 
          html_text() 
        
        information<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", information, perl=TRUE)
        information<-gsub("\r"," ",information)
        information<-gsub("Qualificação: Qualificação:","Qualificação:",information)
        information<-gsub("Descrição: Descrição:","Descrição:",information)
        information<-gsub("Formação: Formação","Formação",information)
        information<-gsub("Local de Trabalho: Local de Trabalho:","Local de Trabalho:",information)
        information<-gsub(":"," ",information)
        information<-gsub(",","",information)
        information<-gsub(">","",information)
        #information<-gsub("\\.","",information)
        information<-gsub("\\/","",information)
        
        if(identical(information, character(0))) information<-""
          
          if(i==1){
            temp2<-data.frame("Position"=position,"Firm"=firm,"Information"=information,"Category"=links[i,3])
            write.table(temp2, file="Data\\EmpregosScraping8483.txt",sep = "@", append=FALSE, col.names = TRUE, row.names=FALSE, quote = FALSE)
          }
          else{
            temp2<-data.frame("Position"=position,"Firm"=firm,"Information"=information,"Category"=links[i,3])
            write.table(temp2, file="Data\\EmpregosScraping8483.txt",sep = "@", append=TRUE, col.names = FALSE, row.names=FALSE, quote = FALSE)
          }
    setTxtProgressBar(pb, i)}
   , error = function(err) {
    print(paste("MY_ERROR:  ",err,"iter=",i))
    i<- (i-1)
    Sys.sleep(3) 
  })
}
close(pb)


