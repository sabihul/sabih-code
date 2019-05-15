install.packages("selectr")
install.packages("xml2")
install.packages("rvest")
library(selectr)
library(xml2)
library(rvest)

data_creation<-function(url,link){
  url<-url
  webpage <- read_html(url)
  rank_data_html <- html_nodes(webpage,link)
  rank_data <- html_text(rank_data_html)
  data<-as.data.frame(rank_data)
  return(data)
}

name<-data_creation(url = 'https://www.cricbuzz.com/cricket-stats/icc-rankings/men/batting',link ='.text-bold.cb-font-16')  


