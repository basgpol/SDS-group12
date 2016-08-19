library("readr")
library("rvest")
library("stringr")
library("purrr")
library("dplyr")
setwd("~/Dropbox/Universitet/SocialDataScience/SDS-group12/Exam project")
partlyclean=read_csv("player_data_partclean.csv")

## NB: Im not sure it is working for more than 300 players at a time

GoogleHits <- function(input)  #Function that seach for the input specified
{
  require(XML)
  require(RCurl)
  input = gsub(" ", "+", input)
  #url <- paste("https://www.google.com/search?q=\"",
  #             input, "\"", sep = "")
  url = paste0("https://www.google.com/search?q=",
               input)
  
  CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
  script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
  doc <- htmlParse(script)
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
  #cat("\nNo. of Hits:\n")
  return(as.integer(gsub("[^0-9]", "", res)))
}
partlyclean.test=partlyclean[1:5,]

search.1=dQuote(partlyclean.test$name)  #Put quotation marks around name of the player
search.2=paste(search.1,"footballer",partlyclean.test$nationality[1:5], sep=" ") #Paste name of footballer, the word footballer and nationality
search.2


partlyclean.test$searchresults=unlist(lapply(search.2, GoogleHits)) #New column reporting number of search results

