##### ------- Script for STV2022/Wishful Article writing -----------

library(stortingscrape)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(tidyr)
library(viridis)
library(rvest)
library(xml2)
library(httr)

##Packages done, lets find the data

sessions <- get_parlsessions()

sessions <- sessions %>% 
  filter(!(id %in% c("2022-2023", "2023-2024", "2024-2025", 
                     "1995-96", "1994-95", "1993-94",
                     "1992-93", "1991-92", "1990-91", "1989-90",
                     "1988-89", "1987-88", "1986-87")))


a <- list()
b <- list()
c <- list()

for(x in unique(sessions$id)){
  it <- 100*(which(unique(sessions$id) == x)/length(unique(sessions$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  a[[x]] <- get_session_questions(sessionid = x, 
                                  q_type = "interpellasjoner", 
                                  status = NA,
                                  good_manners = 0)
  b[[x]] <- get_session_questions(sessionid = x, 
                                  q_type = "sporretimesporsmal",
                                  status = NA,
                                  good_manners = 0)
  try(c[[x]] <- get_session_questions(sessionid = x,
                                  q_type = "skriftligesporsmal",
                                  status = NA,
                                  good_manners = 0))
}

aex <- do.call("rbind", a)
bex <- do.call("rbind", b)
cex <- do.call("rbind", c)

dlist <- list(aex, bex, cex)

questionsmeta <- do.call("rbind", dlist)

rm(aex, bex, cex, dlist)

save(questionsmeta, file = "data/questionsmeta.Rdata")

## 51673 questions, seems alright

## Now the real task, getting the text, fair warning, this code takes time

d <- list()

for(x in unique(questionsmeta$id)){
  it <- 100*(which(unique(questionsmeta$id) == x) / length(unique(questionsmeta$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  try(d[[x]] <- get_question(questionid = x, good_manners = 0))
}

questiontext <- do.call("rbind", d)

checks <- questionsmeta %>% 
  filter(!(questionsmeta$id %in% questiontext$id))

save(questiontext, file = "data/questiontext.Rdata")
save(checks, file = "data/outliers.Rdata")

get_question(questionid = "77134", good_manners = 0)


table(questionsmeta$type)
