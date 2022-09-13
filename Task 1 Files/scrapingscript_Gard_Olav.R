###### Script for Task 1: assembling the data ###########

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

## First i find the relevant sessions, and I throw away the sessions I don't need

sessions <- get_parlsessions()

sessions <- sessions %>% 
  filter(!(id %in% c("2022-2023", "2023-2024", "2024-2025", 
                     "1995-96", "1994-95", "1993-94",
                     "1992-93", "1991-92", "1990-91", "1989-90",
                     "1988-89", "1987-88", "1986-87")))
head(allmps$substitute_mp)

## Creating some empty lists to put questionmeta data into

a <- list()
b <- list()
c <- list()

## Looping over the sessions to retrieve metadata on all questions

for(x in unique(sessions$id)){
  it <- 100*(which(unique(sessions$id) == x)/length(unique(sessions$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  a[[x]] <- get_session_questions(sessionid = x, 
                                  q_type = "interpellasjoner", 
                                  status = NA,
                                  good_manners = 1)
  b[[x]] <- get_session_questions(sessionid = x, 
                                  q_type = "sporretimesporsmal",
                                  status = NA,
                                  good_manners = 1)
  try(c[[x]] <- get_session_questions(sessionid = x,
                                      q_type = "skriftligesporsmal",
                                      status = NA,
                                      good_manners = 1))
}

## unlist them

aex <- do.call("rbind", a)
bex <- do.call("rbind", b)
cex <- do.call("rbind", c)

dlist <- list(aex, bex, cex)

questionsmeta <- do.call("rbind", dlist)

## Now we have a frame, lets remove reduntant objects

rm(aex, bex, cex, dlist)

save(questionsmeta, file = "data/questionsmeta.Rdata")

## 51673 questions, seems alright

## Now the real task, getting the text, fair warning, this code takes time

d <- list()

for(x in unique(questionsmeta$id)){
  it <- 100*(which(unique(questionsmeta$id) == x) / length(unique(questionsmeta$id)))
  cat(paste0(sprintf("Progress: %.4f%%             ", it), "\r"))
  
  try(d[[x]] <- get_question(questionid = x, good_manners = 1))
}

## Code is try to catch some frames that return empty question sets. 
## Seems to be a lack on stortingets end. 


questiontext <- do.call("rbind", d)

## Checking for patterns in the calls that return empty XML's

checks <- questionsmeta %>% 
  filter(!(questionsmeta$id %in% questiontext$id))

## Saving the objects, both data and outliers (just incase)

save(questiontext, file = "data/questiontext.Rdata")
save(checks, file = "data/outliers.Rdata")

## Removing redundant objects

rm(d, it, x)

## Loading the data

load("data/questiontext.Rdata")

## Checking the spread of questiontypes.

table(questionsmeta$type)


##### Now to join it with the MP info.

### Retrieving the MP's, roughly same procedure as last chunk miss Sophie. 

## MP data is stored in parliament periods, instead of sessions.

periods_storting <- get_parlperiods()

periods_storting <- periods_storting %>%
  filter(id %in% c("2021-2025", "2017-2021", "2013-2017", "2009-2013", 
                   "2005-2009", "2001-2005", "1997-2001", "1993-97"))

## Same procedure as every chunk James

j <- list()

for (x in periods_storting$id) {
  j[[x]] <- get_parlperiod_mps(periodid = x, substitute = TRUE, 
                               good_manners = 0)
}

## Schall!

allmps <- do.call("rbind", j)

save(allmps, file = "data/mpdata.Rdata")

## Limiting the data somewhat, putting the frame on a diet to make it thinner

allmpsparties <- allmps %>% 
  select(question_from_id = mp_id, firstname,
         lastname, gender, party_id, county_id, period_id)
rm(j)

### Prepping the target data for merging. Giving the questions a period id.

questiontext <- questiontext %>%
  mutate(period_id = case_when(session_id == "2021-2022" ~ "2021-2025",
                               session_id == "2020-2021" ~ "2017-2021",
                               session_id == "2019-2020" ~ "2017-2021",
                               session_id == "2018-2019" ~ "2017-2021",
                               session_id == "2017-2018" ~ "2017-2021",
                               session_id == "2016-2017" ~ "2013-2017",
                               session_id == "2015-2016" ~ "2013-2017",
                               session_id == "2014-2015" ~ "2013-2017",
                               session_id == "2013-2014" ~ "2013-2017",
                               session_id == "2012-2013" ~ "2009-2013",
                               session_id == "2011-2012" ~ "2009-2013",
                               session_id == "2010-2011" ~ "2009-2013",
                               session_id == "2009-2010" ~ "2009-2013",
                               session_id == "2008-2009" ~ "2005-2009",
                               session_id == "2007-2008" ~ "2005-2009",
                               session_id == "2006-2007" ~ "2005-2009",
                               session_id == "2005-2006" ~ "2005-2009",
                               session_id == "2004-2005" ~ "2001-2005",
                               session_id == "2003-2004" ~ "2001-2005",
                               session_id == "2002-2003" ~ "2001-2005",
                               session_id == "2001-2002" ~ "2001-2005",
                               session_id == "2000-2001" ~ "1997-2001",
                               session_id == "1999-2000" ~ "1997-2001",
                               session_id == "1998-99" ~ "1997-2001",
                               session_id == "1997-98" ~ "1997-2001", 
                               session_id == "1996-97" ~ "1993-1997"))

## Lets have a gander

table(questiontext$session_id, questiontext$period_id)

# Looks alright

### Joining the MP's

df <- left_join(questiontext, allmpsparties, by = c("question_from_id", "period_id"))

save(df, file = "data/usedata.Rdata")

### Houston we have a dataframe
