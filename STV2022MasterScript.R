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
library(SnowballC)
library(spacyr)

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

############## ----------- Week 2: Creating a dfm --------------------

## In order to get to the core of the issue, we need to gather all the text in variables

## The journey today is tokenize the text, stem it, remove the stop words,
## create the dfm, all fun and games. 

## Act I: And I stood upon the sand of the sea, and saw a beast
##rise up out of the sea, having seven heads and ten horns, and
##upon his horns ten crowns, and upon his heads the name of blasphemy.

## Scene I, our players, the questiondata, named df, or usedata as a file,
##enters the stage. 

load("data/usedata.Rdata")

## Alas torn by its varying levels of inputs, it felt conflicted

## The LORD, smit him down and split him into several text-inputs


justificationtexts <- df %>% 
  select(text = justification, 
         id) %>% 
  mutate(type = "justification")

questiontexts <- df %>% 
  select(text = question_text,
         id) %>% 
  mutate(type = "question")

answertexts <- df %>% 
  select(text = answer_text,
         id) %>% 
  mutate(type = "answer")

## And the lamb broke the first seal, and there was too much missing in the data

## Scene II: The lamb broke the second seal, and removed all the empty texts

justificationtexts <- justificationtexts %>%
  mutate(notapplicable = ifelse(text == "", NA, 1)) %>%
  drop_na(notapplicable) %>%
  select(-notapplicable)

questiontexts <- questiontexts %>%
  mutate(notapplicable = ifelse(text == "", NA, 1)) %>%
  drop_na(notapplicable) %>%
  select(-notapplicable)

answertexts <- answertexts %>%
  mutate(notapplicable = ifelse(text == "", NA, 1)) %>%
  drop_na(notapplicable) %>%
  select(-notapplicable)

## The lamb saw that the frames had been shortened, and that it was good. 
## The lamb also thought that surely there was an easier way to do this for all
##units, but thought nothing of it for now. 

## I could have just rbound them first and then dropped the NA's actually...

## Scene III, The lamb broke the third seal, and blo- I mean the texts were combined

fulltexts <- rbind(justificationtexts, questiontexts, answertexts)

## And blood washed away the unnecessary objects, that the LORD no longer needed

rm(checks, justificationtexts, mps2021, questiontexts, questiontext2, 
   stragglers, answertexts)

### Act II, Shall I compare thee to a token's night eve

## We're doing shakespeare now. 

## Romeo: "oh dataframe, oh dataframe, wherefore art thou in different caps,
## deny thy higher strings and become only lower strings".

fulltexts <- fulltexts %>% 
  mutate(text_l = str_to_lower(text))

## Ok this is too much. Can't be bothered anymore. Next step I tokenize the text

tokensquestions <- fulltexts %>% 
  unnest_tokens(input = text_l, ## Input from the df that I want tokenized
                output = word,  ## name of the column that it creates
                token = "words")## what type of tokenizing it does


tokensquestions <- tokensquestions %>% 
  select(-text)

## A nice low number of 20 603 391 tokens

graphframe <- questiontokens %>% 
  group_by(id) %>% 
  summarise(amount = n())

graphframe %>% 
  ggplot(aes(x = amount)) + 
  geom_density() + 
  theme_bw()

mean(graphframe$amount, na.rm = TRUE)

## Average number of tokens in each question is 410. Note the multiple peaks
##indicating a mixed model distribution

### Lets work on some fancy visualisation, I'll throw these into the pdf when I get to it

visframe <- questiontokens %>% 
  count(word) %>% 
  arrange(desc(n))

## I totally did not steal this from the script from the seminar...

visframe %>% head(300) %>% 
  ggplot(., aes(x = 1:300, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = word)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rank (log)", y = "Frequency (log)", title = "Zipf's law illustration")

## BEHOLD, the beast raised its head and cried out "No longer shall we abandon 
##the fancy ways of writing... And also! This demonstrates a high frequency of 
##irrelevant stopwords, such do not belong in the world of the BEAST".

## The beasts of the earth an sky cried out for their maker, begging the LORD
##to remove its pointless tokens. 

## And the great angel sounded the horns, and the plan was formed. The plan 
##told of the great tf-idfs, and such it was made. 

Sys.setlocale("LC_ALL", "") ## This line is due to weirdness on my comp
                            ## For some reason my locale is set to C by default
                            ## And i mean C the programming language

questiontokens <- fulltexts %>% 
  group_by(id) %>% 
  unnest_tokens(input = text_l,
                output = word,
                token = "words") %>%
  count(word)

idf_stop <- questiontokens %>%
  bind_tf_idf(word, id, n) %>% 
  ungroup() %>% 
  select(word, idf) %>% 
  unique() %>% 
  arrange(idf)

print(n=150, head(idf_stop, n=150))

## Word number 78 is secure, which I believe can have 
##relevance in regards to topic modelling, it may uncover
##topic that discusses future situations. I chose this
##value then as a cutoff. 

idf_stopper <- idf_stop %>% 
  filter(idf < 1.286)

tokensquestions2 <- tokensquestions %>%
  anti_join(idf_stopper, by = "word") # Joining against the stopwords dataframe to get rid of cells with stopwords

## Sucesfull removal of around 9 million lines.

## If you could look into the see- i mean stems, of time
##and say which will grow, and which will not. Speak then
##to me, who neither beg nor fear
##Your favors, nor your hate.

tokensquestions2 <- tokensquestions2 %>% 
  mutate(stem = quanteda::char_wordstem(word, language = "no"))

## Once again visualising the results. 

visframe2 <- tokensquestions2 %>% 
  count(stem) %>% 
  arrange(desc(n))


visframe2 %>% head(300) %>% 
  ggplot(., aes(x = 1:300, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = stem)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rank (log)", y = "Frequency (log)", title = "Zipf's law illustration")

## Thats... kinda worse actually...

## Or only marginally better, more under the line early, but approahces the line
##quicker

print(tokensquestions2[which(tokensquestions2$stem == "mul"),])

## Lets check density and averages again

graphframe2 <- tokensquestions2 %>% 
  group_by(id) %>% 
  summarise(amount = n())

graphframe2 %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") + 
  theme_bw()

mean(graphframe2$amount, na.rm = TRUE)

## The average tokens has been reduced to 224. The peaks illustrate that 
##there has been a slight oversight. This is due to genre differences between
##question types. Lets try to visualise these differences before and after

## Connecting both frames to the id types. 

typeqs <- df %>% 
  select(id, type)

graphframe <- left_join(graphframe, typeqs, by = c("id"))

graphframe <- graphframe %>% 
  mutate(treated = "No")

graphframe2 <- left_join(graphframe2, typeqs, by = c("id"))

graphframe2 <- graphframe2 %>% 
  mutate(treated = "Yes")

biggraphframe <- rbind(graphframe, graphframe2)

biggraphframe %>% 
  filter(type == "interpellasjon") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(treated)) + 
  theme_bw()

biggraphframe %>% 
  filter(type == "muntlig_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(treated)) + 
  theme_bw()

biggraphframe %>% 
  filter(type == "skriftlig_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(treated)) + 
  theme_bw()

biggraphframe %>% 
  filter(type == "sporretime_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(treated)) + 
  theme_bw()


## Now that this has all been dealt with, the great beast approacheth
## The monster, the terror, the DFM (aaaa, run for your lives)

## This code also takes quite a long while to run (i think, im writing this
##comment before ive ran it myself.)

tokensquestionsdfm <- tokensquestions2 %>%
  count(id, stem, name = "count") %>% # By default, the count-variable is called "n". Use name = "" to change it.
  cast_dfm(id, # Specify the douments in your analysis (becoming the rows)
           stem, # Specify the tokens in your analysis (becoming the columns)
           count) # Specify the number of times each token shows up in each document (becoming the cells)

## I was wrong, it only took about 20 minutes. 

tokensquestionsdfm

## Saving these objects, ready for analysis, note that the tokens dataframe
## is far too large to be uploaded to GitHub

save(tokensquestionsdfm, file = "data/questionsdfm.Rdata")

## The dfm fits, but the tokens dont, I save those locally instead

## Apparently saving these items takes longer time than running the dfm.

save(tokensquestions, file = "C:/Users/gardi/OneDrive/Documents/STV2022/data/fulltokens.Rdata")
save(tokensquestions2, file = "C:/Users/gardi/OneDrive/Documents/STV2022/data/fulltokenstrimmed.Rdata")
