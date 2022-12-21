##### ------- Script for STV2022 Candidate Number: 17112 -----------

## Section 1: collecting data

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
library(ggwordcloud)
library(xtable)
library(stargazer)

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

############## ----------- Section 2: Processing --------------------

## In order to get to the core of the issue, we need to gather all the text in variables

## The journey today is tokenize the text, stem it, remove the stop words,
## create the dfm, all fun and games. 

## Start by loading the dataset created previously

load("data/usedata.Rdata")

## Break them down into individual sources of text, that all signify the topic

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

## Removing the empty text, replacing with NA, then dropping them

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


## I could have just rbound them first and then dropped the NA's actually...

## Combining all to one frame

fulltexts <- rbind(justificationtexts, questiontexts, answertexts)

## Removing reduntant objects

rm(checks, justificationtexts, mps2021, questiontexts, questiontext2, 
   stragglers, answertexts)

### Tokenizing

## Transforming to lower strings

fulltexts <- fulltexts %>% 
  mutate(text_l = str_to_lower(text))

## tokenizing

tokensquestions <- fulltexts %>% 
  unnest_tokens(input = text_l, ## Input from the df that I want tokenized
                output = word,  ## name of the column that it creates
                token = "words")## what type of tokenizing it does


tokensquestions <- tokensquestions %>% 
  select(-text)

questiontokens <- fulltexts %>% ## I dont remember if i use this object, and im too scared to remove it
  group_by(id) %>% 
  unnest_tokens(input = text_l,
                output = word,
                token = "words") %>%
  count(word)

## A nice low number of 20 603 391 tokens

graphframe <- tokensquestions %>% 
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
                                                    ## I got to it, thanks past me
visframe <- questiontokens %>% 
  count(word) %>% 
  arrange(desc(n))

## I totally did not steal this from the script from the seminar...
## Visualising using zipfs law

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

## High frequency of stopwords

## Next step is to remove the useless tokens

## Utilising tf-idf

Sys.setlocale("LC_ALL", "") ## This line is due to weirdness on my comp
                            ## For some reason my locale is set to C by default
                            ## And i mean C the programming language

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

## I stem the words using quanteda

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

## the stem mul illuminates the issue of stemming

print(tokensquestions2[which(tokensquestions2$stem == "mul"),])

## Both the adjectiv possible, and the noun possibility is caught

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

biggraphframe <- biggraphframe %>% 
  mutate(status_c = ifelse(treated == "Yes", "Processed", "Non-processed"))

## Visualisations of pre and post processing.

biggraphframe %>% 
  filter(type == "interpellasjon") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(status_c)) + 
  labs(title = "Interpellations") +
  theme_bw()

biggraphframe %>% 
  filter(type == "muntlig_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(status_c)) + 
  labs(title = "Oral Questions") +
  theme_bw()

biggraphframe %>% 
  filter(type == "skriftlig_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(status_c)) +
  labs(title = "Written Questions") +
  theme_bw()

biggraphframe %>% 
  filter(type == "sporretime_sporsmal") %>% 
  ggplot(aes(x = amount)) + 
  geom_density(colour = "red") +  
  facet_wrap(vars(status_c)) +
  labs(title = "Question Hour Questions") +
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

save(tokensquestions, file = "data/fulltokens.Rdata")
save(tokensquestions2, file = "data/fulltokenstrimmed.Rdata")




###### -------------- Section 4 ------------------------

## Goal this section is to create the stm, and furthermore, 
## create the attached visuals, and all the other important stuff

## Loading the old items

load("data/fulltokens.Rdata")
load("data/fulltokenstrimmed.Rdata")
load("data/questionsdfm.Rdata")
load("data/usedata.Rdata")

## We start by attempting a to roughly estimate what is a prudent
## number of topics to assign the model. To do this, I run a sample
## function to see what gives the most semantic coherence and other
## measures

## Transforming the dfm into an stm format that can be used in a
##searchK function

stm_counts <- convert(tokensquestionsdfm, "stm")

## Creating some random values

K <- c(2, 5, 10, 30, 50, 70, 100, 150, 200)

set.seed(6457)

## Searching, headsup, this takes 3 days to run in an i7-6700K processor.

k_results <- searchK(documents = stm_counts$documents,
                     vocab = stm_counts$vocab, K = K,
                     data = stm_counts$meta)

# Plotting these findings in ggplot

searchplot <- tibble(Exclusivity = unlist(k_results$results$exclus),
                            Coherence = unlist(k_results$results$semcoh),
                            Heldout = unlist(k_results$results$heldout),
                            Residual = unlist(k_results$results$residual),
                            K = unlist(k_results$results$K)) %>% 
  pivot_longer(., cols = c("Exclusivity", "Coherence", "Heldout", "Residual"))

searchplot_results <- ggplot(searchplot, aes(x = K, y = value)) +
  facet_wrap(~name, scales = "free_y") +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(y = NULL, x = "Antall emner")

ggsave("ksearch.jpg", plot = searchplot_results)

## When the good number of K has been found, run it through the stm

topicmodel1 <- stm(tokensquestionsdfm, K = 210,
                   init.type = "Spectral", max.em.its = 500,
                   emtol = 1e-05, verbose = TRUE, reportevery = 10)

## After that we uncover what this topic model has to reveal to us
## start by turning it into a tidy table

topicmodel1_topics <- tidy(topicmodel1, 
                          matrix = "beta")

tokenexploringframe <- topicmodel1_topics %>%
  group_by(topic) %>% # Getting the top term per topic, thus using group_by
  slice_max(beta, n = 10) %>% # Fetching the 10 terms with the highest beta
  ungroup() # Ungrouping to get the dataframe back to normal

tokenexploringframesub1 <- tokenexploringframe %>% 
  filter(topic %in% c(1:6))

tokenexploringframesub1 %>% 
  ggplot(aes(label = term, size = beta)) + 
  geom_text_wordcloud() + 
  theme_minimal() + 
  scale_size_area(max_size = 20) +
  facet_wrap(vars(topic))

for(i in 1:210){
  tmp1 <- tokenexploringframe %>% filter(topic == i)
  
  tmp2 <- ggplot(tmp1, aes(label = term, size = beta)) + 
    geom_text_wordcloud() + theme_minimal() + scale_size_area(max_size=20)
  
  ggsave(tmp2, filename = (paste0("topic", i, "wordcloud.png")))
}

## Through these we find that five topics might be of some relevance, 
##though primarily three. 119 is about pregnancy healthcare, 135 is about
##gender equality, topic 164 is about abortion. In addition there are two
##topics with a "gendered" view, that might be tangentially related,
##topic138 is about genital mutilation, and topic 158 is about forced 
##marriage

## It appears the dfm dropped a few documents, lets limit the df

df2 <- df %>% 
  filter(id %in% c(docnames(tokensquestionsdfm)))

## Slight check as to why they are missing

df3 <- df %>% 
  filter(!(id %in% c(docnames(tokensquestionsdfm))))

## These questions are indeed empty

## lets do some slight simplifications

df2 <- df2 %>% 
  mutate(gndr = ifelse(is.na(gender), 0, ifelse(gender == "mann", 0, 1)))

## Lets join the topic gammas of each document to the df
## First we create a topic assignment dataframe

topicassigner <- tidy(topicmodel1, matrix = "gamma",
                      document_names = rownames(tokensquestionsdfm))

## Creating a dataframe for the average values on the topics for women and men

a <- list()

for(i in c(119, 135, 138, 158, 164)){
  newdf <- topicassigner %>% filter(topic == i) %>% 
    rename(id = document)
  
  meanframe <- left_join(df2, newdf, by = c("id"))
  
  a[[i]] <- aggregate(meanframe$gamma, list(meanframe$gender), FUN=mean)
}

attemptfram <- unlist(a)

attemptframe <- data.frame(gndr = c(attemptfram[1:2]), 
                           top119 = c(attemptfram[3:4]),
                           top135 = c(attemptfram[7:8]),
                           top138 = c(attemptfram[11:12]),
                           top158 = c(attemptfram[15:16]),
                           top164 = c(attemptfram[19:20]))

attemptframe[,2:6] <- sapply(attemptframe[,2:6], as.numeric)

## Producing a LateX output for the main report

xtable(attemptframe, digits = c(5))


## Investigating the gender distribution of the top scoring documents on 
##each topic

## first check whats a good gamma cutoff

## 1000 gives us a lowest gamma of 0.00292, a bit too low
## 500 gives 0.26, still too low, 300 is 0.137, better.
## 250 gives 0.1972, 200 gives 0.2778, still a bit low, but decent

topic119docs <- topicassigner %>% 
  filter(topic == 119) %>% 
  slice_max(order_by = gamma, n = 200)

summary(topic119docs$gamma)

## Plotting results

topic119frame <- df2 %>% 
  filter(id %in% topic119docs$document) %>% 
  mutate(year = as.numeric(str_extract(session_id, "[:digit:]{4}")))

topic119frame %>% 
  ggplot(aes(x = year, fill = gender)) + 
  geom_bar() + 
  theme_bw()

## Overwhelming amount of women

## Lets try 135, first we check appropriate amount of docs.

## 500 is a bit much, 0.06308 as lowest. 300 is still at 0.1211
## 200 is still only 0.1695, 150 keeps it at 0.2206, which is better.

topic135docs <- topicassigner %>% 
  filter(topic == 135) %>% 
  slice_max(order_by = gamma, n = 150)

summary(topic135docs$gamma)

## plotting results

topic135frame <- df2 %>% 
  filter(id %in% topic135docs$document) %>% 
  mutate(year = as.numeric(str_extract(session_id, "[:digit:]{4}")))

topic135frame %>% 
  ggplot(aes(x = year, fill = gender)) + 
  geom_bar() + 
  theme_bw()

## Time for the one of most interest, abortion, topic 164

## 500, only 0.02135, 200 drops it to 0.1987, acceptable.

topic164docs <- topicassigner %>% 
  filter(topic == 164) %>% 
  slice_max(order_by = gamma, n = 200)

summary(topic164docs$gamma)

## Plotting results

topic164frame <- df2 %>% 
  filter(id %in% topic164docs$document) %>% 
  mutate(year = as.numeric(str_extract(session_id, "[:digit:]{4}")))

topic164frame %>% 
  ggplot(aes(x = year, fill = gender)) + 
  geom_bar() + 
  theme_bw()


## I don't trust built in functions of stm for estimating effect, ill make my own. 

## I manually recode the document probabilities into log-odds
## First i join in the topics that i want as df's and left_join them into
## the main dataframe

teststuffthing <- topicassigner %>% 
  filter(topic %in% c(119, 135, 138, 158, 164)) %>% 
  pivot_wider(names_from = topic, names_prefix = "Topic",
              values_from = gamma) %>% 
  rename(id = document)

df3 <- left_join(df2, teststuffthing, by = c("id"))

# I dont know what this 't' does, but at this point I'm too scared to remove it
t <- c(119, 135, 138, 158, 164)

# I am too worried I have forgotten how operation orders work, so everything is wrapped in parentheses

t2 <- data.frame(sapply(df3[,36:40], function(x){y <- (log(x/(1-x))) 
return(y)
}))

colnames(t2) <- paste("Logit", colnames(t2))

df3.1 <- data.frame(df3, t2)

hist(df3.1$Logit.Topic119)

## Distribution is surprisingly normal

## Regressions on the individual topics, all have significant gender coefficients

m119.1 <- lm(Logit.Topic119 ~ gndr, data = df3.1)

summary(m119.1)

m119.2 <- lm(Logit.Topic119 ~ gndr + factor(party_id) + gndr*factor(party_id),
             data = df3.1)

summary(m119.2)

## Fuck you estimateEffect, i got significant results

## Lets try with the issue of gender equality in the workplace 

m135.1 <- lm(Logit.Topic135 ~ gndr, data = df3.1)

summary(m135.1)

m135.2 <- lm(Logit.Topic135 ~ gndr + factor(party_id) + gndr*factor(party_id),
             data = df3.1)

summary(m135.2)

## Finally, abortion rights, and parental rights

m164.1 <- lm(Logit.Topic164 ~ gndr, data = df3.1)

summary(m164.1)

m164.2 <- lm(Logit.Topic164 ~ gndr + factor(party_id) + gndr*factor(party_id),
             data = df3.1)

summary(m164.2)

## Lets check our bonus topics 

m138.1 <- lm(Logit.Topic138 ~ gndr, data = df3.1)

summary(m138.1)

m138.2 <- lm(Logit.Topic138 ~ gndr + factor(party_id) + gndr*factor(party_id),
             data = df3.1)

summary(m138.2)

m158.1 <- lm(Logit.Topic158 ~ gndr, data = df3.1)

summary(m158.1)

m158.2 <- lm(Logit.Topic158 ~ gndr + factor(party_id) + gndr*factor(party_id),
             data = df3.1)

summary(m158.2)

# Normal gndr for 158 seems a bit lower than other topics

## Checking internal correlations between chosen topics

thingwetry <- topicCorr(topicmodel1, method = "simple")

plot.topicCorr(thingwetry, topics = c(119, 135, 138, 158, 164))

# 158 seems less related than the others, this is also reflected in later regressions

### Another model for estimating a collected "women's issues"

## First i create a two new vars, one that is the sum of the main 
##womens issues and the second is that of all the women's issues. 

## This is a dumb solution for creating the new variables

for(i in 1:nrow(df3)){
  df3$WTop1[i] <- df3$Topic119[i] + df3$Topic135[i] + df3$Topic164[i]
  
  df3$WTop2[i] <- df3$Topic119[i] + df3$Topic135[i] + df3$Topic164[i] + df3$Topic138[i] + df3$Topic164[i]
}

t2 <- data.frame(sapply(df3[,41:42], function(x){y <- (log(x/(1-x))) 
return(y)
}))

colnames(t2) <- paste("Logit", colnames(t2))

df3.2 <- data.frame(df3, t2)

## Cool stuff, lets run some regressions. 

m1.1 <- lm(Logit.WTop1 ~ gndr, data = df3.2)

m1.2 <- lm(Logit.WTop1 ~ gndr + factor(party_id) + gndr*factor(party_id),
           data = df3.2)

m2.1 <- lm(Logit.WTop2 ~ gndr, data = df3.2)

m2.2 <- lm(Logit.WTop2 ~ gndr + factor(party_id) + gndr*factor(party_id),
           data = df3.2)

stargazer(m1.1, m1.2, m2.1, m2.2, type = "text", omit = c("Uv", "Kp", "TF", "PF"))

## Without the interaction link

m1.3 <- lm(Logit.WTop1 ~ gndr + factor(party_id), data = df3.2)

m2.3 <- lm(Logit.WTop2 ~ gndr + factor(party_id), data = df3.2)

stargazer(m1.3, m2.3, type = "text", omit = c("Uv", "Kp", "TF", "PF"))

## Combined table reported in the main task

stargazer(m1.1, m1.3, m1.2, m2.1, m2.3, m2.2, omit = c("factor"),
          omit.stat=c("f", "ser"), covariate.labels = c("Female"))

## Combined table reported in the appendix.

stargazer(m1.1, m1.3, m1.2, m2.1, m2.3, m2.2, omit = c("Uv", "Kp", "TF", "PF"),
          omit.stat=c("f", "ser"))


## Super cool, very wow, much significance

### Finding the most representative documents

df3$question_text[which.max(df3$Topic119)]

df3$question_text[which.max(df3$Topic135)]

df3$question_text[which.max(df3$Topic138)]

df3$question_text[which.max(df3$Topic158)]

df3$question_text[which.max(df3$Topic164)]

## All seem good 



########### ----------- Appendix of items not included in the analysis ---------

## EstimateEffect doesnt like missing values

## We start by estimating the effect of gender on its own

willthiswork <- estimateEffect(c(119,135,138,164,158) ~ gndr, 
                               stmobj = topicmodel1,
                               metadata = df2)

plot.estimateEffect(willthiswork, covariate = "gndr", 
                    method = "pointestimate")

## Thats dissappointing, lets see what party does

df2 <- df2 %>% 
  mutate(party_id = ifelse(is.na(party_id), "Uv", party_id))

yesitdid <- estimateEffect(c(119,135,138,164,158) ~ factor(party_id),
                           stmobj = topicmodel1,
                           metadata = df2)

## Can check the topics individually

plot.estimateEffect(yesitdid, topics = 119, covariate = "party_id", 
                    method = "pointestimate")
# No major differences,
plot.estimateEffect(yesitdid, topics = 135, covariate = "party_id", 
                    method = "pointestimate")
# Nothing major, KrF slightly ahead
plot.estimateEffect(yesitdid, topics = 138, covariate = "party_id", 
                    method = "pointestimate")
# Red party is very involved, interesting
plot.estimateEffect(yesitdid, topics = 164, covariate = "party_id", 
                    method = "pointestimate")
# Literally no differences, maybe Krf is a bit lower
plot.estimateEffect(yesitdid, topics = 158, covariate = "party_id", 
                    method = "pointestimate")
# Krf a bit higher

estimate.t1 <- estimateEffect(c(119,135,138,164,158) ~ gndr*factor(party_id),
                              stmobj = topicmodel1, metadata = df2)

plot.estimateEffect(estimate.t1, covariate = "gndr",
                    method = "pointestimate")

## Literally nothing is significant, just like my contribution to this world


### Final bit is for predicted probabilities, likely to not be included,
### but demonstrates the difference in predicted probabilities. 

predictionset1 <- data.frame(gndr = c(1,0))

predictionset2 <- data.frame(gndr = c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                             party_id = c("A", "A", "H", "H", "V", "V", "FrP", "FrP", "KrF", "KrF",
                                          "MDG", "MDG", "SV", "SV", "Sp", "Sp", "R", "R"))

predictionset1$pred1 <- predict(m1.1, predictionset1)
predictionset1$pred2 <- predict(m2.1, predictionset1)

sapply(predictionset1[,2:3], function(x){ 
  y <- exp(x)/(1+exp(x))})

predictionset2$pred1 <- predict(m1.2, predictionset2)
predictionset2$pred2 <- predict(m2.2, predictionset2)

sapply(predictionset2[,3:4], function(x){ 
  y <- exp(x)/(1+exp(x))
  return(y)})

exporttable <- data.frame(gndr = c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                          party_id = c("A", "A", "H", "H", "V", "V", "FrP", "FrP", "KrF", "KrF",
                                       "MDG", "MDG", "SV", "SV", "Sp", "Sp", "R", "R"),
                          mod1 = sapply(predictionset2[,3], function(x){ 
                            y <- exp(x)/(1+exp(x))
                            return(y)}),
                          mod2 = sapply(predictionset2[,4], function(x){
                            y <- exp(x)/(1+exp(x))
                            return(y)
                          }))


