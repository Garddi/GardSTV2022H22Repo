############## ----------- Week 2: Creating a dfm --------------------

## In order to get to the core of the issue, we need to gather all the text in variables

## The journey today is tokenize the text, stem it, remove the stop words,
## create the dfm, all fun and games. 

## Act I: And I stood upon the sand of the sea, and saw a beast
##rise up out of the sea, having seven heads and ten horns, and
##upon his horns ten crowns, and upon his heads the name of blasphemy.

## Or how I learned to stop worrying and cry over my amounts of data.

## Scene I, our players, the questiondata enters the stage. 

## Alas torn by its varying levels of inputs, it felt conflicted

## The LORD, smit him down and split him into several text-inputs

load("data/usedata.Rdata")

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

## Ok this is too much. Can't be bothered anymore. Next step I tokennize the text

tokensquestions <- fulltexts %>% 
  unnest_tokens(input = text_l,
                output = word,
                token = "words")


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

## Average number of tokens in each question is 410.

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
## For some reason my locale is set to C by default, as in the programming language C

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
