#### Seminar 2: Preprosessering av tekstdata i R ####

## Pakker ## 
library(tidyverse)

### Preprosessering ###

## Sekk med ord ## 

regndans <- readLines("./data/regndans.txt")

bow <- regndans %>%
  str_split("\\s") %>%
  unlist()

set.seed(984301)

cat(bow[sample(1:length(bow))])

regndans[which(str_detect(regndans, "dragepust"))]

## Fjerne trekk ## 
library(janeaustenr)
library(dplyr)
library(tidytext)
library(ggplot2)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text) %>% 
  count(word) %>% 
  arrange(desc(n))

tidy_books %>% head(300) %>% 
  ggplot(., aes(x = 1:300, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = word)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rangering (log)", y = "Frekvens (log)", title = "Zipf's lov illustrasjon")

# Stoppord # 

library(tidytext)

load("./data/no4.rda")

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst) %>%
  count(token)

# Med stoppord
no4_tokens %>%
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)

# Uten stoppord
no4_tokens %>%
  filter(token %in% quanteda::stopwords("no") == FALSE) %>% 
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)

idf_stop <- no4_tokens %>%
  bind_tf_idf(token, titler, n) %>% 
  ungroup() %>% 
  select(token, idf) %>% 
  unique() %>% 
  arrange(idf)

idf_stop

idf_stop <- idf_stop %>% 
  filter(idf < 1)

no4_tokens %>%
  filter(token %in% idf_stop$token == FALSE) %>% 
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)

# Punktsetting og tall #

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))

no4_tokens$token %>% 
  .[which(str_detect(., "[[:punct:]]"))]

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_punct = FALSE) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_numeric = TRUE) 

table(str_detect(no4_tokens$token, "[0-9]"))

## Rotform av ord: stemming ## 

stem1 <- tokenizers::tokenize_words("det satt to katter på et bord") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("det satt en katt på et bordet") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

cbind(stem1, stem2, samme = stem1 == stem2)

stem1 <- tokenizers::tokenize_words("jeg har én god fot og én dårlig hånd") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("jeg har to gode føtter og to dårlige hender") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

## Rotform av ord: lemmatisering ##

# Må ha installert Python for å bruke spacyr
# library(spacyr) 
# spacy_initialize("nb_core_news_lg")

# spacy_eksempel <- spacy_parse(c("jeg har én god fot og én dårlig hånd",
                                # "jeg har to gode føtter og to dårlige hender"))


# spacy_eksempel

tekst2 <- stortingscrape::read_obt("./data/tekst2_tag.txt")

tekst2

## Taledeler ## 

# grei1 <- "den snegler seg fremover"
# grei2 <- "det er mange snegler her"

# grei <- spacy_parse(c(grei1, grei2)) %>% 
  tibble() %>% 
  select(doc_id, token, pos) %>% 
  filter(str_detect(token, "snegl")) %>% 
  mutate(token_pos = str_c(token, ":", pos))

# grei

## n-grams ## 
no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "words")

no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "ngrams",
                n = 2)

## Word embeddings ## 
stoppord <- stopwords::stopwords("Norwegian") # Finner stoppord fra den norske bokmålslista til "stopwords" pakken

stoppord_boundary <- str_c("\\b", stoppord, "\\b", # Lager en vektor med "word boundary" for å ta ut ord fra en streng
                           collapse = "|") # Setter | mellom hver ord for å skille dem fra hverandre med "eller"-operator

no4_prepped <- no4 %>%
  mutate(tekst = str_to_lower(tekst), # Setter all tekst til liten bokstav
         tekst = str_replace_all(tekst, "[0-9]+", ""), # Fjerner tall fra teksten
         tekst = str_squish(tekst), # Fjerner whitespace
         tekst = str_replace_all(tekst, "\\b\\w{1,1}\\b", ""), # Fjerner enkeltbokstaver
         tekst = str_replace_all(tekst, stoppord_boundary, ""), # Fjerner stoppord
         tekst = str_replace_all(tekst, "[:punct:]", "")) # Fjerner all punktsetting

no4_tekster <- tempfile() # Oppretter en midlertidig fil på PCen
writeLines(text = no4_prepped %>% pull(tekst), con = no4_tekster) # I denne filen skriver vi inn teksten fra datasettet. 

library(fastTextR)

ft_cbow <- ft_train(no4_tekster, 
                    type = "cbow", # Velger cbow modell
                    control = ft_control(window_size = 5L)) # Setter kontekstvinduet til 5

ft_skipgram <- ft_train(no4_tekster, 
                        type = "skipgram", # Velger skipgram modell
                        control = ft_control(window_size = 5L))

ft_word_vectors(ft_cbow, c("fordi", "himmel"))

ft_nearest_neighbors(ft_cbow, "himmel", k = 5L)

