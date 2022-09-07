#### Seminar 1: Anskaffe tekst og lage dtm i R####

### Pakker ###
library(tidyverse)

### Laste inn tekstdata ###

## To-dimensjonale datasett ## 
library(stortingscrape)
saker <- cases$root

saker %>% 
  select(id, document_group, status, title_short) %>% 
  mutate(title_short = str_sub(title_short, 1, 30)) %>% 
  tail()

# .rda og .RData # 
save(saker, file = "./data/saker.rda")

load("./data/saker.rda")

# .csv # 
# library(readr)

# saker <- read_csv("./data/saker.csv", show_col_types = FALSE)

# .sav (SPSS) og .dta (STATA)
# library(haven)
# saker <- read_sav("./data/saker.sav")

# saker <- read_dta("./data/saker.dta")

## Rå tekstfiler ##
filer <- list.files("./data/txt", pattern = ".txt", full.names = TRUE)
filer

titler <- lapply(filer, readLines)
class(titler)

titler[[1]]

names(titler) <- str_extract(filer, "[0-9]+")
names(titler)

saker_txt <- data.frame(titler = unlist(titler),
                        id = names(titler))

saker_merge <- left_join(saker_txt, saker[, c("id", "title")], by = "id")

saker_merge$titler == saker_merge$title

## Tekstfiler med overhead ## 
unzip("data/ba_thesis.docx", exdir = "data/wordfiles")

list.files("data/wordfiles/")

readLines("./data/ba_thesis.docx", n = 2)

# .docx #
library(textreadr)

ba_docx <- read_docx("./data/ba_thesis.docx")

ba_docx[43:46]

# .pdf # 
ba_pdf <- read_pdf("./data/ba_thesis.pdf")

ba_pdf <- ba_pdf$text[4] %>% 
  strsplit("\\n") %>% 
  unlist()

ba_pdf[11:14]

### Anskaffelse av tekst ###

## html-skraping ##

# Skrape en nettside # 
library(rvest)

# Laste ned en html-fil #
download.file("https://en.wikipedia.org/wiki/Orange_(fruit)", # Last ned en html-fil ...
              destfile = "./data/links/Oranges.html") # ... inn i en spesifikk mappe

# Hvis du har mac, må du sette tilde (~) istedenfor punktum (.)
# Husk å være oppmerksom på hvor du har working directory, sjekk med getwd() og sett nytt working directory med setwd()

# Lese en nedlastet html-fil #
library(rvest)

## read_html("https://en.wikipedia.org/wiki/Orange_(fruit)") # Les inn direkte fra nettside

read_html("./data/links/Oranges.html") # Les inn fra din nedlastede fil

# Laste ned tekst fra html-fil # 
read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_text(trim = TRUE)

# Laste ned tabell fra html-fil # 
read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table()

read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table() %>%
  na_if("") %>% # Erstatter "" med NA (missing)
  na.omit() # Fjerner alle NA

# Skrape lenker #
read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_elements("a") %>%
  html_attr("href")

links <- read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_extract("/wiki.*") %>% # Samle bare de URL-ene som starter med "/wiki", fulgt av hva som helst (.*)
  na.omit() %>% # Alle andre strenger blir NA, vi fjerner disse
  str_c("https://en.wikipedia.org/", .) # str_c limer sammen to strenger, vi limer på første halvdel av URL-en.


linkstopic <- str_remove(links, "https://en.wikipedia.org//wiki/")

for(i in 1:length(links)) { # For alle lenkene...
  
  download.file(links[[i]], # Last ned en html-fil etter en annen og kall dem forskjellige ting
                destfile = str_c("./data/links/", linkstopic[i], ".html"))
}

fruit_files <- list.files("./data/links", full.names = TRUE) # Liste med filene vi har lastet ned

info <- list() # Lag et liste-objekt hvor du kan putte output fra løkken

for (i in 1:length(fruit_files)) { # For hver enhet (i) som finnes i links, fra plass 1 til sisteplass i objektet (gitt med length(links))...
  
  page <- read_html(fruit_files[i]) # ... les html-filen for hver i
  
  page <- page %>% # Bruk denne siden
    html_elements("p") %>% # Og få tak i avsnittene
    html_text() # Deretter, hent ut teksten fra disse avsnittene
  
  info[[i]] <- page # Plasser teksten inn på sin respektive plass i info-objektet
  
}

info[[1]][3]

## Andre formater og APIer ## 

# .json #

library(jsonlite)

person1 <- read_json("./data/swapi/person1.json")

names(person1)

class(person1)

person1$name

person1$starships

# .xml #

if(file.exists("./data/ruter.xml") == FALSE){
  download.file(url = "https://api.entur.io/realtime/v1/rest/et?datasetId=RUT",
                destfile = "./data/ruter.xml")
}
library(rvest)

ruter <- read_html("./data/ruter.xml")

stopp <- ruter %>% html_elements("recordedcall")

alle_stopp <- pbmcapply::pbmclapply(stopp, function(x){
  
  
  tibble::tibble(
    stop_id = x %>% html_elements("stoppointref") %>% html_text(),
    order = x %>% html_elements("order") %>% html_text(),
    stopp_name = x %>% html_elements("stoppointname") %>% html_text(),
    aimed_dep = x %>% html_elements("aimeddeparturetime") %>% html_text(),
    actual_dep = x %>% html_elements("actualdeparturetime") %>% html_text()
  )
  
}, mc.cores = parallel::detectCores()-1)

alle_stopp <- bind_rows(alle_stopp)

head(alle_stopp)

stop_name_count <- alle_stopp %>% 
  count(stopp_name) %>%             # vi teller stoppnavn
  arrange(desc(n)) %>%              # sorterer data etter # linjer
  filter(nchar(stopp_name) > 3) %>% # tar bort korte stoppnavn
  slice_max(n = 30, order_by = n)   # tar med bare de 30 mest brukte stoppene

library(ggwordcloud)

# Setter opp tilfeldige farger
cols <- sample(colors(),
               size = nrow(stop_name_count),
               replace = TRUE)

# Lager plot
stop_name_count %>% 
  ggplot(., aes(label = stopp_name, 
                size = n,  
                color = cols)) +
  geom_text_wordcloud_area()+
  scale_size_area(max_size = 10) +
  ggdark::dark_theme_void()

## Kravling ## 

# library(Rcrawler)

# Rcrawler("http://virksommeord.no/", # Nettsiden vi skal kravle
         # DIR = "./crawl",           # mappen vi lagrer filene i
         # no_cores = 4,              # kjerner for å prosessere data
         # dataUrlfilter = "/tale/",  # subset filter for kravling
         # RequestsDelay = 2 + abs(rnorm(1)))

