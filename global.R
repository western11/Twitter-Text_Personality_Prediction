# global

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(rtweet)
library(ggplot2)
library(plotly)
library(stringr)
library(textclean)
library(tm)
library(SnowballC)
library(recipes)
library(ranger)
library(parsnip)
library(tidyr)
library(tidytext)



token <- readRDS("data/token_1.rds")

#model mbti
mod.rf.ei <- readRDS("data/mod_ei.rds")
mod.rf.jp <- readRDS("data/mod_jp.rds")
mod.rf.ns <- readRDS("data/mod_ns.rds")
mod.rf.tf <- readRDS("data/mod_tf.rds")

# model traits
mod.rf.agr <- readRDS("data/mod_agr.rds")
mod.rf.con <- readRDS("data/mod_con.rds")
mod.rf.ext <- readRDS("data/mod_ext.rds")
mod.rf.neu <- readRDS("data/mod_neu.rds")
mod.rf.opn <- readRDS("data/mod_opn.rds")

# names
names_traits <- readRDS("data/names_trait.rds")
names_mbti <- readRDS("data/names_mbti.rds")

# Model evaluation
eval_mbti <- readRDS("data/mbti_eval.rds")
eval_person <- readRDS("data/person_eval.rds")

# function twutter text cleaner
textcleaner_twt <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_html(symbol = FALSE) %>% # remove html
    replace_url(replacement = "") %>% # remove url
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim()
}

# traits dtm
traits_dtm_converter <- function(x){
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removePunctuation)
  
  # mengubah corpus menjadi document term matrix
  return(DocumentTermMatrix(xdtm,control = list(
    dictionary = names_traits)
  ))
}

# mbti dtm
dtm_converter <- function(x){
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removePunctuation)
  
  # mengubah corpus menjadi document term matrix
  return(DocumentTermMatrix(xdtm,control = list(
    dictionary = names_mbti)
  ))
}

# function get personality
get_personality <- function(x){
  # text cleaner
  tweet <- x %>% filter(is_retweet == "TRUE") %>%
    select(text)
  tweet <- sapply(tweet, FUN = textcleaner_twt) %>%
    data.frame()
  tweet_combined <- tweet %>% paste(sep = ",") %>% str_remove_all(pattern = "[[:punct:]]")
  tweet_traits <- substr(tweet_combined,start = 1, stop = 15000)
  tweet_mbti <- substr(tweet_combined,start = 1, stop = 1500)
  
  # tokenizer traits
  dtm_traits <- traits_dtm_converter(tweet_traits)
  dtm_traits <- as.data.frame(as.matrix(dtm_traits), stringsAsFactors = F)
  colnames(dtm_traits) <- make.names(colnames(dtm_traits))
  # tokenizer mbti
  dtm_mbti <- dtm_converter(tweet_mbti)
  dtm_mbti <- as.data.frame(as.matrix(dtm_mbti), stringsAsFactors = F)
  
  # Predict traits
  predict.ext <- predict(mod.rf.ext, dtm_traits) %>%
    setNames("Extraversion")
  predict.agr <- predict(mod.rf.agr, dtm_traits) %>%
    setNames("Agreeableness")
  predict.con <- predict(mod.rf.con, dtm_traits) %>%
    setNames("Conscientiousness")
  predict.neu <- predict(mod.rf.neu, dtm_traits) %>%
    setNames("Neuroticism")
  predict.opn <- predict(mod.rf.opn, dtm_traits) %>%
    setNames("Openness")
  
  personality <- cbind(predict.ext,predict.agr,predict.con,
                       predict.neu,predict.opn)
  
  # predict mbti class
  predict.ei <- predict(mod.rf.ei, dtm_mbti, type = "class")
  predict.ns <- predict(mod.rf.ns, dtm_mbti, type = "class")
  predict.tf <- predict(mod.rf.tf, dtm_mbti, type = "class")
  predict.jp <- predict(mod.rf.jp, dtm_mbti, type = "class")
  # predict mbti prob
  predict.ei_p <- predict(mod.rf.ei, dtm_mbti, type = "prob") %>% setNames(c("Extraverted","Introverted"))
  predict.ns_p <- predict(mod.rf.ns, dtm_mbti, type = "prob") %>% setNames(c("Intuitive","Observant"))
  predict.tf_p <- predict(mod.rf.tf, dtm_mbti, type = "prob") %>% setNames(c("Thinking","Feeling"))
  predict.jp_p <- predict(mod.rf.jp, dtm_mbti, type = "prob") %>% setNames(c("Judging","Prospecting"))
  
  mbti_combine <- paste(predict.ei$.pred_class, predict.ns$.pred_class,
                        predict.tf$.pred_class, predict.jp$.pred_class,
                        sep = "")
  mbti_combine <- data.frame(MBTI = mbti_combine)
  
  mbti_prob <- cbind(predict.ei_p,predict.ns_p,predict.tf_p,predict.jp_p)
  
  return(cbind(personality,mbti_combine,mbti_prob))
  
}


# Run App
source("ui.R")
source("server.R")
shinyApp(ui,server,)



