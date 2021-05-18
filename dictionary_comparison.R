## dictionary tokens
#
# description:
#
#
# author: AM
# last update: 2021-05-18


# ------ Load packages ---------------------------------------------------------
library(here)
library(readxl)
library(readtext)
library(stringr)
library(tidyverse)
library(quanteda)


# ------ Load Excel file with cik numbers --------------------------------------

# load fashion companies form excel file with cik number
fashion <- read_excel(here("companies", "fashion_cik.xlsx"), sheet = "Sheet1", col_names = TRUE)

# keeping only the cik number
fashion_cik <- fashion[,13]

# check if the cik number was been selected
head(fashion_cik)


# ------ Load 10Ks from files  -------------------------------------------------

all <- NULL

# a loop collects all reports for all companies in list_of_companies_fashion
for (i in fashion_cik$CIK) {
  output <- readtext(here("Edgar filings_HTML view", "Form 10-K", i))
  all <- rbind(all, output)
}
# takes some minutes
# should be 436 observations

head(all)


# ------ Simple Cleaning -------------------------------------------------------

all$text <- str_remove_all(all$text, "\n•")
all$text <- str_remove_all(all$text, "\n")


# ------ define columns --------------------------------------------------------

# make the 1st column into cik, report type and year
all <- separate(
  all,
  1,
  into = c("cik", "type", "year", NA),
  sep = "_",
  remove = TRUE)


# ------ only keep year --------------------------------------------------------

#keep the year and not the full date
all <- separate(
  all,
  "year",
  into = c("year", NA),
  sep = "-",
  remove = TRUE)

head(all)


# ------ Corpus ----------------------------------------------------------------

all_corpus <- corpus(all)

head(all_corpus)


# ------ Tokenization ----------------------------------------------------------

all_tokens <- all_corpus %>%
  tokens(remove_punct = TRUE, # remove punctuation
         remove_numbers = TRUE, # remove numbers
         remove_symbols = TRUE,
         remove_url = TRUE) %>% # remove symbols
  tokens_tolower() # everything to lower cases
# takes some minutes!


words <- c(stopwords("english"), "million", "fiscal", "january", "february",
           "march", "april", "may", "june", "july", "august", "september",
           "october", "november", "december", "business", "net", "s")

all_tokens <- all_tokens %>%
  tokens_remove(words)

head(all_tokens)


# ------ Load the dictionaries -------------------------------------------------

### CSR dict ###

dictionary_csr <- dictionary(file = "Corporate Social Responsibility.cat")
# load the 1st dictionary CSR

## Overview

lengths(dictionary_csr)
# 4 Categories: Human Rights (297), Employee (319), Social and Community (361) and Environment (451)
# words can and do occur multiple times across categories

head(dictionary_csr$`HUMAN RIGHTS`) # first words in human rights
head(dictionary_csr$EMPLOYEE) # first words in employee
head(dictionary_csr$`SOCIAL AND COMMUNITY`) # first words in social and community
head(dictionary_csr$ENVIRONMENT) # first words in environment

str(dictionary_bbk)


### BBK dict ###

dictionary_bbk <- read_excel(here("BaierBerningerKiesel.xlsx"), sheet = "Sheet1", col_names = TRUE)
# load the 2nd dictionary BBK Excel-file

dictionary_bbk <- read.csv(paste0(getwd(), "/BaierBerningerKiesel.csv"), sep = ";")
# load the 2nd dictionary BBK CSV-file

dictionary_bbk <- read_excel(here("BaierBerningerKiesel.xlsx"), sheet = "short", col_names = TRUE)
# load the 2nd citionary BBK Excel-file without categories and sub-categories

lengths(dictionary_bbk)
# 482 entries/words

unique(dictionary_bbk$Topic)
# 3 topics: Governance, Environmental and Social

unique(dictionary_bbk$Word)
# 482 unique words

unique(dictionary_bbk$Category)
# 11 categories:  Corporate Governance, Business Ethics, -, Sustainability Management and Reporting,
#                 Climate Change, Ecosystem Service, Environmental Management, Public Health, Human Rights,
#                 Labor Standards, Society

unique(dictionary_bbk$Subcategory)
# 35 subcategories

head(dictionary_bbk[dictionary_bbk$Topic == "Governance",][1])
# first words in the topic Governance

head(dictionary_bbk[dictionary_bbk$Topic == "Environmental",][1])
# first words in the topic Environmental


# ------ Selection of dictionary tokens ----------------------------------------

### CSR ###

just_csr_tokens <- tokens_select(all_tokens, dictionary_csr)

csr_env_tok <- tokens_select(all_tokens, dictionary_csr$ENVIRONMENT)


### BBK ###

just_bbk_tokens <- tokens_select(all_tokens, dictionary_bbk)

bbk_env_tok <- tokens_select(all_tokens, (dictionary_bbk[dictionary_bbk$Topic == "Environmental",][1]))


# ------ Document feature matrix -----------------------------------------------

all_dfm <- dfm(all_tokens)
# DFM: all tokens without stopwords

just_csr_dfm <- dfm(just_csr_tokens)
# DFM: all tokens occuring in the dictionary CSR

just_bbk_dfm <- dfm(just_bbk_tokens)
# DFM: all tokens occurring in the dictionary BBK

csr_env_dfm <- dfm(csr_env_tok)
# DFM: all tokens occurring in the category environmental from the dictionary CSR

bbk_env_dfm <- dfm(bbk_env_tok)
# DFM: all tokens occurring for the topic environmental from the dictionary BBK

# check number of documents (should be the initial 436)
ndoc(all_dfm)
ndoc(just_csr_dfm)
ndoc(just_bbk_dfm)
ndoc(csr_env_dfm)
ndoc(bbk_env_dfm)

# count token
count_tokens <- ntoken(all_dfm)

head(count_tokens)
head(ntoken(just_csr_dfm))
head(ntoken(just_bbk_dfm))
head(ntoken(csr_env_dfm))
head(ntoken(bbk_env_dfm))


# ------ most used words -------------------------------------------------------

all_dfm %>%
  textstat_frequency(n= 20)
# most used words without stopwords

just_csr_dfm %>%
  textstat_frequency(n= 20)
# most used words included in the CSR dictionary

just_bbk_dfm %>%
  textstat_frequency(n = 20)
# most used words included in the CSR dictionary

csr_env_dfm %>%
  textstat_frequency(n=20)
# most used words included in the category environment in the CSR dictionary

textplot_wordcloud(just_csr_dfm, max_words = 50, color = c("grey80", "darkgoldenrod1", "tomato"))
# wordcloud for most used words in the CSR dictionary


# ------ Apply the dictionaries ------------------------------------------------


### CSR ###

all_esg_words <- dfm(all_dfm, dictionary = dictionary_csr)
# set up a dfm with word count by category for each text per year

CSR_tokens <- ntoken(all_esg_words)

head(CSR_tokens)
# amount of dictionary tokens 
# difference to ntoken after applying the dictionary (due to multiple matches in different dictionaries?)