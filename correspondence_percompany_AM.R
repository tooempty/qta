## Correspondence Analysis per company of 10Ks for the fashion industry
#
# description:
#
#
# author: AM
# last update: 2021-05-14


# ------ Load packages ---------------------------------------------------------
library(readxl)
library(dplyr)
library(here)
library(readtext)
library(stringr)
library(tidyr)
library(quanteda)
library(quanteda.textmodels)



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


# ------ Select one company ----------------------------------------------------

# analysis only on one year across companies to have only 27 data points (otherwise way too much)
comp <- all %>%
  filter(cik == 723603)

comp

# ------ Corpus ----------------------------------------------------------------

comp_corpus <- corpus(comp)

head(comp_corpus)

summary(comp_corpus)
# takes some seconds


# ------ Tokenization ----------------------------------------------------------

comp_tokens <- comp_corpus %>%
  tokens(remove_punct = TRUE, # remove punctuation
         remove_numbers = TRUE, # remove numbers
         remove_symbols = TRUE) %>% # remove symbols
  tokens_tolower() # everything to lower cases
# takes some minutes!

head(comp_tokens)

comp_tokens <- comp_tokens %>%
  tokens_remove(c(stopwords("english"), "million", "fiscal", "january", "february",
                  "march", "april", "may", "june", "july", "august", "september",
                  "october", "november", "december", "business", "net", "s"))


# ------ Document feature matrix -----------------------------------------------

comp_dfm <- dfm(comp_tokens)

# check number of documents
ndoc(comp_corpus)

# check by most frequent words
comp_dfm %>%
  textstat_frequency(n= 20)


# ------ Correspondence analysis (1D) ------------------------------------------
comp_ca <- textmodel_ca(comp_dfm)

textplot_scale1d(comp_ca)
# takes some seconds


# ----- Correspondence analysis (2D) -------------------------------------------

comp_ca2 <- data.frame(dim1 = coef(comp_ca, doc_dim = 1)$coef_document, 
                       dim2 = coef(comp_ca, doc_dim = 2)$coef_document,
                       doc = comp_ca$rownames)

comp_ca2 %>%
  ggplot(aes(x=dim1, y=dim2)) +
  geom_text(aes(label = doc)) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2")

# ------ Results: 798081 Lakeland Industries Inc ------
# 1D: 2019 / 2020
# 2D: 2019 / 2020
# (2D: 2021)

# ------ Results: 723603 Culp Inc ------
# 1D: split between: 2004-2009 and 2010-2020
# 2D: 2009 / 2004-2008 and 2010-2020
