## Correspondence Analysis of 10Ks for the fashion industry
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


# ------ Select one year -------------------------------------------------------

# analysis only on one year across companies to have only 27 data points (otherwise way too much)
year <- all %>%
  filter(year == 2020)


# ------ Corpus ----------------------------------------------------------------

year_corpus <- corpus(year)

head(year_corpus)

summary(year_corpus)
# takes some seconds


# ------ Tokenization ----------------------------------------------------------

year_tokens <- year_corpus %>%
  tokens(remove_punct = TRUE, # remove punctuation
         remove_numbers = TRUE, # remove numbers
         remove_symbols = TRUE) %>% # remove symbols
  tokens_tolower() # everything to lower cases
# takes some minutes!

head(year_tokens)

year_tokens <- year_tokens %>%
  tokens_remove(c(stopwords("english"), "million", "fiscal", "january", "february",
                  "march", "april", "may", "june", "july", "august", "september",
                  "october", "november", "december", "business", "net", "s"))


# ------ Document feature matrix -----------------------------------------------

year_dfm <- dfm(year_tokens)

# check number of documents
ndoc(year_corpus)

# check by most frequent words
year_dfm %>%
  textstat_frequency(n= 20)


# ------ Correspondence analysis (1D) ------------------------------------------
year_ca <- textmodel_ca(year_dfm)

textplot_scale1d(year_ca)
# takes some seconds


# ----- Correspondence analysis (2D) -------------------------------------------

year_ca2 <- data.frame(dim1 = coef(year_ca, doc_dim = 1)$coef_document, 
                       dim2 = coef(year_ca, doc_dim = 2)$coef_document,
                       doc = year_ca$rownames)

year_ca2 %>%
  ggplot(aes(x=dim1, y=dim2)) +
  geom_text(aes(label = doc)) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2")
