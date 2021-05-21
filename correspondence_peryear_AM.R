## Correspondence Analysis per year of 10Ks for the fashion industry
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
library(ggplot2)
library(plotly)



# ------ Load Excel file with cik numbers --------------------------------------

# load fashion companies form excel file with cik number
fashion <- read_excel(here("companies", "fashion_cik.xlsx"), sheet = "Sheet1", col_names = TRUE)

# keeping only the cik number
fashion_cik <- fashion[,13]

# check if the cik number was been selected
head(fashion_cik)


# ------ Load 10Ks from files  -------------------------------------------------

fashion_10k <- NULL

# a loop collects all reports for all companies in list_of_companies_fashion
for (i in fashion_cik$CIK) {
  output <- readtext(here("Edgar filings_HTML view", "Form 10-K", i))
  fashion_10k <- rbind(fashion_10k, output)
}

head(fashion_10k)


# ------ Simple Cleaning -------------------------------------------------------

fashion_10k$text <- str_remove_all(fashion_10k$text, "\n•")
fashion_10k$text <- str_remove_all(fashion_10k$text, "\n")


# ------ define columns --------------------------------------------------------

# make the 1st column into cik, report type and year
fashion_10k <- separate(
  fashion_10k,
  1,
  into = c("cik", "type", "year", NA),
  sep = "_",
  remove = TRUE)


# ------ only keep year --------------------------------------------------------

#keep the year and not the full date
fashion_10k <- separate(
  fashion_10k,
  "year",
  into = c("year", NA),
  sep = "-",
  remove = TRUE)

head(fashion_10k)


# ------ Select one year -------------------------------------------------------

# analysis only on one year across companies to have only 27 data points (otherwise way too much)
fashion_year <- fashion_10k %>%
  filter(year == 2015)


# ------ Corpus ----------------------------------------------------------------

fashion_y_corp <- corpus(fashion_year)

head(fashion_y_corp)

summary(fashion_y_corp)
# takes some seconds


# ------ Tokenization ----------------------------------------------------------

fashion_y_tokens <- fashion_y_corp %>%
  tokens(remove_punct = TRUE, # remove punctuation
         remove_numbers = TRUE, # remove numbers
         remove_symbols = TRUE) %>% # remove symbols
  tokens_tolower() # everything to lower cases
# takes some minutes!

head(fashion_y_tokens)

fashion_y_tokens <- fashion_y_tokens %>%
  tokens_remove(c(stopwords("english"), "million", "fiscal", "january", "february",
                  "march", "april", "may", "june", "july", "august", "september",
                  "october", "november", "december", "business", "net", "s"))


# ------ Document feature matrix -----------------------------------------------

fashion_y_dfm <- dfm(fashion_y_tokens)

# check number of documents
ndoc(fashion_y_corp)

# check by most frequent words
fashion_y_dfm %>%
  textstat_frequency(n= 20)


# ------ Correspondence analysis (1D) ------------------------------------------
fashion_y_ca <- textmodel_ca(fashion_y_dfm)

textplot_scale1d(fashion_y_ca)
# takes some seconds


# ----- Correspondence analysis (2D) -------------------------------------------

fashion_y_ca2 <- data.frame(dim1 = coef(fashion_y_ca, doc_dim = 1)$coef_document, 
                       dim2 = coef(fashion_y_ca, doc_dim = 2)$coef_document,
                       doc = fashion_y_ca$rownames)

fashion_y_ca2 %>%
  ggplot(aes(x=dim1, y=dim2)) +
  geom_text(aes(label = doc)) +
  theme_classic() +
  labs(x="Dimension 1", y="Dimension 2")

# ------ Results: 2020 ------
# 2D: text 11 = cik(798081) = Lakeland Industries Inc.  2D
# 2D: text 5 = cik(723603) = Culp Inc                   low 1D
# 2D: text 24 = cik(100726) = Unifi Inc.                1D

# ------ Results: 2019 ------
# 1D: text 10 = cik(798081) = Lakeland Industries Inc.  strong
# 2D: text 10 = cik(798081) = Lakeland Industries Inc.  2D
# 2D: text 23 = cik(100726) = Unifi Inc                 1D
# 2D: text 5 = cik(723603) = Culp Inc                   low 1D

# ------ Results: 2015 ------
# 1D: text 10 = cik(798081) = Lakeland Industries Inc.   strong
# 2D: text 10 = cik(798081) = Lakeland Industries Inc.   2D
# 2D: text 23 = cik(100726) = Unifi Inc.                1D
# 2D: text 5 = cik(723603) = Culp Inc                   low 1D
# 2D: text 20&16 = cik(1065837&895456) = Skechers USA Inc & Rocky Brands Inc low 1D

# ------ Results: 2010 ------
# 2D: text 9 = cik(798081) = Lakeland Industries Inc.   2D
# 2D: text 18 = cik(913241) = Steven Madden LTD         1D
# 2D: text 22 = cik(100726) = Unifi Inc.                1D

# ------ Results: 2004 ------
# 1D & 2D: text 18 = cik(100726) = Unifi Inc.
# 1D & 2D: text 7 = cik(798081) = Lakeland Industries Inc.
# 2D: text 4&16 = cik(910521&1065837) = Deckers Outdoor Corp & Skechers USA Inc.
# 2D: text 17 = cik(1116132) = Tapestry Inc.
