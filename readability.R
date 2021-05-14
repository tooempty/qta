## Readability of 10Ks for the fashion industry
#
# description: 
#
#
# author: AM
# date: 2021-05-14


# ------ Load packages ---------------------------------------------------------
library(readtext)
library(dplyr)
library(here)
library(ggplot2)
library(readxl)
library(quanteda)
library(stringr)
library(tidyr)


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
head(all$text[413])


# ------ Overview --------------------------------------------------------------

length(unique(all$cik))
# 27 different companies

ndoc(all)
# 436 10-Ks

ndoc(all)/length(unique(all$cik))
# average of 16.148 10-Ks

# ------ Corpus ----------------------------------------------------------------

all_corpus <- corpus(all)

head(all_corpus)

summary(all_corpus)
# takes some seconds


# ------ Readability with Flesch -----------------------------------------------

# for Flesch measure: the higher the index-number the more easily it is readable

rd_flesch <- all_corpus %>%
  textstat_readability(measure = "Flesch", remove_hyphens = TRUE)
# takes a few minutes

head(rd_flesch)

mean(rd_flesch$Flesch)
# deckers: 13.59 (0-30: very difficult to read, best understood by university graduates)
# 7 companies: 12.45195
# 27 companies: 12.33

# all companies
ggplot(rd_flesch, aes(x=all$year, y =Flesch, color=all$cik)) + geom_point() +
  ggtitle("Flesch - Readability")

# filter one company out
ggplot(rd_flesch, aes(x=all$year, y =Flesch, color=all$cik=="78239")) + geom_point() +
  ggtitle("Flesch - Readability") + scale_color_brewer(palette="Dark2")


# ------ Readability with FOG --------------------------------------------------

# for Fog measure: the higher the index-number the more complex it is
# universal understanding generally need an index less than 8
# 12: for a wide audience generally

rd_fog <- all_corpus %>%
  textstat_readability(measure = "FOG", remove_hyphens = TRUE)
# takes a few minutes

head(rd_fog)

mean(rd_fog$FOG)
# 7 companies: 24.37 (needs college graduate)
# 27 companies: 23.996 (needs college graduate)

# all companies
ggplot(rd_fog, aes(x=all$year, y =FOG, color=all$cik)) + geom_point() +
  ggtitle("FOG - Readability")

# filter one company out
ggplot(rd_fog, aes(x=all$year, y =FOG, color=all$cik=="78239")) + geom_point() +
  ggtitle("FOG - Readability") + scale_color_brewer(palette="Dark2")


# ------ Sentences (Work in progress) -------------------------------------------------------------

all_sent <- all_corpus %>%
  corpus_reshape("sentences")

format(ndoc(all_sent), big.mark = "'") # in total 571'608 sentences

average_sent <- round(ndoc(all_sent)/ndoc(all), digits = 0)

average_sent # average of 1'311 sentences per 10 K


# QUESTION: How to calculate the average sentences per company??
cik_sent <- all %>%
  group_by("cik") %>%
  corpus() %>%
  corpus_reshape("sentences")

format(ndoc(cik_sent), big.mark = "'") # same output: 571'609 sent


# ------ Readability based on file size (Work in progress) ----------------------------------------

# Checks
file.size(here("Edgar filings_HTML view", "Form 10-K", "78239")) # 640 bytes, 47.8 MB
file.size(here("Edgar filings_HTML view", "Form 10-K", "1760965")) #128 bytes, 7.4 MB

# file size

for (cik in all$cik) {
  all[all$cik==cik,]$file_size <- file.size(here("Edgar filings_HTML view", "Form 10-K", cik))
}

# Overview
print(all, n = 30)
print(unique(all$file_size))

# relative file size

docs <- NULL

for (i in all$cik) {
  docs <- all %>%
    filter(cik==i) %>%
    nrow()
  all[all$cik==i,]$file_size <- file.size(here("Edgar filings_HTML view", "Form 10-K", "78239")) / docs
}

# Overview
print(all, n = 30)
print(unique(all$file_size))

all %>%
  filter (file_size == 320)
# QUESTION: Why 320 for cik = 1760965 (only two documents with total file size of 120 -> see Check 2)

# Check 1
file.size(here("Edgar filings_HTML view", "Form 10-K", "78239")) # 640 bytes, 47.8 MB
all %>%
  filter(cik=="78239") %>%
  nrow() # 18

640/18 # = 35.55556
all[all$cik==i,]$file_size / docs
file.size(here("Edgar filings_HTML view", "Form 10-K", "78239")) / 18

# Check 2
file.size(here("Edgar filings_HTML view", "Form 10-K", "1760965")) # 128 bytes
all %>%
  filter(cik=="1760965") %>%
  nrow() # 2

128/2 # = 64
all[all$cik==i,]$file_size / docs
file.size(here("Edgar filings_HTML view", "Form 10-K", "1760965")) / 2

# all companies
ggplot(all, aes(x=year, y = file_size, color=cik)) + geom_point() +
  ggtitle("Readability based on file size")
