library(readr)
library(here)
library(stringr)
library(rvest)
library(readtext)
library(tidyverse)
library(quanteda)
library("quanteda.textmodels") 
library("quanteda.textplots")

#### Marina's sandbox #####

list_of_companies_fashion <- c("72573",
                               "75288",
                               "78239",
                               "95574",
                               "100726") # expand later

all_texts <- NULL

# a loop collects all reports for all companies in list_of_companies_fashion
for (i in list_of_companies_fashion) {
  output <- readtext(here("Form10-K", i))
  all_texts <- rbind(all_texts, output)
}

# clean up the file
all_texts[2] <- gsub(pattern = "<.*?>", "", x = all_texts[2])

# make the 1st column into cik, report type and year
all_texts <- separate(
  all_texts,
  1,
  into = c("cik", "type", "year", NA),
  sep = "_",
  remove = TRUE)

#keep the year and not the full date
all_texts <- separate(
  all_texts,
  "year",
  into = c("year", NA),
  sep = "-",
  remove = TRUE)

write.csv(all_texts, "all_texts.csv")

## prep over
text_corpus <- corpus(all_texts)
summary_content <- summary(text_corpus)

summary(summary_content)

summary_content %>% 
  summarise(
    count = n(),
    avg_length = mean(Sentences),
    min_length = min(Sentences), 
    max_length = max(Sentences)   ) 

ggplot(summary_content, aes(x=year, y=Sentences, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2")

# dictionary -  CSR dictionary (corporate social responsibility)
# do a graph for ESG wording per company-year

dfm_texts = dfm(text_corpus, 
             tolower = TRUE, 
             remove = stopwords(),  # this removes English stopwords
             remove_punct = TRUE,   # this removes punctuation
             remove_numbers = TRUE, # this removes digits
             remove_symbol = TRUE,  # this removes symbols 
             remove_url = TRUE )     # this removes urls


dictionary_csr <- dictionary(file = "Corporate Social Responsibility.cat")

all_esg_words <- dfm(dfm_texts, dictionary = dictionary_csr)

all_esg_words <- convert(all_esg_words, to = "data.frame")

all_esg_words <- cbind(all_esg_words, all_texts[1:4])

ggplot(all_esg_words, aes(x=year, y=ENVIRONMENT, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Number of ENVIRONMENT words")


# to-do: add a column with n_tokens to the initial df & compute relative share of ESG wording

## BaierBerningerKiesel_ESG-Wordlist dictionary






## merged dictionaries




######## Alexandre's sandbox ###########







######## Martin's sandbox ###########







