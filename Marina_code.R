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
                               "100726",
                               "103379") # expand later

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

# write.csv(all_texts, "all_texts.csv")

text_corpus <- corpus(all_texts)
summary_content <- summary(text_corpus)

count_tokens <- ntoken(text_corpus) #counts total tokens in the text
count_tokens <- as.data.frame(count_tokens) # convert to df for easy merge

summary(summary_content)

summary_content %>%     # get a summary of the data
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


dictionary_csr <- dictionary(file = "Corporate Social Responsibility.cat") #load the 1st dictionary

all_esg_words <- dfm(dfm_texts, dictionary = dictionary_csr) # set up a dfm with word count by category

all_esg_words <- convert(all_esg_words, to = "data.frame") # convert it to df for easy merge

all_esg_words <- cbind(all_esg_words, all_texts[1:4]) # merge with cik and year data
all_esg_words <- cbind(all_esg_words, count_tokens) # merge with the token count
all_esg_words$year <- as.Date(all_esg_words$year, format = "%Y")
# find the relative ESG-related words 

total_ESG_only <- all_esg_words %>% 
        select(ENVIRONMENT, EMPLOYEE, 'HUMAN RIGHTS', 'SOCIAL AND COMMUNITY') %>% 
        rowSums(na.rm=TRUE)
  
all_esg_words <- cbind(all_esg_words, total_ESG_only) 
  
all_esg_words <- all_esg_words %>% mutate(relative_env = ENVIRONMENT/count_tokens)
all_esg_words <- all_esg_words %>% mutate(relative_social = `SOCIAL AND COMMUNITY`/count_tokens)
all_esg_words <- all_esg_words %>% mutate(relative_employee = EMPLOYEE/count_tokens)
all_esg_words <- all_esg_words %>% mutate(relative_humans = `HUMAN RIGHTS`/count_tokens)
all_esg_words <- all_esg_words %>% mutate(relative_overall = total_ESG_only/count_tokens)



ggplot(all_esg_words, aes(x=year, y=relative_env, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of ENVIRONMENT words")

ggplot(all_esg_words, aes(x=year, y=relative_social, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of social words")

ggplot(all_esg_words, aes(x=year, y=relative_employee, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of employee caring words")

ggplot(all_esg_words, aes(x=year, y=relative_humans, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of human rights words")

ggplot(all_esg_words, aes(x=year, y=relative_overall, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of ESG-related words")


## BaierBerningerKiesel_ESG-Wordlist dictionary

words_BBK <- read.csv(paste0(getwd(), "/BaierBerningerKiesel.csv"), sep = ";")
dictionary_csr <- dictionary(file = "Corporate Social Responsibility.cat") #load the 1st dictionary

Governance_BBK <- words_BBK[words_BBK$Topic == 'Governance',]
Governance_BBK <- Governance_BBK[1]

Environmental_BBK <- words_BBK[words_BBK$Topic == 'Environmental',]
Environmental_BBK <- Environmental_BBK[1]

Social_BBK <- words_BBK[words_BBK$Topic == 'Social',]
Social_BBK <- Social_BBK[1]

dictionary_BBK <- dictionary(list(Governance = Governance_BBK, 
                                  Environmental = Environmental_BBK, 
                                  Social = Social_BBK))


all_esg_words_BBK <- dfm(dfm_texts, dictionary = dictionary_BBK, valuetype = "glob") # set up a dfm with word count by category

all_esg_words_BBK <- convert(all_esg_words_BBK, to = "data.frame") # convert it to df for easy merge

all_esg_words_BBK <- cbind(all_esg_words_BBK, all_texts[1:4]) # merge with cik and year data
all_esg_words_BBK <- cbind(all_esg_words_BBK, count_tokens) # merge with the token count
all_esg_words_BBK$year <- as.Date(all_esg_words_BBK$year, format = "%Y")
# find the relative ESG-related words 

total_ESG_only_BBK <- all_esg_words_BBK %>% 
  select(Governance.Word, Environmental.Word, Social.Word) %>% 
  rowSums(na.rm=TRUE)

all_esg_words_BBK <- cbind(all_esg_words_BBK, total_ESG_only_BBK) 

all_esg_words_BBK <- all_esg_words_BBK %>% mutate(relative_govern = Governance.Word/count_tokens)
all_esg_words_BBK <- all_esg_words_BBK %>% mutate(relative_env = Environmental.Word/count_tokens)
all_esg_words_BBK <- all_esg_words_BBK %>% mutate(relative_soc = Social.Word/count_tokens)
all_esg_words_BBK <- all_esg_words_BBK %>% mutate(relative_overall = total_ESG_only_BBK/count_tokens)


ggplot(all_esg_words_BBK, aes(x=year, y=relative_govern, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of governance words")

ggplot(all_esg_words_BBK, aes(x=year, y=relative_env, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of environment words")

ggplot(all_esg_words_BBK, aes(x=year, y=relative_soc, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of society caring words")

ggplot(all_esg_words_BBK, aes(x=year, y=relative_overall, color=cik)) +
  geom_point() + scale_color_brewer(palette="Dark2") + 
  ggtitle("Relatvie share of all ESG-related words")


######## Alexandre's sandbox ###########







######## Martin's sandbox ###########







