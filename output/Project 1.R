# Part 1
# We are going to perform text processing in Part 1 to produce the output file: processed_moments.csv.
# This file is going to be used for text analysis in Part 2
library(tm)
library(tidytext)
library(tidyverse)
library(DT)

### Step 1 - Load the data to be cleaned and processed
urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/cleaned_hm.csv'
hm_data <- read_csv(urlfile)

### Step 2 - Preliminary cleaning of text
# We clean the text by converting all the letters to the lower case, and removing punctuation, numbers, empty words and extra white space.
corpus <- VCorpus(VectorSource(hm_data$cleaned_hm))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

### Step 3 - Stemming words and converting tm object to tidy object
# Stemming reduces a word to its word *stem*. We stem the words here and then convert the "tm" object to a "tidy" object for much faster processing.
stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  select(text)

### Step 4 - Creating tidy format of the dictionary to be used for completing stems
# We also need a dictionary to look up the words corresponding to the stems.
dict <- tidy(corpus) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

### Step 5 - Removing stopwords that don't hold any significant information for our data set
# We remove stopwords provided by the "tidytext" package and also add custom stopwords in context of our data.
data("stop_words")

word <- c("happy","ago","yesterday","lot","today","months","month",
          "happier","happiest","last","week","past", "day", "time", "moment")

stop_words <- stop_words %>%
  bind_rows(mutate(tibble(word), lexicon = "updated"))

### Step 6 - Combining stems and dictionary into the same tibble
# Here we combine the stems and the dictionary into the same "tidy" object
completed <- stemmed %>%
  mutate(id = row_number()) %>%
  unnest_tokens(stems, text) %>%
  bind_cols(dict) %>%
  anti_join(stop_words, by = c("dictionary" = "word"))

### Step 7 - Stem completion
# Lastly, we complete the stems by picking the corresponding word with the highest frequency.
completed <- completed %>%
  group_by(stems) %>%
  count(dictionary) %>%
  mutate(word = dictionary[which.max(n)]) %>%
  ungroup() %>%
  select(stems, word) %>%
  distinct() %>%
  right_join(completed) %>%
  select(-stems)

### Step 8 - Pasting stem completed individual words into their respective happy moments
# We want our processed words to resemble the structure of the original happy moments. So we paste the words together to form happy moments.
completed <- completed %>%
  group_by(id) %>%
  summarise(text = str_c(word, collapse = " ")) %>%
  ungroup()

### Step 9 - Keeping a track of the happy moments with their own ID
hm_data <- hm_data %>%
  mutate(id = row_number()) %>%
  inner_join(completed)

### Step 10 - Exporting the processed text data into a CSV file
# Note the file is being saved to the Desktop of your local computer
write.csv(hm_data, file = "~/Desktop/processed_moments.csv")

# ------------------------------------------------------------------------------------------

# Part 2
#We are now going to perform text analysis on processed_moments.csv

## Loading the necessary libraries
library(tm)
library(tidytext)
library(tidyverse)
library(DT)
library(scales)
library(countrycode)
library(dplyr)
library(ggplot2)
library(NLP)
library(tibble)
library(topicmodels)
library(wordcloud2)
library(gridExtra)
library(ngram)

# Reading "processed_moments.csv" created in Part 1
hm_data <- read_csv("~/Desktop/processed_moments.csv")

# Reading the demographics file
demographics<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv'
demo_data <- read_csv(demographics)

# Happiness takes on various meanings for different women. In this analysis, I aim to explore these unique definitions by examining the words they associate with their happiness. The analysis aims to explore the diverse meanings of happiness for women and uncover the words they connect with this cherished emotion.

#### Section 1: What do women associate happiness with?
all_women <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))

datatable(all_women)

# Creating a bag of words using the text data
bag_of_words_all_women <-  all_women %>%
  unnest_tokens(word, text)

word_count_all_women <- bag_of_words_all_women %>%
  count(word, sort = TRUE)

##### Overview of Words that Women Associate Happiness With
# making a word cloud
wordcloud2(word_count_all_women, size = 0.6, rotateRatio = 0)

# creating bigrams using the text data
hm_bigrams_all_women <- all_women %>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts_all_women <- hm_bigrams_all_women %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)

top_n_bigrams <- 10

top_bigrams_all_women <- bigram_counts_all_women %>%
  head(top_n_bigrams)

#### Top 10 Pairs of Words Women Associate Happiness With
ggplot(top_bigrams_all_women, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() + 
  ggtitle("Top 10 Pairs of Words Women Associate Happiness With") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

top_words_all_women <- word_count_all_women%>%
  head(top_n_bigrams)

#### Top 10 Words of Words Women Associate Happiness With
ggplot(top_words_all_women, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Words Tied to Women's Happiness") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

corpus_all_women <- Corpus(VectorSource(all_women$text))
dtm_all_women <- DocumentTermMatrix(corpus_all_women,
                                    control = list(
                                      wordLengths=c(2, Inf),
                                      bounds = list(global = c(1,Inf)),
                                      removeNumbers = TRUE,
                                      weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_all_women <- LDA(dtm_all_women, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_all_women <- as.data.frame(terms(lda_model_all_women, k = 100))


##### Top 10 Topics Women Associate Happiness With
# Top 10 topics (with 10 terms each)  women associate happiness with
head(lda_terms_all_women, 10)

# plotting these topics into a bar chart
lda_topics <- topics(lda_model_all_women, k = 1)
all_women$topic <- as.factor(lda_topics)
ggplot(data = all_women) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Family", "2. Food", "3. Children", "4. Job", 
                                 "5. Education", "6. Reading", 
                                 "7. Shopping", "8. Nature", "9. Celebration", "10. Entertainment")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Section 2: Is there a difference between the sources of happiness between Unmarried and married women?
# Note that single, widowed and divorced women are considered unmarried for the purposes of the analysis.

# making a datatable that consists of unmarried women. It is formed by combining hm_data with demo_data and adding the necessary filters.
unmarried_women <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))%>%
  filter(marital %in% c("single","divorced","widowed"))
# making a datatable that consists of married women. It is formed by combining hm_data with demo_data and adding the necessary filters.
married_women <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))%>%
  filter(marital %in% c("married"))

# Creating a bag of words using the text data
bag_of_words_unmarried_women <-  unmarried_women %>%
  unnest_tokens(word, text)
bag_of_words_married_women <-  married_women %>%
  unnest_tokens(word, text)

word_count_unmarried_women <- bag_of_words_unmarried_women %>%
  count(word, sort = TRUE)
word_count_married_women <- bag_of_words_married_women %>%
  count(word, sort = TRUE)

##### Overview of Words that Unmarried Women Associate Happiness With
wordcloud2(word_count_unmarried_women, size = 0.6, rotateRatio = 0)

##### Overview of Words that Married Women Associate Happiness With
wordcloud2(word_count_married_women, size = 0.6, rotateRatio = 0)

# creating bigrams using the text data
hm_bigrams_unmarried_women <- unmarried_women%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
hm_bigrams_married_women <- married_women%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts_unmarried_women <- hm_bigrams_unmarried_women %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)
bigram_counts_married_women <- hm_bigrams_married_women %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)

top_n_bigrams <- 10

top_bigrams_unmarried_women <- bigram_counts_unmarried_women %>%
  head(top_n_bigrams)
top_bigrams_married_women <- bigram_counts_married_women %>%
  head(top_n_bigrams)


#### Top 10 Pairs of Words Unmarried Women Associate Happiness With
ggplot(top_bigrams_unmarried_women, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Pairs of Words Unmarried Women Associate Happiness With") +theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Pairs of Words Married Women Associate Happiness With
ggplot(top_bigrams_married_women, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Pairs of Words Married Women Associate Happiness With") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

top_words_unmarried_women <- word_count_unmarried_women %>%
  head(top_n_bigrams)
top_words_married_women <- word_count_married_women %>%
  head(top_n_bigrams)

#### Top 10 Words Unmarried Women Associate Happiness With
ggplot(top_words_unmarried_women, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() + 
  ggtitle("Top 10 Words Tied to Unmarried Women's Happiness") + 
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Words Married Women Associate Happiness With
ggplot(top_words_married_women, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Words Tied to Married Women's Happiness") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


corpus_unmarried_women <- Corpus(VectorSource(unmarried_women$text))
dtm_unmarried_women <- DocumentTermMatrix(corpus_unmarried_women,
                                          control = list(
                                            wordLengths=c(2, Inf),
                                            bounds = list(global = c(1,Inf)),
                                            removeNumbers = TRUE,
                                            weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_unmarried_women <- LDA(dtm_unmarried_women, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_unmarried_women <- as.data.frame(terms(lda_model_unmarried_women, k = 100))

corpus_married_women <- Corpus(VectorSource(married_women$text))
dtm_married_women <- DocumentTermMatrix(corpus_married_women,
                                        control = list(
                                          wordLengths=c(2, Inf),
                                          bounds = list(global = c(1,Inf)),
                                          removeNumbers = TRUE,
                                          weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_married_women <- LDA(dtm_married_women, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_married_women <- as.data.frame(terms(lda_model_married_women, k = 100))


#### Top 10 Topics Unmarried Women Associate Happiness With
head(lda_terms_unmarried_women, 10)
# Ordered Bar plot of topics
lda_topics_unmarried_women <- topics(lda_model_unmarried_women, k = 1)
unmarried_women$topic <- as.factor(lda_topics_unmarried_women)
ggplot(data = unmarried_women) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Family", "2. Nature", "3. Education", "4. Pets", "5. Shopping", "6. Job", "7. Celebration", "8. Going Out", "9. Food", "10. Entertainment")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Unmarried Women's Happiness") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Topics Married Women Associate Happiness With
head(lda_terms_married_women, 10)
lda_topics_married_women <- topics(lda_model_married_women, k = 1)
married_women$topic <- as.factor(lda_topics_married_women)
ggplot(data = married_women) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Children", "2. Shopping", "3. Reading", "4. Food",  "5. Family", "6. Going Out",  "7. Celebration", "8. Pets", "9. Job", "10. Education")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Married Women's Happiness") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Word Proportion Scatterplot for Unmarried and Married Women
plot_data <- bag_of_words_all_women %>%
  mutate(married = if_else(marital == "married", 'married', 'unmarried'))%>%
  count(married, word) %>%
  group_by(married) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(married, proportion)
ggplot(plot_data, 
       aes_string(x = colnames(plot_data)[2], y = colnames(plot_data)[3]),
       color = abs(colnames(plot_data)[3] - colnames(plot_data)[2])) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 1, width = 0.3, height = 0.3) +
  labs(title="Happy Moments for Unmarried v/s Married Women")+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +theme(plot.title = element_text(hjust = 0.5))

### Section 3: Is there a difference between the sources of happiness between women in developed regions and those in undeveloped regions?
# Note the definitions of 'developed' and 'undeveloped' regions are as per the United Nations website. 
# For this analysis, I'll be using countrycode library to identify the continents each of female respondent belongs to. Following this, based on their continent's economic status as per United Nations, female respondents are going to be split into developed and undeveloped categories.


# checking all unique country codes in the data set
unique(all_women$country)
# manually mapping out the country codes to their names
country_mapping <- data.frame(
  country_code = c(
    "USA", "DNK", "IND", "KWT", "FIN", "VEN", "CAN", "IRL", "GBR", "JAM", "ESP",
    "MEX", "ARM", "NGA", "PHL", "GRC", "LTU", "BGR", "TUR", "DZA", "IDN", "ZAF",
    "AUT", "LKA", "PAK", "NZL", "SRB", "ETH", "PRI", "NIC", "NLD", "EGY", "AUS",
    "BEL", "DEU", "ITA", "ASM", "THA", "UGA", "ARE", "JPN", "DOM", "UMI", "CYP",
    "PRT", "MYS", "FRA", "BRB", "CZE", "BHS", "ISL", "SUR", "MKD", "TCA", "TTO",
    "SGP", "BRA", "ZMB", "AFG", "TWN", "VIR", "SLV", "GTM", "NOR", "COL", "MDA"
  ),
  country_name = c(
    "United States", "Denmark", "India", "Kuwait", "Finland", "Venezuela", "Canada", "Ireland", "United Kingdom", "Jamaica", "Spain",
    "Mexico", "Armenia", "Nigeria", "Philippines", "Greece", "Lithuania", "Bulgaria", "Turkey", "Algeria", "Indonesia", "South Africa",
    "Austria", "Sri Lanka", "Pakistan", "New Zealand", "Serbia", "Ethiopia", "Puerto Rico", "Nicaragua", "Netherlands", "Egypt", "Australia",
    "Belgium", "Germany", "Italy", "American Samoa", "Thailand", "Uganda", "United Arab Emirates", "Japan", "Dominican Republic", "United States", "Cyprus",
    "Portugal", "Malaysia", "France", "Barbados", "Czech Republic", "Bahamas", "Iceland", "Suriname", "North Macedonia", "Turks and Caicos Islands", "Trinidad and Tobago",
    "Singapore", "Brazil", "Zambia", "Afghanistan", "Taiwan", "U.S. Virgin Islands", "El Salvador", "Guatemala", "Norway", "Colombia", "Moldova"
  )
)
# adding 2 new columns in all_women datatable.
# country_full = full name of the country of the respondent
all_women$country_full <- country_mapping$country_name[match(all_women$country, country_mapping$country_code)]
# continent = continent of the respondent
all_women$continent <- countrycode(
  sourcevar = all_women$country_full,
  origin = "country.name",
  destination = "region23"
)
unique(all_women$continent)

# splitting the female respondents into developed and undeveloped regions
all_women_developed<- all_women %>%
  filter(gender %in% c("f"))%>%
  filter(continent %in% c("Northern America", "Northern Europe", "Southern Europe", "Eastern Europe", "Western Europe", "Australia and New Zealand", "Eastern Asia"))
all_women_undeveloped<- all_women %>%
  filter(gender %in% c("f"))%>%
  #filter(marital %in% c("married"))%>%
  filter(continent %in% c("Southern Asia", "Western Asia", "South America", "Caribbean", "Central America", "Western Africa", "South-Eastern Asia", "Northern Africa", "Southern Africa", "Eastern Africa", "Polynesia"))


# Creating a bag of words using the text data
bag_of_words_all_women_developed <-  all_women_developed %>%
  unnest_tokens(word, text)
bag_of_words_all_women_undeveloped <-  all_women_undeveloped %>%
  unnest_tokens(word, text)

word_count_all_women_developed <- bag_of_words_all_women_developed %>%
  count(word, sort = TRUE)
word_count_all_women_undeveloped <- bag_of_words_all_women_undeveloped %>%
  count(word, sort = TRUE)


# Creating a bag of words using the text data
bag_of_words_all_women_developed <-  all_women_developed %>%
  unnest_tokens(word, text)
bag_of_words_all_women_undeveloped <-  all_women_undeveloped %>%
  unnest_tokens(word, text)

word_count_all_women_developed <- bag_of_words_all_women_developed %>%
  count(word, sort = TRUE)
word_count_all_women_undeveloped <- bag_of_words_all_women_undeveloped %>%
  count(word, sort = TRUE)

##### Overview of Words that Women in Developed Regions Associate Happiness With
wordcloud2(word_count_all_women_developed, size = 0.6, rotateRatio = 0)

##### Overview of Words that Women in Undeveloped Regions Associate Happiness With
wordcloud2(word_count_all_women_undeveloped, size = 0.6, rotateRatio = 0)

# creating bigrams using the text data
hm_bigrams_all_women_developed <- all_women_developed%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
hm_bigrams_all_women_undeveloped <- all_women_undeveloped%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts_all_women_developed <- hm_bigrams_all_women_developed %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)
bigram_counts_all_women_undeveloped <- hm_bigrams_all_women_undeveloped %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)

top_n_bigrams <- 10

top_bigrams_all_women_developed <- bigram_counts_all_women_developed %>%
  head(top_n_bigrams)
top_bigrams_all_women_undeveloped <- bigram_counts_all_women_undeveloped %>%
  head(top_n_bigrams)

#### Top 10 Pairs of Words Women in Developed Regions Associate Happiness With
ggplot(top_bigrams_all_women_developed, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +
  ggtitle("Top 10 Pairs of Words Women in Developed Regions Find Joy In") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Pairs of Words Women in Undeveloped Regions Associate Happiness With
ggplot(top_bigrams_all_women_undeveloped, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Pairs of Words Women in Undeveloped Regions Find Joy In") +theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

top_words_all_women_developed <- word_count_all_women_developed %>%
  head(top_n_bigrams)
top_words_all_women_undeveloped <- word_count_all_women_undeveloped %>%
  head(top_n_bigrams)

#### Top 10 Words Women in Developed Regions Associate Happiness With
ggplot(top_words_all_women_developed, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +
  ggtitle("Top 10 Words Tied to Women's Happiness in Developed Regions") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Words Women in Undeveloped Regions Associate Happiness With 
ggplot(top_words_all_women_undeveloped, aes(x = reorder(word, n), y = n)) + geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") + coord_flip()+
  ggtitle("Top 10 Words Tied to Women's Happiness in Undeveloped Regions") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


corpus_all_women_developed <- Corpus(VectorSource(all_women_developed$text))
dtm_all_women_developed <- DocumentTermMatrix(corpus_all_women_developed,
                                              control = list(
                                                wordLengths=c(2, Inf),
                                                bounds = list(global = c(1,Inf)),
                                                removeNumbers = TRUE,
                                                weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_all_women_developed <- LDA(dtm_all_women_developed, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_all_women_developed <- as.data.frame(terms(lda_model_all_women_developed, k = 100))

corpus_all_women_undeveloped <- Corpus(VectorSource(all_women_undeveloped$text))
dtm_all_women_undeveloped <- DocumentTermMatrix(corpus_all_women_undeveloped,
                                                control = list(
                                                  wordLengths=c(2, Inf),
                                                  bounds = list(global = c(1,Inf)),
                                                  removeNumbers = TRUE,
                                                  weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_all_women_undeveloped <- LDA(dtm_all_women_undeveloped, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_all_women_undeveloped <- as.data.frame(terms(lda_model_all_women_undeveloped, k = 100))

#### Top 10 Topics Women in Developed Regions Associate Happiness With
head(lda_terms_all_women_developed, 10)
lda_topics_all_women_developed <- topics(lda_model_all_women_developed, k = 1)
all_women_developed$topic <- as.factor(lda_topics_all_women_developed)
ggplot(data = all_women_developed) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Nature", "2. Family", "3. Vacation", "4. Shopping", "5. Pets", "6. Education",  "7. Entertainment", "8. Food", "9. Job", "10. Home")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness in Developed Regions") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


##### Top 10 Topics Women in Undeveloped Regions Associate Happiness With
head(lda_terms_all_women_undeveloped, 10)
lda_topics_all_women_undeveloped <- topics(lda_model_all_women_undeveloped, k = 1)
all_women_undeveloped$topic <- as.factor(lda_topics_all_women_undeveloped)
ggplot(data = all_women_undeveloped) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Family", "2. Shopping", "3. Adventure", "4. Food",  "5. Celebration", "6. Parents",   "7. Education", "8. Entertainment", "9. Reading", "10. Nature")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness in Undeveloped Regions") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


#### Word Proportion Scatterplot for Women in Undeveloped v/s Developed Regions
bag_of_words_all_women_with_continent <-  all_women %>%
  unnest_tokens(word, text)
temp_data <- bag_of_words_all_women_with_continent %>%
  mutate(developed = if_else(continent %in% c("Northern America", "Northern Europe", "Southern Europe", "Eastern Europe", "Western Europe", "Australia and New Zealand", "Eastern Asia"), 'developed', 'undeveloped'))%>%
  count(developed, word) %>%
  group_by(developed) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(developed, proportion)
ggplot(temp_data, 
       aes_string(x = colnames(temp_data)[2], y = colnames(temp_data)[3]),
       color = abs(colnames(temp_data)[3] - colnames(temp_data)[2])) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 1, width = 0.3, height = 0.3) +
  labs(title="Happy Moments for Women in Undeveloped v/s Developed Regions")+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")

#### Section 4: Is there a difference between the sources of happiness of women belonging to different age groups?
# Now, we will split the data into 3 groups: twenties (women of age 20+ but less than 30), thirties (women of age 30 but less than 40) and forties_and_over (women of age 40 but less than 100)

unique(all_women$age)
twenties <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))%>%
  filter(age %in% c(20:29))
thirties <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))%>%
  filter(age %in% c(30:39))
forties_and_over <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("f"))%>%
  filter(age %in% c(40:100))

# creating bigrams using the text data
bag_of_words_twenties <-  twenties %>%
  unnest_tokens(word, text)
bag_of_words_thirties <-  thirties %>%
  unnest_tokens(word, text)
bag_of_words_forties_and_over <-  forties_and_over %>%
  unnest_tokens(word, text)

word_count_twenties <- bag_of_words_twenties %>%
  count(word, sort = TRUE)
word_count_thirties <- bag_of_words_thirties %>%
  count(word, sort = TRUE)
word_count_forties_and_over <- bag_of_words_forties_and_over %>%
  count(word, sort = TRUE)

##### Overview of Words that Women in Their 20s Associate Happiness With
wordcloud2(word_count_twenties, size = 0.6, rotateRatio = 0)

##### Overview of Words that Women in Their 30s Associate Happiness With
wordcloud2(word_count_thirties, size = 0.6, rotateRatio = 0)

##### Overview of Words that Women 40+ Associate Happiness With
wordcloud2(word_count_forties_and_over, size = 0.6, rotateRatio = 0)

hm_bigrams_twenties <- twenties%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
hm_bigrams_thirties <- thirties%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
hm_bigrams_forties_and_over <- forties_and_over%>%
  filter(count != 1) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_counts_twenties <- hm_bigrams_twenties %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)
bigram_counts_thirties <- hm_bigrams_thirties %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)
bigram_counts_forties_and_over <- hm_bigrams_forties_and_over %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)%>%
  filter(word1 != word2)

top_n_bigrams <- 10

top_bigrams_twenties <- bigram_counts_twenties %>%
  head(top_n_bigrams)
top_bigrams_thirties <- bigram_counts_thirties %>%
  head(top_n_bigrams)
top_bigrams_forties_and_over <- bigram_counts_forties_and_over %>%
  head(top_n_bigrams)


#### Top 10 Pairs of Words Women in Their 20s Associate Happiness With
ggplot(top_bigrams_twenties, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Pairs of Words Women in Their 20s Associate Happiness With") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Pairs of Words Women in Their 30s Associate Happiness With
ggplot(top_bigrams_thirties, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Pairs of Words Women in Their 30s Associate Happiness With") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Pairs of Words Women 40+ Associate Happiness With
ggplot(top_bigrams_forties_and_over, aes(x = reorder(paste(word1, word2, sep = " "), n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Pair of Words", y = "Frequency") +
  coord_flip() +
  ggtitle("Top 10 Pairs of Words Women 40+ Associate Happiness With") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

top_words_twenties <- word_count_twenties %>%
  head(top_n_bigrams)
top_words_thirties <- word_count_thirties %>%
  head(top_n_bigrams)
top_words_forties_and_over <- word_count_forties_and_over %>%
  head(top_n_bigrams)

#### Top 10 Words Women in Their 20s Associate Happiness With
ggplot(top_words_twenties, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +  
  ggtitle("Top 10 Words Tied to Women's Happiness in Their 20s") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#### Top 10 Words Women in Their 30s Associate Happiness With
ggplot(top_words_thirties, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +
  ggtitle("Top 10 Words Tied to Women's Happiness in Their 30s") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


#### Top 10 Words Women 40+ Associate Happiness With
ggplot(top_words_forties_and_over, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "dark green") +
  labs(x = "Word", y = "Frequency") +
  coord_flip() +
  ggtitle("Top 10 Words Tied to Women's Happiness in Their 40s and Above") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

corpus_twenties <- Corpus(VectorSource(twenties$text))
dtm_twenties <- DocumentTermMatrix(corpus_twenties,
                                   control = list(
                                     wordLengths=c(2, Inf),
                                     bounds = list(global = c(1,Inf)),
                                     removeNumbers = TRUE,
                                     weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_twenties <- LDA(dtm_twenties, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_twenties <- as.data.frame(terms(lda_model_twenties, k = 100))

corpus_thirties <- Corpus(VectorSource(thirties$text))
dtm_thirties <- DocumentTermMatrix(corpus_thirties,
                                   control = list(
                                     wordLengths=c(2, Inf),
                                     bounds = list(global = c(1,Inf)),
                                     removeNumbers = TRUE,
                                     weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_thirties <- LDA(dtm_thirties, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_thirties <- as.data.frame(terms(lda_model_thirties, k = 100))

corpus_forties_and_over <- Corpus(VectorSource(forties_and_over$text))
dtm_forties_and_over <- DocumentTermMatrix(corpus_forties_and_over,
                                           control = list(
                                             wordLengths=c(2, Inf),
                                             bounds = list(global = c(1,Inf)),
                                             removeNumbers = TRUE,
                                             weighting = weightTf)
)
# Performing Latent Dirichlet Allocation (LDA) topic modeling on the DTM with 10 topics
lda_model_forties_and_over <- LDA(dtm_forties_and_over, 10, method = "Gibbs", control = list(seed=20180910, burnin=1000))
# Extracting the top 100 terms associated with each of the 10 topics
lda_terms_forties_and_over <- as.data.frame(terms(lda_model_forties_and_over, k = 100))

##### Top 10 Topics Women in Their 20s Associate Happiness With
head(lda_terms_twenties, 10)
lda_topics_twenties <- topics(lda_model_twenties, k = 1)
twenties$topic <- as.factor(lda_topics_twenties)
ggplot(data = twenties) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Food", "2. Celebration", "3. Entertainment", "4. Family", "5. Adventure", "6. Vacation", "7. Pets", "8. Education", "9. Reading", "10. Friends")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness in Their 20s") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

##### Top 10 Topics Women in Their 30s Associate Happiness With
head(lda_terms_thirties, 10)
lda_topics_thirties <- topics(lda_model_thirties, k = 1)
thirties$topic <- as.factor(lda_topics_thirties)
ggplot(data = thirties) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Shopping", "2. Friends", "3. Reading", "4. Celebration", "5. Family", "6. Nature", "7. Food", "8. Going out", "9. Entertainment", "10. Children")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness in Their 30s") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))


##### Top 10 Topics Women 40+ Associate Happiness With
head(lda_terms_forties_and_over, 10)
lda_topics_forties_and_over <- topics(lda_model_forties_and_over, k = 1)
forties_and_over$topic <- as.factor(lda_topics_forties_and_over)
ggplot(data = forties_and_over) +
  geom_bar(stat = "count", aes(x = topic, fill = topic)) +
  scale_fill_discrete(name = "Topics",
                      labels = c("1. Pets", "2. Going out", "3. Family", "4. Shopping",  "5. Food", "6. Education",  "7. Friends", "8. Job", "9. Entertainment", "10. Nature")) +
  ylab("Number of Happy Moments") + xlab("Topics")+ggtitle("Top 10 Topics Tied to Women's Happiness in Their 40s and Above") + theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------------------------------------------