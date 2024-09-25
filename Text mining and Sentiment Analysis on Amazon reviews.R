#------------------------------------------------------------------

#      ASSIGNMENT -  Text Mining and sentiment analysis 
#                 Scraping an Amazon product 

#                Barteselli Roberta (1070063)
#                   Cerro Valentina (1089888)
#                   Kosmach Yelyena (1060187)

#------------------------------------------------------------------

#Product: GoPro Hero10 Black Bundle 
#ID: B09JDKPTSV

library(tidyverse)
library(rvest)
library(tidytext)
library(ggplot2)

#---------------------------------
# SCRAPING THE PRODUCT INFORMATION
#---------------------------------

# We define the url 
url = "https://www.amazon.co.uk/GoPro-HERO10-Black-Bundle-Rechargeable/dp/B09JDKPTSV/ref=sr_1_1_sspa?crid=1F8IUWIVP9I30&keywords=gopro%2Bhero%2B10&qid=1685997078&sprefix=go%2Bpro%2Bhero10%2B%2Caps%2C97&sr=8-1-spons&sp_csd=d2lkZ2V0TmFtZT1zcF9hdGY&th=1"
html = read_html(url)

product_title = html %>%
  html_elements("[class='a-size-large product-title-word-break']") %>%
  html_text2()
print(product_title)

product_details = html %>%
  html_elements("[class='a-normal a-spacing-micro']") %>%
  html_text2()
print(product_details)

product_info = html %>%
  html_elements("[class='a-section a-spacing-medium a-spacing-top-small']") %>%
  html_text2()
print(product_info)

availability = html %>%
  html_elements("[class='a-size-medium a-color-success']") %>%
  html_text2()
print(availability)

customer_ratings <- html %>%
  html_elements("[class='a-size-base']") %>%
  html_text2() %>%
  .[grepl(" ratings", .)]
print(customer_ratings[1])


fastest_delivery <- html %>%
  html_node("#mir-layout-DELIVERY_BLOCK-slot-PRIMARY_DELIVERY_MESSAGE_LARGE") %>%
  html_node("span.a-text-bold") %>%
  html_text()
print(fastest_delivery)

#---------------------------------
#     SCRAPING THE REVIEWS
#---------------------------------
amazon_reviews = function(id, page) {
  
  url = paste0("https://www.amazon.co.uk/product-reviews/",
               id, "/?pageNumber=", page)
  html <- read_html(url)
  
  # Review title (UK and not-UK)
  title = html %>%
    html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text2()
  
  title = html %>%
    html_elements("[class='a-size-large.product-title-word-break']") %>%
    html_text2()
  
  title = title %>%
    c(html %>%
        html_elements("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
        html_text2())
  
  # Review text (the same for UK and not-UK)
  text = html %>%
    html_elements("[class='a-size-base review-text review-text-content']") %>%
    html_text2()
  
  # Review stars (UK and not-UK)
  star = html %>%
    html_elements("[data-hook='review-star-rating']") %>%
    html_text2()
  
  star = star %>%
    c(html %>%
        html_elements("[data-hook='cmps-review-star-rating']") %>%
        html_text2())
  
  # Return a tibble
  tibble(title, text, star, page = page) %>%
    return()
}

id = "B09JDKPTSV"
page = 1:50

#We create the dataframe 
data_gopro = map_df(page, ~ amazon_reviews(id = "B09JDKPTSV", page = .))

data_gopro$doc_id = 1:nrow(data_gopro)

#We save the dataframe 
save(data_gopro, file = "data_gopro.rda")

#We load the dataframe 
load("data_gopro.rda")
data_gopro

#---------------------------------
# Data Cleaning 
#---------------------------------
library(cld2)
#Detect language of title and text 
data_gopro$title_lang = detect_language(data_gopro$title)
data_gopro$text_lang = detect_language(data_gopro$text)

table(Text = data_gopro$text_lang, Title = data_gopro$title_lang, useNA = "always")

#We filter for the comments that are only in English 
data_gopro = data_gopro %>%
  filter(text_lang == "en")
data_gopro

#SCORES - We extract the scores from the stars 
data_gopro = data_gopro %>%
  mutate(score = as.numeric(substring(star, 1, 1)))
data_gopro

summary(data_gopro$score)

#We want to see the frequencies
data_gopro %>%
  count(score) %>%
  mutate(p = round(n/sum(n), 2))


#---------------------------------
# Data Visualization
#---------------------------------
data_gopro %>%
  ggplot(aes(x = score)) + 
  geom_bar(aes(y = after_stat(count)), fill = "#ff8080", width = 0.7, alpha = 0.8) +
  labs(title = "Amazon reviews' stars", 
       subtitle = "GoPro HERO10 Black Bundle",
       x = "Stars", y = "Number of comments") + 
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 13, face = "bold"), 
        plot.subtitle = element_text(color = "grey", size = 8),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Save the plot as an PNG file
ggsave("Amazon reviews' stars.png")

data_gopro = data_gopro %>%
  mutate(star_sent = ifelse(score >= 4, "positive", "negative"))
data_gopro

sentiment_counts <- data_gopro %>%
  count(star_sent) %>%
  mutate(p = n/sum(n))
sentiment_counts

rm(sentiment_counts)


#Compare the length of positive and negative reviews 
data_gopro$nchar = str_length(data_gopro$text)
data_gopro

library(ggplot2)
data_gopro %>% 
  ggplot(aes(x = star_sent, y = nchar, fill = star_sent)) +
  geom_boxplot() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#ff3333", "#55cc55")) +
  labs(x = "Star sentiment", y = "Number of characters")

ggsave("Positive-Negative boxplot.png")


# FURTHER DATA CLEANING 
tidy_data_gopro = data_gopro %>% 
  unnest_tokens(word, text) %>%  # tokenization
  anti_join(stop_words) %>%      # remove stop words 
  filter(!str_detect(word, "[[:digit:]]")) %>%   #remove digits  
  mutate(word = case_when( word %in% c("issue", "issues") ~ "issue", TRUE ~ word)) %>% 
  filter(word != "hero") 
print(tidy_data_gopro)

#---------------------------------
#     SENTIMENT ANALYSIS 
#       Tidy approach 
#---------------------------------

# Implement Bing Lexicon Approach
bing = get_sentiments("bing")

# Identify words in the tidy data that are present in the bing lexicon and perform SA
data_gopro_pol = tidy_data_gopro %>% 
  inner_join(bing, by ="word") %>%
  count(doc_id, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)   # overall sentiment score
View(data_gopro_pol)


# Create Histogram to visualise the sentiment distribution
data_gopro_pol %>%
  ggplot(aes(sentiment)) +
  geom_histogram(binwidth = 2,
                 aes(fill = after_stat(count)),
                 col = "black",
                 size = 0.1) +
  xlab("Polarity") +
  ylab("Frequency") +
  ggtitle("Sentiment distribution - tidy approach") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), 
        axis.title = element_text(face = "bold")) +
  scale_fill_gradient(low = "#E6E6FA", high = "#AFAFD3")

ggsave("Sentiment distribution - tidy approach.png")

# Summarise the positive and negative sentiments 
summary(data_gopro_pol$sentiment)


data_gopro_word_count = tidy_data_gopro %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()


# Word contribution to each sentiment category
dev.new(width = 3000, height = 1500, unit = "px")
library(ggplot2)
data_gopro_word_count %>% 
  group_by(sentiment) %>% 
  slice_max(n, n=10, with_ties = F) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word,n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ sentiment, scales = "free_y") +
  xlab(NULL) + 
  ylab(bquote(bold("Contribution to sentiment"))) + 
  coord_flip()+
  scale_fill_manual(values = c("positive" = "#9ACD32", "negative" = "#FF6347"))+
  theme_minimal()+
  theme(strip.text = element_text(face = "bold"))

data_gopro_word_count
ggsave("Contribution to sentiment.png")

# Word cloud based on frequency of positive and negative words
library(wordcloud2)
library(wordcloud)
tidy_data_gopro %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  column_to_rownames(var = "word") %>% 
  comparison.cloud(colors = c("#FF6347", "#9ACD32"), max.words = 100)
dev.off()


#---------------------------------
#     SENTIMENT ANALYSIS 
#      Udpipe approach 
#---------------------------------
library(udpipe)
# Implement SA using Udpipe approach 
bing = get_sentiments("bing")
data_udpipe = udpipe(data_gopro, "english-gum")

bing_dict = get_sentiments("bing") %>% 
  mutate(sentiment = ifelse(sentiment == "negative", -1, 1)) %>% 
  rename(term = word, polarity = sentiment)

sent_udpipe = txt_sentiment(x=data_udpipe, 
                            term = "token", 
                            polarity_terms = bing_dict, 
                            polarity_negators = c("not", "neither", "no", "without"),
                            polarity_amplifiers = c("really", "very", "definitely", "super"),
                            polarity_deamplifiers = c("barely", "hardly"),
                            amplifier_weight = 0,
                            n_before = 3, 
                            n_after = 3,
                            constrain=F)

data_gopro$udpipe = sent_udpipe$overall$sentiment_polarity


hist(data_gopro$udpipe, col = "lightgreen", border = "grey",  
     xlab = "Polarity",
     ylab = "Frequency",
     main = "Sentiment distribution - udpipe")

summary(data_gopro$udpipe)

# Comparison between the polarity distribution of the two approaches using histograms 
par(mfrow = c(1, 2))

hist(scale(data_gopro_pol$sentiment), 
     main = "Polarity distribution - tidy",
     col = "lightblue",
     border = "grey",
     xlab = "Scaled Polarity",
     ylab = "Frequency",
     xlim = c(-3, 3),
     ylim = c(0, 20))

hist(scale(data_gopro$udpipe), 
     main = "Polarity distribution - udpipe",
     col = "lightgreen",
     border = "grey",
     xlab = "Scaled Polarity",
     ylab = "Frequency",
     xlim = c(-3, 3),
     ylim = c(0, 20))
ggsave("Comparison tidy-udpipe approches.png")


# Calculate summary statistics
summary(scale(data_gopro_pol$sentiment))
summary(scale(data_gopro$udpipe))

#---------------------------------
#     TOPIC MODELLING  
#---------------------------------

library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)

# Create a document-term matrix
dtm <- tidy_data_gopro %>%  
  select(doc_id, word) %>% 
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, word, n)
dtm 

dim(dtm)

# Set the seed for reproducibility
set.seed(1234)

# Create training dataset IDs
train <- sample(rownames(dtm), nrow(dtm)*0.75)
head(train)

# Subset the document-term matrix (dtm) for the training dataset
dtm_train <- dtm[rownames(dtm) %in% train, ]
dim(dtm_train) 

# Subset the document-term matrix (dtm) for the test dataset
dtm_test <- dtm[!rownames(dtm) %in% train, ]
dim(dtm_test)  

# for each topic and corresponding perplexity value
topic <- data.frame(k=c(5, 10, 15, 20, 25, 30, 35), perplexity = NA)
for (i in 1:nrow(topic)){
  print(topic$k[i])
  model_lda = LDA(dtm_train, method = "Gibbs", k=topic$k[i], control = list(alpha=0.01, seed = 1234))
  topic$perplexity[i] = perplexity(model_lda, dtm_train)
}

topic

# We plot the perplexity 
library(ggplot2)
ggplot(topic, aes(x = k, y = perplexity)) +
  geom_line(col="steelblue") +
  geom_point(size=1, col = ifelse(topic$k == 15, "red", "steelblue")) +
  geom_vline(xintercept = 15, linetype = "dotted", color = "red") +
  labs(x = "Number of Topics (k)", y = "Perplexity") +
  ggtitle("Optimal Number of Topics: Perplexity Scores") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("Optimal Number of Topics: Perplexity Scores.png")

# The we run the model and we analyse it 
model_lda = LDA(dtm, method = "Gibbs", k=15, control = list(alpha = 0.01, seed = 1234))
model_lda


# we can observe the per topic word probabilities 
terms(model_lda, 5)

tidy_topics <- tidy(model_lda, matrix="beta")
tidy_topics


# we can plot the results 
top_terms = tidy_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n=7, with_ties = F) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
top_terms


dev.new(width = 3000, height = 1500, unit = "px")

top_terms %>% 
  mutate(term=reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=as.factor(topic)))+
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free")+
  scale_y_reordered()+
  labs(x="Beta", y="Top Terms")+
  ggtitle("Top Terms for Each Topic")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))
ggsave("Top Terms for Each Topic.png")
dev.off()

# then we can see the word assignment to each topic
word_ass = augment(model_lda, dtm)
word_ass

# we can also see the topic proportion in documents
tidy_gamma = tidy(model_lda, matrix="gamma")
tidy_gamma

doc_class <- tidy_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup() %>%
  arrange(as.numeric(document))

doc_class

doc_class %>% 
  select(document) %>% 
  mutate(dup = duplicated(document)) %>% 
  filter(dup=="TRUE")


doc_class <- doc_class %>%
  anti_join(doc_class %>%
              select(document) %>%
              mutate(dup = duplicated(document)) %>%
              filter(dup == "TRUE"))
doc_class

doc_ass <- data_gopro %>%
  mutate(id = as.character(doc_id)) %>%
  rename(document = "id") %>%
  inner_join(doc_class)
doc_ass


# we can count the topics
topic_counts <- doc_ass %>% 
  count(topic)
topic_counts


# Plot the topic counts
ggplot(topic_counts, aes(x = topic, y = n)) +
  geom_bar(stat = "identity", fill = "#81D8D0") +
  labs(x = "Topic", y = "Document Count") +
  ggtitle("Topic Distribution in Documents") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("Topic distribution in Documents.png")

# Interactive visualization
library(LDAvis)
dtm <- dtm[slam::row_sums(dtm) > 0, ]
phi <- as.matrix(posterior(model_lda)$terms)
theta <- as.matrix(posterior(model_lda)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json <- createJSON(phi = phi, theta = theta, vocab = vocab, doc.length = doc.length,
                   term.frequency = term.freq)
serVis(json)
