################################################################################
#                       Sentiment Analysis with R                              #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(janeaustenr)
library(tidytext)
library(textdata)


# text example -----------------------------------------------------------------
text <- c(
  "I love R",
  "R is magic",
  "It is hard and boring at the beginning",
  "but once you have started",
  "you just can't stop using it"
)

text_as_df <- tibble(
  line = 1:5,
  text = text
  )

unnest_tokens(
  tbl = text_as_df,
  output = word,
  input = text
  )

# jane austen books ------------------------------------------------------------
jane_tidy <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number()) %>% 
  mutate(chapter = str_detect(text, "CHAPTER") %>% cumsum()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# remove stop words ------------------------------------------------------------
jane_tidy_words <- jane_tidy %>% 
  anti_join(stop_words)

# plot amount of words by chapter ----------------------------------------------
jane_tidy_words %>% 
  filter(book == "Emma") %>% 
  group_by(chapter) %>% 
  summarise(n = n()) %>%
  ggplot(aes(chapter,n)) + 
  geom_line() + 
  geom_smooth(se=FALSE)

# access to bing sentiment lexicon ----------------------------------------------
bing_lexicon <- get_sentiments("bing")

# evaluate sentiment for all words ---------------------------------------------
jane_austen_sentiment <- jane_tidy_words %>%
  inner_join(bing_lexicon) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# access to afinn sentiment lexicon --------------------------------------------
afinn_sentiments <- get_sentiments("afinn")

# evaluate sentiment for all words ---------------------------------------------
jane_tidy_words %>% 
  inner_join(afinn_sentiments) %>%
  group_by(book, chapter) %>% 
  summarise(avg_score = mean(score)) %>%
  ggplot(aes(chapter, avg_score, group=book, colour = book)) + 
  geom_line() + 
  geom_smooth(se = FALSE)









