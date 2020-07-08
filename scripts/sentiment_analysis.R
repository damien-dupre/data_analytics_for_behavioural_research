################################################################################
#                       Sentiment Analysis with R                              #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(janeaustenr)

# text example -----------------------------------------------------------------
c(
  "I love R",
  "R is magic",
  "It is hard and boring at the beginning",
  "but once you have started",
  "you just can't stop using it"
  ) %>% 
  as_tibble_col(column_name = "text") %>% 
  rownames_to_column("line") %>%
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words, by = "word") %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(sentiment) %>% 
  ggplot() +
  aes(x = sentiment, y = n/sum(n)) +
  geom_col(fill = c("darkblue", "darkred"), width = 0.5) +
  scale_x_discrete("") +
  scale_y_continuous(
    "Propotion in text", 
    limits = c(0, 1), 
    labels = scales::percent
  ) +
  theme_light() +
  theme(text = element_text(size = 20))

# jane austen books ------------------------------------------------------------
jane_tidy <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number()) %>% 
  mutate(chapter = str_detect(text, "CHAPTER") %>% cumsum()) %>%
  ungroup() %>%
  filter(chapter != 0) %>% 
  unnest_tokens(word, text)

# remove stop words ------------------------------------------------------------
jane_tidy_words <- jane_tidy %>% 
  anti_join(stop_words, by = "word")

# plot amount of words by chapter ----------------------------------------------
jane_tidy_words %>% 
  filter(book == "Emma") %>% 
  count(chapter) %>% 
  ggplot() + 
  aes(chapter, n) +
  geom_line() + 
  geom_smooth(method = "loess", se = FALSE, color = "orange", span = 0.5) +
  scale_x_continuous("Chapter", breaks = seq(0, 60, by = 5)) +
  scale_y_continuous("Amount of words\n(stop words excluded)") +
  ggtitle("Length by chapters in 'Emma'") +
  theme_light() +
  theme(text = element_text(size = 14, family = "mono"))

# access to bing sentiment lexicon ----------------------------------------------
bing_lexicon <- get_sentiments("bing")

# evaluate sentiment for all words ---------------------------------------------
jane_austen_sentiment <- jane_tidy_words %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(book, chapter, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))

ggplot(jane_austen_sentiment, aes(chapter, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) + 
  scale_x_continuous("Chapter") +
  scale_y_continuous("Sentiment Proportion", labels = scales::percent) +
  facet_wrap(~ book, ncol = 2, scales = "free_x") +
  theme_light() +
  theme(
    text = element_text(size = 14, family = "mono"),
    strip.text = element_text(color = "black", face = "bold.italic")
  )

# access to afinn sentiment lexicon --------------------------------------------
afinn_sentiments <- get_sentiments("afinn")

# evaluate sentiment for all words ---------------------------------------------
jane_tidy_words %>% 
  inner_join(afinn_sentiments, by = "word") %>%
  group_by(book, chapter) %>% 
  summarise(avg_score = mean(value)) %>%
  ggplot(aes(chapter, avg_score, group = book, colour = book)) + 
  geom_line() + 
  geom_smooth(se = FALSE)

# identify sentiment differences -----------------------------------------------
lm_model <- jane_austen_sentiment %>% 
  mutate(book = factor(book) %>% fct_relevel("Emma")) %>% 
  lm(formula = sentiment ~ book, data = .)

summary(lm_model)









