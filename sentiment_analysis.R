
# libraries --------------------------------------------------------------------
library(tidyverse)
library(janeaustenr)
library(tidytext)

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

# access to nrc sentiment lexicon ----------------------------------------------
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# evaluate sentiment for all words ---------------------------------------------
jane_tidy_joy <- jane_tidy_words %>% 
  group_by(book,chapter) %>%
  mutate(words = n()) %>%
  inner_join(nrc_joy) %>% 
  group_by(book,chapter,words) %>%
  summarise(joy_words = n()) %>% 
  mutate(joy_rate = joy_words/words) 

jane_tidy_joy %>% 
  ggplot(aes(x=chapter,y=joy_rate,group=book,colour=book)) + 
  geom_line() +
  geom_smooth(se = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5))

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









