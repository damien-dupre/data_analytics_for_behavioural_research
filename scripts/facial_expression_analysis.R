################################################################################
#            Analysis of Facial Expressions of Emotions  with R                #
################################################################################
# install.packages(c("tidyverse", "here", "janitor"))

# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)

# data wrangling ---------------------------------------------------------------
automatic_recognition_tidy <- "data/automatic_recognition_data.csv" %>% 
  here() %>% 
  read_csv() %>% 
  clean_names() %>% 
  select(source, time_stamp, joy, fear, disgust, sadness, anger, surprise) %>% 
  pivot_longer(
    cols = joy:surprise, 
    names_to = "emotion_recognized", 
    values_to = "value"
  ) %>% 
  separate(source, c("emotion_expressed", "gender"), sep = "[.]") %>% 
  mutate(type = case_when(
    emotion_recognized == emotion_expressed ~ "target",
    TRUE ~ "non-target"
  ))

# plot emotion expressed by gender and emotion triggered -----------------------
automatic_recognition_tidy %>% 
  ggplot(aes(time_stamp, value, color = emotion_recognized)) + 
  geom_line(size = 2) +
  scale_x_continuous("Time (s)") +
  scale_y_continuous("Recognition Probability") +
  scale_color_discrete("Emotion Recognized") +
  facet_grid(gender ~ emotion_expressed) + 
  theme(legend.position = "bottom")

automatic_recognition_tidy %>% 
  filter(type == "target") %>% 
  ggplot(aes(time_stamp, value)) + 
  geom_line(size = 2) +
  scale_x_continuous("Time (s)") +
  scale_y_continuous("Recognition Probability (target only)") +
  facet_grid(gender ~ emotion_expressed)

# evaluate emotion expression evolution ----------------------------------------
avg_type <- automatic_recognition_tidy %>% 
  group_by(type) %>% 
  summarise(avg_value = mean(value) %>% round(2))

m_target <- avg_type$avg_value[avg_type$type == "target"]
m_nontarget <- avg_type$avg_value[avg_type$type == "non-target"]

lm_model <- lm(value ~ type, data = automatic_recognition_tidy)

summary(lm_model)
