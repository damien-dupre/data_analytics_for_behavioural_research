
# libraries --------------------------------------------------------------------
library(tidyverse)
library(mgcv)

# preprocess files -------------------------------------------------------------
# automatic_recognition_data <- "C:/Users/dupred/OneDrive/Damien/academic_communications/2018/Com EmotionAware 2018/FE_RAW_DATA/Affectiva_Test" %>%
#   fs::dir_ls(regexp = "\\.csv$") %>%
#   purrr::map_dfr(readr::read_csv, .id = "source") %>%
#   dplyr::mutate(source = tools::file_path_sans_ext(base::basename(source)))
# 
# readr::write_csv(automatic_recognition_data, "automatic_recognition_data.csv")

# data wrangling ---------------------------------------------------------------
automatic_recognition_tidy <- 
  "https://raw.githubusercontent.com/damien-dupre/data/master/automatic_recognition_data.csv" %>% 
  readr::read_csv() %>% 
  dplyr::select(source, TimeStamp, joy, fear, disgust, sadness, anger, surprise) %>% 
  tidyr::gather(emotion_expressed, value, -source, -TimeStamp) %>% 
  tidyr::separate(source, c("emotion_triggered", "gender"), sep = "[.]") %>% 
  dplyr::mutate(type = case_when(
    emotion_expressed == emotion_triggered ~ "taget",
    TRUE ~ "non-target"
  )) %>% 
  dplyr::mutate(emotion_triggered = as.factor(emotion_triggered)) %>% 
  dplyr::mutate(type = as.factor(type))

# plot emotion expressed by gender and emotion triggered -----------------------
automatic_recognition_tidy %>% 
  ggplot(aes(TimeStamp,value, color = emotion_expressed)) + 
  geom_line() +
  facet_grid(gender~emotion_triggered)


# evaluate emotion expression evolution ----------------------------------------
test_gam <- mgcv::gam(
  value ~ s(TimeStamp, by = type),
  correlation = corAR1(),
  data = automatic_recognition_tidy,
  method = "REML"
  )
plot(test_gam)

summary(test_gam)

vis.gam(test_gam,theta=-55,color="heat")

# better but require a better computing power ----------------------------------
test_gamm <- mgcv::gamm(
  value ~ s(TimeStamp, by = type),
  correlation=corAR1(),
  data = automatic_recognition_tidy,
  method = "REML",
  random=list(gender=~1)
)

plot(test_gamm$gam)

summary(test_gamm$gam)
