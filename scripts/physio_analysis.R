################################################################################
#         Analysis of Physiological Correlates of Emotions  with R             #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(zoo)
library(dygraphs)
library(mgcv)
library(here)

# upload data ------------------------------------------------------------------
data_HR_sim <- here("data/data_HR_sim.csv") %>% read_csv()
data_BR_sim <- here("data/data_BR_sim.csv") %>% read_csv()
data_ST_sim <- here("data/data_ST_sim.csv") %>% read_csv()
data_EDA_sim <- here("data/data_EDA_sim.csv") %>% read_csv()

# extract features -------------------------------------------------------------
data_HR <- data_HR_sim %>%
  rename(value = Heartrate) %>%
  mutate(
    measure = "HR",
    time_date = as.POSIXct(time/1000, origin = "1970-01-01", tz = "Europe/London")
  )

data_BR <- data_BR_sim %>%
  rename(value = Breathing.Rate) %>%
  mutate(
    measure = "BR",
    time_date = as.POSIXct(time/1000, origin = "1970-01-01", tz = "Europe/London")
  )

data_ST <- data_ST_sim %>%
  rename(value = Skin.Temperature) %>%
  mutate(
    measure = "ST",
    time_date = as.POSIXct(time/1000, origin = "1970-01-01", tz = "Europe/London")
  )

data_EDA <- data_EDA_sim %>%
  rename(value = EDA_100Microsimens) %>%
  mutate(
    measure = "EDA",
    time_date = as.POSIXct(time/1000, origin = "1970-01-01", tz = "Europe/London")
  )

data_zoo <- loess(value ~ time, data = data_EDA, span = 0.35)
data_EDA$SCL <- data_zoo$fitted
data_EDA$SCR <- data_zoo$residuals

data_EDA %>% 
  ggplot(aes(x = time_date)) +
  geom_line(aes(y = value)) +
  geom_line(aes(y = SCL), color = "green", size = 2) +
  geom_line(aes(y = SCR), color = "lightgreen")

# center scale -----------------------------------------------------------------
data_HR_zoo <- zoo(data_HR$value, order.by = data_HR$time_date) %>% 
  scale(center = TRUE, scale = TRUE)
data_BR_zoo <- zoo(data_BR$value, order.by = data_BR$time_date) %>% 
  scale(center = TRUE, scale = TRUE)
data_ST_zoo <- zoo(data_ST$value, order.by = data_ST$time_date) %>% 
  scale(center = TRUE, scale = TRUE)
data_SCL_zoo <- zoo(data_EDA$SCL, order.by = data_EDA$time_date) %>% 
  scale(center = TRUE, scale = TRUE)

# merge and fill ---------------------------------------------------------------
data_physio <- merge.zoo(
  HR = data_HR_zoo, 
  BR = data_BR_zoo, 
  ST = data_ST_zoo, 
  SCL = data_SCL_zoo
) %>%
  na.approx()

dygraph(data_physio) %>%
  dyRangeSelector() %>%
  dyOptions(colors = c("red", "blue", "orange", "green", "purple")) %>%
  dyLegend(width = 500)

# extract trend ----------------------------------------------------------------
data_physio_tidy <- data_physio %>% 
  fortify.zoo() %>% 
  pivot_longer(-Index, names_to = "measure", values_to = "value") %>% 
  mutate(Index = as.numeric(Index))

gam_physio <- gam(
  formula = value ~ s(Index), 
  data = data_physio_tidy,  
  method = "REML", 
  correlation = corAR1()
)

summary(gam_physio)



