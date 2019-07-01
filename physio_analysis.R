################################################################################
#         Analysis of Physiological Correlates of Emotions  with R             #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(zoo)
library(dygraphs)
library(mgcv)

# simulate data ----------------------------------------------------------------
# time <- seq(0, 5*60000, by = 500)
# Heartrate <- arima.sim(
#   model = list(ar = 0.9) , 
#   n = length(time), 
#   mean = 8, 
#   sd = 5
#   ) %>% 
#   as.numeric()
# data_HR_sim <- data.frame(time,Heartrate)
# 
# readr::write_csv(data_HR_sim, "data_HR_sim.csv")
# 
# 
# time <- seq(0, 5*60000, by = 1500)
# Breathing.Rate <- arima.sim(
#   model = list(ar = 0.9),
#   n = length(time), 
#   mean = 4, 
#   sd = 5
#   ) %>% 
#   as.numeric()
# data_BR_sim <- data.frame(time,Breathing.Rate)
# 
# readr::write_csv(data_BR_sim, "data_BR_sim.csv")
# 
# 
# time <- seq(0, 5*60000, by = 1500)
# Skin.Temperature <- arima.sim(
#   model = list(ar = 0.9), 
#   n = length(time), 
#   mean = 3.7, 
#   sd = 0.5
#   ) %>% 
#   as.numeric()
# data_ST_sim <- data.frame(time,Skin.Temperature)
# 
# readr::write_csv(data_ST_sim, "data_ST_sim.csv")
# 
# 
# time <- seq(0, 5*60000, by = 500)
# EDA_100Microsimens <- arima.sim(
#   model = list(ar = 0.9), 
#   n = length(time), 
#   mean = 100,
#   sd = 120
#   ) %>% 
#   as.numeric()
# data_EDA_sim <- data.frame(time,EDA_100Microsimens)
# 
# readr::write_csv(data_EDA_sim, "data_EDA_sim.csv")

# upload data ------------------------------------------------------------------
data_HR_sim <- readr::read_csv("https://raw.githubusercontent.com/damien-dupre/data/master/data_HR_sim.csv")
data_BR_sim <- readr::read_csv("https://raw.githubusercontent.com/damien-dupre/data/master/data_BR_sim.csv")
data_ST_sim <- readr::read_csv("https://raw.githubusercontent.com/damien-dupre/data/master/data_ST_sim.csv")
data_EDA_sim <- readr::read_csv("https://raw.githubusercontent.com/damien-dupre/data/master/data_EDA_sim.csv")

# extract features -------------------------------------------------------------
data_HR <- data_HR_sim %>%
  dplyr::select(time, Heartrate) %>%
  dplyr::mutate(physio = "HR") %>%
  dplyr::rename(value = Heartrate) %>%
  dplyr::mutate(time_date = as.POSIXct(
    as.numeric(as.character(time)) / 1000,
    origin = "1970-01-01",
    tz = "Europe/London"
  ))

data_BR <- data_BR_sim %>%
  dplyr::select(time, Breathing.Rate) %>%
  dplyr::mutate(physio = "BR") %>%
  dplyr::rename(value = Breathing.Rate) %>%
  dplyr::mutate(time_date = as.POSIXct(
    as.numeric(as.character(time)) / 1000,
    origin = "1970-01-01",
    tz = "Europe/London"
  ))

data_ST <- data_ST_sim %>%
  dplyr::select(time, Skin.Temperature) %>%
  dplyr::mutate(physio = "ST") %>%
  dplyr::rename(value = Skin.Temperature) %>%
  dplyr::mutate(time_date = as.POSIXct(
    as.numeric(as.character(time)) / 1000,
    origin = "1970-01-01",
    tz = "Europe/London"
  ))

data_EDA <- data_EDA_sim %>%
  dplyr::select(time, EDA_100Microsimens) %>%
  dplyr::mutate(physio = "EDA") %>%
  dplyr::rename(value = EDA_100Microsimens) %>%
  dplyr::mutate(time_date = as.POSIXct(
    as.numeric(as.character(time)) / 1000,
    origin = "1970-01-01",
    tz = "Europe/London"
  ))

data_zoo <- loess(value ~ time, data = data_EDA, span = 0.35)
data_EDA$SCL <- data_zoo$fitted
data_EDA$SCR <- data_zoo$residuals

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
  data_HR_zoo,
  data_BR_zoo,
  data_ST_zoo,
  data_SCL_zoo
  )

data_physio <- na.approx(data_physio)
colnames(data_physio) <- c("HR", "BR", "ST", "SCL")

custom_palette <-
  c("red", "blue", "orange", "green", "purple")

dygraph(data_physio) %>%
  dyRangeSelector() %>%
  dyOptions(colors = custom_palette) %>%
  dyLegend(width = 500)

# extract trend ----------------------------------------------------------------
data_physio_tidy <- data_physio %>% 
  zoo::fortify.zoo() %>% 
  tidyr::gather(measure, value, -Index) %>% 
  dplyr::mutate(Index = as.numeric(Index))

gam_physio <- mgcv::gam(
  value ~ s(Index), 
  data = data_physio_tidy,  
  method = "REML", 
  correlation=corAR1()
  )

plot(gam_physio)



