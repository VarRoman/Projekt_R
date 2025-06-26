library(dplyr)
library(moments)

# Wczytanie danych
setwd("C:\\Users\\bezsh\\Desktop\\For_studying\\Polska_uniwersitecia\\4_semester\\R_Data_Mining\\Projekt_R")
data <- read.csv("HR Analytics\\train.csv", stringsAsFactors = TRUE, sep=';')

# Funkcja obliczająca statystyki opisowe dla wektora liczbowego
summary_stats <- function(x) {
  data.frame(
    min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE)
  )
}

# 1. Statystyki opisowe dla zmiennych previous_year_rating i length_of_service
# Grupowanie według płci
stats_by_gender <- data %>%
  group_by(gender) %>%
  summarise(
    previous_year_rating_stats = list(summary_stats(previous_year_rating)),
    length_of_service_stats = list(summary_stats(length_of_service))
  )

cat("Statystyki opisowe według płci:\n")
print(stats_by_gender)

# Grupowanie według awansu
stats_by_promotion <- data %>%
  group_by(is_promoted) %>%
  summarise(
    previous_year_rating_stats = list(summary_stats(previous_year_rating)),
    length_of_service_stats = list(summary_stats(length_of_service))
  )
cat("\nStatystyki opisowe według awansu:\n")
print(stats_by_promotion)