library(readr)
library(dplyr)
library(ggplot2)
#zadanie 2

df1 <- read_delim("C:\\Users\\bezsh\\Desktop\\For_studying\\Polska_uniwersitecia\\4_semester\\Python_R\\project\\ProjectR\\HR Analytics\\train.csv", delim = ";")

pracownicy_departament <- df1 %>%
  count(department, name = "liczba_pracownikow") %>%
  arrange(department)

pracownicy_region <- df1 %>%
  count(region, name = "liczba_pracownikow") %>%
  arrange(region)

print(pracownicy_departament)
print(pracownicy_region)
#zadanie3
tabela_awansow <- table(df1$gender, df1$is_promoted)
colnames(tabela_awansow) <- c("Nie awansowany", "Awansowany")
rownames(tabela_awansow) <- c("Kobiety", "Mezczyzni")
print(tabela_awansow)

#zadanie4
tabela_kanal_awansu <- table(df1$recruitment_channel, df1$is_promoted)
colnames(tabela_kanal_awansu) <- c("Nie awansowany", "Awansowany")
print(tabela_kanal_awansu)

#zadanie5
ggplot(df1, aes(x = gender, y = no_of_trainings, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Liczba szkolen wg plci",
    x = "Plec",
    y = "Liczba szkolen",
    fill = "Plec"
  ) +
  theme_minimal()

ggplot(df1, aes(x = gender, y = length_of_service, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Staz pracy wg plci",
    x = "Plec",
    y = "Lata pracy",
    fill = "Plec"
  ) +
  theme_minimal()
ggplot(df1, aes(x = education, y = avg_training_score, fill = education)) +
  geom_boxplot() +
  labs(
    title = "Wyniki srednich szkolen wg poziomu wyksztalcenia",
    x = "Poziom wyksztalcenia",
    y = "Sredni wynik ze szkolen",
    fill = "Wyksztalcenie"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # obrócenie etykiet x dla czytelności
  )
#zadanie6
ggplot(df1, aes(x = previous_year_rating, y = avg_training_score, color = as.factor(is_promoted))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Zaleznosc miedzy ocena z poprzedniego roku a wynikiem szkolen",
    x = "Ocena z poprzedniego roku",
    y = "Sredni wynik szkolenia",
    color = "Awans"
  ) +
  theme_minimal()
#

#zadanie7
ggplot(df1, aes(x = no_of_trainings, y = age, color = gender)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Liczba szkolen a wiek pracownika z podzialem na plec",
    x = "Liczba szkolen",
    y = "Wiek",
    color = "Plec"
  ) +
  theme_minimal()
#

#zadanie8

conf_int_stats <- function(x) {
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  s2 <- var(x, na.rm = TRUE)
  
  error_margin <- qt(0.975, df = n - 1) * sqrt(s2 / n)
  ci_mean <- c(m - error_margin, m + error_margin)
  
  chi_low <- (n - 1) * s2 / qchisq(0.975, df = n - 1)
  chi_high <- (n - 1) * s2 / qchisq(0.025, df = n - 1)
  ci_var <- c(chi_low, chi_high)
  
  list(mean_ci = ci_mean, var_ci = ci_var)
}


ci_results <- df1 %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    rating_ci = list(conf_int_stats(previous_year_rating)),
    training_score_ci = list(conf_int_stats(avg_training_score))
  )

print(ci_results)
ci_results$rating_ci[[1]]$mean_ci
ci_results$rating_ci[[1]]$var_ci

#zadanie 9
t.test(avg_training_score ~ is_promoted, data = df1)
#

#zadanie 10
var.test(previous_year_rating ~ as.factor(is_promoted), data = df1)
#
