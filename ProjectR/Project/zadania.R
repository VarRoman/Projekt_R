library(readr)
library(dplyr)
library(ggplot2)
#zadanie 2
# Wczytaj dane (dostosuj ścieżkę pliku, jeśli trzeba)
df1 <- read_delim("C:\\Users\\mysyu\\Desktop\\HR Analytics\\HR Analytics\\train.csv", delim = ";")
# Zliczenie pracowników w każdym departamencie
pracownicy_departament <- df1 %>%
  count(department, name = "liczba_pracowników") %>%
  arrange(department)

# Zliczenie pracowników w każdym regionie
pracownicy_region <- df1 %>%
  count(region, name = "liczba_pracowników") %>%
  arrange(region)

# Wyświetlenie wyników
print(pracownicy_departament)
print(pracownicy_region)
#

#zadanie3

# Tabela liczności awansu wg płci
tabela_awansow <- table(df1$gender, df1$is_promoted)

# Wyświetlenie tabeli z czytelnymi nazwami
colnames(tabela_awansow) <- c("Nie awansowany", "Awansowany")
rownames(tabela_awansow) <- c("Kobiety", "Mężczyźni")

# Wynik
print(tabela_awansow)
#

#zadanie4

# Tabela liczności awansów wg kanału rekrutacji
tabela_kanal_awansu <- table(df1$recruitment_channel, df1$is_promoted)

# Ustawienie czytelnych nazw kolumn
colnames(tabela_kanal_awansu) <- c("Nie awansowany", "Awansowany")

# Wyświetlenie tabeli
print(tabela_kanal_awansu)
#

#zadanie5
# Wykres 1: no_of_trainings vs gender
ggplot(df1, aes(x = gender, y = no_of_trainings, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Liczba szkoleń wg płci",
    x = "Płeć",
    y = "Liczba szkoleń",
    fill = "Płeć"
  ) +
  theme_minimal()

# Wykres 2: length_of_service vs gender
ggplot(df1, aes(x = gender, y = length_of_service, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Staż pracy wg płci",
    x = "Płeć",
    y = "Lata pracy",
    fill = "Płeć"
  ) +
  theme_minimal()
#

#zadanie5
ggplot(df1, aes(x = education, y = avg_training_score, fill = education)) +
  geom_boxplot() +
  labs(
    title = "Wyniki średnich szkoleń wg poziomu wykształcenia",
    x = "Poziom wykształcenia",
    y = "Średni wynik ze szkoleń",
    fill = "Wykształcenie"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # obrócenie etykiet x dla czytelności
  )
#

#zadanie6
ggplot(df1, aes(x = previous_year_rating, y = avg_training_score, color = as.factor(is_promoted))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Zależność między oceną z poprzedniego roku a wynikiem szkoleń",
    x = "Ocena z poprzedniego roku",
    y = "Średni wynik szkolenia",
    color = "Awans"
  ) +
  theme_minimal()
#

#zadanie7
ggplot(df1, aes(x = no_of_trainings, y = age, color = gender)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Liczba szkoleń a wiek pracownika z podziałem na płeć",
    x = "Liczba szkoleń",
    y = "Wiek",
    color = "Płeć"
  ) +
  theme_minimal()
#

#zadanie8

# Funkcja do obliczania przedziałów ufności
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

# Użycie funkcji z poprawką
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
#

#zadanie 9
t.test(avg_training_score ~ is_promoted, data = df1)
#

#zadanie 10
var.test(previous_year_rating ~ as.factor(is_promoted), data = df1)
#