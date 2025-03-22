#Biblioteki
library(ggplot2)
library(readr)
library(dplyr)
library(gt)
library(tidyr)

# Dane
dane_df <- read_csv("Hebe.csv")

### Normalizacja składu produktów ###

czysc_skladniki <- function(skladniki) {
  skladniki <- gsub(":.+", "", skladniki)  
  skladniki <- gsub("\n", ", ", skladniki)
  skladniki <- tolower(skladniki)  
  skladniki <- tools::toTitleCase(skladniki)  
  skladniki <- gsub("\\s*,\\s*", ", ", skladniki)
  skladniki <- gsub(",\\s*,", ",", skladniki)
  skladniki <- trimws(skladniki)
  
  return(skladniki)
}

dane_df <- dane_df %>%
  mutate(skladniki = czysc_skladniki(skladniki))  


# Lista potencjalnych alergenów
alergeny <- c("fragrance", "paraben", "alcohol", "sodium lauryl sulfate", "formaldehyde", "propylene glycol")

dane_df <- dane_df %>%
  mutate(
    alergeny = sapply(skladniki, function(x) {
  
      alergeny_found <- sapply(alergeny, function(y) ifelse(grepl(y, tolower(x)), y, NA))
      alergeny_found <- na.omit(alergeny_found)  
      if(length(alergeny_found) > 0) {
        return(paste(alergeny_found, collapse = ", "))  
      } else {
        return(NA)  
      }
    })
  )

# Nadpisanie danych - kolumna alergeny
write.csv(dane_df, "Hebe.csv", row.names = FALSE)


### Rozkład cen dla produktów z pielęgnacji koreańskiej ###

srednia_cena <- mean(dane_df$cena, na.rm = TRUE)

# Wykres
ggplot(dane_df, aes(x = cena)) +
  geom_density(fill = "#69b3a2", alpha = 0.5) +  
  geom_vline(xintercept = srednia_cena, color = "blue", linetype = "dashed", linewidth = 1) +  
  annotate("text", x = srednia_cena, y = 0.005, label = paste0("Średnia: PLN ", round(srednia_cena, 2)),
           color = "blue", vjust = -1, hjust = -0.1, size = 4) +  
  scale_x_continuous(
    labels = scales::dollar_format(prefix = "PLN "), 
    breaks = seq(0, max(dane_df$cena, na.rm = TRUE), by = 50),  
    expand = c(0, 0)
  ) +
  labs(
    title = "Rozkład gęstości cen produktów",
    x = "Cena (PLN)",
    y = "Gęstość",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1)  
  )


### Procentowy udział typów produktów w kategorii pielęgnacja koreańska ###

typy_produktow <- dane_df %>%
  count(typ_produktu) %>%
  mutate(procent = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%  
  head(20)  

# Wykres słupkowy dla 20 produktów
ggplot(typy_produktow, aes(x = reorder(typ_produktu, -procent), y = procent, fill = typ_produktu)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            vjust = -0.5, size = 4, color = "black") +
  labs(
    title = "Procentowy udział 20 najliczniejszych typów produktów",
    x = "Typ produktu",
    y = "Udział procentowy",
    fill = "Typ produktu"
  ) +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),
    legend.position = "none"
  )


### Rozkład cenowy dla 5 najpopularniejszych marek ###

top_marki <- dane_df %>%
  group_by(marka) %>%
  summarise(suma_ocen = sum(liczba_ocen, na.rm = TRUE)) %>%
  arrange(desc(suma_ocen)) %>%
  slice(1:5) %>%  
  pull(marka)  

dane_top_marki <- dane_df %>%
  filter(marka %in% top_marki) %>%
  mutate(marka = factor(marka, levels = top_marki))  

# Wykres pudełkowy
ggplot(dane_top_marki, aes(x = marka, y = cena, fill = marka)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +  
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow") +  
  labs(
    title = "Porównanie rozkładu cen produktów dla top 5 marek",
    x = "Marka",
    y = "Cena (PLN)",
    fill = "Marka"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),  
    legend.position = "none"  
  )


### Oferta najbardziej ocenianej marki (Missha) ###

marka_max_ocen <- dane_df %>%
  group_by(marka) %>%
  summarise(suma_ocen = sum(liczba_ocen, na.rm = TRUE)) %>%
  arrange(desc(suma_ocen)) %>%
  slice(1) %>%
  pull(marka)  

marka_data <- dane_df %>%
  filter(marka == marka_max_ocen) %>%
  count(typ_produktu)  

# Wykresu rozkładu typów produktów 
ggplot(marka_data, aes(x = reorder(typ_produktu, -n), y = n, fill = typ_produktu)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, color = "black") +
  labs(
    title = paste("Typy produktów oferowanych przez markę:", marka_max_ocen),
    x = "Typ produktu",
    y = "Liczba produktów",
    fill = "Typ produktu"
  ) +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),
    legend.position = "none"  
  )


### Tabela 10 najlepiej ocenianych produktów ###

top_produkty <- dane_df %>%
  arrange(desc(liczba_ocen), desc(srednia_ocena)) %>%  
  select(nazwa, marka, srednia_ocena, liczba_ocen, typ_produktu) %>%  
  slice(1:10)  

# Tabela
top_produkty %>%
  gt() %>%
  tab_header(
    title = "Top 10 produktów według liczby i średniej oceny"
  ) %>%
  fmt_number(
    columns = c(srednia_ocena), decimals = 2  
  ) %>%
  cols_label(
    nazwa = "Nazwa Produktu",
    marka = "Marka",
    srednia_ocena = "Średnia Ocena",
    liczba_ocen = "Liczba Ocen",
    typ_produktu = "Typ Produktu"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")  
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_column_labels(columns = everything())  
  )


### Analiza produktów pod względem zawartości potencjalnych alergenów ###

## Wykres ogólny alergenów ##

alergeny_podzial <- dane_df %>%
  mutate(czy_zawiera_alergeny = ifelse(is.na(alergeny), "Bez alergenów", "Z alergenami")) %>%
  count(czy_zawiera_alergeny) %>%
  mutate(procent = n / sum(n) * 100)

# Wykres 1
ggplot(alergeny_podzial, aes(x = "", y = procent, fill = czy_zawiera_alergeny)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Podział produktów według zawartości alergenów",
    fill = "Zawartość alergenów"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


## Wykres szczegółowy ##

alergeny_szczegoly <- dane_df %>%
  filter(!is.na(alergeny)) %>%
  separate_rows(alergeny, sep = ", ") %>%  
  count(alergeny) %>%
  mutate(procent = n / sum(n) * 100)

# Wykres 2
ggplot(alergeny_szczegoly, aes(x = "", y = procent, fill = alergeny)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(alergeny, ": ", round(procent, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Udział poszczególnych alergenów w produktach",
    fill = "Alergeny"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

