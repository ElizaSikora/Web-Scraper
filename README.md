# 📊 Analiza oferty Hebe – Pielęgnacja Koreańska

**Data analizy**: 1 grudnia 2024  
**Typ dokumentu**: HTML (`html_document`)  
**Język analizy**: R / R Markdown  

---

## 📝 Opis projektu

Celem projektu jest przeanalizowanie oferty drogerii **Hebe** w kategorii _pielęgnacja koreańska_. Analiza dotyczy danych produktowych: cen, marek, składów kosmetyków oraz ocen klientów.  

Projekt powstał w środowisku **RStudio** z wykorzystaniem języka R oraz R Markdown.  

---

## 🎯 Cele analizy

- Określenie struktury cenowej produktów koreańskich
- Identyfikacja dominujących typów produktów
- Wskazanie 5 najczęściej występujących marek i porównanie ich cen
- Szczegółowa analiza oferty najpopularniejszej marki
- Przegląd 10 najlepiej ocenianych produktów
- Sprawdzenie zawartości potencjalnych alergenów w składach produktów

---

## 📂 Dane wejściowe

**Plik:** `Hebe.csv`  
**Format:** CSV (Comma Separated Values)  
**Zawiera:**  
- Nazwa produktu  
- Marka  
- Typ produktu  
- Cena  
- Liczba ocen i średnia ocena  
- Skład (Ingredients)

---

## 📦 Wykorzystane pakiety R

```r
library(ggplot2)
library(readr)
library(dplyr)
library(gt)
library(tidyr)
