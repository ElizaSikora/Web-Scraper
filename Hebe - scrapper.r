Sys.setlocale(category = "LC_ALL", locale = "Polish_Poland.UTF8")
library(tidyverse)
library(rvest)
library(httr)
library(stringi)
library(readr)
library(ggplot2)


# Agent użytkownika
my_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"

# Pobieranie linków do produktów
get_product_links <- function(url) {
  strona <- GET(url, user_agent(my_agent))
  hebe <- content(strona)
  
  linki <- hebe %>%
    html_element("ul.product-grid.tiles-container") %>%  
    html_elements("a.product-tile__image") %>%          
    html_attr("href")                                   
  
  linki <- stri_paste("https://www.hebe.pl", linki)
  return(linki)
}

get_page_size <- function(url) {
  strona <- GET(url, user_agent(my_agent))
  hebe <- content(strona)
  
  page_size <- hebe %>%
    html_element("ul.product-grid.tiles-container") %>%
    html_attr("data-size") %>%
    as.numeric()  
  
  return(page_size)
}

# Liczba stron
get_total_pages <- function(url) {
  strona <- GET(url, user_agent(my_agent))
  hebe <- content(strona)
  
  liczba_podstron <- hebe %>%
    html_elements("a.bucket-pagination__link") %>%
    html_text(trim = TRUE) %>%
    str_extract("\\d+$") %>%
    as.numeric() %>%
    max()
  
  return(liczba_podstron)
}

link <- "https://www.hebe.pl/kosmetyki-koreanskie/"
liczba_podstron <- get_total_pages(link)

wszystkie_linki <- c()
for (i in 0:(liczba_podstron - 1)) {
  url <- stri_paste("https://www.hebe.pl/kosmetyki-koreanskie/?start=", i, "&sz=21") 
  page_size <- get_page_size(url)
  url <- stri_paste("https://www.hebe.pl/kosmetyki-koreanskie/?start=", i * page_size, "&sz=", page_size)
  linki_strony <- get_product_links(url)
  wszystkie_linki <- c(wszystkie_linki, linki_strony)
  
  Sys.sleep(2)
}

df_linki <- data.frame(linki = wszystkie_linki, stringsAsFactors = FALSE)

#Zbieranie danych
ceny <- c()
marki <- c()
srednia_ocena <- c()
liczba_ocen <- c()
typy_produktow <- c()

pobierz_dane <- function(link) {
  Sys.sleep(2)  
  strona <- tryCatch(GET(link, user_agent(my_agent)), error = function(e) NULL)
  
  if (!is.null(strona) && strona$status_code == 200) {
    html <- content(strona)
    
    #Cena
    cena_normalna <- html %>%
      html_element("div.price-product__standard span.price-product__amount") %>%
      html_text(trim = TRUE)
    
    if (is.na(cena_normalna) || cena_normalna == "") {
      cena_normalna <- html %>%
        html_element("div.price-omnibus-text__wrapper span.price-omnibus-text__value") %>%
        html_text(trim = TRUE)
    }
    
    if (!is.na(cena_normalna) && grepl("zł$", cena_normalna)) {
      cena_normalna <- str_replace(cena_normalna, " zł", "")  
    }
    
    grosze <- html %>%
      html_element("div.price-product__standard span.price-product__currency > span:nth-child(1)") %>%
      html_text(trim = TRUE)
    
    if (is.na(grosze) || grosze == "") {
      cena <- if (!is.na(cena_normalna) && cena_normalna != "") {
        cena_normalna <- str_replace(cena_normalna, ",", ".")  
        cena_normalna  
      } else {
        NA  
      }
    } else {
      cena <- if (!is.na(cena_normalna) && cena_normalna != "") {
        cena_normalna <- str_replace(cena_normalna, ",", ".")  
        paste0(cena_normalna, ".", grosze)  
      } else {
        NA  
      }
    }
    
    # Marka
    marka <- html %>%
      html_element("p.product-content__brand span") %>%   
      html_text(trim = TRUE)
    
    if (is.na(marka) || marka == "") {
      marka <- html %>%
        html_element("p.product-content__brand a.product-content__link") %>%  # Jeśli brak w <span>, sprawdzamy w <a>
        html_text(trim = TRUE)
    }
    
    marka <- stri_replace_all_regex(marka, "&amp;", "&") %>%  
      stri_unescape_unicode()  
    
    # Średnia
    srednia_ocena_tekst <- html %>%
      html_element("span.reviews-stars__value") %>%
      html_attr("style")
    
    srednia_ocena_liczba <- if (!is.na(srednia_ocena_tekst)) {
      srednia_ocena_czesc <- strsplit(srednia_ocena_tekst, " ")[[1]]
      
      if (length(srednia_ocena_czesc) >= 2) {
        ceiling(substr(srednia_ocena_czesc[2], 1, nchar(srednia_ocena_czesc[2]) - 1) %>%
                  as.numeric() / 20)
      } else {
        NA
      }
    } else {
      NA
    }
    
    # Liczba opinii
    liczba_opinii <- html %>%
      html_element("div.reviews-stars div.reviews-stars__count") %>%
      html_attr("content") %>%
      as.numeric()
    
    if (is.na(liczba_opinii) || liczba_opinii == 0) {
      srednia_ocena_liczba <- NA
    }
    
    
    # Nazwa produktu
    nazwa <- html %>%
      html_element("p.js-product-short-description.product-content__short-description") %>%  
      html_text(trim = TRUE) %>%                                                           
      str_split(",") %>%                                                                   
      .[[1]] %>%                                                                           
      .[1] %>%                                                                             
      str_to_sentence()                                                                    
    
    
    # Składniki
    skladniki <- html %>%
      html_element("div#product-ingredients div.ui-expandable__inner") %>%  
      html_text(trim = TRUE) %>%                                          
      str_split(",\\s*") %>%                                              
      .[[1]]                                                              
    
    
    # Typ produktu
    typ <- html %>%
      html_elements("div.breadcrumb__element") %>%
      .[4] %>%
      html_element("a.breadcrumb__link") %>%
      html_text(trim = TRUE)
    
    # Lista danych
    return(list(cena = cena,
                marka = marka, 
                srednia_ocena = srednia_ocena_liczba,
                nazwa = nazwa,
                skladniki = skladniki,
                liczba_opinii = liczba_opinii, 
                typ_produktu = typ))
  }
  
  return(list(cena = NA, marka = NA, srednia_ocena = NA, liczba_opinii = NA, typ_produktu = NA, skladniki=NA, nazwa=NA))
}

dane_df <- data.frame(
  link = character(),         
  nazwa = character(),        
  cena = numeric(),           
  marka = character(),        
  srednia_ocena = numeric(),  
  liczba_ocen = integer(),    
  typ_produktu = character(), 
  skladniki = character(),    
  stringsAsFactors = FALSE    
)

for (i in seq_along(df_linki$linki)) {
  tryCatch({
    cat(sprintf("Przetwarzanie linku %d z %d: %s\n", i, nrow(df_linki), df_linki$linki[i]))
    link <- df_linki$linki[i]
    dane <- pobierz_dane(link)
    
    # Dodawanie nowego wiersza
    nowy_wiersz <- data.frame(
      link = link,
      nazwa = dane$nazwa,              
      cena = dane$cena,
      marka = dane$marka,
      srednia_ocena = dane$srednia_ocena,
      liczba_ocen = dane$liczba_opinii,
      typ_produktu = dane$typ_produktu,
      skladniki = paste(dane$skladniki, collapse = ", "), 
      stringsAsFactors = FALSE
    )
    dane_df <- rbind(dane_df, nowy_wiersz)
    
  }, error = function(e) {
    cat(sprintf("Błąd podczas przetwarzania linku: %s\n", link))
  })
}

# CSV
write_csv(dane_df, "Hebe.csv")
cat("Plik Hebe.csv został pomyślnie utworzony i zapisany.")
