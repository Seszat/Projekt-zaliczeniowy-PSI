#' ---
#' title: "Analiza składników różnych przepisów"
#' author: "Karolina Rybak, Hanna Jermakowicz, Anna Martyniuk"
#' date:   "07.06.26"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show
#'     number_sections: false
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)

# Ustawienie ziarna losowości 
set.seed(1234)

#' # Wymagane pakiety
# Wymagane pakiety ----
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(tm)


file_path <- "recipes.csv"




#' # Standardowa chmura słów (częstość)
# Standardowa chmura słów (częstość) ----

data_table <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "WINDOWS-1252")

# Indeksowanie wierszy w celu zachowania integralności przepisów po rozbiciu na pojedyncze słowa
data_table <- data_table %>% mutate(recipe_id = row_number())

# Oczyszczanie tekstu
cleaned_data <- data_table %>%
  mutate(
    ingredients_clean = tolower(ingredients),
    #   \u00ae = ®   \u2122 = ™   \u00a9 = ©
    ingredients_clean = gsub("\\[|\\]|\u00ae|\u2122|\u00a9", "", ingredients_clean)
  ) %>%
  separate_rows(ingredients_clean, sep = ",\\s*") %>%
  mutate(

    # Usuwanie całych fraz pieprzu
    ingredients_clean = gsub("\\b(black pepper|white pepper|ground pepper|cayenne pepper)\\b", "", ingredients_clean),

    # Usuwanie soli, wody i zbędnych przymiotników
    ingredients_clean = gsub("\\b(fresh|ground|chopped|sliced|shredded|dried|large|small|cloves|all-purpose|extra-virgin|all purpose|extra virgin|salt|water)\\b", "", ingredients_clean),

    # Czyszczenie spacji
    ingredients_clean = trimws(gsub("\\s+", " ", ingredients_clean))
  ) %>%

  # Usuwanie pustych i za krótkich słow oraz samego słowa "pepper"
  filter(nchar(ingredients_clean) > 2, ingredients_clean != "pepper")


# Formatowanie do chmury za pomocą twardej spacji
cleaned_data <- cleaned_data %>%
  mutate(word_display = gsub("\\s+", "\u00A0", ingredients_clean))


# Zliczanie globalnej częstości
freq_df <- cleaned_data %>%
  count(word_display, sort = TRUE) %>%
  rename(word = word_display, freq = n)


# Rysowanie pierwszej chmury (Częstość)
set.seed(1234)  # powtórzone tuż przed rysowaniem -> identyczny układ chmury przy każdym uruchomieniu
wordcloud(words = freq_df$word, freq = freq_df$freq,
          min.freq = 5, max.words = 40, colors = brewer.pal(8, "Dark2"), random.order = FALSE)

cat("\n--- TOP 15: ZWYKŁA CZĘSTOŚĆ SŁÓW ---\n")
print(head(freq_df, 15))


# WNIOSKI: Chmura pokazuje absolutny fundament naszej kuchni.
# Rządzą tu uniwersalne bazy smakowe (garlic, olive oil, onions)
# oraz podstawy wypieków (flour, eggs, sugar).
# Czyszczenie danych pozwoliło pozbyć się zapychaczy (pepper, salt, water),
# a wielowyrazowe składniki (jak olive oil) nie rozpadły się na pojedyncze słowa.




#' # Analiza TF-IDF
# Analiza TF-IDF ----

# Obliczanie wagi TF-IDF na poziomie każdego przepisu i składnika
tfidf_data <- cleaned_data %>%
  count(recipe_id, word_display) %>%
  bind_tf_idf(word_display, recipe_id, n)

# Wyciąganie maksymalnej wagi
tdm_tfidf_df <- tfidf_data %>%
  group_by(word_display) %>%
  summarise(freq = max(tf_idf)) %>%
  arrange(desc(freq)) %>%
  rename(word = word_display)

# Rysowanie drugiej chmury (TF-IDF)
set.seed(1234)  # ponowne ustawienie ziarna przed drugą chmurą
wordcloud(words = tdm_tfidf_df$word,
          freq = tdm_tfidf_df$freq,
          min.freq = 0.0001,
          max.words = 40,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)

# Wyświetlenie 15 najbardziej unikalnych składników
cat("\n--- TOP 15: WAGI TF-IDF (Maksymalna unikalność) ---\n")
print(head(tdm_tfidf_df, 15))


# W przeciwieństwie do tradycyjnego podejścia z zajęć (opartego na klasycznym pakiecie
# 'tm', macierzach rzadkich i funkcji rowSums), w analizie zastosowałyśmy
# nowoczesną architekturę TidyText oraz agregację za pomocą funkcji max().
#
# RÓŻNICA W MECHANIZMIE DZIAŁANIA:
# 1. Podejście tradycyjne (rowSums): Sumuje wagi TF-IDF w skali całej bazy danych.
#    Jest to rozwiązanie optymalne do klasyfikacji całych dokumentów (np. segregowania
#    artykułów), gdzie globalna powtarzalność słów ma znaczenie.
#    W przypadku przepisów kulinarnych podejście to ma jednak wadę - promuje składniki
#    umiarkowanie unikalne, ale masowo występujące w bazie (np. masło, jajka, cebula),
#    co zniekształca ostateczną chmurę słów.
#
# 2. Podejście nowoczesne (max): W ramach Eksploracyjnej Analizy Danych (EDA) celem
#    było wychwycenie z bazy najbardziej unikalnych i charakterystycznych
#    cech poszczególnych przepisów. Funkcja max() odrzuca globalny szum statystyczny
#    i sprawdza, jaką maksymalną wartość unikalności dany składnik osiągnął w obrębie
#    jednej, konkretnej receptury.
#
# REZULTAT:
# Metoda ta pozwala wyłonić najbardziej wyraziste składniki,
# które najlepiej definiują charakter danej potrawy lub kuchni świata. Daje to znacznie
# bardziej logiczną, czystszą i łatwiejszą do zinterpretowania chmurę słów.




#' # Asocjacje
# Asocjacje ----

# Budowanie macierzy TDM dla asocjacji ----

asoc_corpus <- cleaned_data %>%
  group_by(recipe_id) %>%
  summarise(text = paste(ingredients_clean, collapse = " ")) %>%
  pull(text) %>%
  VectorSource() %>%
  VCorpus()

# Tworzenie oficjalnego obiektu TDM
tdm_assoc <- TermDocumentMatrix(asoc_corpus)




# Generowanie czterech wykresów lizakowych ----

# Definiowanie listy słów kluczowych
slowa <- c("vanilla", "soy", "barbecue", "parmesan")
cor_limit <- 0.08

# Pętla, która automatycznie zrobi analizę dla każdego słowa
for (target_word in slowa) {

  # Szukanie korelacji w nowo utworzonej macierzy tdm_assoc
  associations <- findAssocs(tdm_assoc, target_word, corlimit = cor_limit)
  assoc_vector <- associations[[target_word]]

  if (length(assoc_vector) > 0) {
    assoc_sorted <- sort(assoc_vector, decreasing = TRUE)
    assoc_df <- data.frame(
      word  = as.character(names(assoc_sorted)),
      score = as.numeric(assoc_sorted)
    )

    # Wybranie maks. 25 najwyższych asocjacji dla lepszej czytelności
    assoc_df <- head(assoc_df, 25)

    # Ustawienie kolejności słów na osi Y (rosnąco wg score -> najsilniejsze na górze).
    assoc_df$word <- factor(assoc_df$word,
                            levels = assoc_df$word[order(assoc_df$score)])

    # Rysowanie wykresu
    print(
      ggplot(assoc_df, aes(x = score, y = word, color = score)) +
        geom_segment(aes(xend = 0, yend = word), linewidth = 1.2) +
        geom_point(size = 4) +
        geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
        scale_color_gradient(low = "#a6bddb", high = "#08306b") +
        scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1),
                           expand = expansion(mult = c(0, 0.2))) +
        theme_minimal(base_size = 12) +
        labs(
          title    = paste0("Asocjacje z terminem: '", target_word, "'"),
          subtitle = paste0("Próg r \u2265 ", cor_limit),
          x        = "Współczynnik korelacji Pearsona",
          y        = "Słowo",
          color    = "Natężenie\nskojarzenia"
        ) +
        theme(
          plot.title   = element_text(face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "right"
        )
    )
  }
}

# Analiza asocjacji (korelacja Pearsona r >= 0.08) wyłoniła
# z surowego tekstu cztery odrębne światy kulturowo-kulinarne:
#
# 1. Świat cukierniczy ("vanilla"): Silne powiązanie z frazą "extract"
#    (r=0.78) oraz dalsze pozycje "sugar", "yolks", "powdered", "butter" jednoznacznie
#    definiują bazę pod wypieki, desery i kremy.
#
# 2. Świat azjatycki ("soy"): Dominacja stałego związku "soy sauce" (r=0.71).
#    Algorytm odtworzył podstawy orientalnej poprzez pokazanie silnych
#    połączeń z "sesame" (r=0.54) czy "ginger" (r=0.43).
#
# 3. Świat grillowy ("barbecue"): Najwyższa korelacja ze słowem "back" (r=0.38),
#    odzwierciedlająca popularne amerykańskie danie "baby back ribs" (żeberka).
#    Widoczne cechy stylu barbecue poprzez frazy "rub", "ribs", "buffalo", "maple" oraz "sprite".
#    Po poprawnym wczytaniu kodowania nazwa handlowa "KC Masterpiece" pojawia się już
#    jako czyste "masterpiece" (zniknął wcześniejszy artefakt kodowania ® w postaci "<ae>").
#
# 4. Świat włoski ("parmesan"): Najsilniejsze powiązania z formą podania:
#    "grated" (r=0.61) i "cheese" (r=0.55). Pozostałe słowa to esencja dań
#    śródziemnomorskich: mozzarella, pasta, basil, lasagna oraz sos marinara.
#
# WNIOSEK: Mimo rozproszenia bazy, algorytm odtworzył rzeczywiste
# połączenia składników i stałe kolokacje językowe (np. vanilla extract, soy sauce).




#' # Informacje o środowisku (reproducibility)
# Informacje o środowisku (reproducibility) ----

# Zapis wersji R i pakietów ułatwia odtworzenie analizy na innym komputerze.
sessionInfo()
