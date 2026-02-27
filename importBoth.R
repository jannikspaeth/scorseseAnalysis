
library(tidyverse)
library(jsonlite)
library(tidytext)
library(textstem)

# load metadata
metadata <- read_csv2("data/movie_metadata/movie_meta_data_scorsese.csv") %>%
  rename(imdb_id = 1) %>% 
  mutate(imdb_id_numeric = as.numeric(imdb_id))

# import screenplay
# first part of this section (mapping of dialog) with the help of AI
json_pfade <- list.files("data/screenplay_data/rule_based_annotations/", 
                         pattern = "*.json", full.names = TRUE)

all_dialogues <- map_df(json_pfade, function(datei) {
  raw_list <- fromJSON(datei, simplifyVector = FALSE)
    df <- imap(raw_list, function(scene_block, scene_index) {
    
    map_df(scene_block, function(x) {
      if (!is.list(x)) return(NULL)
      
      # getting speaker
      speaker <- x$head_text$subj %||% 
        x$head_text$`speaker/title` %||% 
        x$head_text$speaker$title %||% NA
      
      # getting text
      txt <- x$text
      
      if (!is.null(txt) && txt != "") {
        
        # not only dialog but also the scene descriptions
        text_type <- case_when(
          x$head_type == "speaker/title" ~ "dialogue",
          x$head_type == "heading" ~ "description",
          TRUE ~ "other"
        )
        
        return(tibble(
          scene_id = scene_index,
          character = ifelse(is.na(speaker), NA, as.character(speaker)),
          text_type = text_type,
          raw_text = as.character(txt)
        ))
      }
      return(NULL)
    })
  }) %>% 
    list_rbind() 
  
  if (nrow(df) > 0) {
    return(df %>% mutate(imdb_id = str_extract(basename(datei), "\\d+")))
  }
  return(NULL)
}) %>%
  mutate(imdb_id_numeric = as.numeric(imdb_id))

# preprocessing 
data("stop_words")
namen_filter <- c( "continue", "contd", "vo", "dont", "hes", "its", "im", "thats", "fuckin", "fuck", "motherfucker", "gotta", "theyre", "dont", 
                   "didnt", "youre", "stuff", "yeah", "hey", "hes", "shes", 
                   "gonna", "back", "look", "looked", "talk", "talkin", "walk", 
                   "doin", "happen", "hand", "time", "day", "night", "start", 
                   "stop", "understand", "something", "whats", "door", "home", 
                   "house", "leave", "begin", "stand", "stay", "watch", "people", 
                   "doesnt", "matter", "suppose", "whats", "look", "back", "thing", "wasnt", "isnt", "aint", "whos")

scorsese_master <- all_dialogues %>%
  inner_join(metadata, by = "imdb_id_numeric") %>%
  unnest_tokens(word, raw_text) %>%
  mutate(word = str_replace_all(word, "[’']", "")) %>%
  mutate(word = textstem::lemmatize_words(word)) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% namen_filter) %>%
  filter(nchar(word) > 2)

# check result
scorsese_master %>%
  group_by(title) %>%
  summarise(
    woerter = n(),
    szenen_anzahl = n_distinct(scene_id) 
  ) %>%
  arrange(desc(woerter))


if (!dir.exists("results")) {
  dir.create("results")
}

# export whole dataset as a csv file
write_csv(scorsese_master, "results/scorsese_cleaned_master_both.csv")

# overview with scene number, total words and unique words
inventur <- scorsese_master %>%
  group_by(title, year) %>%
  summarise(
    total_words = n(),
    unique_words = n_distinct(word),
    scene_count = n_distinct(scene_id),
    .groups = 'drop'
  ) %>%
  arrange(year)

# export overview as a csv file
write_csv(inventur, "results/scorsese_film_summary_both.csv")