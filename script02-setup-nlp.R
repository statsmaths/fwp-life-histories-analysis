library(tidyverse)
library(stringi)
library(cleanNLP)
library(hunspell)
library(reticulate)

options(dplyr.summarise.inform = FALSE)
use_python("/usr/local/bin/python3", required=TRUE)
reticulate::py_discover_config()

paste_na <- function(v) paste(sort(unique(v[!is.na(v)])), collapse = ";")
bp <- file.path("..", "fwp-life-histories", "csv")
bt <- file.path("..", "fwp-life-histories", "text")

dir.create("output", FALSE)
dir.create(file.path("output", "csv"), FALSE)
dir.create(file.path("output", "geo"), FALSE)
dir.create(file.path("output", "json"), FALSE)
dir.create(file.path("output", "models"), FALSE)
dir.create(file.path("output", "xml"), FALSE)

# Need to do some cleaning and joining
interviewee_meta <- read_csv(file.path(bp, "interviewee_crosswalk.csv")) %>%
  left_join(read_csv(file.path(bp, "interviewee.csv")), by = "interviewee") %>%
  group_by(id) %>%
  summarize(
    num_people = n(),
    race = paste_na(race),
    gender = paste_na(gender),
    interviewee = paste_na(interviewee)
  )

writer_meta <- read_csv(file.path(bp, "writer_crosswalk.csv")) %>%
  left_join(
    filter(read_csv(file.path(bp, "writer.csv")), !duplicated(writer)),
    by = "writer"
  ) %>%
  select(id, writer, gender_writer = gender, race_writer = race) %>%
  mutate(
    gender_writer = stri_trans_totitle(gender_writer),
    race_writer = stri_trans_totitle(race_writer)
  ) %>%
  mutate(
    gender_writer = if_else(gender_writer == "White", "Female", gender_writer)
  ) %>%
  mutate(
    race_writer = if_else(race_writer == "Female", "White", race_writer)
  ) %>%
  distinct(id, .keep_all = TRUE)

interview <- read_csv(file.path(bp, "interview.csv")) %>%
  anti_join(read_csv(file.path("input", "textanalysis_remove.csv"))) %>%
  inner_join(interviewee_meta, by = "id") %>%
  group_by(race, gender, interviewee, state, city) %>%
  summarize(base_id = first(id), ids = paste_na(id), copies = n()) %>%
  select(id = base_id, everything()) %>%
  left_join(writer_meta, by = "id") %>%
  ungroup() %>%
  arrange(id)

interview$start_line <- 1
interview$start_line[interview$id == "interview_0456_001"] <- 18
interview$start_line[interview$id == "interview_0709_001"] <- 21
interview$start_line[interview$id == "interview_0741_001"] <- 15

interview$text <- ""
id_list <- stri_split(interview$ids, fixed = ";")
for (i in seq_along(id_list))
{
  interview$text[i] <- stri_paste(unlist(lapply(
    id_list[[i]], function(v) {
      lines <- read_lines(file.path(bt, sprintf("%s.txt", v)))
      lines <- lines[seq(interview$start_line[i], length(lines))]
      lines <- lines[seq(which(stri_length(lines) > 45)[1], length(lines))]
      lines
    }
  )), collapse = "\n")
  interview$text[i] <- gsub(
    "text not clear", "", interview$text[i], ignore.case = TRUE
  )
}

write_csv(interview, file.path("output", "csv", "interview_for_text.csv"))

# create 'raw' tokens
tokens <- lapply(
  stri_split_boundaries(interview$text),
  stri_replace_all, "", regex = "[^\\'\\-\\w]"
)
token <- tibble(
  id = rep(interview$id, sapply(tokens, length)),
  token = unlist(tokens)
) %>%
  group_by(id, token) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(id, n)

unique_token <- unique(token$token)
unique_spell <- as.numeric(hunspell_check(unique_token))
token$spell <- unique_spell[match(token$token, unique_token)]

token_letters <- stri_replace_all(token$token, "", regex = "\\W")
unique_token_letters <- unique(token_letters)
unique_spell <- as.numeric(hunspell_check(unique_token_letters))
token$spell_letters <- unique_spell[match(token_letters, unique_token_letters)]

write_csv(token, file.path("output", "csv", "interview_token.csv"))

cnlp_init_spacy()
anno <- cnlp_annotate(select(interview, doc_id = id, text = text))

write_csv(
  anno$token, file.path("output", "csv", "interview_annotation.csv.bz2")
)
write_csv(
  anno$entity, file.path("output", "csv", "interview_annotation_entity.csv")
)
