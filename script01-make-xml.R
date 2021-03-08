suppressPackageStartupMessages(library(tidyverse))
library(stringi)
library(xml2)

options(dplyr.summarise.inform = FALSE)

bp <- file.path("..", "fwp-life-histories", "csv")
interview <- read_csv(
  file.path(bp, "interview.csv"), col_types = cols()
)
interviewee <- read_csv(
  file.path(bp, "interviewee.csv"), col_types = cols()
)
interviewee_crosswalk <- read_csv(
  file.path(bp, "interviewee_crosswalk.csv"), col_types = cols()
)
occupation <- read_csv(
  file.path(bp, "occupation.csv"), col_types = cols()
)
reviser <- read_csv(
  file.path(bp, "reviser.csv"), col_types = cols()
)
reviser_crosswalk <- read_csv(
  file.path(bp, "reviser_crosswalk.csv"), col_types = cols()
)
writer <- read_csv(file.path(bp, "writer.csv"), col_types = cols()
)
writer_crosswalk <- read_csv(
  file.path(bp, "writer_crosswalk.csv"), col_types = cols()
)

template <- '<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet type="text/xsl" href="../lifehistory.xsl" ?>
<history>
  <title>%s</title>
  <meta>
    %s
  </meta>
  <text>
    %s
  </text>
</history>'
temp_item <- '<item><key>%s</key><val>%s</val></item>'

occs <- occupation %>%
  mutate(occupation = stri_replace_all(occupation, "&amp;", fixed = "&")) %>%
  group_by(id) %>%
  summarize(occupation = paste(occupation, collapse = "; "))

writ <- writer %>%
  mutate(
    race = if_else(race %in% c("black", "chinese", "white"), race, "NA")) %>%
  mutate(gender = if_else(gender %in% c("female", "male"), gender, "NA")) %>%
  mutate(writer_info = sprintf("%s (%s; %s)", writer, race, gender)) %>%
  mutate(
    writer_info = stri_replace_all(writer_info, "", fixed = " (NA; NA)")) %>%
  mutate(writer_info = stri_replace_all(writer_info, ")", fixed = "; NA)")) %>%
  mutate(writer_info = stri_replace_all(writer_info, "(", fixed = "(NA; ")) %>%
  mutate(writer_info = if_else(
    stri_length(writer_info) < 50, writer_info, NA_character_)) %>%
  filter(!is.na(writer_info)) %>%
  left_join(writer_crosswalk, by = "writer") %>%
  group_by(id) %>%
  summarize(writer_info = paste(writer_info, collapse = "; "))

revi <- reviser %>%
  mutate(
    race = if_else(race %in% c("black", "chinese", "white"), race, "NA")) %>%
  mutate(gender = if_else(gender %in% c("female", "male"), gender, "NA")) %>%
  mutate(rev_info = sprintf("%s (%s; %s)", reviser, race, gender)) %>%
  mutate(rev_info = stri_replace_all(rev_info, "", fixed = " (NA; NA)")) %>%
  mutate(rev_info = stri_replace_all(rev_info, ")", fixed = "; NA)")) %>%
  mutate(rev_info = stri_replace_all(rev_info, "(", fixed = "(NA; ")) %>%
  mutate(
    rev_info = if_else(stri_length(rev_info) < 50, rev_info, NA_character_)) %>%
  filter(!is.na(rev_info)) %>%
  left_join(reviser_crosswalk, by = "reviser") %>%
  group_by(id) %>%
  summarize(rev_info = paste(rev_info, collapse = "; "))

interv <- interviewee %>%
  mutate(race = tolower(if_else(race %in% c("Unclear"), "NA", race))) %>%
  mutate(gender = tolower(gender)) %>%
  mutate(interv_info = sprintf("%s (%s; %s)", interviewee, race, gender)) %>%
  mutate(
    interv_info = stri_replace_all(interv_info, "", fixed = " (NA; NA)")) %>%
  mutate(interv_info = stri_replace_all(interv_info, ")", fixed = "; NA)")) %>%
  mutate(interv_info = stri_replace_all(interv_info, "(", fixed = "(NA; ")) %>%
  mutate(interv_info = if_else(
    stri_length(interv_info) < 50, interv_info, NA_character_)) %>%
  filter(!is.na(interv_info)) %>%
  left_join(interviewee_crosswalk, by = "interviewee") %>%
  group_by(id) %>%
  summarize(
    interv_info = paste(interv_info, collapse = "; "),
    pseudo_info = paste(pseudonym, collapse = "; ")
  ) %>%
  mutate(pseudo_info = if_else(
    stri_detect(pseudo_info, fixed = "NA"), NA_character_, pseudo_info)
  )

meta <- interview %>%
  left_join(occs, by = 'id') %>%
  left_join(writ, by = 'id') %>%
  left_join(revi, by = 'id') %>%
  left_join(interv, by = 'id') %>%
  mutate(date = sprintf("%04d-%02d-%02d",
                        interview_year, interview_month, interview_day)) %>%
  mutate(date = stri_replace_all(date, "", fixed = "-NA")) %>%
  mutate(
    date = if_else(stri_detect(date, fixed = "NA"), NA_character_, date)) %>%
  mutate(location = sprintf("%s, %s", city, state)) %>%
  mutate(location = stri_replace_all(location, "", fixed = "NA, ")) %>%
  mutate(location = if_else(stri_detect(
    location, fixed = "NA"), NA_character_, location))


for (j in seq_len(nrow(meta)))
{
  ofile <- file.path("output", "xml", sprintf("%s.xml", meta$id[j]))
  x <- readLines(file.path(
    "..", "fwp-life-histories", "text", sprintf("%s.txt", meta$id[j])
  ), warn = FALSE)
  x <- stri_replace_all(x, "&gt;", fixed = ">")
  x <- stri_replace_all(x, "&lt;", fixed = "<")
  x <- stri_replace_all(x, "&amp;", fixed = "&")
  x <- paste(sprintf("<p>%s</p>", x), collapse = "\n")

  items <- NULL
  if (!is.na(meta$date[j])) {
    items <- c(items, sprintf(temp_item, "Date", meta$date[j]))
  }
  if (!is.na(meta$location[j])) {
    items <- c(items, sprintf(temp_item, "Location", meta$location[j]))
  }
  if (!is.na(meta$occupation[j])) {
    items <- c(items, sprintf(temp_item, "Occupations", meta$occupation[j]))
  }
  if (!is.na(meta$community[j])) {
    items <- c(items, sprintf(temp_item, "Community", meta$community[j]))
  }
  if (!is.na(meta$interv_info[j])) {
    items <- c(items, sprintf(temp_item, "Interviewee", meta$interv_info[j]))
  }
  if (!is.na(meta$pseudo_info[j])) {
    items <- c(items, sprintf(temp_item, "Pseudonym", meta$pseudo_info[j]))
  }
  if (!is.na(meta$writer_info[j])) {
    items <- c(items, sprintf(temp_item, "Writer", meta$writer_info[j]))
  }
  if (!is.na(meta$rev_info[j])) {
    items <- c(items, sprintf(temp_item, "Reviser", meta$rev_info[j]))
  }
  items <- paste(items, collapse = "\n")

  xml_text <- sprintf(template, meta$title[j], items, x)
  xml_doc <- read_xml(xml_text)
  write_xml(xml_doc, ofile)
}
