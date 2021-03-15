library(tidyverse)
library(jsonlite)
library(stringi)

options(dplyr.summarise.inform = FALSE)

# cp output/geo/*.json ~/gh/writing-their-voices/public/data/geo/

###############################################################################
bp <- file.path("..", "fwp-life-histories", "csv")

writer <- read_csv(file.path(bp, "writer_crosswalk.csv")) %>%
  left_join(
    filter(read_csv(file.path(bp, "writer.csv")), !duplicated(writer)),
    by = "writer"
  ) %>%
  filter(!duplicated(id))

ethnic_groups <- c("White", "Black", "Greek", "Cuban", "Chinese")
ethnic <- read_csv(file.path(bp, "interviewee_crosswalk.csv")) %>%
  left_join(
    filter(
      read_csv(file.path(bp, "interviewee.csv")), !duplicated(interviewee)
    ),
    by = "interviewee"
  ) %>%
  arrange(id, order) %>%
  filter(!duplicated(id)) %>%
  mutate(race = if_else(ethnic == "Greek", "Greek", race)) %>%
  mutate(race = if_else(ethnic == "Cuban", "Cuban", race)) %>%
  mutate(ethnicity = if_else(race %in% ethnic_groups, race, "Other")) %>%
  select(id, ethnicity)

occupation <- read_csv(file.path(bp, "occupation.csv")) %>%
  group_by(id) %>%
  mutate(n = n ()) %>%
  filter(
    (occupation != "mother") | (n == 1)
  ) %>%
  filter(
    (occupation != "housewife") | (n == 1)
  ) %>%
  ungroup() %>%
  filter(!duplicated(id)) %>%
  mutate(occ = stri_trans_totitle(occupation)) %>%
  mutate(occ = if_else(occ == "Tenant Farmer", "Farmer", occ)) %>%
  mutate(occ = if_else(stri_detect(occ, fixed = "Wpa"), "WPA", occ)) %>%
  select(id, occ)

dt <- read_csv(file.path(bp, "interview.csv")) %>%
  left_join(writer, by = "id") %>%
  left_join(ethnic, by = "id") %>%
  left_join(occupation, by = "id") %>%
  filter(!is.na(lon), !is.na(lat), !is.na(state), !is.na(city))

city_lon_lat <- dt %>%
  group_by(state, city, lon, lat) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  group_by(state, city) %>%
  slice_head(n = 1) %>%
  select(-n) %>%
  ungroup()

dt <- dt %>%
  select(-lon, -lat) %>%
  left_join(city_lon_lat, by = c("state", "city")) %>%
  ungroup()

###############################################################################
# Map 1: 'All Interviews' => geo_all.json

col_def <- "#7A9D96"

z <- dt %>%
  group_by(lon, lat, state, city) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s, %s", city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_def
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

json_obj <- toJSON(list(points = z, legend = NULL), null = "null")
write_lines(json_obj, file.path("output", "geo", "geo_all.json"))

###############################################################################
# Map 2: 'Prolific Writers' => geo_writers.json

top_writers <- names(sort(table(dt$writer), TRUE)[1:20])
col_set <- c("#005FA7", "#FFB646", "#7E5FBF", "#905225", "#6898C6", "#FF863D",
             "#FFD08C", "#FFB285", "#AD99D7", "#B98F74", "#00AA9F", "#27813E",
             "#DB4642", "#FF8082", "#AEA19B", "#77C7C2", "#7AAE82", "#EA8986",
             "#FFAEAE", "#CBC2BE")

z <- dt %>%
  filter(writer %in% top_writers) %>%
  mutate(writer_id = match(writer, top_writers)) %>%
  group_by(lon, lat, state, city, writer, writer_id) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, writer, writer_id, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s — %s, %s", writer, city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_set[writer_id]
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

legend <- tibble(
  label = top_writers,
  color = col_set[seq_along(top_writers)]
)

json_obj <- toJSON(list(points = z, legend = legend))
write_lines(json_obj, file.path("output", "geo", "geo_writers.json"))

###############################################################################
# Map 3: 'Women Writers' => geo_women_writers.json

z <- dt %>%
  filter(gender == "female") %>%
  group_by(lon, lat, state, city) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s, %s", city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_def
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

json_obj <- toJSON(list(points = z, legend = NULL), null = "null")
write_lines(json_obj, file.path("output", "geo", "geo_women_writers.json"))


###############################################################################
# Map 4: 'Black Writers' => geo_black_writers.json

z <- dt %>%
  filter(race == "black") %>%
  group_by(lon, lat, state, city) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s, %s", city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_def
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

json_obj <- toJSON(list(points = z, legend = NULL), null = "null")
write_lines(json_obj, file.path("output", "geo", "geo_black_writers.json"))

###############################################################################
# Map 5: 'Interviewee Ethnicity' => geo_ethnic.json

ethnic_set <- c("Greek", "Cuban", "Chinese")
col_set <- c("#FFB646", "#7AAE82", "#005FA7")

z <- dt %>%
  filter(ethnicity %in% ethnic_set) %>%
  mutate(ethnic_id = match(ethnicity, ethnic_set)) %>%
  group_by(lon, lat, state, city, ethnicity, ethnic_id) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, ethnicity, ethnic_id, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s — %s, %s", ethnicity, city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_set[ethnic_id]
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

legend <- tibble(
  label = ethnic_set,
  color = col_set[seq_along(ethnic_set)]
)

json_obj <- toJSON(list(points = z, legend = legend))
write_lines(json_obj, file.path("output", "geo", "geo_ethnic.json"))

###############################################################################
# Map 6: 'Occupations' => geo_occupation.json

top_occ <- names(sort(table(dt$occ), TRUE)[1:12])
col_set <- c("#005FA7", "#FFB646", "#7E5FBF", "#905225", "#6898C6", "#FF863D",
             "#FFD08C", "#FFB285", "#AD99D7", "#B98F74", "#00AA9F", "#27813E")

z <- dt %>%
  filter(occ %in% top_occ) %>%
  mutate(occ_id = match(occ, top_occ)) %>%
  group_by(lon, lat, state, city, occ, occ_id) %>%
  mutate(n = n()) %>%
  mutate(wname = if_else(is.na(writer), "Unknown", writer)) %>%
  mutate(title = sprintf("%s — %s", title, wname)) %>%
  select(lon, lat, state, city, occ, occ_id, n, id, title) %>%
  nest(links = c(id, title)) %>%
  mutate(size = (4 + round(sqrt(n - 1))) * 1) %>%
  mutate(
    title = sprintf("%s — %s, %s", occ, city, state),
    subtitle = sprintf("Count: %d", n),
    color = col_set[occ_id]
  ) %>%
  ungroup() %>%
  select(lon, lat, size, color, title, subtitle, links) %>%
  arrange(desc(size))

legend <- tibble(
  label = top_occ,
  color = col_set[seq_along(top_occ)]
)

json_obj <- toJSON(list(points = z, legend = legend))
write_lines(json_obj, file.path("output", "geo", "geo_occupation.json"))
