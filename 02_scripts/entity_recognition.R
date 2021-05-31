# ne fonctionne pas pour le moment.
library(tidyverse)
library(quanteda)
library(here)
library(janitor)
library(lubridate)
library(conflicted)
library(spacyr)
conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

load(file = here("01_data", "Corpus_20210316_motscles.RData"))
source(here("02_scripts", "recodage.R"), encoding = "UTF8")
source(here("02_scripts", "corpuses.R"), encoding = "UTF8")

d <- mutate(d, text = str_remove_all(text, "https?://\\S*"))

spacy_initialize()
do_sp <- spacy_parse(do_cp)
ent <- entity_extract(do_sp)

ent <- as_tibble(ent)


filter(ent, entity_type == "PERSON")

textstat_frequency(do_df)

