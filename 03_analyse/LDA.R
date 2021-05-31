library(tidyverse)
library(quanteda)
library(here)
library(janitor)
library(lubridate)
library(conflicted)
library(topicmodels)
library(ldatuning)
conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

load(here("01_data", "Corpus_20210316_motscles_clean.RData"))

do_dft <- dfm_trim(do_df, min_termfreq = 5)

# do_tpn <- FindTopicsNumber(do_df, topics = seq(10, 50, 5), metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004"))
# FindTopicsNumber_plot(do_tpn)
# save(do_tpn, file = here("04_objects", "lda_topicnumbers.RData"))

set.seed(214578654)
do_lda <- convert(do_dft, to = "topicmodels") %>% 
  LDA(k = 30)

save(do_lda, file = here("04_objects", "lda.RData"))
