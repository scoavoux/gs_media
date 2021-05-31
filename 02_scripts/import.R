library(tidyverse)
library(here)
library(lubridate)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

import_europresse <- function(.file){
 require(tidyverse)
 require(rvest)
# Import de 
  r <- read_html(.file)
  h <- html_nodes(r, "article")
  
  
  one_text <- function(.node){
    tibble(
      publication = html_nodes(.node, "header span.DocPublicationName") %>% html_text(),
      metadata    = html_nodes(.node, "header span.DocHeader") %>% html_text(),
      titre       = html_nodes(.node, "header div.titreArticle") %>% html_text(),
      #authors     = html_nodes(h, "div.docAuthors") %>% html_text(),
      text        = map(html_nodes(.node, "section div.docOcurrContainer"), 
                        ~html_children(.x) %>% html_text() %>% paste(collapse = "\n\n")) %>% 
        unlist()
    )
  }
  d <- map(h, one_text)
  d <- d[!(map(d, ~is.null(.x$text)) %>% unlist())] %>% 
    bind_rows
  d <- mutate(d, across(everything(), str_trim))
  d <- mutate(d, 
              publication = str_remove(publication, ", no.*$"),
              publication = str_remove(publication, "\\s{2,}.*$"))
  
  d <- mutate(d, rawdate = str_extract(d$metadata, "(lundi|mardi|mercredi|jeudi|vendredi|samedi|dimanche) \\d{1,2} \\w+ \\d{4}"),
              date = lubridate::dmy(rawdate))
  return(d)
}

d <- import_europresse(here("01_data", "Corpus_20210316_motscles.HTML"))

# d <- mutate(d, cite_shift = str_detect(text, fixed("shift project", ignore_case = TRUE)),
#             cite_arcep = str_detect(text, fixed("arcep", ignore_case = TRUE)),
#             cite_senat = str_detect(text, regex("s[ée]nat", ignore_case = TRUE)),
#             cite_ademe = str_detect(text, fixed("ademe", ignore_case = TRUE)),
#             cite_greenit = str_detect(text, fixed("greenit", ignore_case = TRUE)))

save(d, file = here("01_data", "Corpus_20210316_motscles.RData"))
