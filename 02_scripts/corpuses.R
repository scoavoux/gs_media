# TODO:
# + enlever les articles en anglais.
# + 

library(tidyverse)
library(here)
library(lubridate)
library(conflicted)
library(quanteda)
library(textreuse)
conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

load(file = here("01_data", "Corpus_20210316_motscles.RData"))

####### Sélection des textes ######

# remove some documents

# Annonces
d <- filter(d, str_detect(text, "Ponts-de-Cé", negate = TRUE))
d <- filter(d, str_detect(titre, "sommaire", negate = TRUE))
d <- filter(d, titre != "Programme")
d <- filter(d, str_detect(titre, "AGENDA DES CONF[ÉE]RENCES", negate = TRUE))
d <- filter(d, str_detect(titre, "Revue de presse achats", negate = TRUE))
d <- filter(d, publication != "Droit de l'Environnement")
d <- filter(d, titre != "Les entretiens Nîmes-Alès : rendez-vous mardi")
d <- filter(d, str_detect(titre, "La veille de mind Media", negate = TRUE))

# ceux du thème 21 en général: regarder les plus associés)
# Mixs trop hétéroclites
d <- filter(d, str_detect(titre, "La veille urbaine", negate=TRUE))
d <- filter(d, str_detect(titre, "Vite vu", negate=TRUE))
d <- filter(d, str_detect(titre, "Les rendez-vous parlementaires de la semaine", negate=TRUE))
d <- filter(d, publication != "Bulletin Quotidien")
d <- filter(d, titre != "La mairie a créé son site internet")
d <- filter(d, str_detect(titre, "Les infos immanquables du jour", negate=TRUE))
d <- filter(d, str_detect(titre, "Agenda France 7 jours", negate=TRUE))
d <- filter(d, str_detect(titre, "Le Magazine n", negate=TRUE))
d <- filter(d, str_detect(titre, "A la Une à" , negate=TRUE))


# Doublons
minhash <- minhash_generator(900, seed = 235)
txcp <- TextReuseCorpus(text = d$text, minhash_func = minhash)
buckets <- lsh(txcp, bands = 50, progress = FALSE)
candidates <- lsh_candidates(buckets)

scores <- lsh_compare(candidates, txcp, jaccard_similarity, progress = FALSE)
arrange(scores, desc(score))
i <- scores$a[scores$a %in% scores$b]

# enlever les doublons
d <- slice(d, -as.numeric(str_remove(scores$b, "doc-")))

minhash <- minhash_generator(900, seed = 235)
txcp <- TextReuseCorpus(text = d$titre, minhash_func = minhash)
buckets <- lsh(txcp, bands = 50, progress = FALSE)
candidates <- lsh_candidates(buckets)

scores <- lsh_compare(candidates, txcp, jaccard_similarity, progress = FALSE)

slice(d, as.numeric(str_remove(scores$b, "doc-")))
arrange(scores, desc(score))
i <- scores$a[scores$a %in% scores$b]
d <- slice(d, -as.numeric(str_remove(scores$b, "doc-")))

####### Recodages #######

d <- mutate(d, 
            titre = str_replace_all(titre, "[\"'’’=\\+°>©]", " "),
            text = str_remove(text, "Cet article est paru dans .*"),
            text = str_replace_all(text, "<.*?>", " "),
            text = str_replace_all(text, "[\\|'’’\"=\\+°>©$€]", " "),
            text = str_remove_all(text, "\\d\\dh\\d\\d"),
            text = str_remove_all(text, "\\d\\dh"))

reco <- function(.var, .x, .y){
  str_replace_all(.var, coll(.x, ignore_case = TRUE, locale = "fr"), .y)
}


d <- mutate(d, 
            text = str_remove_all(text, "\\d\\dh\\d\\d"),
            text = reco(text, "green it", "green_it"),
            text = reco(text, "greenit", "green_it"),
            text = reco(text, "shift project", "shift_project"),
            text = reco(text, "shiftproject", "shift_project"),
            titre = reco(titre, "green it", "green_it"),
            titre = reco(titre, "greenit", "green_it"),
            titre = reco(titre, "shift project", "shift_project"),
            titre = reco(titre, "shiftproject", "shift_project"),
            text = reco(text, "new york times", "new_york_times"),
            text = reco(text, "paula forteza", "paula_forteza"),
            text = reco(text, "mme forteza", "paula_forteza"),
            text = reco(text, "matthieu orphelin", "matthieu_orphelin"),
            text = reco(text, "mathieu orphelin",  "matthieu_orphelin"),
            text = reco(text, "m. orphelin",  "matthieu_orphelin"),
            #text = reco(text, "kairos agency", "kairos_agency"),
            #text = reco(text, "kairos", "kairos_agency"),
            text = reco(text, "barbara pompili", "barbara_pompili"),
            text = reco(text, "mme pompili", "barbara_pompili"),
            # text = reco(text, "pompili", "barbara_pompili"),
            text = reco(text, "aurore berge", "aurore_berge"),
            text = reco(text, "aurore berge", "aurore_berge"),
            text = reco(text, "aurore bergé", "aurore_berge"),
            text = reco(text, "mme berge", "aurore_berge"),
            text = reco(text, "bernard duverneuil-", "bernard_duverneuil"),
            text = reco(text, "bernard duverneuil", "bernard_duverneuil"),
            # text = reco(text, "berge", "aurore_berge"),
            text = reco(text, "emmanuel macron", "emmanuel_macron"),
            text = reco(text, "m. macron",       "emmanuel_macron"),
            # text = reco(text, "macron",          "emmanuel_macron"),
            text = reco(text, "gauthier roussilhe", "gauthier_roussilhe"),
            text = reco(text, "gérard larcher", "gérard_larcher"),
            text = reco(text, "serge abiteboul", "serge_abiteboul"),
            text = reco(text, "patrick chaize", "patrick_chaize"),
            text = reco(text, "m. chaize",      "patrick_chaize"),
            text = reco(text, "chaize",         "patrick_chaize"),
            text = reco(text, "cédric o",       "cédric_o"),
            text = reco(text, "m. o",           "cédric_o"),
            text = reco(text, "decédric o",     "cédric_o"),
            text = reco(text, "corinne le quere", "corinne_le_quéré"),
            text = reco(text, "corinne le quéré", "corinne_le_quéré"),
            text = reco(text, "mme le quere", "corinne_le_quéré"),
            text = reco(text, "mme le quéré", "corinne_le_quéré"),
            text = reco(text, "frédéric bordage", "frédéric_bordage"),
            text = reco(text, "jean-marc jancovici", "jean-marc_jancovici"),
            text = reco(text, "hughes ferreboeuf", "hughes_ferreboeuf"),
            # text = reco(text, "ferreboeuf", "hughes_ferreboeuf"),
            text = reco(text, "shirley jagle", "shirley_jagle"),
            text = reco(text, "jean-pierre turon", "jean-pierre_turon"),
            text = reco(text, "mme laure de la raudière", "laure_de_la_raudière"),
            text = reco(text, "laure de la raudière", "laure_de_la_raudière"),
            text = reco(text, "m. sébastien soriano", "sébastien_soriano"),
            text = reco(text, "sébastien soriano", "sébastien_soriano"),
            text = reco(text, "maxime efoui", "maxime_efoui"),
            text = reco(text, "m. jeff bezos", "jeff_bezos"),
            text = reco(text, "jeff bezos", "jeff_bezos"),
            text = reco(text, "m. cédric villani", "cédric_villani"),
            text = reco(text, "cédric villani", "cédric_villani"),
            text = reco(text, "yannick jadot", "yannick_jadot"),            
            text = reco(text, "yannick chatelain", "yannick_chatelain"),
            text = reco(text, "patrick pouyanné", "patrick_pouyanné"),
            text = reco(text, "jean-luc mélenchon", "jean-luc_mélenchon"),
            text = reco(text, "anne hidalgo", "anne_hidalgo"),
            text = reco(text, "vincent courboulay", "vincent_courboulay"),
            text = reco(text, "elon musk", "elon_musk")
)

d <- mutate(d,
            text = reco(text, "sobriéténumérique",  "sobriété numérique"),
            text = reco(text, "numériquesoutenable",  "numérique soutenable"),
            text = reco(text, "empreinteenvironnementaledunumérique",  "empreinte environnementale du numérique"),
            text = reco(text, "impactenvironnementaldunumérique",  "impact environnemental du numérique"),
            
            
)

d <- mutate(d,
            publication_web = str_detect(publication, "\\(site web\\)"),
            publication = str_remove(publication, "\\(site web\\)") %>% str_trim(),
            dateym = ym(paste0(year(date), month(date))))

####### Publications ######
# write_csv(count(d, publication), "publications.csv")

####### Fabrication des corpus #######

# Paramètres
stw <- c(stopwords("fr"), 
         "a", "h", 
         "si", 
         "elles", "elle", "il", "ils", 
         "plus", "depuis", "afin", "aussi", "comme", "entre", "dont", "après",
         "quand",
         "très", 
         "fait", "faire", "faut", "être", "va",
         "tout", "toute", "tous", "toutes",
         "aujourd", "hui", 
         "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")

# Corpus des documents
do_cp <- corpus(tolower(d$text))
do_df <- dfm(do_cp, 
             remove_punct = TRUE, 
             remove_numbers = TRUE, 
             remove_separators = TRUE,
             remove_url = TRUE,
             remove = stw)
do_fcm <- fcm(do_df)
do_fcm <- fcm_select(do_fcm, names(topfeatures(do_fcm, n = 80)))

# Corpus des titres
ti_cp <- corpus(tolower(d$titre))
ti_df <- dfm(ti_cp, 
             remove_punct = TRUE, 
             remove_numbers = TRUE, 
             remove_separators = TRUE,
             remove = stw)
ti_fcm <- fcm(ti_df)
ti_fcm <- fcm_select(ti_fcm, names(topfeatures(ti_fcm, n = 40)))

save(d, do_cp, do_df, do_fcm, ti_cp, ti_df, ti_fcm, file = here("01_data", "Corpus_20210316_motscles_clean.RData"))
