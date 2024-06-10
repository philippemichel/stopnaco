

#  ------------------------------------------------------------------------
#
# Title : Import Stopnaco
#    By : PhM
#  Date : 2024-06-10
#
#  ------------------------------------------------------------------------


importph <- function(){
library(readODS)
library(tidyverse)
library(lubridate)
library(janitor)
library(baseph)

tt <- read_ods("datas/stopnaco.ods", sheet = 1, na=c("NA", "na", "Na", "AUCUN", "Abs","Manquant")) |>
  janitor::clean_names() |>
  mutate(heure_entree = mdy_hms(heure_entree)) |>
  mutate(heure_incision = mdy_hms(heure_incision)) |>
  mutate(delai_chirurgical = as.numeric(tt$heure_incision-tt$heure_entree)) |>
  mutate(date_sortie = mdy(date_sortie)) |>
  mutate(across(where(is.character), as.factor)) |>
  mutate(bmi = bmiph(bmi)) |>
  mutate(score_charlson = cut(score_charlson, breaks = c(0, 1, 3, 5, 10),
                              labels = c("0", "1-2", "3-4", "5-10"))) |>
  mutate(groupe = as.factor(ifelse(heure_entree < dmy("30/09/2021"),
                                   "Avant", "Après"))) |>
  mutate(groupe = fct_relevel(groupe,
    "Avant", "Après")) |>
  mutate(dernier_cut = cut(dernier_dosage,
              breaks = c(0, 50, 1000),
              labels = c("< 50", "> 50"))) |>
mutate(premier_cut = cut(premier_dosage,
                         breaks = c(0, 50, 1000),
                         labels = c("< 50", "> 50"))) |>
  mutate(age_cut = cut(age,
                           breaks = c(70, 80, 90, 100),
                           labels = c("70-79", "80-89", "90-99")))

save(tt, file = "datas/stopnaco.RData")
}

importph()
load("datas/stopnaco.RData")
names(tt)
