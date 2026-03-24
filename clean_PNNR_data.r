library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)


standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}


load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/TO_CLEAN/PNNR.RData")

names(Data4)


Data4$Finanziamento.PNRR.x
Data4$Finanziamento.PNRR.y


names(Data4)[which(str_ends(names(Data4) , "\\.y|\\.x"))]


Data5 <- Data4 %>% 
  select(which(!duplicated(as.list(.)))) %>% 
  select(-cup)

names(Data5)[which(str_ends(names(Data5) , "\\.y|\\.x"))]

str_replace(Data5$Finanziamento.Regione, ",", ".")
Data5$Finanziamento.Regione == Data5$finanziamento_regione
which(!(str_replace(Data5$Finanziamento.Regione, ",", ".") == Data5$finanziamento_regione))
str_replace(Data5$Finanziamento.Regione, ",", ".")[18606]
Data5$finanziamento_regione[18606]

str_replace(Data5$Finanziamento.Regione, ",", ".")[18607]
Data5$finanziamento_regione[18607]

# names(Data5)[which(str_ends(names(Data5) , "\\.y|\\.x"))] <- str_remove(names(Data5)[which(str_ends(names(Data5) , "\\.y|\\.x"))], "\\.x")


standardize_names(Data5)







####################

df <- Data4 %>% 
  select(1:12, 17:28, Data.Inizio.Progetto.Effettiva)

df <- standardize_names(df)


library(dplyr)
library(lubridate)

df_summary <- df %>%
  mutate(date_col = dmy(date_col),
         year = year(date_col)) %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))



