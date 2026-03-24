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

####################

df <- Data4 %>% 
  select(1:12, 17:28, Data.Inizio.Progetto.Effettiva)

df_renamed <- df[, 13:26] %>% 
  mutate(
    across(1:11, as.numeric)
  )

df_renamed <- standardize_names(df_renamed)

df2 <- df[, 1:12] %>% 
  st_drop_geometry() %>% 
  bind_cols(df_renamed) %>% 
  st_as_sf()

library(dplyr)
library(lubridate)

df_summary <- df2 %>%
  mutate(date_col = dmy(data_inizio_progetto_effettiva),
         year = year(date_col)) %>%
  group_by(year, PRO_COM_T) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup()




