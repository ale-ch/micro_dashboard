library(readxl)
library(tidyverse)

dirpath <- '/Volumes/T7 Shield/FRES/DB_Comunale/RData'



municipality_codes_istat_file <- '/Volumes/T7 Shield/FRES/DB_Comunale/RData/municipality_codes_istat.xlsx'
municipality_codes_istat <- read_excel(municipality_codes_istat_file)

# read shapefile
comuni <- st_read('/Volumes/T7 Shield/FRES/DB_Comunale/Limiti01012025/Com01012025/Com01012025_WGS84.shp', quiet = TRUE)

names(municipality_codes_istat)
tolower(names(municipality_codes_istat))
str_replace(tolower(names(municipality_codes_istat)), "/", "_")
colnames_codes_istat <- str_replace(str_replace(tolower(names(municipality_codes_istat)), "/", "_"), " ", "_")
names(municipality_codes_istat) <- colnames_codes_istat


files <- list.files(dirpath)

paste(dirpath)
filepaths <- lapply(files, function(x) paste0(dirpath, "/", x))
filepaths <- unlist(filepaths)
filepaths

load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/addetti_TOT.RData"                  )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/Altitudine_TOT_2014_2022.RData"     )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/amministrazioni_comunali.RData"     )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/comuni_stats_all.RData"             )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/db_capacita_tota.RData"             )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/df_coesione_fine_mergiato_cut.RData")
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/MAQUI.RData"                        )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/panel_italiana.RData"               )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/panel_straniera.RData"              )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/Redditi_tot.RData"                  )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/results_gini_con_mediana.RData"     )
load("/Volumes/T7 Shield/FRES/DB_Comunale/RData/UL_TOT.RData" )



# Get all data frames in the current environment
df_names <- names(Filter(is.data.frame, mget(ls(), envir = .GlobalEnv)))

# Get column names for each data frame
df_cols <- lapply(df_names, function(x) colnames(get(x, envir = .GlobalEnv)))

names(df_cols) <- df_names


standardize_column_names <- function(df) {
  df %>%
    rename_with(~ .x %>% 
                  # Convert to lowercase
                  str_to_lower() %>% 
                  # Replace any non-alphanumeric characters (including spaces) with _
                  str_replace_all("[^a-z0-9]+", "_") %>% 
                  # Remove any leading or trailing underscores
                  str_remove_all("^_+|_+$")
    )
}


addetti_TOT                   <- standardize_column_names(addetti_TOT)
Altitudine_TOT_2014_2022 <- standardize_column_names(Altitudine_TOT_2014_2022)
amministrazioni_comunali      <- standardize_column_names(amministrazioni_comunali)
comuni_stats_all              <- standardize_column_names(comuni_stats_all)
db_capacita_tota              <- standardize_column_names(db_capacita_tota)
df_coesione_fine_mergiato_cut <- standardize_column_names(df_coesione_fine_mergiato_cut)
MAQUI                         <- standardize_column_names(MAQUI)
panel_italiana                <- standardize_column_names(panel_italiana)
panel_straniera               <- standardize_column_names(panel_straniera)
Redditi_tot                   <- standardize_column_names(Redditi_tot)
results_gini_con_mediana      <- standardize_column_names(results_gini_con_mediana)
N_FIRMS                        <- standardize_column_names(UL_TOT)



library(dplyr)
library(stringr)

prep_data <- function(data, REF_AREA, TIME_PERIOD) {
  
  REF_AREA   <- rlang::ensym(REF_AREA)
  TIME_PERIOD <- rlang::ensym(TIME_PERIOD)
  
  data %>%
    filter(str_detect(!!REF_AREA, "^[0-9.]+$")) %>%
    rename(PRO_COM_T = !!REF_AREA) %>%
    filter(!!TIME_PERIOD == max(!!TIME_PERIOD, na.rm = TRUE))
}

prep_data_procom <- function(data, REF_AREA, TIME_PERIOD) {
  
  REF_AREA   <- rlang::ensym(REF_AREA)
  TIME_PERIOD <- rlang::ensym(TIME_PERIOD)
  
  data %>%
    filter(str_detect(!!REF_AREA, "^[0-9.]+$")) %>%
    rename(PRO_COM = !!REF_AREA) %>%
    filter(!!TIME_PERIOD == max(!!TIME_PERIOD, na.rm = TRUE))
}



addetti_TOT <- prep_data(addetti_TOT, ref_area, time_period)
addetti_TOT <- addetti_TOT %>% 
  rename(
    n_workers = osservazione
  )
Altitudine_TOT_2014_2022 <- prep_data(Altitudine_TOT_2014_2022, codice_istat_del_comune_alfanumerico, anno)
Altitudine_TOT_2014_2022 <- Altitudine_TOT_2014_2022 %>% 
  mutate(
    altitudine_del_centro_metri = as.numeric(altitudine_del_centro_metri)
  )

# prep_data(db_capacita_tota, na, db_capacita_tota$)


panel_italiana <- prep_data(panel_italiana, codice_comune, anno)
panel_italiana <- panel_italiana %>% 
  mutate(
    across(3:26, as.numeric)
  )
panel_straniera <- prep_data(panel_straniera, codice_comune, anno)
panel_straniera <- panel_straniera %>% 
  mutate(
    across(3:26, as.numeric)
  )
Redditi_tot <- prep_data(Redditi_tot, codice_istat_comune, anno_di_imposta)
results_gini_con_mediana <- prep_data(results_gini_con_mediana, codice_istat_comune, anno_di_imposta)
N_FIRMS <- prep_data(N_FIRMS, ref_area, time_period)
N_FIRMS <- N_FIRMS %>% 
  rename(
    n_firms = osservazione
  )


comuni_stats_all <- prep_data_procom(comuni_stats_all, pro_com, anno)
amministrazioni_comunali <- prep_data_procom(amministrazioni_comunali, istat_codice_comune, anno)
MAQUI<- prep_data_procom(MAQUI, codice_comune, anno)
df_coesione_fine_mergiato_cut <- prep_data_procom(df_coesione_fine_mergiato_cut, cod_comune, anno)



seed_number <- 123
n_samples <- 1000

set.seed(seed_number)
sampled_comuni <- sample(comuni$PRO_COM_T, n_samples)

sample_municipalities <- function(df, seed=seed_number, n=n_samples) {
  set.seed(seed)
  sampled_comuni <- sample(comuni$PRO_COM_T, n)
  
  df %>% 
    filter(
      PRO_COM_T %in% sampled_comuni
    )
  
}

sample_municipalities_procom <- function(df, seed=seed_number, n=n_samples) {
  set.seed(seed)
  sampled_comuni <- sample(comuni$PRO_COM_T, n)
  
  df %>% 
    filter(
      PRO_COM %in% sampled_comuni
    )
  
}


addetti_TOT_sampled <- sample_municipalities(addetti_TOT)
Altitudine_TOT_2014_2022_sampled <- sample_municipalities(Altitudine_TOT_2014_2022)
panel_italiana_sampled <- sample_municipalities(panel_italiana)
panel_straniera_sampled <- sample_municipalities(panel_straniera)
Redditi_tot_sampled <- sample_municipalities(Redditi_tot)
results_gini_con_mediana_sampled <- sample_municipalities(results_gini_con_mediana)
N_FIRMS_sampled <- sample_municipalities(N_FIRMS)

comuni_stats_all_sampled <- sample_municipalities_procom(comuni_stats_all)
amministrazioni_comunali_sampled <- sample_municipalities_procom(amministrazioni_comunali)
df_coesione_fine_mergiato_cut_sampled <- sample_municipalities_procom(df_coesione_fine_mergiato_cut)
MAQUI_sampled<- sample_municipalities_procom(MAQUI)


merge_map_data <- function(df, map_data, merge_col = "PRO_COM_T") {
  # join
  left_join(map_data, df, by = merge_col)
}


comuni_sampled <- comuni %>% 
  filter(PRO_COM_T %in% sampled_comuni)

addetti_TOT_sampled_map <- merge_map_data(addetti_TOT_sampled, comuni_sampled)
Altitudine_TOT_2014_2022_sampled_map <- merge_map_data(Altitudine_TOT_2014_2022_sampled, comuni_sampled)
panel_italiana_sampled_map <- merge_map_data(panel_italiana_sampled, comuni_sampled)
panel_straniera_sampled_map <- merge_map_data(panel_straniera_sampled, comuni_sampled)
Redditi_tot_sampled_map <- merge_map_data(Redditi_tot_sampled, comuni_sampled)
results_gini_con_mediana_sampled_map <- merge_map_data(results_gini_con_mediana_sampled, comuni_sampled)
N_FIRMS_sampled_map <- merge_map_data(N_FIRMS_sampled, comuni_sampled)

comuni_stats_all_sampled_map <- merge_map_data(comuni_stats_all_sampled, comuni_sampled, merge_col = "PRO_COM")
amministrazioni_comunali_sampled_map <- merge_map_data(amministrazioni_comunali_sampled, comuni_sampled, merge_col = "PRO_COM")
df_coesione_fine_mergiato_cut_sampled_map <- merge_map_data(df_coesione_fine_mergiato_cut_sampled, comuni_sampled, merge_col = "PRO_COM")
MAQUI_sampled_map <- merge_map_data(MAQUI_sampled, comuni_sampled, merge_col = "PRO_COM")





