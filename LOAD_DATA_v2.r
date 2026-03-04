library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)

# ---------------- PATHS ----------------
base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"
rdata_path <- file.path(base_path, "RData")

# ---------------- HELPERS ----------------
standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}

prep_procom_t <- function(df, ref_area, time) {
  
  ref_area <- rlang::ensym(ref_area)
  time     <- rlang::ensym(time)
  
  df %>%
    as.data.frame() %>% 
    filter(
      str_detect(!!ref_area, "^[0-9.]+$")
      ) %>%
    rename(PRO_COM_T = !!ref_area,
           year      = !!time) %>%
    mutate(
      year = as.integer(year),
      PRO_COM_T = str_pad(as.character(PRO_COM_T), width = 6, side = "left", pad = "0")
    ) %>%
    tidyr::complete(
      PRO_COM_T,
      year = 2014:2024
    ) %>% 
    fill(PRO_COM_T, .direction = "down")
}


# ---------------- LOAD SHAPE ----------------
comuni <- st_read(
  file.path(base_path, "Limiti01012025/Com01012025/Com01012025_WGS84.shp"),
  quiet = TRUE
)

# ---------------- LOAD RDATA IN TEMP ENV ----------------
raw_env <- new.env()
rdata_files <- list.files(rdata_path, pattern="\\.RData$", full.names=TRUE)
walk(rdata_files, ~ load(.x, envir = raw_env))

data_list <- mget(ls(raw_env), envir = raw_env) %>%
  keep(is.data.frame) %>%
  map(standardize_names)



rm(raw_env)

# ---------------- PREP ----------------
data_list$addetti_TOT <-
  prep_procom_t(data_list$addetti_TOT, ref_area, time_period) %>%
  rename(n_workers = osservazione) %>% 
  select(-data_type)

data_list$Altitudine_TOT_2014_2022 <-
  prep_procom_t(data_list$Altitudine_TOT_2014_2022,
                codice_istat_del_comune_alfanumerico, anno) %>%
  mutate(altitudine_del_centro_metri = as.numeric(altitudine_del_centro_metri))


names(data_list$panel_italiana)[3:26] <- paste0(names(data_list$panel_italiana)[3:26], "_italian")
data_list$panel_italiana <-
  prep_procom_t(data_list$panel_italiana, codice_comune, anno) %>%
  mutate(across(3:26, as.numeric))


names(data_list$panel_straniera)[3:26] <- paste0(names(data_list$panel_straniera)[3:26], "_foreign")
data_list$panel_straniera <-
  prep_procom_t(data_list$panel_straniera, codice_comune, anno) %>%
  mutate(across(3:26, as.numeric))


data_list$db_capacita_tota <-
  prep_procom_t(data_list$db_capacita_tota, na, x2014) %>%
  mutate(across(3:26, as.numeric))

data_list$Redditi_tot <-
  prep_procom_t(data_list$Redditi_tot, codice_istat_comune, anno_di_imposta)

data_list$results_gini_con_mediana <-
  prep_procom_t(data_list$results_gini_con_mediana,
                codice_istat_comune, anno_di_imposta)

data_list$UL_TOT <-
  prep_procom_t(data_list$UL_TOT, ref_area, time_period) %>%
  rename(n_firms = osservazione) %>% 
  select(-data_type)

data_list$comuni_stats_all <-
  prep_procom_t(data_list$comuni_stats_all, pro_com, anno)

data_list$amministrazioni_comunali <-
  prep_procom_t(data_list$amministrazioni_comunali,
              istat_codice_comune, anno)

data_list$df_coesione_fine_mergiato_cut <-
  prep_procom_t(data_list$df_coesione_fine_mergiato_cut,
              cod_comune, anno)

data_list$MAQUI <-
  prep_procom_t(data_list$MAQUI, codice_comune, anno)

# ---------------- SAMPLING ----------------
draw_samples <- TRUE

if(isTRUE(draw_samples)) {
  set.seed(123)
  sampled_codes_T <- sample(comuni$PRO_COM_T, 5)
  
  comuni_sampled_T <- comuni %>%
    filter(PRO_COM_T %in% sampled_codes_T)
} else {
  comuni_sampled_T <- comuni
}


# ---------------- STEP 1 ----------------
# Drop geometry → pure data.frame
comuni_meta <- comuni_sampled_T %>%
  st_drop_geometry() %>%
  as.data.frame()

# ---------------- STEP 2 ----------------
merge_meta_T <- function(df) {
  left_join(comuni_meta, df, by = "PRO_COM_T")
}

merged_T <- list(
  merge_meta_T(data_list$addetti_TOT),
  merge_meta_T(data_list$Altitudine_TOT_2014_2022),
  merge_meta_T(data_list$Redditi_tot),
  merge_meta_T(data_list$results_gini_con_mediana),
  merge_meta_T(data_list$UL_TOT),
  merge_meta_T(data_list$panel_italiana),
  merge_meta_T(data_list$panel_straniera),
  merge_meta_T(data_list$comuni_stats_all),
  merge_meta_T(data_list$amministrazioni_comunali),
  merge_meta_T(data_list$df_coesione_fine_mergiato_cut),
  merge_meta_T(data_list$MAQUI)
)



all_dfs <- c(merged_T)

# ---------------- STEP 3 ----------------
data_merged <- Reduce(
  function(x, y) full_join(x, y,
                           by = intersect(names(x), names(y))),
  all_dfs
) %>%
  as.data.frame()

# ---------------- STEP 4 ----------------
data_final <- left_join(
  comuni_sampled_T,
  data_merged,
  by = intersect(names(comuni_meta), names(data_merged))
) %>% 
  select(
    all_of(1:13),
    where(is.numeric)
  ) 

# RESULT:
# data_final  → single sf object with all variables + geometry

rm(list = setdiff(ls(), "data_final"))
