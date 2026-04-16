base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"

municipal_data_merged <- readRDS(file.path(base_path, "RData/Merged/municipal_data_merged.RDS"))

municipal_data_merged$PRO_COM_T

n <- 10
sampled_codes <- sample(municipal_data_merged$PRO_COM_T, n, replace = FALSE)

municipal_data_merged <- municipal_data_merged %>% 
  filter(
    PRO_COM_T %in% sampled_codes
  )
