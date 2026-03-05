# TEST

source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/merge_munic_data_nuts.r")

nuts_code <- "NUTS1"

compute_median_by_nuts <- function(municipal_data_nuts, nuts_code) {
  municipal_data_nuts %>%
    st_drop_geometry() %>% 
    group_by(
      .data[[nuts_code]],
      #NUTS2_Code, 
      year
    ) %>% 
    reframe(
      across(14:163, \(x) median(x, na.rm = TRUE))
    ) %>% 
    filter(
      !is.na(.data[[nuts_code]])
    ) %>% 
    left_join(
      shapes_df_list[[nuts_code]]
    )
}

compute_median_by_nuts(municipal_data_nuts, "NUTS3")

