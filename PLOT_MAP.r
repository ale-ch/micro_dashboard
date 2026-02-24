library(sf)
library(dplyr)
library(ggplot2)

# read shapefile
comuni <- st_read('/Volumes/T7 Shield/FRES/DB_Comunale/Limiti01012025/Com01012025/Com01012025_WGS84.shp', quiet = TRUE)


library(tmap)


# Firms
tm_shape(addetti_TOT_sampled_map) +
  tm_polygons("n_workers")

tmap_save(p1, "n_workers.html")



p2 <- tm_shape(N_FIRMS_sampled_map) +
  tm_polygons("n_firms")

tmap_save(p2, "n_firms.html")


# Geography
tm_shape(Altitudine_TOT_2014_2022_sampled_map) +
  tm_polygons("altitudine_del_centro_metri")

tm_shape(comuni_stats_all_sampled_map) +
  tm_polygons("area_km2")

# Demografia
tm_shape(panel_italiana_sampled_map) +
  tm_polygons("nati_maschi")

tm_shape(panel_italiana_sampled_map) +
  tm_polygons("nati_femmine")

tm_shape(panel_italiana_sampled_map) +
  tm_polygons("popolazione_inizio_totale")

tm_shape(panel_italiana_sampled_map) +
  tm_polygons("popolazione_fine_totale")


tm_shape(panel_straniera_sampled_map) +
  tm_polygons("nati_maschi")


# Income
tm_shape(Redditi_tot_sampled_map) +
  tm_polygons("reddito_da_lavoro_dipendente_e_assimilati_ammontare")


# Municipality
tm_shape(amministrazioni_comunali_sampled_map) +
  tm_polygons("eta_sindaco")

tm_shape(df_coesione_fine_mergiato_cut_sampled_map) +
  tm_polygons("finanziamenti_tot_sum")


# Indices
tm_shape(results_gini_con_mediana_sampled_map) +
  tm_polygons("gini_con_mediana")

tm_shape(MAQUI_sampled_map) +
  tm_polygons("pillar1_bur")

tm_shape(MAQUI_sampled_map) +
  tm_polygons("pillar2_pol")

tm_shape(MAQUI_sampled_map) +
  tm_polygons("pillar3_econ")






# Set output directory
out_dir <- "/Volumes/T7 Shield/FRES/DB_Comunale/plots"

# Firms
tmap_save(tm_shape(addetti_TOT_sampled_map) + tm_polygons("n_workers"), 
          file.path(out_dir, "n_workers.html"))

tmap_save(tm_shape(N_FIRMS_sampled_map) + tm_polygons("n_firms"), 
          file.path(out_dir, "n_firms.html"))

# Geography
tmap_save(tm_shape(Altitudine_TOT_2014_2022_sampled_map) + tm_polygons("altitudine_del_centro_metri"), 
          file.path(out_dir, "altitudine.html"))

tmap_save(tm_shape(comuni_stats_all_sampled_map) + tm_polygons("area_km2"), 
          file.path(out_dir, "area_km2.html"))

# Demografia
tmap_save(tm_shape(panel_italiana_sampled_map) + tm_polygons("nati_maschi"), 
          file.path(out_dir, "nati_maschi_ita.html"))

tmap_save(tm_shape(panel_italiana_sampled_map) + tm_polygons("nati_femmine"), 
          file.path(out_dir, "nati_femmine_ita.html"))

tmap_save(tm_shape(panel_italiana_sampled_map) + tm_polygons("popolazione_inizio_totale"), 
          file.path(out_dir, "pop_inizio_tot.html"))

tmap_save(tm_shape(panel_italiana_sampled_map) + tm_polygons("popolazione_fine_totale"), 
          file.path(out_dir, "pop_fine_tot.html"))

tmap_save(tm_shape(panel_straniera_sampled_map) + tm_polygons("nati_maschi"), 
          file.path(out_dir, "nati_maschi_str.html"))

# Income
tmap_save(tm_shape(Redditi_tot_sampled_map) + tm_polygons("reddito_da_lavoro_dipendente_e_assimilati_ammontare"), 
          file.path(out_dir, "reddito_lavoro.html"))

# Municipality
tmap_save(tm_shape(amministrazioni_comunali_sampled_map) + tm_polygons("eta_sindaco"), 
          file.path(out_dir, "eta_sindaco.html"))

tmap_save(tm_shape(df_coesione_fine_mergiato_cut_sampled_map) + tm_polygons("finanziamenti_tot_sum"), 
          file.path(out_dir, "finanziamenti.html"))

# Indices
tmap_save(tm_shape(results_gini_con_mediana_sampled_map) + tm_polygons("gini_con_mediana"), 
          file.path(out_dir, "gini_mediana.html"))

tmap_save(tm_shape(MAQUI_sampled_map) + tm_polygons("pillar1_bur"), 
          file.path(out_dir, "pillar1_bur.html"))

tmap_save(tm_shape(MAQUI_sampled_map) + tm_polygons("pillar2_pol"), 
          file.path(out_dir, "pillar2_pol.html"))

tmap_save(tm_shape(MAQUI_sampled_map) + tm_polygons("pillar3_econ"), 
          file.path(out_dir, "pillar3_econ.html"))






