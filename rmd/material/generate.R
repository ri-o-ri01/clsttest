
# Inputs ------------------------------------------------------------------

no_cores = 4
## For clustering_model_generation.Rmd--------------------------------------------------------------------
exe_clustering_model=TRUE
cmg_name = "GMM_PROD_EXPER1"

## For clustering_invest_strategy_generation.Rmd ---------------------------
exe_clustering_invest_strategy=TRUE
cisg_name = "GMM_PROD_EXPER1-SIMP_PROD_EXPER1"

# Setup -------------------------------------------------------------------
library(tidyverse)
library(rmarkdown)
library(here)
library(R6)

sim_data_path  = paste0(here(""), "/sim_data")
rcode_path     = paste0(here(""), "/R")
data_save_path = paste0(sim_data_path, "/data/") 
data_save_file_path = paste0(data_save_path, "/eq_tri_tbl.rds")
for (subfolder in c("cluster", "backtest", "utils", "finance")) {
  list.files(here("R", subfolder), "\\.R$", full.names = TRUE) %>% 
    lapply(source)
  list.dirs(here("R", subfolder), recursive = FALSE) %>% 
    list.files("\\.R$", full.names = TRUE) %>% 
    lapply(source)
}

source(paste0(rcode_path, "/run_setup.R"))

tmp_folder_path = here("rmd", "material","tmp")
dir.create(tmp_folder_path)
dir.create( here("rmd", "material", "html"))

temp_params = getListFromObjs(tmp_folder_path, cmg_name, cisg_name,
                              exe_clustering_model, exe_clustering_invest_strategy)
temp_params %>% 
  saveRDS(paste0(tmp_folder_path, "/temp_params"))
assign("temp_params", temp_params, envir =  globalenv())


# Execute -----------------------------------------------------------------
## For clustering_model_generation.Rmd-----------------------------------------------------
temp_params = readRDS(paste0(tmp_folder_path, "/temp_params"))
list2env(temp_params, envir = .GlobalEnv)
if(exe_clustering_model){
  temp_vec = CLUST_CONFIG_SETTING$ENUM_PARAM_TBL[[cmg_name]] %>% 
    pull(clust_trial) 
  temp_vec %>% 
    saveRDS(here(tmp_folder_path, "temp_vec"))
  assign("temp_vec", temp_vec, envir =  globalenv())
  for (iii in 1:length(temp_vec)) {
    print(iii)
    print(temp_vec[iii])
    clust_trial = temp_vec[iii]
    source(paste0(here(""), "/rmd/material/", "/background_job_helper.R"))
    tgt_params = getListFromObjs(clust_trial,  no_cores)
    my_env$my_render("clustering_model_generation.Rmd", tgt_params)
    temp_vec = readRDS(here("rmd", "material","tmp", "temp_vec"))
  }
  file.remove(here("rmd", "material","tmp", "temp_vec"))
}

## For clustering_invest_strategy_generation.Rmd ------------------------------------------------
temp_params = readRDS(paste0(here("rmd", "material","tmp"), "/temp_params"))
list2env(temp_params, envir = .GlobalEnv)
if(exe_clustering_invest_strategy){
  temp_info_tbl = ENUM_HIER_CLUST_INVEST_TBL[[cisg_name]]
  temp_info_tbl %>% 
    saveRDS(here(tmp_folder_path, "/temp_info_tbl"))
  assign("temp_info_tbl", temp_info_tbl, envir =  globalenv())
  for (iii in 1:nrow(temp_info_tbl)) {
    row = temp_info_tbl[iii, ]
    print(iii)
    print(temp_info_tbl[iii, ])
    clust_trial  = row[1, ]$clust_trial
    clust_trial2 = row[1, ]$clust_trial2
    securities   = deparse(as.call(list(as.name("c"), row[1, ]$securities[[1]])))　
    source(paste0(here(""), "/rmd/material/", "/background_job_helper.R"))
    tgt_params2 = list(
      clust_trial = clust_trial,
      clust_trial2 = clust_trial2,
      no_cores = no_cores,
      securities = securities
    )
    my_env$my_render("clustering_invest_strategy_generation.Rmd", tgt_params2)
    temp_info_tbl = readRDS(here("rmd", "material","tmp", "temp_info_tbl"))
  }
  file.remove(here("rmd", "material","tmp", "temp_info_tbl"))
  
}
