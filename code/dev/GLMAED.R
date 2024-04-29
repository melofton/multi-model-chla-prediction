# Make iterative chl-a predictions with update IC using GLM-AED
# Author: Mary Lofton
# Date: 29APR24

# Purpose: Make iterative chl-a predictions with update IC using GLM-AED

GLMAED <- function(spinup_folder = "./code/model_files/GLM-AED/spinup",
                   prediction_folder = "./code/model_files/GLM-AED/prediction",
                   rerun_spinup = TRUE){
  
  if(rerun_spinup == TRUE){
    message("re-running spinup")
    GLM3r::run_glm(sim_folder = spinup_folder,
                   nml_file = "glm3.nml",
                   verbose = TRUE)
    message("end spinup model run")
  }
  
  
}