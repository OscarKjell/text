## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----extended_installation_condaenv, eval = FALSE-----------------------------
# library(text)
# library(reticulate)
# 
# # Install text-required Python packages in a Conda environment
# text::textrpp_install()
# 
# # Show available Conda environments
# reticulate::conda_list()
# 
# # Initialize the installed Conda environment
# text::textrpp_initialize(save_profile = TRUE)
# 
# # Test that textEmbed works
# textEmbed("hello")
# 

## ----install_other_pacakge, eval = FALSE--------------------------------------
# install.packages("dplyr")

## ----reticulate_installation, eval = FALSE------------------------------------
# library(reticulate)
# reticulate::install_miniconda()

## ----initialise, eval = FALSE-------------------------------------------------
# text::textrpp_initialize(
#   condaenv = "textrpp_condaenv",
#   refresh_settings = TRUE,
#   save_profile = TRUE,
# )

## ----install_github, eval = FALSE---------------------------------------------
# # install.packages("devtools")
# devtools::install_github("oscarkjell/text")

## ----textrpp_install(), eval = FALSE------------------------------------------
# library(text)
# text::textrpp_install(
#   update_conda = TRUE,
#   force_conda = TRUE
# )

## ----textDiagnostics(), eval = FALSE------------------------------------------
# library(text)
# log <- text::textDiagnostics()
# log

## ----extended_installation_venv, eval = FALSE---------------------------------
# # Create a virtual environment with text required python packages.
# # Note that you have to provide a python path.
# text::textrpp_install_virtualenv(rpp_version = c("torch==1.7.1", "transformers==4.12.5", "numpy", "nltk"),
#                                  python_path = "/usr/local/bin/python3.9",
#                                  envname = "textrpp_virtualenv")
# 
# # Initialize the virtual environment.
# text::textrpp_initialize(virtualenv = "textrpp_virtualenv",
#                          condaenv = NULL,
#                          save_profile = TRUE)
# 

