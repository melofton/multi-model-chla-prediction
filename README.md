# Code repository associated with Lofton et al., "Predictive skill of ecological models varies according to time horizon and environmental conditions"
Submitted as a research article to Ecological Applications

## Guide for peer reviewers:
This repository contains all data, modeling code, and model output associated with the manuscript. Because running the modeling workflow takes hours to days and required high-performance computing for some models, we have provided an example modeling workflow for the purposes of peer review using the ARIMA model in the manuscript, as well as a script that uses final model output to generate all final figures for the manuscript.

This example workflow is found in the `example_prediction_workflow.Rmd` file in the top level directory of the repository. Output from this workflow will be written to the `example_workflow_output` folder as well as rendered as an `.html` file. Users who wish to attempt the workflow for other models will find additional guidelines at the bottom of the RMarkdown file. However, we note that some models (LSTM and GLM-AED) have more complex workflows due to model structural complexities, long run times, and substantial dependencies. For these models, workflows were run on a high-performance computing cluster in a containerized environment. Specifically, the LSTM model workflow was run using a slightly modified version of the `ml-verse` container which can be downloaded at `https://hub.docker.com/repository/docker/melofton/ml-verse/general`, and the GLM-AED model workflow was run using a container developed by R. Quinn Thomas for the Forecasting Lake and Reservoir Ecosystems (FLARE) platform which can be downloaded at `https://hub.docker.com/layers/rqthomas/flare-rocker/4.4/images/sha256-3f6c7df54b67dc7bd4dbec54f69edf8068c1a786c831381e1907b5fb33678d3c`. Workflows for these models will likely require additional effort to set up an appropriate computing environment to run the workflow.

We have also provided a script, `generate_final_figures.R`, that uses the final model output from all models to generate all the final figures in the manuscript, as well as the supplementary figures related to modeling results.

## Repository file structure:

1. `code` contains all project code
  a. `archive` code that is no longer in use
  b. `function_library` custom functions associated with the project; each sub-folder within this directory contains functions associated with each stage of the workflow (e.g., formatting data or generating prediction)
  c. `model_files` additional files for complex models (LSTM, GLM-AED, and OneDProcessModel)
  d. `workflow_scripts` scripts that scale up the example_prediction_workflow to be applied across many models
2. `data` contains processed project data; note that all of the raw project data is downloaded directly from the [Environmental Data Initiative repository](https://edirepository.org/) and the [Virginia Ecoforecast Reservoir Analysis forecasting challenge](https://www.ltreb-reservoirs.org/vera4cast/) using custom functions and therefore raw data ares not stored in this repository
3. `figures` contains figures associated with the project; final figures are in the `final_figures` sub-folder
4. `model_output` contains all model output associated with the project; the final results are provided in `validation_output.csv`

