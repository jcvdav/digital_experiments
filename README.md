# Data and code for "_Digital technologies and the study of adaptation in small-scale fisheries_"

Juan Carlos Villaseñor-Derbez et al.

[![DOI](https://zenodo.org/badge/727395040.svg)](https://doi.org/10.5281/zenodo.18305914)


## Reproducibility

The repository contains all the data and code required to replicate our findings.
It also includes [`renv`](https://rstudio.github.io/renv/articles/renv.html#getting-started)
to make sure you use the exact same package version we used. To reproduce our
current environment, use `renv::restore()`. This will use the metadata in our
lockfile to install exactly the same version of every package.


## Repository structure 

This repository is organized in a standard R project structure. The `scripts/` folder contains numbered R scripts (01-07) that should be run sequentially to reproduce all analyses, from data download through event study results. Raw data files are stored in `data/raw/`, processed datasets in `data/processed/`, and all outputs (figures and tables) are saved to `results/img/` and `results/tab/`. The `renv/` folder manages package dependencies for reproducibility.

```
-- data
   |__processed
      |__tabular_game_data.rds
   |__raw
      |__Baja baseline data games 2015.xls
      |__edades_alcance_post.csv
      |__lugares_alcance_post.csv
      |__mexico_fishing_regions.gpkg
      |__raw_game_data.rds
-- digital_experiments.Rproj
-- README.md
-- renv
-- renv.lock
   |__activate.R
   |__library
      |__macos
   |__settings.json
   |__staging
-- results
   |__img
      |__fig1_screenshots.pdf
      |__fig1_screenshots.png
      |__fig2_fb_map.pdf
      |__fig2_fb_map.png
      |__fig3_survival.pdf
      |__fig3_survival.png
      |__fig4_state_vars.pdf
      |__fig4_state_vars.png
      |__fig5_effects.pdf
      |__fig5_effects.png
      |__fig6_event_study.pdf
      |__fig6_event_study.png
      |__figS1_supp_event_study.pdf
      |__figS1_supp_event_study.png
   |__tab
      |__tab1_effects.tex
      |__tabS1_event_study.tex
-- scripts
   |__01_download_data.R
   |__02_build_tabular_data.R
   |__03_usage_stats.R
   |__04_survival.R
   |__05_behavior_plots.R
   |__06_validation.R
   |__07_event_study.R
```

---------
