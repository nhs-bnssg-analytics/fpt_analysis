
# Future Performance Tool

This repository contains the data download, data manipulation and the
modelling that underpins the [Future Performance
Tool](https://sw-dsn.shinyapps.io/future-performance-tool/). The Shiny
code for the Future Performance Tool is
[here](https://github.com/nhs-bnssg-analytics/fpt_tool), which was
developed using a {golem} framework.

See a description of the project and the findings as it progresses
[here](https://nhs-bnssg-analytics.github.io/fpt_analysis/outputs/01_index.html).

## Using this repository

The repository is managed using `renv`. The aim is that anyone that
wants to reproduce this analysis uses the same version of R that
performed this analysis. After cloning and opening the project for the
first time, run `renv::restore()` in the console to install all the
versions of the packages used in this work. You may need to restart
RStudio after doing this.

The file `R/02_data.R` should download and clean all of the data files
from the internet into a [tidy
format](https://tidyr.tidyverse.org/articles/tidy-data.html). The files
will be summarised into single files in a folder called `data`. This
step takes up to 30 minutes (depending on network connection speeds) to
run as the source files are numerous and some are quite large. The
source files are stored within the `data-raw` folder to allow users to
refer back to them.

A lot of the source files are spreadsheets, and generally they are
created manually by different organisations and uploaded to a server for
public use. As a result, filenames, sheet names and sheet formatting can
be inconsistent. The code in this project attempts to negate the
inconsistency, but there will be occasions where an unpredictable manual
step has occurred that the code hasn’t accounted for, and will result in
an error. Please report these errors through the
[issues](https://github.com/nhs-bnssg-analytics/fpt_analysis/issues)
page.
