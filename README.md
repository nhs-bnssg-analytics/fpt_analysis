
# Spatio-temporal prediction of demand and capacity-related performance

The goal of this repository is to explore the possibility of modelling
high level NHS performance at Integrated Care System geography using
publicly available data.

**NOTE, this is currently work in progress**

## Using this repository

The repository is managed using `renv`, so the aim is for it to be
reproducible by anyone with access to the same versions of R and RStudio
that I use. After cloning and opening the project for the first time,
run `renv::restore()` in the console to install all the versions of the
packages used in this work. You may need to restart RStudio after doing
this.

The file `R/02_data.R` should download and clean all of the data files
from the internet into a [tidy
format](https://tidyr.tidyverse.org/articles/tidy-data.html). The files
will be summarised into single files in a folder called `data`. This
step takes up to 30 minutes to run as the source files are numerous and
some are quite large. The source files are stored within the `data-raw`
folder to allow users to refer back to them.

A lot of the source files are spreadsheets, and generally they are
created manually by different organisations and uploaded to a server for
public use. As a result, filenames, sheet names and sheet formatting can
be inconsistent. The code in this project attempts to negate the
inconsistency, but there will be occasions where an unpredictable manual
step has occurred that the code hasn’t accounted for, and will result in
an error. Please report these errors through the
[issues](https://github.com/nhs-bnssg-analytics/d_and_c/issues) page.

## Limitations

There are limitations with the data processing related to:

### Geographies

Data are published at different geographies. While some geographies map
coterminously with Integrated Care System geographies, some approximate
methods of aggregation are needed for the ones that don’t.

### Time periods of published data

Data are published at different time periods and frequencies. Data are
generally published based on regulatory requirements. Metrics can change
over time as well. Therefore, metrics may exist for a few years and then
stop being published. Equally, metrics may only be introduced very
recently.

### Frequencies of published data

Metrics can be published by month, quarter, and year (both financial or
calendar). The outputs of the project are intended to inform long term
strategic planning, so the shorter frequency metrics are aggregated up
to an annual time period to allow for annual predictions.

### Comparable metrics

Data are generally published as raw figures. Without context, it is
difficult to evaluate whether a figure is high or low. For added
context, the metric needs to be calculated with some sort of
denominator. This enables comparison with other areas. The choice of
denominator determines the metric value and it can have a large
influence on how the metric differs between areas. The description of
the metric, along with the denominator and numerator description, can be
found in the
[configuration-table.csv](https://github.com/nhs-bnssg-analytics/d_and_c/blob/master/data/configuration-table.csv)
file.

### Interpretation

There are challenges around interpretation of metrics. The value of a
metric can be influenced by many factors. To ensure that the outputs are
practical, these factors need to be considered at all stages to ensure
the metrics are most useful to the end user.
