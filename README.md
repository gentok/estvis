# <code>estvis</code> Package
Developed by Gento Kato (Last Updated: 04/18/2019) 

## Description

Helps to visualize the estimates from various models.

## Installation

<code>devtools::install_github("gentok/estvis")</code>

## Main Functions

* <code>plot_coef</code> Draw coefficients plot from either model object or the coefficients matrix.
* <code>table_coef</code> Export coefficients tables from model output (uses <code>[texreg](https://cran.r-project.org/web/packages/texreg/index.html)</code> behind).
* <code>simu_pred</code> Export simulated predictions from model object.
* <code>plot_simu</code> Draw prediction plot from either <code>simu_pred</code> output or the matrix with coefficients and confidence intervals.
* <code>simu_interact</code> Export conditional coefficients from model object that includes interaction terms.
* <code>plot_interact</code> Draw condition effect interaction plot from either <code>simu_interact</code> model ouput or the matrix.

## Supplemental Functions

* <code>plot_footnote</code> Add footnotes to the ggplot object.
* <code>plot_save</code> Save ggplot/gtable object to file.
* <code>extract_gofchr</code> Extract Goodness-of-Fit statistics from model object (uses <code>[texreg](https://cran.r-project.org/web/packages/texreg/index.html)</code> behind).
* <code>matrix_coefci</code> Export matrix of coefficients and confidence interval from model object.
* <code>matrix_coeftest</code> Export matrix of coefficients, Standard Errors and P-values from model object.
* <code>ilogit</code> Calculate inverse logit (borrowed from <code>[faraway](https://cran.r-project.org/web/packages/faraway/index.html)</code> package).

*more will be added in the future...*

## Updates Log

* 08/21/2019 Version 0.1.08 (small bug fix in table_coef)
* 07/28/2019 Version 0.1.07 (change simu_pred CI from one-sided to two-sided)
* 07/24/2019 Version 0.1.06 (bug fixes in plot_coef)
* 04/18/2019 Version 0.1.05 (facet.x.scale argument added to plot_coef)
* 04/17/2019 Version 0.1.04 (adjustment in ilogit function to avoid NaN)
* 04/12/2019 Version 0.1.03 (bug fixes in plot_simu)
* 04/12/2019 Version 0.1.02 (bug fixes in plot_coef and table_coef)
* 03/13/2019 Version 0.1.01 (small bug fixes)
* 02/13/2019 Version 0.1.0 released
