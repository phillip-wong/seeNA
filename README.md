# seeNA

<br>
`seeNA` is a `R` package developed to explore and impute missing values, allowing for diverse exploration and conservation of data sets.
<br>
`seeNA` grants users access to functions that help visualize and recode data sets with missing values. Some glaring issues when confronted with a new data set is
the ability to clean and wrangle the data set. Many times when starting projects, analysts are encouraged to delete missing values and observations from a data 
set without the chance to explore the mysteries those hidden values can have. The tools and functions within the `seeNA` package are designed for ease of use 
with any data frame or tibble, allowing the curious analyst to view their data from new perspectives.

## Installation
<br>

In order to install the package, please have `devtools` installed. Use the `install_github()` function and URL to install. See below for explicit instructions:
```
if(!require(devtools)){
  install.packages("devtools")
}
devtools::install_github("phillip-wong/seeNA")
```

