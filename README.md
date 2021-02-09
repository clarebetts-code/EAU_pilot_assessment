# OIFassessment
Contains the skeleton code necessary to run the pilot 25 Year Plan Outcome Indicators Framework Assessment.

# Installation

Run this code in r, replacing the username and password with your credentials.


```r
#Install the package "devtools" if you don't already have it
if (!require("devtools")) install.packages("devtools")

#Enter your username
username<-"your demeter username"

#Enter your password
password<-"your demeter password"

#Generate proxy address
proxy_address<-paste0("http://",username,":",password,"@148.253.235.216:80")

#Create environment variable
Sys.setenv(http_proxy=proxy_address, https_proxy=proxy_address)
  
#Install package
devtools::install_github("https://github.com/clarebetts-code/EAU_pilot_assessment")
```

# Usage

## User set some inputs

```r
long_term <- 10
short_term <- 5
target_term <- 5 
```

## Loading & processing data
outputs are assigned to the global environment

```r
load_process_metadata("data\\metadata.csv") 

load_process_data("data\\25.year.data.csv") 
```

## Calculating smoothed trend
Use a loess smoother to model value ~ time, and extract predicted values. Some warnings are produced 
because some indicators have too few data.
User needs to make an informed choice about how appropriate it is to do trend assessment, a visual 
assessment of appropriateness can be done by using the save_smoothed_trend() function.

This function saves plots of each trend to file:
```r
save_smoothed_trend(dat_list)
```
This function retrieves the smoothed trends and returns them as a list:
```r
smoothed_trend <- get_smoothed_trend(dat_list)
```

## Do assessment and save output
```r
do_assessment(
  variables = variables,
  targets = targets,
  thresholds = thresholds,
  smoothed_trend = smoothed_trend,
  short_term = short_term,
  long_term = long_term,
  target_term = target_term
) %>%
  purrr::map2(.y = names(.),
              ~readr::write_csv(.x, 
                                file = paste0("data\\", .y, ".csv")))
```

## Visualise result

Set up a dataframe containing teh colours we want to use

```r
colour_lookup <- data.frame(trend = c("Strong improvement",
                                      "Moderate improvement",
                                      "Little change",
                                      "Moderate decline", 
                                      "Strong decline",
                                      "Unknown",
                                      "Target met",
                                      "Substantial progress",
                                      "Some progress towards target",
                                      "Insufficient  progress",
                                      "No target"),
                            cols = c("#006164", "#57C4AD", "#E6E1BC", "#EDA247", "#DB4325", "grey","#006164", "#006164", "#57C4AD",  "#DB4325", "grey")) %>%
  #specify factors with a set order
  mutate(trend = factor(trend, levels = trend),
         cols = factor(cols, levels = unique(cols)))
```

Summarise by primary goal of Clean Air
```r
visualise_assessment(classification = "Primary.goal", 
                     group = "Clean air",
                     x = assessment_long,
                     myLab = "Long term")
```

Summarise indicator A1
```r
visualise_assessment(classification = "Indicator", 
                     group = "A1",
                     x = assessment_target,
                     myLab = "Target assessment")
```

Summarise by natural capital framework of Pressure
```r
visualise_assessment(classification = "natural.capital.framework", 
                     group = "Pressure",
                     x = assessment_target,
                     myLab = "Target assessment")
```
