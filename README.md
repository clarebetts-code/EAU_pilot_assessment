# EAU pilot assessment
Contains the skeleton code necessary to run the pilot indicators assessment.

Improvements needed:
1 - introduce a switch / catch for data which requires a transformation before smoothing
    CB 18/08/21 introduced a switch into the metadata ('transformation' field), if switched on (1) then a log10(x+1) transformation is applied.
    This is not ideal and not flexible enough, however I ran out of time.
    Ideally the user should be able to specify the transformation (easily done in the metadata), and the code correctly apply 
    the appropriate back transformation, but this is hard, especially to correctly back transform the se is hard. I have tried but don't
    have the time to see this through now.

2 - smooth only the data feeding into the assessment
    CB 18/08/21 done, uses either the entire time series or the length specified in years_long in the metadata

3 - 5 years of data for short term assessment, full time series for long term, both need overrides to adapt to the data set.
    CB 18/08/21 done, overrides available in the metadata file, defaults specified in line.

4 - Switches for the method - 1) total proportional change, 2) annual average change (total divided by the time series length),
    3) average annual change (rate of change applied annually)
    CB 25/08/21 done, switch added to do_assessment() function

5 - Use penultimate year not final year as final year of smoothed trend can be erratic (Buckland, S., Johnston, A., 2017.
    Monitoring the biodiversity of regions: key principles and possible pitfalls. Biol. Conserv. 214, 23-34.)
    CB 18/08/21 currently uses second and penultimate, set at the beginning of trend_assess_this() and target_assess_this()

6 - Use of second smoothed value instead of first? - requires some investigation first
    CB 18/08/21 currently uses second and penultimate, set at the beginning of trend_assess_this() and target_assess_this()

7 - weighting not yet implemented

# Installation

To install the package, run this code in r.

```r
devtools::install_github("https://github.com/clarebetts-code/EAU_pilot_assessment")
```

# Usage

## Loading & processing data
outputs are assigned to the global environment

```r
load_process_metadata("metadata.csv") 

load_process_data("25.year.data.csv") 
```

## Calculating smoothed trend
Use a loess smoother to model value ~ time, and extract predicted values. smoothing is done for only those data feeding into the assessment (i.e. either the entire time series, or the the time series set by the 'years_long' field in the metadata. The data are smoothed only once, and the relevant smoothed values extracted for the short and long term assessments.

Some warnings are produced because some indicators have too few data. User needs to make an informed choice about how appropriate it is to do trend assessment, a visual assessment of appropriateness can be done by using the save_smoothed_trend() function which saves an image of teh raw and smoothed data, and a .csv file containing the smoothed data. Particular care should be paid to time series which change dramatically, and might result in -ve smoothed values, in which case using a transformation should be considered.

This function retrieves the smoothed trends and returns them as a list:
```r
smoothed_trend <- get_smoothed_trend(dat_list, thresholds)
```

This function saves plots of each trend to file, as well as the smoothed data to a .csv file:
you need to run get_smoothed_trend() first
I haven't implemented back transforming the SE so this won't appear on the graphs of indicators which have been transformed
```r
save_smoothed_trend(x = dat_list, 
                    y = smoothed_trend,
                    filepath = "smoothed_trends\\")
```


## Do assessment and save output
By default the whole time series is used for the long term assessment, unless
an override is specified in the metadata file. similarly 6 years of change data are used
as the default for the short term unless an override is specified.
You need to run get_smoothed_trend() first
```r
do_assessment(
  variables = variables,
  targets = targets,
  thresholds = thresholds,
  smoothed_trend = smoothed_trend,
  trend_method = "total proportional change",
  target_method = "annual change"
) %>%
  purrr::map2(.y = names(.),
              ~readr::write_csv(.x, 
                                file = paste0("data\\", .y, ".csv")))
```

## Visualise result

Add some metadata to the assessment results:

```r
assessment_long <- assessment_long %>%
  left_join(goal_indicator_lookup)

assessment_short <- assessment_short %>%
  left_join(goal_indicator_lookup)

assessment_target <- assessment_target %>%
  left_join(goal_indicator_lookup)

```

Set up a data frame containing the colours we want to use

```r
colour_lookup <- data.frame(trend = c("Strong improvement",
                                      "Moderate improvement",
                                      "Little change",
                                      "Moderate decline", 
                                      "Strong decline",
                                      "Unknown",
                                      "Outcome achieved",
                                      "Substantial progress",
                                      "Some progress",
                                      "Insufficient  progress",
                                      "Not applicable"),
                            cols = c("#006164", 
                                     "#57C4AD", 
                                     "#E6E1BC", 
                                     "#EDA247", 
                                     "#DB4325", 
                                     "grey",
                                     "#006164", 
                                     "#57C4AD", 
                                     "#E6E1BC", 
                                     "#DB4325",
                                     "grey")) %>%
  #specify factors with a set order
  mutate(trend = factor(trend, levels = trend),
         cols = factor(cols, levels = unique(cols)))
```

Summarise by primary goal of Clean Air
```r
clean_air_indicators <- sort(unique(c(assessment_short$Indicator[assessment_short$Primary_goal == "Clean air"],
                                      assessment_short$Indicator[assessment_short$Secondary_goal == "Clean air"],
                                      assessment_short$Indicator[assessment_short$Third_goal == "Clean air"])))


assessment_short2 <- assessment_short %>%
  dplyr::filter(Indicator %in% clean_air_indicators) 

assessment_long2 <- assessment_long %>%
  dplyr::filter(Indicator %in% clean_air_indicators) 

assessment_target2 <- assessment_target %>%
  dplyr::filter(Indicator %in% clean_air_indicators) 

visualise_all_assessments(assessment_short2,
                          assessment_long2,
                          assessment_target2,
                          title = "Clean Air Indicators")
```

Generate a simplified results table:

```r
tab <- assessment_short2 %>%
  select(variable,
         "Short term trend" = category) %>%
  left_join(.,
            assessment_long2 %>%
              select(variable,
                     "Long term trend" = category)) %>%
  left_join(.,
            assessment_target2 %>%
              select(variable,
                     "Target assessment" = category)) %>%
  as.data.frame()
```


Summarise those which are connected to the Clean Air indicators

read in links:
```r
links <- read.csv("IndicatorData_Links.csv")

connected_indicators <- unique(c(links$FromIndicatorNo[links$ToIndicatorNo %in% clean_air_indicators],
                                 links$ToIndicatorNo[links$FromIndicatorNo %in% clean_air_indicators])) %>%
  setdiff(., 
          clean_air_indicators) %>%
  sort()
  
connected_indicators <- c("D2", "E3")
```

How many indicators involved in "clean air"

```r
unique(assessment_short$Indicator[assessment_short$Indicator %in% clean_air_indicators])
```

How many components

```r
unique(assessment_short$variable[assessment_short$Indicator %in% clean_air_indicators])
```

How many indicators connected to "clean air"

```r
unique(assessment_short$Indicator[assessment_short$Indicator %in% connected_indicators])
```

How many components

```r
unique(assessment_short$variable[assessment_short$Indicator %in% connected_indicators])
```

Visualise:
```r
assessment_short2 <- assessment_short %>%
  dplyr::filter(Indicator %in% connected_indicators) 

assessment_long2 <- assessment_long  %>%
  dplyr::filter(Indicator %in% connected_indicators)  

assessment_target2 <- assessment_target %>%
  dplyr::filter(Indicator %in% connected_indicators) 

visualise_all_assessments(assessment_short2,
                          assessment_long2,
                          assessment_target2,
                          title = "Clean air - connected indicators")
```

Results table:

```r
tab <- assessment_short2 %>%
  select(variable,
         "Short term trend" = category) %>%
  left_join(.,
            assessment_long2 %>%
              select(variable,
                     "Long term trend" = category)) %>%
  left_join(.,
            assessment_target2 %>%
              select(variable,
                     "Target assessment" = category)) %>%
  as.data.frame()
```
