#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Helper functions to support the pilot assessment
# written by clare betts january 2021
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process metadata file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "variable", "target", "Indicator", "Primary.goal", "natural.capital.framework", 
# "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend"
load_process_metadata <- function(dat){
  # store targets
  targets <- unique(dat[, c("variable", "target", "target_year", "target_trend")])
  
  # store thresholds
  thresholds <- dat[, c("variable", "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend")]
  
  goal.indicator.lookup <- unique(dat[, c("variable", "Indicator", "Primary.goal", "natural.capital.framework")]) 
  
  return(list(targets = targets,
              thresholds = thresholds, 
              goal.indicator.lookup = goal.indicator.lookup))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process raw data file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "value", "variable"
load_process_data <- function(dat){
  dat <- dat %>% 
    mutate(
      # value as numeric
      value = as.numeric(value),
      # variable as factor
      variable = as.factor(variable)
    ) %>%
    # get rid of NAs
    drop_na() %>%
    # get rid of unused factor levels
    droplevels() %>%
    # group by variable
    group_by(variable)
  
  variables <- levels(dat$variable)
  
  dat_list <- dat  %>%
    # split in to a list of data frames
    group_split() %>%
    # assign names
    purrr::set_names(variables)
  
  return(list(dat_list = dat_list,
              variables = variables))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01
# this function doesn't yet distinguish between those indicators which are traveling away 
# from their target because the target is already met, and those where the indicators are s
# imply moving in teh wrong direction
years_until_target_reached <- function(rate.of.change, temp_target, final.value){
  
  # if the trend is decreasing
  if (rate.of.change < 0){
    years <- case_when(
      # if target is greater than the final value
      # i.e. target met already
      temp_target > final.value ~ Inf,
      # everything else calculate number of years
      TRUE ~ abs(log(final.value/temp_target)/log(1 + rate.of.change)))
  }
  
  # if the trend is increasing
  if (rate.of.change > 0) {
    years <- case_when(
      # if the target is smaller than the final value 
      # i.e. target met already
      temp_target < final.value ~ Inf,
      # everything else calculate number of years
      TRUE ~ abs(log(final.value/temp_target)/log(1 + rate.of.change)))
  }
  # no condition for if trend == 0
  
  return(years)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do trend assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# x <- variables[1]
trend_assess_this <- function(x, term = c("short", "long"),
                              targets, 
                              thresholds, 
                              smoothed_trend){
  
  if (term == "long"){
    # test if there are enough years of data
    if(length(smoothed_trend[[x]]) >= long.term){
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(long.term) 
    } else {
      temp_dat <- {}
    }
  } 
  
  if(term == "short"){
    # test if there are enough years of data
    if(length(smoothed_trend[[x]]) >= short.term){
      temp_dat <- smoothed_trend[[x]] %>%
        tail(short.term)
      } else {
          temp_dat <- {}
        } 
    }
  
  final.value <- rev(temp_dat)[1]
  first.value <- temp_dat[1]
  
  rate.of.change <- ((final.value-first.value)/first.value)/length(temp_dat)
  
  # get threshold
  temp_threshold <- thresholds %>%
    dplyr::filter(variable == x) 
  
  target_trend <- temp_threshold$target_trend
  
  # select just thresholds
  temp_threshold <- temp_threshold %>%
    select(Threshold1, Threshold2, Threshold3, Threshold4) 
  
  # if target trend is to decrease then reverse the labels
  trend_labels <- c("strong decline", "moderate decline", "little change", "moderate improvement", "strong improvement") 
  
  if (target_trend == "increase") {
    trend_labels <- trend_labels
  } 
  if (target_trend == "decrease") {
    trend_labels <- rev(trend_labels)
  }
  
  # assign rate of change assessment
  # this seems inelegant, return to this when i have more time
  if (is_empty(rate.of.change)){
    first.value <- NA
    final.value <- NA
    rate.of.change <- NA
    category <- "unknown"
    
    } else {
      category <- case_when(rate.of.change < temp_threshold[1] ~ trend_labels[1],
                        rate.of.change >= temp_threshold$Threshold1 & rate.of.change < temp_threshold$Threshold2 ~ trend_labels[2],
                        rate.of.change >= temp_threshold$Threshold2 & rate.of.change < temp_threshold$Threshold3 ~ trend_labels[3],
                        rate.of.change >= temp_threshold$Threshold3 & rate.of.change < temp_threshold$Threshold4 ~ trend_labels[4],
                        rate.of.change > temp_threshold$Threshold4 ~ trend_labels[5]) 

  }
  # return result
  return(list(first.value = first.value,
              final.value = final.value,
              rate.of.change = rate.of.change,
              category = category))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do target assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# x <- variables[1]
target_assess_this <- function(x, 
                               targets, 
                               smoothed_trend){
  

  # test if there are enough years of data
  # currently uses 5 years of data
    if(length(smoothed_trend[[x]]) < 5){
      stop("Too few years of data for a target assessment")
    } else {
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(5) 
    }
  
  final.value <- rev(temp_dat)[1]
  first.value <- temp_dat[1]
  
  rate.of.change <- ((final.value-first.value)/first.value)/length(temp_dat)
  
  temp_target <- targets$target[targets$variable == x]
  
  temp_target_trend <- targets$target_trend[targets$variable == x]
  
  temp_target_year <- targets$target_year[targets$variable == x]
    
  years <- years_until_target_reached(rate.of.change, temp_target, final.value)
  
  category <- case_when(
    # already reached target & target met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value <= temp_target & temp_target_trend == "decrease" ~ "Target met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value >= temp_target & temp_target_trend == "increase" ~ "Target met",
    
    # already reached target & target not met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value <= temp_target & temp_target_trend == "increase" ~ "Target not met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value >= temp_target & temp_target_trend == "decrease" ~ "Target not met",
    
    
    # moving away from target
    rate.of.change < 0 & final.value < temp_target ~ "Insufficient  progress",
    rate.of.change > 0 & final.value > temp_target ~ "Insufficient  progress",
    
    # moving in right direction
    (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) < years ~ "Some progress towards target",
    (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) >= years ~ "Substantial progress"
      )

  return(list(first.value = first.value,
              final.value = final.value,
              rate.of.change = rate.of.change, 
              years = years, 
              category = category))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to do teh three assessments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

do_assessment <- function(variables, 
                          targets,
                          thresholds,
                          smoothed_trend){
  # a data frame to store assessment results in
  assessment.short <- data.frame("variable" = variables,
                                 "first.value" = NA,
                                 "final.value" = NA,
                                 "rate.of.change" = NA, 
                                 "category" = NA)
  
  assessment.short <- left_join(assessment.short, targets)
  
  assessment.long <- assessment.target <- assessment.short
  
  # i <- 1
  for (i in 1:length(variables)){
    # Do a long term assessment on all variables
    long.term.assessment <- trend_assess_this(variables[i], 
                                              term = "long",
                                              targets = targets, 
                                              thresholds = thresholds, 
                                              smoothed_trend = smoothed_trend)
    
    assessment.long$first.value[i] <- long.term.assessment$first.value
    assessment.long$final.value[i] <- long.term.assessment$final.value
    assessment.long$rate.of.change[i] <- long.term.assessment$rate.of.change
    assessment.long$category[i] <- long.term.assessment$category

    # Do a short term assessment on all variables
    short.term.assessment <- trend_assess_this(variables[i], 
                                               term = "short",
                                               targets = targets, 
                                               thresholds = thresholds, 
                                               smoothed_trend = smoothed_trend)
    
    assessment.short$first.value[i] <- short.term.assessment$first.value
    assessment.short$final.value[i] <- short.term.assessment$final.value
    assessment.short$rate.of.change[i] <- short.term.assessment$rate.of.change
    assessment.short$category[i] <- short.term.assessment$category

    # Do an assessment against targets
    trend.assessment <- target_assess_this(variables[i],
                                           targets = targets, 
                                           smoothed_trend = smoothed_trend)
    
    assessment.target$first.value[i] <- trend.assessment$first.value
    assessment.target$final.value[i] <- trend.assessment$final.value
    assessment.target$rate.of.change[i] <- trend.assessment$rate.of.change
    assessment.target$category[i] <- trend.assessment$category
    assessment.target$years[i] <- trend.assessment$years
  }     
  
  return(list(assessment.long = assessment.long,
              assessment.target = assessment.target,
              assessment.short = assessment.short))
}

