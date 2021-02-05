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
# foo <- "metadata.csv"


#' @title load and process metadata
#' load_porcess_metadata takes an input csv and parses it to return three
#' dataframes, targets, thresholds and goal.indicator.lookup. These outputs are
#' assigned directly to the global environment.
#'
#' @param filename the name of the file which the data are to be read from.
#' Each row of the table appears as one line of the file. If it does not
#' contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#' 
#' @return Three dataframes.
#' @export
#'
load_process_metadata <- function(filename){
  
  # read in data
  dat <- read.csv(filename)
  
  # store targets
  targets <- unique(dat[, c("variable", "target", "target_year", "target_trend")])
  
  # store thresholds
  thresholds <- dat[, c("variable", "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend")]
  
  goal.indicator.lookup <- unique(dat[, c("variable", "Indicator", "Primary.goal", "natural.capital.framework")]) 
  
  # assign objects to global environment
  assign("targets", targets, envir = .GlobalEnv)
  assign("thresholds", thresholds, envir = .GlobalEnv)
  assign("goal.indicator.lookup", goal.indicator.lookup, envir = .GlobalEnv)
  
  #return(NULL)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process raw data file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "value", "variable"
# foo <- "25.year.data.csv"
load_process_data <- function(foo){
  
  # read in data
  dat <- read.csv(foo)
  
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
  
  # assign objects to global environment
  assign("dat_list", dat_list, envir = .GlobalEnv)
  assign("variables", variables, envir = .GlobalEnv)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01

years_until_target_reached <- function(rate.of.change, final.value, temp_target, temp_target_trend){
  
    years <- case_when(
      # if the desired trend is decreasing, and target is greater than the final value
      # i.e. target met already
      temp_target_trend == "decrease" & temp_target > final.value ~ 0,
      # if the trend is increasing, and the target is smaller than the final value 
      # i.e. target met already
      temp_target_trend == "increase" & temp_target < final.value ~ 0,
      
      # if target is less than final value, but rate of change is positive
      # i.e. moving away from target
      temp_target < final.value & rate.of.change > 0 ~ Inf,
      # if target is greater than final value, but rate of change is negative
      # i.e. moving away from target
      temp_target > final.value & rate.of.change < 0 ~ Inf,
      
      # everything else calculate number of years
      TRUE ~ abs(log(final.value/temp_target)/log(1 + rate.of.change))
    )

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
  trend_labels <- c("Strong decline", "Moderate decline", "Little change", "Moderate improvement", "Strong improvement") 
  
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
    category <- "Unknown"
    
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
  
  first.value <- temp_dat[1]
  final.value <- rev(temp_dat)[1]
  
  rate.of.change <- ((final.value-first.value)/first.value)/length(temp_dat)
  
  temp_target <- targets$target[targets$variable == x]
  
  temp_target_trend <- targets$target_trend[targets$variable == x]
  
  temp_target_year <- targets$target_year[targets$variable == x]
    
  years <- years_until_target_reached(rate.of.change, final.value, temp_target, temp_target_trend)
 
  category <- case_when(
    # if no target exists then years == NA
    is.na(years) ~ "No target",
    
    # already reached target value & year & target met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value <= temp_target & temp_target_trend == "decrease" ~ "Target met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value >= temp_target & temp_target_trend == "increase" ~ "Target met",
    
    # already reached target value & year & target not met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value <= temp_target & temp_target_trend == "increase" ~ "Target not met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final.value >= temp_target & temp_target_trend == "decrease" ~ "Target not met",
    
    # already passed target
    temp_target_trend == "decrease" & final.value <= temp_target ~ "Target met",
    temp_target_trend == "increase" & final.value >= temp_target ~ "Target met",
    
    # moving away from target
    rate.of.change < 0 & final.value < temp_target ~ "Insufficient  progress",
    rate.of.change > 0 & final.value > temp_target ~ "Insufficient  progress",
    
    # moving in right direction
    # if target will be missed by more than 5 years
    years > (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) + 5 ~ "Insufficient  progress",
    # if target will be missed but will reach it within 5 years of target year
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) + 5 ~ "Some progress towards target",
    # if target will be reached before the target year
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) ~ "Substantial progress"
      )

  return(list(first.value = first.value,
              final.value = final.value,
              rate.of.change = rate.of.change, 
              years = years, 
              category = category))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to do the three assessments
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to build the assessment table which the visualisations are built from
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- temp
# cols <- colour.lookup
assessment.table.builder <- function(x, cols = colour.lookup){
  
  # count the number of variables in each category
  ass.tab <- reshape::melt(table(x$category)/sum(table(x$category))) %>%
    dplyr::select(
      trend = Var.1,
      value
    ) %>%
    # only over zero
    dplyr::filter(
      value > 0
    ) %>%
    left_join(., cols) 
  
  # get trend factor levels in the correct order
  ass.tab$trend <- factor(ass.tab$trend,
                          levels = levels(cols$trend))
  ass.tab$cols <- factor(ass.tab$cols,
                          levels = levels(cols$cols))
  
  return(droplevels(ass.tab))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to convert the assessment table to a plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assessment.plot <- function(x){
  ggplot(x, aes(x = 1, y = value, fill = trend)) +
    geom_bar(position="stack", stat="identity") + 
    scale_y_continuous(labels = scales::percent) + 
    scale_fill_manual(values = levels(x$cols)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 25),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "") + 
    geom_text(aes(label = trend), 
              size = 4, 
              position = position_stack(vjust = 0.5)) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment.target
# myLab <- "Long term"
# classification <- "Primary.goal"
# group <- "Clean air"
visualise_assessment <- function(classification = "Primary.goal", 
                                 group = "Clean air",
                                 x,
                                 myLab = "assessment"){
  
  temp <- data.frame(classification = x[classification],
                     "category" = x$category) %>%
    dplyr::filter(.data[[eval(classification)]] == group)
  
  assessment.table <- assessment.table.builder(temp) 
  
  figure <- assessment.plot(assessment.table) +
    labs(title = group,
         x = myLab)
  
  print(figure)
  
}