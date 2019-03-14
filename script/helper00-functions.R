

# write Mplus syntax

mplus_lcgm <- function(df, k_classes, starts_1, starts_2){
  # from Mplus users guide example 8.9
  lcgm <- mplusObject(
    TITLE = glue::glue("LCGM;"),
    VARIABLE = glue::glue("USEVARIABLES ARE sex y16-y25 freq;
                          CLASSES = c ({k_classes});
                          FREQWEIGHT = freq;
                          CATEGORICAL ARE y16-y25;"),
    ANALYSIS = glue::glue("
                          TYPE = MIXTURE;
                          STARTS = {starts_1} {starts_2};
                          PROCESSORS = 3;"),
    MODEL = "
    %OVERALL%
    i s | y16@0.0 y17@0.1 y18@0.2 y19@0.3 y20@0.4 y21@0.5
    y22@0.6 y23@0.7 y24@0.8 y25@0.9;
    c ON sex",
    OUTPUT = "TECH1 RESIDUAL;",
    PLOT = "
    TYPE IS PLOT3;
    SERIES IS y16-y25 (s);",
    rdata = df)
}

mplus_gmm <- function(df, k_classes, starts_1, starts_2){
  # from Mplus users guide example 8.1
  gmm <- mplusObject(
    TITLE = glue::glue("GMM;"),
    VARIABLE = glue::glue("USEVARIABLES ARE sex y16-y25 freq;
                          CLASSES = c ({k_classes});
                          FREQWEIGHT = freq;
                          CATEGORICAL ARE y16-y25;"),
    ANALYSIS = glue::glue("
                          ALGORITHM = INTEGRATION;
                          TYPE = MIXTURE;
                          STARTS = {starts_1} {starts_2};
                          PROCESSORS = 3;"),
    MODEL = "
    %OVERALL%
    i s | y16@0.0 y17@0.1 y18@0.2 y19@0.3 y20@0.4 y21@0.5
    y22@0.6 y23@0.7 y24@0.8 y25@0.9;
    c ON sex",
    OUTPUT = "TECH1 RESIDUAL;",
    rdata = df)
}


tidy_model_fit <- function(model){
  tibble(model_type = model$results$input$title,
         k = nrow(model$results$class_counts$modelEstimated),
         AIC = model$results$summaries$AIC,
         BIC = model$results$summaries$BIC,
         aBIC = model$results$summaries$aBIC,
         entropy = model$results$summaries$Entropy,
         LL = model$results$summaries$LL)
}


export_trajs <- function(mplus_model, k){
  
 
  name_k_by_number <- function(k){
    k_rep <- seq(1, k)
    class <- tibble(class = rep(glue::glue("{k_rep}"), length(6:22)*2))
    class <- class %>% arrange(class)
    class <- as.vector(class$class)
    return(class)
  }
  
  extract_class_sizes <- function(model){
    
    estimated <- model$results$class_counts$`modelEstimated`
    
    estimated <- 
      estimated %>% 
      arrange(desc(count)) %>% 
      mutate(ordered_class = as.character(row_number()),
             measure = "model_estimated",
             class = as.character(class))
    
    most_likely <- model$results$class_counts$`mostLikely`
    
    most_likely <- 
      most_likely %>% 
      arrange(desc(count)) %>% 
      mutate(ordered_class = as.character(row_number()),
             measure = "most_likely",
             class = as.character(class))
    
    results <- bind_rows(estimated, most_likely)
    return(results)
  }
  
  
  trajs <- as_tibble(mplus_model[["results"]][["parameters"]]$probability.scale)
  trajs$est_se <- as.character(trajs$est_se)
  
  trajs <- 
    trajs %>% 
    mutate(k = name_k_by_number(k))
  
  # reordering based on class size
  
  class_size <- 
    extract_class_sizes(mplus_model)
  
  
  trajs <- 
    left_join(trajs, class_size, by = c("k" = "class")) %>% 
    rename(class = k)
  
  return(trajs)
}

most_likely_class <- function(mplus_model){
  mplus_model$results$class_counts$mostLikely
}



# extract class sizes -----------------------------------------------------


extract_class_sizes <- function(model){
  
  estimated <- model$results$class_counts$`modelEstimated`
  
  estimated <- 
    estimated %>% 
    arrange(desc(count)) %>% 
    mutate(ordered_class = as.character(row_number()),
           measure = "model_estimated",
           class = as.character(class))
  
  most_likely <- model$results$class_counts$`mostLikely`
  
  most_likely <- 
    most_likely %>% 
    arrange(desc(count)) %>% 
    mutate(ordered_class = as.character(row_number()),
           measure = "most_likely",
           class = as.character(class))
  
  results <- bind_rows(estimated, most_likely)
  return(results)
}


# get LL warnings ---------------------------------------------------------


LL_warning <- function(mplus_object){
  if(any(str_detect(mplus_object$results$warnings, "BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED"))) TRUE
  else FALSE
}



# bootstrapping under 5 frequencies ---------------------------------------

replace_na <- function(var){
  if(is.na(var)) sample(x = seq(2, 5), size = 1, prob = c(0.05, 0.25, 0.3, 0.4))
  else(var)
}



# extract starts from model object ----------------------------------------

extract_starts <- function(mplus_model) {
  starts <- str_sub(mplus_model$ANALYSIS, start = 17, end = str_length(soi_nest$k_3[[1]]$ANALYSIS) - 17)
  return(starts)
}




# lcgm all data -----------------------------------------------------------

mplus_lcgm_all <- function(df, k_classes, starts_1, starts_2){
  # from Mplus users guide example 8.9
  lcgm <- mplusObject(
    TITLE = glue::glue("LCGM;"),
    VARIABLE = glue::glue("USEVARIABLES ARE sex y16-y25 freq birth_year;
                          CLASSES = c ({k_classes});
                          FREQWEIGHT = freq;
                          CATEGORICAL ARE y16-y25;"),
    ANALYSIS = glue::glue("
                          TYPE = MIXTURE;
                          STARTS = {starts_1} {starts_2};
                          PROCESSORS = 3;"),
    MODEL = "
    %OVERALL%
    i s | y16@0.0 y17@0.1 y18@0.2 y19@0.3 y20@0.4 y21@0.5
    y22@0.6 y23@0.7 y24@0.8 y25@0.9;
    c ON sex;
    c ON birth_year;",
    OUTPUT = "TECH1 RESIDUAL;",
    PLOT = "
    TYPE IS PLOT3;
    SERIES IS y16-y25 (s);",
    rdata = df)
}



# LRT ---------------------------------------------------------------------



mplus_lcgm_lmr <- function(df,
                           k_classes,
                           ref_model,
                           k_1_starts_1,
                           k_1_starts_2,
                           stscale = 5){
  # from Mplus users guide example 8.9
  
  optseed <- extract_optseed(ref_model)
  
  lcgm <- mplusObject(
    TITLE = glue::glue("LCGM;"),
    VARIABLE = glue::glue("USEVARIABLES ARE sex y16-y25 freq;
                          CLASSES = c ({k_classes});
                          FREQWEIGHT = freq;
                          CATEGORICAL ARE y16-y25;"),
    ANALYSIS = glue::glue("
                          TYPE = MIXTURE;
                          STARTS = 0;
                          OPTSEED = {optseed};
                          K-1STARTS = {k_1_starts_1} {k_1_starts_2};
                          STSCALE = {stscale};
                          PROCESSORS = 3;"),
    MODEL = "
    %OVERALL%
    i s | y16@0.0 y17@0.1 y18@0.2 y19@0.3 y20@0.4 y21@0.5
    y22@0.6 y23@0.7 y24@0.8 y25@0.9;
    c ON sex",
    OUTPUT = "TECH11;",
    rdata = df)
  
  return(lcgm)
}



# classifying trajectories ------------------------------------------------


low <- function(x){
  x <= 0.333
}

medium <- function(x){
  x > 0.333 & x <= 0.667
}

high <- function(x){
  x > 0.667
}



# classify trajectories ---------------------------------------------------


classify_trajectories <- function(df){
  
  ordered_trajs <- 
    c(  
      "high-high-high",
      "high-high-mid",
      "high-high-low",
      "high-mid-high",
      "high-mid-mid",
      "high-mid-low",
      "high-low-high",
      "high-low-mid",
      "high-low-low",
      "medium-high-high",
      "medium-high-mid",
      "medium-high-low",
      "medium-mid-high",
      "medium-mid-mid",
      "medium-mid-low",
      "medium-low-high",
      "medium-low-mid",
      "medium-low-low",
      "low-high-high",
      "low-high-mid",
      "low-high-low",
      "low-mid-high",
      "low-mid-mid",
      "low-mid-low",
      "low-low-high",
      "low-low-mid",
      "low-low-low")
  
  df <- 
  df %>% 
    filter(measure == "model_estimated",
           category == 2) %>% 
    select(birth_year, param, est, class, data) %>%
    spread(param, est) %>% 
    mutate(class_description = case_when(
      low(Y16) & low(Y20) &  low(Y25) ~ "low-low-low",
      low(Y16) & low(Y20) &  medium(Y25) ~ "low-low-mid",
      low(Y16) & low(Y20) &  high(Y25) ~ "low-low-high",
      low(Y16) & medium(Y20) &  low(Y25) ~ "low-mid-low",
      low(Y16) & medium(Y20) &  medium(Y25) ~ "low-mid-mid",
      low(Y16) & medium(Y20) &  high(Y25) ~ "low-mid-high",
      low(Y16) & high(Y20) &  low(Y25) ~ "low-high-low",
      low(Y16) & high(Y20) &  medium(Y25) ~ "low-high-mid",
      low(Y16) & high(Y20) &  high(Y25) ~ "low-high-high",
      medium(Y16) & low(Y20) &  low(Y25) ~ "medium-low-low",
      medium(Y16) & low(Y20) &  medium(Y25) ~ "medium-low-mid",
      medium(Y16) & low(Y20) &  high(Y25) ~ "medium-low-high",
      medium(Y16) & medium(Y20) &  low(Y25) ~ "medium-mid-low",
      medium(Y16) & medium(Y20) &  medium(Y25) ~ "medium-mid-mid",
      medium(Y16) & medium(Y20) &  high(Y25) ~ "medium-mid-high",
      medium(Y16) & high(Y20) &  low(Y25) ~ "medium-high-low",
      medium(Y16) & high(Y20) &  medium(Y25) ~ "medium-high-mid",
      medium(Y16) & high(Y20) &  high(Y25) ~ "medium-high-high",
      high(Y16) & low(Y20) &  low(Y25) ~ "high-low-low",
      high(Y16) & low(Y20) &  medium(Y25) ~ "high-low-mid",
      high(Y16) & low(Y20) &  high(Y25) ~ "high-low-high",
      high(Y16) & medium(Y20) &  low(Y25) ~ "high-mid-low",
      high(Y16) & medium(Y20) &  medium(Y25) ~ "high-mid-mid",
      high(Y16) & medium(Y20) &  high(Y25) ~ "high-mid-high",
      high(Y16) & high(Y20) &  low(Y25) ~ "high-high-low",
      high(Y16) & high(Y20) &  medium(Y25) ~ "high-high-mid",
      high(Y16) & high(Y20) &  high(Y25) ~ "high-high-high",
      TRUE ~ NA_character_
    ),
    class_description = as.factor(class_description)) %>% 
    mutate(
      class_description = fct_relevel(class_description, ordered_trajs))
  
  return(df)
}



# plot trajectories -------------------------------------------------------

# uses interaction() to get combinations of k and data

plot_trajectories <- function(df){
  df %>% 
    filter(measure == "model_estimated") %>% 
    ggplot(aes(x = param, y = est, group = interaction(data, class_description))) +
    geom_ribbon(aes(ymax = ci_high,
                    ymin = ci_low,
                    fill = class_description),
                alpha = 0.25) +
    geom_line(aes(linetype = data,
                  colour = class_description,
                  size = proportion)) +
    scale_size(range = c(0.1, 1)) +
    facet_wrap(vars(birth_year))
}



# export_censored_boot_results --------------------------------------------


export_combine_trajectories <- function(df, k_var, k){
  

  k_var2 <- enquo(k_var)
  
  
  
  censored_results <- 
    df %>% 
    mutate(trajectories = map2(!! k_var2, k, export_trajs)) %>% 
    select(birth_year, trajectories) %>% 
    unnest() %>% 
    mutate(data = "censored")
  
  boot_results <- 
    df %>% 
    mutate(trajectories = map2(!! k_var2, k, export_trajs)) %>% 
    select(birth_year, trajectories) %>% 
    unnest() %>% 
    mutate(data = "boot")
  
  # combining
  results <- bind_rows(censored_results, boot_results)
  
  return(results)
}


# testing equality between k and k-1 LL -----------------------------------



ll_test <- function(k_lrt_model, k_1_model){
  
  all.equal(
    k_1_model$results$summaries$LL,
    k_lrt_model$results$summaries$T11_KM1LL
  )
  
}



# extacting tech11 p-values -----------------------------------------------

tech_11_pval <- function(tech11_model){

tibble::tribble(~test, ~p_value,
        "VLMR", tech11_model$results$summaries$T11_VLMR_PValue,
        "LMR_adjusted", tech11_model$results$summaries$T11_LMR_PValue)

}



# extract OPTSEED ---------------------------------------------------------


library(tidyverse)


# helper one - parse_into_sections ----------------------------------------


parse_into_sections <- function (outfiletext) { 
  # taken from MplusAutomation
  headers <- c("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS", 
               "SUMMARY OF DATA", "SUMMARY OF DATA FOR THE FIRST DATA SET", 
               "SUMMARY OF DATA FOR THE FIRST REPLICATION", "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION", 
               "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST DATA SET", 
               "SUMMARY OF MISSING DATA PATTERNS", "SUMMARY OF CATEGORICAL DATA PROPORTIONS", 
               "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION", 
               "COVARIANCE COVERAGE OF DATA", "UNIVARIATE SAMPLE STATISTICS", 
               "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
               "THE MODEL ESTIMATION TERMINATED NORMALLY", "SAMPLE STATISTICS", 
               "SAMPLE STATISTICS FOR THE FIRST REPLICATION", "RESULTS FOR BASIC ANALYSIS", 
               "CROSSTABS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES", 
               "SUMMARY OF CENSORED LIMITS", "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES", 
               "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES", 
               "TESTS OF MODEL FIT", "MODEL FIT INFORMATION", "MODEL FIT INFORMATION FOR .*", 
               "CLASSIFICATION QUALITY", "SUMMARY OF MODEL FIT INFORMATION", 
               "RESULTS FOR EXPLORATORY FACTOR ANALYSIS", "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES", 
               "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS", 
               "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN", 
               "Average Latent Class Probabilities for Most Likely Latent Class Pattern \\(Row\\)", 
               "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL", 
               "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE", 
               "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP", 
               "Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)", 
               "Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)", 
               "Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)", 
               "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)", 
               "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)", 
               "MODEL RESULTS", "MODEL RESULTS FOR .*", "LOGISTIC REGRESSION ODDS RATIO RESULTS", 
               "RESULTS IN PROBABILITY SCALE", "IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC METRIC", 
               "IRT PARAMETERIZATION IN TWO-PARAMETER PROBIT METRIC", 
               "IRT PARAMETERIZATION", "BRANT WALD TEST FOR PROPORTIONAL ODDS", 
               "BETWEEN-LEVEL FACTOR SCORE COMPARISONS", "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION", 
               "LATENT CLASS ODDS RATIO RESULTS", "LOGRANK OUTPUT", 
               "STANDARDIZED MODEL RESULTS", "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER \\d+", 
               "R-SQUARE", "QUALITY OF NUMERICAL RESULTS", "QUALITY OF NUMERICAL RESULTS FOR .*", 
               "TECHNICAL OUTPUT", "TECHNICAL \\d+ OUTPUT", "TECHNICAL \\d+ OUTPUT FOR .*", 
               "TECHNICAL 5/6 OUTPUT", "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", 
               "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES", 
               "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS \\(CAUSALLY-DEFINED EFFECTS\\)", 
               "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", 
               "CONFIDENCE INTERVALS OF MODEL RESULTS", "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS", 
               "CREDIBILITY INTERVALS OF MODEL RESULTS", "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS", 
               "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS", 
               "CONFIDENCE INTERVALS IN PROBABILITY SCALE", "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", 
               "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,", 
               "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", 
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING POSTERIOR PROBABILITY-BASED", 
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE", 
               "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE 3-STEP PROCEDURE", 
               "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES", 
               "THE FOLLOWING DATA SET\\(S\\) DID NOT RESULT IN A COMPLETED REPLICATION:", 
               "RESIDUAL OUTPUT", "MODEL MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES", 
               "SUMMARIES OF PLAUSIBLE VALUES \\(N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS\\)", 
               "SUMMARY OF PLAUSIBLE STANDARD DEVIATION \\(N = NUMBER OF OBSERVATIONS\\)", 
               "Available post-processing tools:", "FACTOR SCORE INFORMATION \\(COMPLETE DATA\\)", 
               "SUMMARY OF FACTOR SCORES", "PLOT INFORMATION", "SAVEDATA INFORMATION", 
               "RESULTS SAVING INFORMATION", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", 
               "DIAGRAM INFORMATION", "Beginning Time:\\s*\\d+:\\d+:\\d+", 
               "MUTHEN & MUTHEN")
  headerRegexpr <- paste("(", paste(gsub("(.*)", "^\\\\s*\\1\\\\s*$", 
                                         headers, perl = TRUE), sep = "", collapse = "|"), ")", 
                         sep = "")
  headerLines <- grep(headerRegexpr, outfiletext, perl = TRUE)
  attr(outfiletext, "headerlines") <- headerLines
  return(outfiletext)
}



# helper two - extract model object filename ------------------------------


# extract filename from model object

extract_mplus_out <- function(model_object){
  
  # take the filename and directory from the model object  
  
  filename <- attr(model_object$results, "filename")[[1]]
  
  # and now only the file name so it works with here::here
  
  filename <- 
    str_split(filename,
              pattern = "/")[[1]][[6]]
  
  # read in data using filename
  rawtext <- readLines(here::here("mplus", "boot", glue::glue("{filename}")))
  
  return(rawtext)
}


# helper 3 - extract optseed from text ------------------------------------


get_optseed <- function(section_text){
  
  model_text <- tibble::tibble(text = section_text)
  
  filter_lines <- c("RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
                    "THE MODEL ESTIMATION TERMINATED NORMALLY")
  
  row_numbers <- 
    model_text %>% 
    mutate(row_numbers = row_number()) %>% 
    filter(text %in% filter_lines)
  
  model_text %>% 
    mutate(row_n = row_number()) %>% 
    filter(row_n >= min(row_numbers$row_numbers) &
             row_n <= max(row_numbers$row_numbers)) %>% 
    mutate(row_n = row_number()) %>% 
    filter(row_n == 5) %>% 
    mutate(text = str_squish(text)) %>% 
    mutate(text = str_split(text, " ")) %>% 
    unnest() %>% 
    mutate(row_n = row_number()) %>% 
    filter(row_n == 2) %>% 
    pull(text)
}



# combine into single function --------------------------------------------

extract_optseed <- function(mplus_model_object){
  raw_text <- extract_mplus_out(mplus_model_object)
  model_text <- parse_into_sections(raw_text)
  optseed <- get_optseed(model_text)
  return(optseed)
}


