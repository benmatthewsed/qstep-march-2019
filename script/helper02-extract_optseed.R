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

