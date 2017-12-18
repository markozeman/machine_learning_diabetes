preprocess <- function() {

  IDs_mapping <- read.table('dataset_diabetes/IDs_mapping.csv', header = TRUE, sep = ',');
  diabetes <- read.table('dataset_diabetes/diabetic_data.csv', header = TRUE, sep = ',');
  
  # check if all encounter ids are different
  length(unique(diabetes[, 1])) == length(diabetes[, 1]);
  
  # make rows unique according to patient number (so each row is different patient)
  diabetes <- diabetes[!duplicated(diabetes[, c("patient_nbr")]), ];
  
  diabetes$encounter_id <- NULL;
  diabetes$patient_nbr <- NULL;
  diabetes$weight <- NULL;
  diabetes$payer_code <- NULL;
  diabetes$medical_specialty <- NULL;
  diabetes$examide <- NULL;
  diabetes$citoglipton <- NULL;
  diabetes$glimepiride.pioglitazone <- NULL;
  
  # remove instances with unknown gender
  diabetes <- diabetes[diabetes$gender == 'Male' | diabetes$gender == 'Female', ];
  
  # change ? in race with Unknown 
  r <- vector();
  r[diabetes$race == 'AfricanAmerican'] <- "AfricanAmerican";
  r[diabetes$race == 'Asian'] <- "Asian";
  r[diabetes$race == 'Caucasian'] <- "Caucasian";
  r[diabetes$race == 'Hispanic'] <- "Hispanic";
  r[diabetes$race == 'Other'] <- "Other";
  r[diabetes$race == '?'] <- "Unknown";
  diabetes$race <- as.factor(r);
  
  # put diagnoses into groups according to ICD-9 codes
  is.between <- function(x, a, b) {
    x >= a & x <= b
  }
  
  group_diseases <- function(column_data) {
    d <- rep("Other", length(column_data));
    d[is.between(as.double(as.character(column_data)), 390, 459) | as.double(as.character(column_data)) == 785] <- "Circulatory";
    d[is.between(as.double(as.character(column_data)), 460, 519) | as.double(as.character(column_data)) == 786] <- "Respiratory";
    d[is.between(as.double(as.character(column_data)), 520, 579) | as.double(as.character(column_data)) == 787] <- "Digestive";
    d[is.between(as.double(as.character(column_data)), 250, 251)] <- "Diabetes";
    d[is.between(as.double(as.character(column_data)), 800, 999)] <- "Injury";
    d[is.between(as.double(as.character(column_data)), 710, 739)] <- "Musculoskeletal";
    d[is.between(as.double(as.character(column_data)), 580, 629) | as.double(as.character(column_data)) == 788] <- "Genitourinary";
    d[is.between(as.double(as.character(column_data)), 140, 239)] <- "Neoplasms";
    d[as.character(column_data) == "?"] <- "Missing";
    d[substring(as.character(column_data), 1, 1) == "V" | substring(as.character(column_data), 1, 1) == "E"] <- "Other";
    d
  }
  
  d_1 = group_diseases(diabetes$diag_1);
  diabetes$diag_1 <- as.factor(d_1);
  
  d_2 = group_diseases(diabetes$diag_2);
  diabetes$diag_2 <- as.factor(d_2);
  
  d_3 = group_diseases(diabetes$diag_3);
  diabetes$diag_3 <- as.factor(d_3);
  
  
  
  # make target variable binary, classes are after more equally represented
  t <- vector();
  t[diabetes$readmitted == '<30' | diabetes$readmitted == '>30'] <- "YES";
  t[diabetes$readmitted == 'NO'] <- "NO";
  diabetes$readmitted <- as.factor(t);
  
  diabetes
}





