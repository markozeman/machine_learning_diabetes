
IDs_mapping <- read.table('dataset_diabetes/IDs_mapping.csv', header = TRUE, sep = ',');
diabetes <- read.table('dataset_diabetes/diabetic_data.csv', header = TRUE, sep = ',');

# check if all encounter ids are different
length(unique(diabetes[, 1])) == length(diabetes[, 1]);

# make rows unique according to patient number (so each row is different patient)
diabetes_unique = diabetes[!duplicated(diabetes[, c("patient_nbr")]), ];



