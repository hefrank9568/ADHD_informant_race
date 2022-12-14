####load pacakges####
#install.packages("here")
library(here)
#####set path to data#####
path_dat <- ""##change this for your environment
here()

####Load separate data and select relevant variables####
#####Teacher form####
#https://nda.nih.gov/data_structure.html?short_name=abcd_ssbpmtf01

#####Parent CBCL####
#https://nda.nih.gov/data_structure.html?short_name=abcd_cbcls01

#####Parent KSADS####
#https://nda.nih.gov/data_structure.html?short_name=abcd_ksad501
##Present ADHD symptoms
sapply(seq(394,409,by=1), function(X) paste0("ksads_14_",X,"_t")) 
##ADHD diagnoses (present, past, partial remission, unspecified)
sapply(seq(853,856,by=1), function(X) paste0("ksads_14_",X,"_t")) 

####merge into a single data object####


####check mean, range, NA, missing data####


####save to RData####

