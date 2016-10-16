#D:\NGR3-Female-Questionnaire-v11\instances

root_dir <- "D:"
forms_of_interest <- ("NGR3-Female-Questionnaire-v11")
instances <- 'instances'

#Path to instances
all_ins_dirs <- file.path(root_dir, forms_of_interest, instances)

# RSCRIPTS directory
setwd('C:/Users/Shyamalee/Desktop/PMA2020/PMA2020')

# R code in the RSCRIPTS directory
source('analyze-dir.R')

# Instances with data
src_dir <- "D:/NGR3-Female-Questionnaire-v11/instances"

#Get submission, log and image file sizes
fun <- function(path) {
  submission <- 'submission.xml'
  submission_path <- file.path(path, submission)
  xml_size <- file.info(submission_path)$size

  log <- 'log.txt'
  log_path <- file.path(path, log)
  log_size <- file.info(log_path)$size

  allfiles <- list.files(path)
  jpeg <- allfiles[grep("*.\\.[jJ][pP][eE]?[gG]$", allfiles)]
  jpeg_path <- file.path(path, jpeg)
  jpeg_size <- file.info(jpeg_path)$size
  jpeg_size<- ifelse(length(jpeg_size)== 0, 0, jpeg_size)

  df <- data.frame(path=path, xml=xml_size, log=log_size, jpeg = jpeg_size)
  df
}

df <- analyze_dir(src_dir, fun)

write.csv(df, "name.csv")
