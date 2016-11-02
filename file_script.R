##Get the RScripts directory from user
#C:\Users\Shyamalee\Desktop\PMA2020\PMA2020
#rscript_dir <- readline("Enter the R scripts directory")

file_size <- function(rscript_dir, src_dir){
  #Set working directory as the rscripts_dir
  setwd(rscript_dir)
  
  #R Code in Rscripts directory
  source('analyze-dir.R')
  
  ##Get the instance directory from user
  #D:\NGR3-Female-Questionnaire-v11\instances
  #src_dir <- readline("Enter the instance directory")
  
  #Get submission, log and image file sizes
  fun <- function(path) {
    submission <- 'submission.xml'
    submission_path <- file.path(path, submission)
    xml_size <- file.info(submission_path)$size
    xml_size<- ifelse(length(xml_size)== 0, NA, xml_size)
    
    log <- 'log.txt'
    log_path <- file.path(path, log)
    log_size <- file.info(log_path)$size
    log_size<- ifelse(length(log_size)== 0, NA, log_size)
    
    allfiles <- list.files(path)
    jpeg <- allfiles[grep("*.\\.[jJ][pP][eE]?[gG]$", allfiles)]
    jpeg_path <- file.path(path, jpeg)
    jpeg_size <- file.info(jpeg_path)$size
    jpeg_size<- ifelse(length(jpeg_size)== 0, NA, jpeg_size)
    jpeg_size <- ifelse(length(jpeg_size) >1, Reduce("+",jpeg_size), jpeg_size)
    
    df <- data.frame(path=path, xml=xml_size, log=log_size, jpeg = jpeg_size)
    df
  }
  
  df <- analyze_dir(src_dir, fun)
  write.csv(df, file=file.choose())
}
file_size(rscript_dir, src_dir)