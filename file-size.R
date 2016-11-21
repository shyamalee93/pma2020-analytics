# Logging analysis

## Ratio of Log file size to XML file size

#' Get file size information for a directory
#'
#' @param sub_dir The directory to examine
#' @return A data frame with fields for the submission.xml size 
#' (xml_size), the log.txt size (log_size), and the size of all jpeg 
#' photos (jpeg_size)
#'
get_file_size_df <- function(sub_dir) {
    submission <- 'submission.xml'
    submission_path <- file.path(sub_dir, submission)
    xml_size <- file.info(submission_path)$size
    xml_size<- ifelse(length(xml_size)== 0, NA, sum(xml_size))
    
    log <- "log.txt"
    log_path <- file.path(sub_dir, log)
    log_size <- file.info(log_path)$size
    log_size<- ifelse(length(log_size)== 0, NA, sum(log_size))
    
    allfiles <- list.files(sub_dir)
    jpeg <- allfiles[grep("\\.[jJ][pP][eE]?[gG]$", allfiles)]
    jpeg_path <- file.path(sub_dir, jpeg)
    jpeg_size <- file.info(jpeg_path)$size
    jpeg_size<- ifelse(length(jpeg_size)== 0, NA, sum(jpeg_size))
    
    data.frame(xml_size=xml_size, log_size=log_size, jpeg_size=jpeg_size)
}
