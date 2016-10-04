# Analyze submission

library(stringr)

LOG_FILE <- "log.txt"
SUBMISSION_FILE <- "submission.xml"

read_submission <- function(f) {
    if (is.factor(f)) {
        f <- as.character(f)
    }
    readChar(f, file.info(f)$size)
}

#         info <- list(
#             start=c("<start>", "</start>"),
#             end=c("<end>", "</end>"),
#             num_hh=c("<num_HH_members>","</num_HH_members>"),
#             re_name=c("<your_name>","</your_name>"),
#             cur_rec_user=c("<current_or_recent_user>","</current_or_recent_user>"),
#             birth_events=c("<birth_events>","</birth_events>"),
#             FRS_result=c("<FRS_result>","</FRS_result>"),
#             previous_PMA=c("<previous_PMA>","</previous_PMA>"),
#             facility_type=c("<facility_type>","</facility_type>"),
#             advanced_facility=c("<advanced_facility>","</advanced_facility>"),
#             managing_authority=c("<managing_authority>","</managing_authority>"),
#             SDP_result=c("<SDP_result>","</SDP_result>")
#         )

extract_by_tags <- function(f, tags) {
    out_list <- lapply(f, function(this_file) {
        if (file.exists(this_file)) {
            full_df <- data.frame(submission.xml=TRUE)
            submission <- read_submission(this_file)
            
            all_found <- sapply(names(tags), function(x) {
                these_tags <- tags[[x]]
                re <- paste0(these_tags[1], ".+", these_tags[2])
                found <- str_extract(submission, re)
                if (!is.na(found)) {
                    substr(found, nchar(these_tags[1]) + 1, nchar(found) - nchar(these_tags[2]))
                } else {
                    NA
                }
            })
            
            df <- data.frame(as.list(all_found))
            cbind(full_df, df)
        } else {
            full_df <- data.frame(submission.xml=NA)
            out <- rep(NA, length(tags))
            names(out) <- names(tags)
            df <- data.frame(as.list(out))
            cbind(full_df, df)
        }
    })
    do.call(rbind, out_list)
} 

#' Create a data frame of one row per file in f (vector or character). The 
#' headers for the data frame are the names defined in "info" variable
#' 
#' @value f A file name or vector of file names.
#' @return Data frame, one row per entry in f, with what is found from 
#' searching for XML tags defined by info
summarize_sub_timing <- function(f) {
    out_list <- lapply(f, function(this_file) {
        info <- list(
            start=c("<start>", "</start>"),
            end=c("<end>", "</end>"),
            num_hh=c("<num_HH_members>","</num_HH_members>"),
            re_name=c("<your_name>","</your_name>"),
            cur_rec_user=c("<current_or_recent_user>","</current_or_recent_user>"),
            birth_events=c("<birth_events>","</birth_events>"),
            FRS_result=c("<FRS_result>","</FRS_result>"),
            previous_PMA=c("<previous_PMA>","</previous_PMA>"),
            facility_type=c("<facility_type>","</facility_type>"),
            advanced_facility=c("<advanced_facility>","</advanced_facility>"),
            managing_authority=c("<managing_authority>","</managing_authority>"),
            SDP_result=c("<SDP_result>","</SDP_result>")
        )
        
        if (file.exists(this_file)) {
            full_df <- data.frame(submission.xml=TRUE)
            submission <- read_submission(this_file)
            
            all_found <- sapply(names(info), function(x) {
                tags <- info[[x]]
                re <- paste0(tags[1], ".+", tags[2])
                found <- str_extract(submission, re)
                if (!is.na(found)) {
                    substr(found, nchar(tags[1]) + 1, nchar(found) - nchar(tags[2]))
                } else {
                    NA
                }
            })
            
            df <- data.frame(as.list(all_found))
            cbind(full_df, df)
        } else {
            full_df <- data.frame(submission.xml=NA)
            out <- rep(NA, length(info))
            names(out) <- names(info)
            df <- data.frame(as.list(out))
            cbind(full_df, df)
        }
    })
    do.call(rbind, out_list)
}
