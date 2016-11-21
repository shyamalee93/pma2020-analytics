# Analyze submission

library(stringr)


read_submission <- function(f) {
    if (is.factor(f)) {
        f <- as.character(f)
    }
    readChar(f, file.info(f)$size)
}


#' Create a data frame of one row per file in f (vector or 
#' character). The headers for the data frame are the names defined 
#' in "tags" variable.
#' 
#' @param sub_dir The directory to examine
#' @param tags A list with names that translate to data frame column 
#' names. The values are start and end tags. Be careful that tags 
#' appear only once (not in repeats).
#' @return Data frame, one row per entry in f, with what is found from 
#' searching for XML tags defined by info
extract_by_tags <- function(sub_dir, tags) {
    if (is.null(tags)) {
        return(NULL)
    }
    xml_file <- file.path(sub_dir, "submission.xml")
    if (file.exists(xml_file)) {
        submission <- read_submission(xml_file)
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
        data.frame(as.list(all_found))
    } else {
        out <- rep(NA, length(tags))
        names(out) <- names(tags)
        data.frame(as.list(out))
    }
}


#   EXAMPLE
#
#         tags <- list(
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
