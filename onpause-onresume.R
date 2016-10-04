# Verify a log onPause and onResume

source("pmalog.R")
source("pmainstance.R")


# CONSTANTS
# OR_OP_TOL <- 300 # catches weird cases
OR_OP_TOL <- 1500
BREAK_TOL <- 3*60*60*1000


#' 
#'
#' @param instance_dirs A list of directories that contain log and submission 
#' data
#' @return A data frame with results of analyzing all instance logs and 
#' submissions
get_timing_info <- function(instance_dirs) {
    do.call(rbind, lapply(instance_dirs, function(this_dir) {
        log_file <- file.path(this_dir, LOG_FILE)
        log_df <- summarize_log_timing(log_file, OR_OP_TOL, BREAK_TOL)
        
        sub_file <- file.path(this_dir, SUBMISSION_FILE)
        sub_df <- summarize_sub_timing(sub_file)
        
        cbind(data.frame(folder=this_dir), log_df, sub_df)
    }))
}

#'
#'
#' @param src_dir A directory that contains instance directories
#' @return A data frame with results of analyzing all instance logs and 
#' submissions
get_timing_info_from_dir <- function(src_dir) {
    contents <- file.path(src_dir,dir(src_dir))
    get_timing_info(contents)
}









#INSTANCES_DIR <- "~/Documents/odkbriefcase/ODK Briefcase Storage/forms/RJR1-Female-Questionnaire-v12/instances"
#INSTANCES_DIR <- "~/Documents/odkbriefcase/ODK Briefcase Storage/forms/RJR1-Household-Questionnaire-v12/instances"
#INSTANCES_DIR <- "~/Documents/odkbriefcase/ODK Briefcase Storage/forms/RJR1-Listing-v16/instances"
#setwd(INSTANCES_DIR)







# analyze_bad_runs <- function(row, step) {
#     delta <- diff(step)
#     bad_states <- which(delta == 0)
#     if (length(bad_states) > 0) {
#         chunk_diff <- diff(bad_states)
#         chunk_div <- which(chunk_diff > 1)
#         chunk_diff[chunk_diff == 1] <- 0
#         chunk_diff[chunk_diff > 1] <- 1
#         chunk_groups <- c(0, cumsum(chunk_diff))
#         group_list <- split(bad_states, chunk_groups)
#         lapply(group_list, function(x) {
#             first_ind <- x[1]
#             run_length <- length(x)
#             last_ind <- first_ind + run_length
#             row[first_ind:last_ind]
#         })
#     } 
# }








# summarize_log <- function(f) {
#     result <- list()
#     result$file <- f
#     if (file.exists(f)) {
#         df <- read_log(f)
#         timestamp <- df$V1
#         code <- df$V2
#         orop <- condense_orop(timestamp, code)
#         # Fix missing onPause, remove trailing onResume
#         cleaned <- clean_orop(orop, df)
#         # cleaned <- orop
#         valid <- check_orop(cleaned$code)
#         if (!valid) {
#             bad_result <- list()
#             bad_runs <- analyze_bad_runs(rownames(orop),orop$step)
#             names(bad_runs) <- NULL
#             bad_result$runs <- bad_runs
#             bad_result$file <- f
#             result$invalid <- bad_result
#         }
#         resumed <- ifelse(valid, resumed_time(orop$timestamp), -1)
#         paused <- ifelse(valid, paused_time(orop$timestamp), -1)
#         df <- data.frame(file=f, valid=valid, resumed=resumed, paused=paused)
#         result$df <- df
#     }
#     result
# }


# mine_submission <- function(f) {
#     info <- list(
#         start=c("<start>", "</start>"),
#         end=c("<end>", "</end>")
#     )
#     
#     if (file.exists(f)) {
#         submission <- readChar(f, file.info(f)$size)
#         
#         all_found <- sapply(names(info), function(x) {
#             tags <- info[[x]]
#             re <- paste0(tags[1], ".+", tags[2])
#             found <- str_extract(submission, re)
#             if (!is.na(found)) {
#                 substr(found, nchar(tags[1]) + 1, nchar(found) - nchar(tags[2]))
#             } else {
#                 NA
#             }
#         })
#         
#         df <- data.frame(as.list(all_found))
#         df
#     } else {
#         out <- rep(NA, length(info))
#         names(out) <- names(info)
#         df <- data.frame(as.list(out))
#         df
#     }
# }

# as.numeric(strptime(value, format="%FT%T", tz="UTC"))

# all_info <- lapply(dir(), function(f) {
#     log_df <- summarize_log(file.path(f, LOG_FILE))
#     sub_df <- mine_submission(file.path(f, SUBMISSION_FILE))
#     if (! is.null(log_df$df)) {
#         log_df$df <- cbind(log_df$df, sub_df)
#     }
#     
#     if (!all(dim(log_df$df) == c(1,6))) {
#         stop(f, dim(log_df$df))
#     }
#     log_df
# })



# summary_df <- do.call(rbind, lapply(all_info, function(x) {
#     x$df
# }))
# 
# bad_runs <- lapply(all_info, function(x) {
#     x$invalid
# })
# bad_runs <- bad_runs[!sapply(bad_runs, is.null)]

# print(sum(summary_df$valid == FALSE))


# resumed <- summary_df$resumed[summary_df$resumed > 0]
# resumed <- resumed[order(resumed)]/(1000*60)
# FUN <- ecdf(resumed)
# edges <- quantile(resumed, probs=c(0,0.975))
# hist(resumed[resumed > edges[1] & resumed < edges[2]])
# 
# paused <- summary_df$paused[summary_df$paused >= 0]
# paused <- paused[order(paused)]/(1000*60)
# FUN <- ecdf(paused)
# edges <- quantile(paused, probs=c(0.0,0.42))
# hist(paused[paused > edges[1] & paused < edges[2]])
# 
# summary_df$start_sec <- as.numeric(strptime(summary_df$start, format="%FT%T", tz="UTC"))
# summary_df$end_sec <- as.numeric(strptime(summary_df$end, format="%FT%T", tz="UTC"))
# summary_df$xml_time <- summary_df$end_sec - summary_df$start_sec
# xml_time <- summary_df$xml_time[order(summary_df$xml_time)]/60
# FUN <- ecdf(xml_time)
# edges <- quantile(xml_time, probs=c(0,0.59))
# hist(xml_time[xml_time > edges[1] & xml_time < edges[2]])
# 
# summary_df$log_total <- summary_df$resumed + summary_df$paused
# log_total <- summary_df$log_total[order(summary_df$log_total)]/(1000*60)
# edges <- quantile(log_total, probs=c(0.01,0.43))
# hist(log_total[log_total > edges[1] & log_total < edges[2]])


# Look at the files that are not valid. Look at the files that have negative times. Convert to seconds. Get time comparisons from XML.
