# Make the super analyzing function
source("file-size.R")
source("instance.R")
source("xml-analysis.R")


#' A function that initializes and returns a function
#'
#'
do_all_analysis <- function(prompts=NULL, milestones=NULL, tags=NULL, resumed_tol=NULL, break_tol=NULL) {
    analyzer <- function(sub_dir) {
        # identify the data with the uuid
        uuid_df <- data.frame(dir_uuid=basename(sub_dir))
        # from file-size.R, get file sizes from the directory
        file_size_df <- get_file_size_df(sub_dir)
        # from instance.R, get total questionnaire timing
        log_file <- file.path(sub_dir, "log.txt")
        log_df <- NULL
        if (file.exists(log_file)) {
            log_df <- read_log(log_file)
        }
        # from instance.R
        overall_timing_df <- summarize_log_timing(log_df, resumed_tol, break_tol)
        # from instance.R
        prompts_df <- summarize_screen_timing(log_df, prompts=prompts)
        # from instance.R
        milestones_df <- get_milestone_timing(log_df, milestones=milestones)
        # from xml-analysis.R
        tags_df <- extract_by_tags(sub_dir, tags=tags)
        
        # ........... COMBINE EVERYTHING TOGETHER ............ #
        # First look at the super data frame
        super_df <- cbind(uuid_df, file_size_df, overall_timing_df)
        # Add on prompts data frame if exists
        if (!is.null(prompts_df)) {
            super_df <- cbind(super_df, prompts_df)
        }
        # Add on milestones data frame if exists
        if (!is.null(milestones_df)) {
            super_df <- cbind(super_df, milestones_df)
        }
        # Add on xml tags data frame if exists
        if (!is.null(tags_df)) {
            super_df <- cbind(super_df, tags_df)
        }
        # Return all dataframes combined
        super_df
    }
    analyzer
}
