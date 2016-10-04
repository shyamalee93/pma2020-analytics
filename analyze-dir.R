# FUNCTION TO APPLY TO EACH INSTANCE DIR IN A PARENT (FORM) DIRECTORY

#' Apply a function to each supplied directory
#' 
#' @param sub_dirs A list of directories
#' @param fun A function that accepts the path to a directory with 
#' files for analysis and returns one row of a common data frame
#' @return A joined data frame, one row per sub_dir
apply_fun_dir <- function(sub_dirs, fun) {
    do.call(rbind, lapply(sub_dirs, function(this_dir) {
        fun(this_dir)
    }))
}

#' Apply a function to each subdirectory
#'
#' This is the main entry point for analysis. Supply the directory
#' (e.g. for all instances of a form) and the function that does the 
#' analysis.
#' 
#' @param src_dir A parent directory filled with only subdirectories
#' @param fun A function that accepts the path to a directory with 
#' files for analysis and returns one row of a common data frame
#' @return A joined data frame, one row per subdirectory in src_dir
analyze_dir <- function(src_dir, fun) {
    sub_dir <- file.path(src_dir,dir(src_dir))
    apply_fun_dir(sub_dir, fun)
}
