# Analyze log


OR_CODE <- "oR"
OP_CODE <- "oP"
EP_CODE <- "EP"
LP_CODE <- "LP"
rV_CODE <- "rV"
rS_CODE <- "rS"
SUBMISSION_FILE <- "submission.xml"
LOG_FILE <- "log.txt"

TIME_COL_NAME <- "V1"
EVENT_COL_NAME <- "V2"
SCREEN_COL_NAME <- "V3"
VALUE_COL_NAME <- "V4"



read_log <- function(f) {
    if (is.factor(f)) {
        f <- as.character(f)
    }
    read.table(f, sep='\t', quote="")
}

distill_orop <- function(log_df, OR_OP_TOL) {
    pared_df <- data.frame(timestamp=log_df$V1, code=log_df$V2)
    orop <- subset(pared_df, code==OR_CODE | code==OP_CODE)
    orop$code <- droplevels(orop$code)
    # crazy code to get a split based on changes of groups of oP -> oR and oR -> oP
    moment <- c(0, cumsum(abs(diff(as.numeric(orop$code)))))
    my_split <- split(orop, moment)
    df <- unsplit(lapply(my_split, function(x) {
        if (nrow(x) > 1) {
            min_time <- min(x$timestamp)
            new_times <- ((x$timestamp - min_time) %/% OR_OP_TOL) * OR_OP_TOL + min_time
            x$timestamp <- new_times
        }
        x
    }), moment)
    condensed <- df[!duplicated(df),]
    condensed$step <- 0
    condensed$step[condensed$code == "oR"] <- 1
    condensed$step[condensed$code == "oP"] <- -1
    condensed
}

check_orop <- function(orop) {
    zeros <- rep(0, length(orop))
    zeros[orop == OR_CODE] <- 1
    zeros[orop == OP_CODE] <- -1
    stopifnot(sum(zeros == 0) == 0)
    on_off <- cumsum(zeros)
    return(on_off[1] == 1 && on_off[length(on_off)] == 0 && sum(on_off > 1) == 0 && sum(on_off < 0) == 0 )
}

resumed_time <- function(timestamp) {
    # Assume valid
    delta <- diff(timestamp)
    # Keep odd periods because deltas are on-off-on-off-on-off-...
    odd_ind <- seq(1, length(delta), by=2)
    this_resumed <- delta[odd_ind]
    sum(this_resumed)
}

paused_time <- function(timestamp) {
    if (length(timestamp) <= 2) {
        return(0)
    }
    # Assume valid
    delta <- diff(timestamp)
    # Keep even periods because deltas are on-off-on-off-on-off-...
    even_ind <- seq(2, length(delta), by=2)
    this_paused <- delta[even_ind]
    sum(this_paused)
}

stoppage_time <- function(timestamp, BREAK_TOL) {
    if (length(timestamp) <= 2) {
        return(0)
    }
    # Assume valid
    delta <- diff(timestamp)
    # Keep even periods because deltas are on-off-on-off-on-off-...
    even_ind <- seq(2, length(delta), by=2)
    this_paused <- delta[even_ind]
    sum(max(0, this_paused[this_paused < BREAK_TOL]))
}

clean_orop <- function(orop, full_df) {
    ## FIX MISSING ONPAUSE
    runs <- rle(orop$step)
    # Searching for oR, then oR again. Length should be 2, value 1
    double_or <- which(runs$lengths >= 2 & runs$values == 1)
    for (i in rev(double_or)) {
        # Most recent oR that leads to correct behavior
        orop_ind <- sum(runs$lengths[1:i])
        # First oR in the run, then second oR is after missed oP
        first_or_ind <- orop_ind - runs$lengths[i] + 1
        second_or_ind <- first_or_ind + 1
        full_ind <- as.numeric(rownames(orop)[second_or_ind])
        #full_ind <- as.numeric(rownames(orop)[orop_ind])
        offset <- 1
        prev_code <- as.character(full_df[full_ind - offset, 2])
        code_first_letter <- substr(prev_code, 1, 1)
        while (code_first_letter == "r" && offset < full_ind) {
            offset <- offset + 1
            prev_code <- as.character(full_df[full_ind - offset, 2])
            code_first_letter <- substr(prev_code, 1, 1)
        }
        prev_timestamp <- full_df[full_ind - offset, 1]
        new_timestamp <- prev_timestamp + 1
        first_chunk <- orop[1:first_or_ind,]
        # first_chunk <- orop[1:(orop_ind-1),]
        new_chunk <- data.frame(timestamp=new_timestamp, code=OP_CODE, step=-1)
        rownames(new_chunk) <- paste0(full_ind - offset, ".5")
        last_chunk <- orop[orop_ind:nrow(orop),]
        orop <- rbind(first_chunk, new_chunk, last_chunk)
    }
    
    ## FIX ONRESUME BEING THE LAST ITEM
    if (orop[nrow(orop),]$code == OR_CODE) {
        full_ind <- nrow(full_df)
        offset <- 0
        prev_code <- as.character(full_df[full_ind - offset, 2])
        code_first_letter <- substr(prev_code, 1, 1)
        while (code_first_letter == "r" && offset < full_ind) {
            offset <- offset + 1
            prev_code <- as.character(full_df[full_ind - offset, 2])
            code_first_letter <- substr(prev_code, 1, 1)
        }
        prev_timestamp <- full_df[full_ind - offset, 1]
        new_timestamp <- prev_timestamp + 1
        new_chunk <- data.frame(timestamp=new_timestamp, code=OP_CODE, step=-1)
        rownames(new_chunk) <- paste0(full_ind - offset, ".5")
        orop <- rbind(orop, new_chunk)
    }
    
    orop
}

#' Create a data frame of one row per file in f (vector or character). The 
#' headers for the data frame are "log.txt", "resumed", and "paused"
#' 
#' @value f A file name or vector of file names.
#' @return Data frame, one row per entry in f, with totals of resumed and 
#' paused time, as well as whether or not file is found
summarize_log_timing <- function(f, OR_OP_TOL, BREAK_TOL) {
    out_list <- lapply(f, function(this_file) {
        if (file.exists(this_file)) {
            df <- read_log(this_file)
            orop <- distill_orop(df, OR_OP_TOL)
            # Fix missing onPause, add onPause after trailing onResume
            cleaned <- clean_orop(orop, df)
            valid <- check_orop(cleaned$code)
            resumed <- ifelse(valid, resumed_time(cleaned$timestamp), NA)
            paused <- ifelse(valid, paused_time(cleaned$timestamp), NA)
            stoppage <- ifelse(valid, stoppage_time(cleaned$timestamp, BREAK_TOL), NA)
            df <- data.frame(log.txt=valid, resumed=resumed, paused=paused, stoppage=stoppage)
            df
        } else {
            df_headers <- c("log.txt", "resumed", "paused", "stoppage")
            out <- rep(NA, length(df_headers))
            names(out) <- df_headers
            df <- data.frame(as.list(out))
            df
        }
    })
    do.call(rbind, out_list)
}

summarize_screen_timing <- function(f, screen) {
    out_list <- lapply(f, function(this_file) {
        if (file.exists(this_file)) {
            df <- read_log(this_file)
            screen_starts <- which(df[, SCREEN_COL_NAME] == screen & df[, EVENT_COL_NAME] == EP_CODE)
            if (length(screen_starts) != 0) {
                screen_times <- sapply(screen_starts, function(screen_start) {
                    if (df[screen_start + 1, EVENT_COL_NAME] == LP_CODE) {
                        df[screen_start + 1, TIME_COL_NAME] - df[screen_start, TIME_COL_NAME]
                    } else {
                        NA
                    }
                })
                all_screen_times <- na.omit(screen_times)
                if (length(all_screen_times) == 0) {
                    outdf <- data.frame(screen_count=length(screen_starts), valid=length(all_screen_times), max=NA, min=NA, median=NA)
                    outdf
                } else {
                    this_max <- max(all_screen_times)
                    this_min <- min(all_screen_times)
                    this_med <- median(all_screen_times)
                    outdf <- data.frame(screen_count=length(screen_starts), valid=length(all_screen_times), 
                               max=this_max, min=this_min, median=this_med)
                    outdf
                }
            } else {
                outdf <- data.frame(screen_count=NA, valid=NA, max=NA, min=NA, median=NA)
                return(outdf)
            }
        } else {
            outdf <- data.frame(screen_count=NA, valid=NA, max=NA, min=NA, median=NA)
            return(outdf)
        }
    })
    out <- do.call(rbind, out_list)
}
