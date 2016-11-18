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

# 0.4 second
ONE_SWIPE <- 400
# 30 minutes
THIRTY_MIN <- 1000*60*30

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

#' Create a one-row data frame summarizing the timing of a log. The 
#' headers for the data frame are "valid_log", "resumed", "paused", 
#' and "short_break".
#' 
#' @param log_df A log in data frame format
#' @param OR_OP_TOL The tolerance for different questions that show 
#' consecutive onResume. Necessary because two questions on the same 
#' screen might not "OR" at the same time. Same for "OP"
#' @param BREAK_TOL The tolerance for short breaks.
#' @return A one-row data frame. First column is whether or not the 
#' log timings are valid with totals of resumed, paused, and 
#' short_break timing. 
summarize_log_timing <- function(log_df, OR_OP_TOL=NULL, BREAK_TOL=NULL) {
    if (is.null(OR_OP_TOL)) {
        OR_OP_TOL <- ONE_SWIPE
    }
    if (is.null(BREAK_TOL)) {
        BREAK_TOL <- THIRTY_MIN
    }
    if (!is.null(log_df)) {
        orop <- distill_orop(log_df, OR_OP_TOL)
        # Fix missing onPause, add onPause after trailing onResume
        cleaned <- clean_orop(orop, log_df)
        valid <- check_orop(cleaned$code)
        resumed <- ifelse(valid, resumed_time(cleaned$timestamp)/1000, NA)
        paused <- ifelse(valid, paused_time(cleaned$timestamp)/1000, NA)
        short_break <- ifelse(valid, stoppage_time(cleaned$timestamp, BREAK_TOL)/1000, NA)
        df <- data.frame(
            valid_log=valid, 
            resumed=resumed, 
            paused=paused, 
            short_break=short_break
        )
        df
    } else {
        df_headers <- c("valid_log", "resumed", "paused", "short_break")
        out <- rep(NA, length(df_headers))
        names(out) <- df_headers
        df <- data.frame(as.list(out))
        df
    }
}

#' Create a one-row data frame summarizing the timing of specific 
#' questions using the log. The headers for the data frame are 
#' the question names.
#' 
#' @param log_df A log in data frame format
#' @param prompts A vector of prompts to examine
#' @return A one-row data frame with one header for each prompt in 
#' prompts. 
summarize_screen_timing <- function(log_df, prompts) {
    if (is.null(prompts)) {
        return(NULL)
    }
    if (is.null(log_df)) {
        out <- rep(NA, length(prompts))
        names(out) <- prompts
        df <- data.frame(as.list(out))
        df
    } else {
        out <- lapply(prompts, function(prompt) {
            # Keep the rows that have the prompt in it
            df <- subset(log_df, grepl(prompt, log_df[,SCREEN_COL_NAME]))
            # TODO generate for "prompt" a 1-row data frame with:
            #         (1) "prompt_time"       -> total time spent on prompt
            #         (2) "prompt_visits"     -> total visits to prompt
            #         (3) "prompt_constraint" -> total times a constraint was contravened
        })
        # join all the results together
        do.call(cbind, out)
    }
}
