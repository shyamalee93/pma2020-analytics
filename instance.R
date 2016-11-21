# Analyze log

OR_CODE <- "oR"
OP_CODE <- "oP"
EP_CODE <- "EP"
LP_CODE <- "LP"
rV_CODE <- "rV"
rS_CODE <- "rS"
CC_CODE <- "CC"
SUBMISSION_FILE <- "submission.xml"
LOG_FILE <- "log.txt"

TIME_COL <- 1
EVENT_COL <- 2
PROMPT_COL <- 3
VALUE_COL <- 4

# 0.4 second
ONE_SWIPE <- 400
# 30 minutes
THIRTY_MIN <- 1000*60*30

read_log <- function(f) {
    if (is.factor(f)) {
        f <- as.character(f)
    }
    df <- read.table(f, sep='\t', quote="")
    colnames(df) <- c("TIME", "EVENT", "PROMPT", "VALUE")
    return(df)
}

# on_codes=c(OR_CODE, EP_CODE)   **OR**    on_codes=OR_CODE
distill_on_off <- function(log_df, on_codes, off_codes, tol=ONE_SWIPE) {
    on_off_df <- subset(log_df, log_df$EVENT %in% c(on_codes, off_codes))
    on_off_df$EVENT <- droplevels(on_off_df$EVENT)
    # Give a 1 for all codes that turn "on" and give a 0 for all codes that turn "off"
    on_off_df$ON <- as.numeric(on_off_df$EVENT %in% on_codes)
    if (tol > 0) {
        # The next goal is to split into sections of ON and OFF
        # if ON is c(1,1,0,1,1,0,0,1,1,0,1,0), then this returns
        #          c(0,0,1,2,2,3,3,4,4,5,6,7)
        moment <- c(0, cumsum(abs(diff(on_off_df$ON))))
        my_split <- split(on_off_df, moment)
        # Collapse timestamps together and unsplit the data frame
        on_off_df <- unsplit(lapply(my_split, function(x) {
            if (nrow(x) > 1) {
                min_time <- min(x$TIME)
                # Integer division gets us the time collapsing
                new_times <- ((x$TIME - min_time) %/% tol) * tol + min_time
                x$TIME <- new_times
            }
            x
        }), moment)
    }
    # Remove duplicate rows (for multi-prmopt)
    on_off_df[!duplicated(on_off_df[, c("TIME", "ON")]),]
}


#' @param on_off Vector of 1 and 0 for ON and OFF. Created previously with 
#' distill_on_off
check_on_off <- function(on_off) {
    zeros <- rep(1, length(on_off))
    zeros[on_off == 0] <- -1
    status <- cumsum(zeros)
    starts <- status[1] == 1
    ends <- status[length(status)] == 0
    not_too_many <- sum(status > 1) == 0 && sum(status < 0) == 0
    return(starts & ends & not_too_many)
}

distill_orop <- function(log_df, OR_OP_TOL=ONE_SWIPE) {
    pared_df <- data.frame(timestamp=log_df$TIME, code=log_df$EVENT)
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
#' @param resumed_tol The tolerance for different questions that show 
#' consecutive onResume. Necessary because two questions on the same 
#' screen might not "OR" at the same time. Same for "OP"
#' @param break_tol The tolerance for short breaks.
#' @return A one-row data frame. First column is whether or not the 
#' log timings are valid with totals of resumed, paused, and 
#' short_break timing. 
summarize_log_timing <- function(log_df, resumed_tol=NULL, break_tol=NULL) {
    if (is.null(resumed_tol)) {
        resumed_tol <- ONE_SWIPE
    }
    if (is.null(break_tol)) {
        break_tol <- THIRTY_MIN
    }
    if (!is.null(log_df)) {
        orop <- distill_orop(log_df, resumed_tol)
        # Fix missing onPause, add onPause after trailing onResume
        cleaned <- clean_orop(orop, log_df)
        valid <- check_orop(cleaned$code)
        resumed <- ifelse(valid, round(resumed_time(cleaned$timestamp)/1000), NA)
        paused <- ifelse(valid, round(paused_time(cleaned$timestamp)/1000), NA)
        short_break <- ifelse(valid, round(stoppage_time(cleaned$timestamp, break_tol)/1000), NA)
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
        # TODO: dynamically find prompts
        return(NULL)
    }
    if (is.null(log_df)) {
        name_cc <- paste0(prompts, "_CC")
        name_time <- paste0(prompts, "_time")
        name_visits <- paste0(prompts, "_visits")
        all_names <- c(rbind(name_cc, name_time, name_visits))
        out <- rep(NA, length(all_names))
        names(out) <- all_names
        df <- data.frame(as.list(out))
        df
    } else {
        out <- lapply(prompts, function(prompt) {
            # Keep the rows that have the prompt in it
            this_prompt <- paste0("(^|/)",prompt, "\\[1\\]")
            df <- log_df[grep(this_prompt, log_df$PROMPT),]
            if (nrow(df) > 0) {
                # TODO generate for "prompt" a 1-row data frame with:
                #         (1) "prompt_constraint" -> total times a constraint was contravened
                constraint <- sum(df$EVENT == CC_CODE)
                # For the next two, get the ON/OFF dataframe
                on_off_df <- distill_on_off(df, on_codes=c(EP_CODE, OR_CODE), off_codes=c(LP_CODE, OP_CODE), tol=ONE_SWIPE)
                #         (2) "prompt_time"       -> total time spent on prompt
                valid <- check_on_off(on_off_df$ON)
                time <- NA
                if (valid) {
                    time <- round(resumed_time(on_off_df$TIME)/1000)
                }
                #         (3) "prompt_visits"     -> total visits to prompt
                visits <- sum(on_off_df$ON)
                result <- data.frame(CC=constraint, time=time, visits=visits)
                colnames(result) <- paste(prompt, colnames(result), sep="_")
                return(result)
            } else {
                result <- data.frame(CC=NA, time=NA, visits=NA)
                colnames(result) <- paste(prompt, colnames(result), sep="_")
                return(result)
            }
        })
        # join all the results together
        do.call(cbind, out)
    }
}

#' Create a one-row data frame with the timestamp for the first time 
#' milestone prompt was reached.
#' 
#' @param log_df A log in data frame format
#' @param milestones A vector of milestone prompts to examine. 
#' Milestones are reported in order.
#' @return A one-row data frame with one header for each prompt in 
#' prompts. 
get_milestone_timing <- function(log_df, milestones) {
    if (is.null(milestones)) {
        return(NULL)
    }
    if (is.null(log_df)) {
        name_ms <- paste0("MS_",prompts)
        out <- rep(NA, length(name_ms))
        names(out) <- all_names
        data.frame(as.list(out))
    } else {
        cur_min <- 0
        out <- sapply(milestones, function(prompt){
            this_prompt <- paste0("(^|/)", prompt, "\\[1\\]")
            df <- subset(log_df, (EVENT == EP_CODE | EVENT == OR_CODE) & grepl(this_prompt, PROMPT) & TIME > cur_min)
            if (nrow(df) > 0) {
                time <- df$TIME[1]
                cur_min <<- time
                round(time/1000)
            } else {
                NA
            }
        })
        data.frame(as.list(out))
    }
}
