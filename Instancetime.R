##Get the RScripts directory from user
#C:\Users\Shyamalee\Desktop\PMA2020\PMA2020
#rscript_dir <- readline("Enter the R scripts directory")
##Get the instance directory from user
#D:\NGR3-Female-Questionnaire-v11\instances
#src_dir <- readline("Enter the instance directory")

OR_CODE <- "oR"
OP_CODE <- "oP"
EP_CODE <- "EP"
LP_CODE <- "LP"
rV_CODE <- "rV"
rS_CODE <- "rS"

TIME_COL_NAME <- "V1"
EVENT_COL_NAME <- "V2"
SCREEN_COL_NAME <- "V3"
VALUE_COL_NAME <- "V4"  

OR_OP_TOL = 500
BREAK_TOL = 3600000

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

file_size <- function(src_dir, output){
  #Set working directory as the rscripts_dir
  #R Code in Rscripts directory
  source('analyze-dir.R')
  
  #Get submission, log and image file sizes
  fun <- function(path) {
    submission <- 'submission.xml'
    submission_path <- file.path(path, submission)
    xml_size <- file.info(submission_path)$size
    xml_size<- ifelse(length(xml_size)== 0, NA, xml_size)
    
    log <- "log.txt"
    log_path <- file.path(path, log)
    log_size <- file.info(log_path)$size
    log_size<- ifelse(length(log_size)== 0, NA, log_size)
    
    allfiles <- list.files(path)
    jpeg <- allfiles[grep("*.\\.[jJ][pP][eE]?[gG]$", allfiles)]
    jpeg_path <- file.path(path, jpeg)
    jpeg_size <- file.info(jpeg_path)$size
    jpeg_size<- ifelse(length(jpeg_size)== 0, NA, sum(jpeg_size))
    #jpeg_size <- ifelse(length(jpeg_size) >1, Reduce("+",jpeg_size), jpeg_size)
    
    #summarize_log_timing <- function(f, OR_OP_TOL, BREAK_TOL) {
    out_list <- lapply(log_path, function(this_file) {
      if (file.exists(this_file)) {
        df <- read_log(this_file)
        orop <- distill_orop(df, OR_OP_TOL)
        # Fix missing onPause, add onPause after trailing onResume
        cleaned <- clean_orop(orop, df)
        valid <- check_orop(cleaned$code)
        resumed <- ifelse(valid, resumed_time(cleaned$timestamp), NA)
        paused <- ifelse(valid, paused_time(cleaned$timestamp), NA)
        stoppage <- ifelse(valid, stoppage_time(cleaned$timestamp, BREAK_TOL), NA)
        #df <- data.frame(Instance =basename(getwd()), resumed=resumed, paused=paused, stoppage=stoppage)
        df <- data.frame(Resume_Time= round(resumed/1000, 0), Paused_Time= round(paused/1000,0), Stoppage_Time= round(stoppage/1000,0))
        df
        } 
      else {
        #df_headers <- c("Instance", "resumed", "paused", "stoppage")
        df_headers <- c("Resume_Time", "Paused_Time", "Stoppage_Time")
        out <- rep(NA, length(df_headers))
        names(out) <- df_headers
        df <- data.frame(as.list(out))
        df
        }
      })
      df2 <- do.call(rbind, out_list)
      df2
    #}
    df <- data.frame(Path=path, XML_Size=xml_size, Log_Size=log_size, JPEG_Size = jpeg_size)
    #df
    main_df <- cbind(df,df2)
    main_df
  }
  df <- analyze_dir(src_dir, fun)
  write.csv(df, file = output, row.names=F)
}
