# Logging analysis

## Ratio of Log file size to XML file size

library(ggplot2)

root_dir <- '~/Documents/odkbriefcase/ODK Briefcase Storage/forms'

forms_of_interest <- c('RJR1-Female-Questionnaire-v12', 'RJR1-Household-Questionnaire-v12', 'RJR1-SDP-Questionnaire-v12')
instances <- 'instances'

all_ins_dirs <- file.path(root_dir, forms_of_interest, instances)

flog <- 'log.txt'
fxml <- 'submission.xml'

get_form_type <- function(fname) {
    if (grepl('Female-Questionnaire', fname)) {
        'FQ'
    } else if (grepl('Household-Questionnaire', fname)) {
        'HQ'
    } else if (grepl('SDP-Questionnaire', fname)) {
        'SQ'
    } else {
        'NA'
    }
}

dfs <- lapply(all_ins_dirs, function(root) {
    full_uuid <- file.path(root, dir(root))
    all_jpg <- sapply(full_uuid, function(x) {
        jpg <- dir(x)[grep('.jpg$', dir(x))]
        if (length(jpg) >= 1) {
            this_jpg <- file.path(x, jpg[1])
            file.info(this_jpg)$size
        } else {
            NA
        }
    })
    all_log <- file.path(full_uuid, flog)
    all_xml <- file.path(full_uuid, fxml)
    log_size <- sapply(all_log, function(x) {
        file.info(x)$size
    })
    xml_size <- sapply(all_xml, function(x) {
        file.info(x)$size
    })
    form_type <- get_form_type(root)
    data.frame(Form=form_type, log=log_size, xml=xml_size, jpg=all_jpg)
})

df <- do.call(rbind, dfs)
row.names(df) <- NULL

df$ratio <- df$log / df$xml
df$log_ratio <- log(df$ratio)

title1 <- 'Density plot of logarithm of ratio of logging file size and XML submission file size\n'
title1 <- paste0(title1, 'nFQ=', table(df$Form)['FQ'],', nHQ=', table(df$Form)['HQ'], ', nSQ=', table(df$Form)['SQ'])

p1 <- ggplot(df, aes(x=log_ratio))
p1 + geom_density(aes(color=Form)) + ggtitle(title1) + labs(x='Log ratio', y='')

df2 <- data.frame(Type=rep(c('log.txt', 'submission.xml'), each=nrow(df)), Size=(c(df$log,df$xml)/1000))

title2 <- 'Density plot of the file sizes of XML submissions and logging text files\n'
title2 <- paste0(title2, "n=",nrow(df), ' for each type')

p2 <- ggplot(df2, aes(x=Size))
p2 + geom_density(aes(color=Type)) + ggtitle(title2) + labs(x='File size (KB)', y='') + xlim(0,75)
