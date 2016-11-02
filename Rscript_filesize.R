#!/usr/bin/env Rscript

#library(argparse)

#parser <- ArgumentParser(description='Get the file size(s)')
#parser$add_argument('R Script Directory', metavar='N', type="character", nargs='+',
#                    help='The directory where your R scripts are present')
#parser$add_argument('Instances directory', 
#                    help='Directory where your form instance is present')
#parser$print_help()
# default args for ArgumentParser()$parse_args are commandArgs(TRUE)
# which is what you'd want for an Rscript but not for interactive use
#args <- parser.parse_args() 


#library("optparse")
#option_list = list(
#  make_option(c("-f", "--file"), type="character", default=NULL, 
#              help="R Scripts directory", metavar="character"),
#  make_option(c("-o", "--input"), type="character",
#              help="Instances working directory", metavar="character")
#); 

#opt_parser = OptionParser(option_list=option_list);
#opt = parse_args(opt_parser, args = commandArgs(TRUE));


args <- commandArgs(TRUE)
rscript_dir <- args[1]
src_dir <- args[2]
#Set working directory as the rscripts_dir
setwd(rscript_dir)
source("C:\\Users\\Shyamalee\\Desktop\\PMA2020\\PMA2020\\file_script.R")
file_size(rscript_dir, src_dir)

#C:\Users\Shyamalee\Desktop\PMA2020\PMA2020\Rscript_filesize.R
#Rscript C:\Users\Shyamalee\Desktop\PMA2020\PMA2020\Rscript_filesize.R C:\Users\Shyamalee\Desktop\PMA2020\PMA2020  D:\NGR3-Female-Questionnaire-v11\instances
#Rscript C:\Users\Shyamalee\Desktop\PMA2020\PMA2020\Rscript_filesize.R C:\Users\Shyamalee\Desktop\PMA2020\PMA2020 E:\Ghana\Round5\ODK Briefcase Storage\forms\GHR5-Female-Questionnaire-v12\instances