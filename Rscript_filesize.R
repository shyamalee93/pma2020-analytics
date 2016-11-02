#!/usr/bin/env Rscript
#library("optparse")
#option_list = list(
#  make_option(c("-f", "--file"), type="character", default=NULL, 
#              help="dataset file name", metavar="character"),
#  make_option(c("-o", "--out"), type="character", default="out.txt", 
#              help="output file name [default= %default]", metavar="character")
#); 

#opt_parser = OptionParser(option_list=option_list);
#opt = parse_args(opt_parser, args = commandArgs(TRUE));

#if (is.null(opt$file)){
#  print_help(opt_parser)
#  stop("At least one argument must be supplied (input file).n", call.=FALSE)
#}

args <- commandArgs(TRUE)
rscript_dir <- args[1]
src_dir <- args[2]
source("C:\\Users\\Shyamalee\\Desktop\\PMA2020\\PMA2020\\file_script.R")

#C:\Users\Shyamalee\Desktop\PMA2020\PMA2020\Rscript_filesize.R
#Rscript C:\Users\Shyamalee\Desktop\PMA2020\PMA2020\Rscript_filesize.R C:\Users\Shyamalee\Desktop\PMA2020\PMA2020  D:\NGR3-Female-Questionnaire-v11\instances