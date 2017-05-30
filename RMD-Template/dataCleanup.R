# use this script to import io.R and clean up your data
# carry out all anaylsis in our RMD Script

if(!exists("xls_to_dataframe", mode="function")) source("/Users/Valentin/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/0 Allgemeine Zusatzdokumente/Code Samples/Code Samples R/io.R")

gwt <- xls_to_dataframe(path='Übung_Histogramm.xlsx',sheet=1,range=NULL,header=TRUE)
gwt1 <- gwt[,1] # 1. Spalte Achtung hier als Tibble, bei read.csv2() import wäre gwt1 ein vector
hist(gwt1)
as.data.frame(gwt)
PAK1 <- PAK[!is.na(PAK)]
