# use this script to import io.R and clean up your data
# carry out all anaylsis in our RMD Script

if(!exists("xls_to_dataframe", mode="function")) source("io.R")

gwt <- xls_to_dataframe(path='/Users/Valentin/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/2 Semester/4 Geostatistik/Ü1 Histogram/Übung_Histogramm.xlsx',sheet=1,range=NULL,header=TRUE)
gwt1 <- gwt[,1] # 1. Spalte Achtung hier als Tibble, bei read.csv2() import wäre gwt1 ein vector
hist(gwt1)
as.data.frame(gwt)
PAK1 <- gwt$PAK[!is.na(gwt$PAK)]

PAK2 <- PAK1 * 1000
# Save session to file
print(getwd())
save.image(file = 'cleandata.RData')
