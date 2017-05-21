# Written by V.M. on 22.04.2017
# xls_to_dataframe is just a small wrapper around the readxl library, that enable to select a specific range of cells,
# but keep the first row of the excel sheet as row headers.
# your excel files should be formatted like this:
#  A        B 
#1 header_1 header_2
#2 --- rows that should be omitted , e.g. [mg/L] etc ---
#3 data that should be imported


library(readxl)

xls_to_dataframe <- function(path='path/to/file.xls', sheet='name or index', range='NULL or "A3:C10"', header = 'TRUE or FALSE or c("A", "B") or range_with_first_line_as_header'){
  # HEADER OPTIONS: TRUE; FALSE; c('A','B','C'); 'range_with_first_line_as_header'
  # TRUE: take first row as header line from range or if range is not specified take first row of excel sheet
  # FALSE: interpret everthing of range as data or if range is not specified take the whole spreadsheet as data. Generic row names will be generated X__1, X__2, etc.
  # c('A','B','C'): everything will be interpreted as data and specified vector will be used for Column names (length must be the same as data rows)
  # 'range_with_first_line_as_header': keep the first row of the excel file as input and keep the range as data (only works with the first row of excel file and a specified range)

  # Possibilites: col_types=c("skip", "guess", "logical", "numeric", "date", "text","list")
  if(header=='range_with_first_line_as_header'){
    header <- read_excel(path=path,
                    sheet=sheet,
                    range=gsub('\\(?[0-9,.]+','A1:C11',replacement = '1'), #regex that finds all numbers from 0-infinity and replaces them by 1
                    col_names=TRUE)
    print(header)
    df <- read_excel(path=path,
                    sheet=sheet,
                    range=range,
                    col_names = FALSE
                    )
    colnames(df) <- names(as.vector(header))
    return(df) # returns tibble, dataframe like object
  }
  if (!(length(range)<=0) & !(header=='range_with_first_line_as_header')){
    df <- read_excel(path=path,
                    sheet=sheet,
                    range=range,
                    col_names = header
                    )
    return (df)
  }
  if (length(range)<=0){
    df <- read_excel(path=path,
                    sheet=sheet,
                    col_names = header
                    )
    return (df)
  }
}

# Dates and Times: https://www.stat.berkeley.edu/~s133/dates.html
# Forcing conversion from String to Boolean: TRUE is returned if and only if the target string is "true" (ignoring any capitalization).
# Any other string will return FALSE.
# Forcing conversion from Numeric to DateTime: since Excel understands Dates/Times as Numerics with some additional formatting,
# a conversion from a Numeric to a DateTime is actually possible. Numerics in this case represent the number of days since
# 1900-01-00 (yes, day 00! - see http://www.cpearson.com/excel/datetime.htm).
# Note that in R 0 is represented as 1899-12-31 since there is no 1900-01-00. Fractional days represent hours, minutes, and seconds.
