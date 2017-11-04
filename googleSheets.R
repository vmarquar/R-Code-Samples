# Created by V.Marquart on 16.08.2017
# Fetches Data from a google spreadsheet and displays borehole data

# 1. Get Data from google sheets which is connected to a google form
library(googlesheets)
borehole_sheet <- gs_title("Schichtenverzeichnis_Antworten")
borehole_data <- gs_read(borehole_sheet)
# 2. Data Validation

# 3. Plot borehole diagram

# read in latest borehole log
borehole <- borehole_data[-1, ]


# construct layer object
# attributes:
# 1. depth (endteufe)
# 2. short soil description (dictionary after GeoDIN)
# 3. long description (derived from dictionary)
# 4. plasticity
# 5. lagerungsdichte
# 6. 





