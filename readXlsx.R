#### BOILERPLATE 1. READ IN XLS/XLSX DATA
library(readxl)
library(ggplot2)
library(scales)
path = '/Users/Valentin/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/Vertiefung Hydrogeologie/2-2 Pumpversuch Poröse Medien.xlsx'
# column types: "date", "numeric", "blank" or "text"
col_types = c("date","numeric","numeric","numeric","numeric","numeric")
# read_excel reads both xls and xlsx files
# Specify sheet with a number or name, starting with 1, skip = skip rows
dataset = read_excel(path = path, sheet = "Raw Data",col_names = TRUE,col_types = col_types, skip = 0)


#### BOILERPLATE 2. Clean Data, Modify Rows/Cols, Tranform Data
# https://sites.ualberta.ca/~ahamann/teaching/renr690/R_Cheat_Data.pdf

#remove rows 1,2
dataset=dataset[-c(1,2),]

# dat1[1:10, 1:5] returns the first 10 rows and the first 5 columns of table dat1.
# dataset[rows,cols]
# dataset[3,-1] # 3rd row, without first col

#dat2=transform(dat1, VAR1=VAR1*0.4). Multiplies VAR1 by 0.4
#dataset1 = transform(dataset, dataset[,3:6] = dataset[,3:6]*-1)

t = dataset[8:304,2]
well1 = dataset[8:304,3]*-1
well2 = dataset[8:304,4]*-1
well_pump = dataset[8:304,5]*-1
well3 = dataset[8:304,6]*-1

wells = list(well1,well2,well_pump,well3)
titles = list("well1","well2","well_pump","well3")
x_labels = list ("Time t","Time t","Time t","Time t")
y_labels = list("Drawdown s","Drawdown s","Drawdown s","Drawdown s")

#### BOILERPLATE 3. Plot data, fit plots to data, regression, analysis

for (well in 1:length(wells)) {
  #plot(t,well)
  #print(well)
  df = data.frame(t,wells[well])
  title = titles[[well]]
  x_lab = x_labels[[well]]
  y_lab = y_labels[[well]]
  colnames(df) <- c("x", "y")
  p = ggplot(df,aes(x,y))+ geom_point()+scale_x_log10()+coord_cartesian(xlim = c(0,100))+
      labs(title=title,x=x_lab,y=y_lab)+theme_bw()
  print(p)

}
