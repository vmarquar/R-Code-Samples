# Written by V.M. on 21.05.17
# Plots stable isotopes and optionally adds a regression line, GMWL etc.
source("io.R") # simple I/O Wrapper
source("TUMColors.R") #import TUM Colors
library(ggplot2)
library(plyr)

# Data Import
data_path <- "/Users/Valentin/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/2 Semester/7 Isotopenhydrogeologie/Ü2/daten_irland.xlsx"
precipitation <- xls_to_dataframe(path=data_path, sheet=1, range='A2:C49',header=c('Date','O18','H2'))
summer2008 <- xls_to_dataframe(path=data_path, sheet=2, range='A3:C36',header=c('Name','O18','H2'))
winter2009 <- xls_to_dataframe(path=data_path, sheet=2, range='D3:F15',header=c('Name','O18','H2'))
mean_weighted <- data.frame('O18'=-5.13,'H2'=-35.8,'group'='mean_weighted' )
d_excess_mean <- data.frame('winter'=10.5, 'summer'=4.5,'group'='d_excess_mean')
GMWL <- data.frame('O18'=c(-13.5,-5,0,8), 'H2'=c(-100,-32,-1,0),'group'='GMWL')

# Data Wrangling
precipitation <- data.frame(precipitation,'group'='precipitation')
summer2008 <- data.frame(summer2008,'group'='summer2008')
winter2009 <- data.frame(winter2009,'group'='winter2009')

#df <- merge(prec_tmp,summer_tmp, all.x = TRUE, all.y=TRUE) # if there are two datasets
isotopes <- join_all(list(precipitation,summer2008,winter2009,mean_weighted), type = 'full', match='all') # for more than two ds, requires plyr


# Simple Visualizations
summary(precipitation) 

#qplot(x=O18, y=H2, data = precipitation, xlim = c(-15,0), ylim=c(-100,0))
# simple point plot
ggplot(data=precipitation) +
  geom_point(aes(x=O18,y=H2)) +
  lims(x = c(-15,0), y=c(-100,0)) +
  labs(x=expression(paste(delta^{18}, "O (\u2030)")),y=expression(paste(delta^{2}, "H (\u2030)"))) + 
  theme_classic() # with grid theme_bw | without grid: theme_classic


# Point plot with GMWL
ggplot(data=precipitation) +
  geom_point(aes(x=O18,y=H2)) +
  geom_smooth(data=GMWL, aes(x=O18,y=H2),method='lm',formula=y~x,color = 'red', se=FALSE) +
  lims(x = c(-15,0), y=c(-100,0)) +
  labs(x=expression(paste(delta^{18}, "O (\u2030)")),y=expression(paste(delta^{2}, "H (\u2030)"))) + 
  theme_classic() # with grid theme_bw | without grid: theme_classic

# Point plot with GMWL + additional data
ggplot(data=precipitation) +
  geom_point(aes(x=O18,y=H2)) +
  geom_point(data=summer2008, color= TUMBlauDunkel, aes(x=O18,y=H2)) +
  geom_density2d(data=summer2008, color=TUMBlauDunkel, aes(x=O18,y=H2)) +
  geom_point(data=winter2009, color= TUMElfenbein, aes(x=O18,y=H2)) +
  geom_density2d(data=winter2009, color=TUMElfenbein, aes(x=O18,y=H2)) +
  geom_smooth(data=GMWL, aes(x=O18,y=H2),method='lm',formula=y~x,color = 'red', se=FALSE) +
  lims(x = c(-15,0), y=c(-100,0)) +
  labs(x=expression(paste(delta^{18}, "O (\u2030)")),y=expression(paste(delta^{2}, "H (\u2030)"))) + 
  #scale_shape_discrete(name = "Groups", labels=c("A", "B", "C")) +
  theme(legend.position = "bottom") +
  guides(fill = 'none') +
  theme_classic() # with grid theme_bw | without grid: theme_classic


# create unified datasubsets to simplify plotting
ggplot(isotopes)+
  geom_point(aes(x=O18, y=H2,colour = factor(group))) +
  geom_smooth(data=GMWL, aes(x=O18,y=H2),method='lm',formula=y~x,color = 'red', se=FALSE) +
  lims(x = c(-15,0), y=c(-100,0)) +
  labs(x=expression(paste(delta^{18}, "O (\u2030)")),y=expression(paste(delta^{2}, "H (\u2030)"))) + 
  theme(legend.position = "bottom") +
  guides(fill = 'none') +
  theme_bw() # with grid theme_bw | without grid: theme_classic

  

