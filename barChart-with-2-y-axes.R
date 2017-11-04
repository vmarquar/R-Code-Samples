library(reshape2)
library(ggplot2)
library(geohydvm) # get TUM colors, own import function etc

# 1. Data Conversion into plottable format
df <- data.frame(Person = c("Mr.A","Mr.B"), Cats = c(3,4), Dogs = c(1,2), Frogs = c(2,3), Monkeys = c(2,2))
# df
#   Person Cats Dogs
# 1   Mr.A    3    1
# 2   Mr.B    4    2

# 2. Data conversion
df$Cats <- df$Cats*0.5 # or whatever the formula is!!!
data.m <- melt(df, id.vars='Person') # TODO: Put grouping variable here, e.g. group by Person
data.m
# Person variable value
# 1   Mr.A     Cats     3
# 2   Mr.B     Cats     4
# 3   Mr.A     Dogs     1
# 4   Mr.B     Dogs     2


# 2. Define plot environment and colors 
# label expressions (!): http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
# demo(plotmath)
left_axis_name <- expression(paste(delta^{2}, "H (\u2030)"))
right_axis_name <- expression(SO[1]^2) # ohne "" text-kennzeichner eingeben! alpha, beta, gamma usw.
axis_conversion <- sec_axis(~.*0.5, name= right_axis_name) # formula for data conversion on axis, e.g ~.+10 to add 10 linerally; ~.*0.5 to half the axis
                                                          

primary_color <- geohydvm::TUMBlauDunkel
secondary_color <- geohydvm::TUMBlauHell
tertiary_color <- geohydvm::TUMElfenbein
fourth_color <- geohydvm::TUMGrau

# 3. Plot Bar Charts with two Y-Axes
# Define Grouping variable: e.g. Person (same as above!)
# Define data vector, e.g. value
ggplot(data.m, aes(Person, value)) + geom_bar(aes(fill = variable), 
  width = 0.4, position = position_dodge(width=0.5), stat="identity") +
  theme_minimal() +
  # Create a simple secondary axis
  scale_y_continuous(sec.axis = axis_conversion, name= left_axis_name) +
  theme(legend.position = "bottom") + 
  # Actually it's recommened to use a named color vector
  # scale_fill_manual("",values = c("Cats" = primary_color, "Dogs" = secondary_color))
  scale_fill_manual(values=c(primary_color, secondary_color, tertiary_color, fourth_color))


