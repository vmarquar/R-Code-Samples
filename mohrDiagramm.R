# Written by vmarquar on 09.05.2017
# Can be used to evaluate cohesion and angle of friction by (pocket-)penetrometer and (pocket-)shear-taster
# Disclaimer: This has been written quick&dirty and probably contains bugs. Feel free to fork / submit improvements :)
# Output are two ggplot: 
# 1. Minimal Mohr Circle Plot without tiks and labels
# 2. Labeled Mohr Plot with grid

library(ggplot2)
library(grid)

# Inputdata of the field penetrometer & vane/shear-tester
# Mittelwerte der Flügelsonde und Penetromer [kN/m^2]
shear = mean(c(6.5,6.0,6.5,7.0,7.0))
penetrometer = 8.92*10 #[kN/m^2]

# circle function
createCircle <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

computeOuterTangent <- function(centerR1=c(0,0),centerR2=c(1,0),radiusR1=3,radiusR2=5){
  # see: https://en.wikipedia.org/wiki/Tangent_lines_to_circles
  #R1: smaller circle
  #R2: big circle
  x1 <- centerR1[1]
  x2 <- centerR2[1]
  y1 <- centerR1[2]
  y2 <- centerR2[2]

  # Calulate direction alpha:
  gamma <- atan((y1-y2)/(x2-x1))
  beta <- asin((radiusR2-radiusR1)/(sqrt((x2-x1)^2 + (y2-y1)^2)))
  alpha <- gamma - beta

  # Calculate tangent points:
  x3 <- x1 + radiusR1 * cos((pi/2)-alpha)
  y3 <- y1 + radiusR1 * sin((pi/2)-alpha)
  x4 <- x2 + radiusR2 * cos((pi/2)-alpha)
  y4 <- y2 + radiusR2 * sin((pi/2)-alpha)

  return(data.frame(x3=x3,x4=x4,y3=y3,y4=y4,alpha=alpha))
}
# AUSWERTUNG 1: (Taschen-)Penetrometer und (Taschen-)Flügelsonde // vane (shear)-tester and penetrometer test
mohrAuswertung <- function(shear,penetrometer){
  r1 <- shear
  r2 <- penetrometer/2
  small_circle <- createCircle(center = c(0,0),diameter = shear*2)
  big_circle <- createCircle(center = c(r2,0),diameter = penetrometer)
  tangent_points <- computeOuterTangent(centerR1 = c(0,0),centerR2 = c(r2,0),radiusR1 = r1,radiusR2 = r2)
  plot_data <- data.frame(small_circle,big_circle,x_tanget=c(tangent_points$x3,tangent_points$x4),y_tanget=c(tangent_points$y3,tangent_points$y4))
  
  fit <- lm(plot_data$y_tanget ~ plot_data$x_tanget)
  cohesion <- fit$coefficients[1]
  m <- fit$coefficients[2]
  friction <- atan(1.62)*180/pi
  #x=0 @ (y-t)/m=x with y=0:
  x_00 <- (-1*cohesion)/m # x-Wert an dem die X-Achse geschnitten wird
  y_end <- (r2+r2*0.2)
  x_end <- (y_end-cohesion)/m
  
  fitting_line <- data.frame('x'=c(x_00,plot_data$x_tanget[1:2],x_end),'y'=c(0,plot_data$y_tanget[1:2],y_end))
  label_friction <- paste('varphi', " == ", round(friction,digits=2))
  label_r1 <- paste('r1',' == ',round(penetrometer,digits=2))
  label_r2 <- paste('r2',' == ',round(shear,digits=2))
  
  print(label_friction)
  p1 <- ggplot(plot_data) +
    # plot small circle:
    geom_path(aes(x=x, y = y)) +
    # plot big circle:
    geom_path(aes(x=x.1, y = y.1)) +
    # plit center and end points of circle
    geom_point(data=data.frame('x'=c(0,penetrometer/2,penetrometer),'y'=c(0,0,0)), aes(x=x,y=y)) +
    # plot fitting line
    geom_smooth(data=fitting_line, aes(y=y,x=x),method='lm',formula=y~x,color = 'black') +
    #limits
    ylim(0,penetrometer-0.2*penetrometer) + 
    # Labels: 1.Cohesion 2. Friction 3. Y-Axis 4. X-Axis 5. R1-Label 6. R2-Label
    geom_text(x=-8,y=cohesion,parse = TRUE,aes(label=paste('c == ',round(cohesion,digits=2)))) +
    geom_text(x=x_end,y=y_end+y_end*0.05,parse = TRUE, aes(label= label_friction)) + 
    geom_text(x=0,y=y_end+y_end*0.05,parse = TRUE, aes(label= 'Tau')) + 
    geom_text(x=(2.5+penetrometer+penetrometer*0.20),y=0,parse = TRUE, aes(label= 'sigma')) + 
    geom_text(x=(penetrometer*3/4),y=2,parse = TRUE, aes(label= label_r1)) + 
    geom_text(x=(shear*2+0.3),y=2,parse = TRUE, aes(label= label_r2)) + 
    
    labs(x=expression(sigma),y=expression(tau)) + 
    # create x axis arrow:
    geom_segment(aes(x = x_00+penetrometer*-0.2, y = 0, xend = (penetrometer+penetrometer*0.2), yend = 0), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # create y axis arrow:
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = (r2+r2*0.2)), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # Fix aspect ratio
    coord_fixed() +
    theme_void()
  print(p1)
  
  p2 <- ggplot(plot_data) +
    # plot small circle:
    geom_path(aes(x=x, y = y)) +
    # plot big circle:
    geom_path(aes(x=x.1, y = y.1)) +
    # plit center and end points of circle
    geom_point(data=data.frame('x'=c(0,penetrometer/2,penetrometer),'y'=c(0,0,0)), aes(x=x,y=y)) +
    # plot fitting line
    geom_smooth(data=fitting_line, aes(y=y,x=x),method='lm',formula=y~x,color = 'black') +
    #limits
    ylim(0,penetrometer-0.2*penetrometer) + 
    # Labels: 1.Cohesion 2. Friction 3. Y-Axis 4. X-Axis 5. R1-Label 6. R2-Label
    geom_text(x=-8,y=cohesion,parse = TRUE,aes(label=paste('c == ',round(cohesion,digits=2)))) +
    geom_text(x=x_end,y=y_end+y_end*0.05,parse = TRUE, aes(label= label_friction)) + 
    geom_text(x=0,y=y_end+y_end*0.05,parse = TRUE, aes(label= 'Tau')) + 
    geom_text(x=(2.5+penetrometer+penetrometer*0.20),y=0,parse = TRUE, aes(label= 'sigma')) + 
    geom_text(x=(penetrometer*3/4),y=2,parse = TRUE, aes(label= label_r1)) + 
    geom_text(x=(shear*2+0.3),y=2,parse = TRUE, aes(label= label_r2)) + 
    
    labs(x=expression(sigma),y=expression(tau)) + 
    # create x axis arrow:
    geom_segment(aes(x = x_00+penetrometer*-0.2, y = 0, xend = (penetrometer+penetrometer*0.2), yend = 0), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # create y axis arrow:
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = (r2+r2*0.2)), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # Fix aspect ratio
    coord_fixed() +
    theme_minimal()
  print(p2)
  
  #return(plot_data)
  return(data.frame(cohesion=cohesion,friction_angle=friction))
}

plot_data <- mohrAuswertung(shear = shear, penetrometer = penetrometer)

# dat <- createCircle(c(1,-1),1,npoints = 100)
# #geom_path will do open circles, geom_polygon will do filled circles
# ggplot(dat,aes(x,y)) + geom_path()

#input to draw two circles:

# #Big circle
# sigma1_big <- 50
# sigma3_big <- 10
# tau_big <- 20
#
# #small circle
# sigma1_small <-  30
# sigma3_small <- 5
#
#
# # Mohr Calcs
# radius = (sigma1-sigma3)/2
# pol_length = (sigma1+sigma3)/2
# M = pol_length + sqrt(radius^2-tau^2)


