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

# FLINZLEHM
shear_1b_parallel = mean(c(3.25,3,3.25,3.5,3.5))
kN_shear_1b_parallel <- shear_1b_parallel*25
shear_1b_perpend = mean(c(5.5,5.0,5.75,6.5,5.5))
kN_shear_1b_perpend <- shear_1b_perpend*25

kN_penetrometer_1b_parallel <- mean(c(450,450,400,450,450))
kN_penetrometer_1b_perpendicular <- mean(c(450,450,450,400,450))

# SANDPROBE
shear_1a_parallel = mean(c(3,6.5,6.5,6.75,4.25))
kN_shear_1a_parallel <- shear_1a_parallel*2
shear_1a_perpend = mean(c(3,2.5,5,2.5,2.5))
kN_shear_1a_perpend <- shear_1a_perpend*2

kN_penetrometer_1a_parallel <- mean(c(325,360,250,450,375))
kN_penetrometer_1a_perpendicular <- mean(c(250,450,440,425,400))







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
  # R1: smaller circle
  # R2: big circle
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

mohrAuswertung <- function(shear,penetrometer,title){
  r1 <- shear
  r2 <- penetrometer/2
  small_circle <- createCircle(center = c(0,0),diameter = shear*2)
  big_circle <- createCircle(center = c(r2,0),diameter = penetrometer)
  tangent_points <- computeOuterTangent(centerR1 = c(0,0),centerR2 = c(r2,0),radiusR1 = r1,radiusR2 = r2)
  plot_data <- data.frame(small_circle,big_circle,x_tanget=c(tangent_points$x3,tangent_points$x4),y_tanget=c(tangent_points$y3,tangent_points$y4))

  fit <- lm(plot_data$y_tanget ~ plot_data$x_tanget)
  cohesion <- fit$coefficients[1]
  m <- fit$coefficients[2]
  friction <- atan(m)*180/pi
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
    annotate("text", x=-8,y=cohesion,parse = TRUE,label=paste('c == ',round(cohesion,digits=2))) +
    annotate("text",x=x_end,y=y_end+y_end*0.05,parse = TRUE, label= label_friction) +
    annotate("text", x=0,y=y_end+y_end*0.05,parse = TRUE, label= 'Tau') +
    annotate("text", x=(2.5+penetrometer+penetrometer*0.20),y=0,parse = TRUE, label= 'sigma') +
    annotate("text", x=(penetrometer*3/4),y=2,parse = TRUE, label= label_r1) +
    annotate("text", x=(shear/2), y=2, label = label_r2, parse = TRUE) +

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
    annotate("text", x=-8,y=cohesion,parse = TRUE,label=paste('c == ',round(cohesion,digits=2))) +
    annotate("text",x=x_end,y=y_end+y_end*0.05,parse = TRUE, label= label_friction) +
    annotate("text", x=0,y=y_end+y_end*0.05,parse = TRUE, label= 'Tau') +
    annotate("text", x=(2.5+penetrometer+penetrometer*0.20),y=0,parse = TRUE, label= 'sigma') +
    annotate("text", x=(penetrometer*3/4),y=2,parse = TRUE, label= label_r1) +
    annotate("text", x=(shear/2), y=2, label = label_r2, parse = TRUE) +

    labs(x=expression(sigma~'[kN/m\u0032]'),y=expression(tau~'[kN/m\u0032]')) +
    # create x axis arrow:
    geom_segment(aes(x = x_00+penetrometer*-0.2, y = 0, xend = (penetrometer+penetrometer*0.2), yend = 0), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # create y axis arrow:
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = (r2+r2*0.2)), arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
    # Fix aspect ratio
    coord_fixed() +
    ggtitle(title) +
    theme_minimal()
  print(p2)

  #return(plot_data)
  return(data.frame(cohesion=cohesion,friction_angle=friction))
}
#plot_data <- mohrAuswertung(shear = shear, penetrometer = penetrometer)

# AUSWERTUNG FLINZLEHM
parallel_1b <- mohrAuswertung(shear = kN_shear_1b_parallel, penetrometer = kN_penetrometer_1b_parallel, title='Probe Ton 1b - parallel')
perpend_1b <- mohrAuswertung(shear = kN_shear_1b_perpend, penetrometer = kN_penetrometer_1b_perpendicular, title='Probe Ton 1b - senkrecht')

# AUSWERTUNG OSM-SAND
parallel_1a <- mohrAuswertung(shear = kN_shear_1a_parallel, penetrometer = kN_penetrometer_1a_parallel,title='Probe Sand 1a - parallel')
perpend_1a <- mohrAuswertung(shear = kN_shear_1a_perpend, penetrometer = kN_penetrometer_1a_perpendicular, title='Probe Sand 1a - senkrecht')

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
