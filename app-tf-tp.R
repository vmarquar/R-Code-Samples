################# HELPER FUNCTIONS #########
library(ggplot2)
library(grid)
library(shiny)


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


################## SERVER ##################
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
  
    # hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    
    auswertung <- mohrAuswertung(shear = input$tf, penetrometer = input$tp, title='Auswertung der Taschenpenetrometer & Taschenflügelsonde')
    
    
  })
  observe({
    # limit the scherfestigkeit to max the half size of druckfestigkeit
    updateSliderInput(session, "tf", value = input$tf,
                      min = 0, max = input$tp/2)
  })
  
}


################## UI ##################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("tp", "Druckfestigkeit [kN/m2]:", min = 10, max = 500, value = 250),
      
      sliderInput("tf", "Scherfestigkeit [kN/m2]: Maximal die Hälfte der Druckfestigkeit", min = 10, max = 500, value = 100)
      
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)