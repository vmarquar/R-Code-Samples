################# HELPER FUNCTIONS #########
library(ggplot2)
library(shiny)

mg_meq_conversion <- function(Mg, Ca, K, Na, NH4, Cl, Fl, SO4, NO2, NO3, HCO3){
  # input as mg/L or g/L(datatype vector or float)
  # output as mmol/L or mol/L (depends on input)
  return(as.data.frame(list("Ca"=(20.04 * Ca),"Mg"=(12.156 * Mg),"K"=(39.01*K),"Na"=(22.99*Na),'NH4'=(18.039 * NH4),"Cl"=(35.453 * Cl),"Fl"=(18.9984 * Fl),"SO4"=(48.03 * SO4),"NO2"=(46.006 * NO2),"NO3"=(62.005 * NO3),"HCO3"=(61.02 * HCO3))))
}

meq_mmol_percent_conversion <- function(Mg, Ca, K, Na, NH4, Cl, Fl, SO4, NO2, NO3, HCO3){
  # converts mol equivalents from mmol/L into meq %
  cations = data.frame("Ca"=Ca,"Mg"=Mg,"K"=K,"Na"=Na,"NH4"=NH4)
  anions = data.frame("Cl"=Cl,"SO4"=SO4,"Fl"=Fl,"NO2"=NO2, "NO3"=NO3, "HCO3"=HCO3)

  meq_sum_cations <- rowSums(cations)
  cat(sprintf('Sum of cations in [mmol/L] per row: %s\n',meq_sum_cations))
  meq_sum_anions <- rowSums(anions)
  cat(sprintf('Sum of anions in [mmol/L] per row: %s\n',meq_sum_anions))
  meq_cations_percent <- (cations/meq_sum_cations)*100
  meq_anions_percent <- (anions/meq_sum_anions)*100
  return(data.frame(meq_cations_percent,meq_anions_percent))
}

transform_piper_data <- function(Mg, Ca, Cl, SO4, name=NULL){
  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y1 <- Mg * 0.86603
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * 0.86603
  x2 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)
  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=name,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y))
}


ggplot_piper <- function() {
  library(ggplot2)
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))

  p <- ggplot() +
    ## left hand ternary plot
    geom_segment(aes(x=0,y=0, xend=100, yend=0)) +
    geom_segment(aes(x=0,y=0, xend=50, yend=86.603)) +
    geom_segment(aes(x=50,y=86.603, xend=100, yend=0)) +
    ## right hand ternary plot
    geom_segment(aes(x=120,y=0, xend=220, yend=0)) +
    geom_segment(aes(x=120,y=0, xend=170, yend=86.603)) +
    geom_segment(aes(x=170,y=86.603, xend=220, yend=0)) +
    ## Upper diamond
    geom_segment(aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
    geom_segment(aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
    geom_segment(aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
    ## Add grid lines to the plots
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ### Labels and grid values
    #geom_text(aes(50,-10, label="Ca^2"), parse=T, size=4) + # Commented out, as parse=TRUE can cause issues

    geom_text(aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(c(95,85,75,65),grid1p3$y2, label=c(80, 60, 40, 20)), size=3) +
    # geom_text(aes(17,50, label="Mg^2"), parse=T, angle=60, size=4) +
    coord_equal(ratio=1)+
    geom_text(aes(17,50, label="Mg^{2+phantom()}"), angle=60, size=3, parse=TRUE) +
    geom_text(aes(82.5,50, label="Na^{+phantom()} + K^{+phantom()}"), angle=-60, size=3, parse=TRUE) +
    geom_text(aes(50,-10, label="Ca^{2+phantom()}"), size=3, parse=TRUE) +


    geom_text(aes(170,-10, label="Cl^-phantom()"), size=3, parse=TRUE) +
    geom_text(aes(205,50, label="SO[4]^{2-phantom()}"), angle=-60, size=3, parse=TRUE) +
    geom_text(aes(137.5,50, label="Alkalinity~as~HCO^3"), angle=60, size=3, parse=TRUE) +
    geom_text(aes(72.5,150, label="SO[4]^{2-phantom()}~+~Cl^-phantom()"), angle=60, size=3, parse=TRUE) +
    geom_text(aes(147.5,150, label="Ca^{2+phantom()}~+~Mg^{2+phantom()}"), angle=-60, size=3, parse=TRUE) +

    geom_text(aes(c(155,145,135,125),grid2p2$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    geom_text(aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    geom_text(aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
  return(p)
}



################## SERVER ##################
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    #data=as.data.frame(list("Mg"=c(16.39), "Ca"=c(81.8), "K"=c(1.1), "Na"=c(5.0),"NH4"=c(0.018), "Cl"=c(10.35),
    #                       "Fl"=c(0.048), "SO4"=c(18.75), "NO2"=c(0.013),
    #                       "NO3"=c(6.41), "HCO3"=c(335.6),"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))
    print(data)
    #data=as.data.frame(list("Mg"=input$Mg, "Ca"=input$Ca, "K"=input$K, "Na"=input$Na,"NH4"=input$NH4, "Cl"=input$Cl, "Fl"=input$Fl, "SO4"=input$SO4, "NO2"=input$NO2, "NO3"=input$NO3, "HCO3"=input$HCO3,"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))
    data=as.data.frame(list("Mg"=input$Mg, "Ca"=input$Ca, "K"=input$K, "Na"=input$Na,"NH4"=input$NH4,
                            "Cl"=input$Cl, "Fl"=input$Fl, "SO4"=input$SO4, "NO2"=input$NO2,
                            "NO3"=input$NO3, "HCO3"=input$HCO3,"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))


    #data=as.data.frame(list(input$Mg, input$Ca, input$K, input$Na,input$NH4,input$Cl,input$Fl,input$SO4,input$NO2,input$NO3,input$HCO3,"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))
    ### convert from mg/L to mmol/L (or from g/L to mol/L)
    data_meq_mmol = mg_meq_conversion(Mg=data$Mg, Ca=data$Ca, K=data$K, Na=data$Na, NH4=data$NH4, Cl=data$Cl, Fl=data$Fl, SO4=data$SO4, NO2=data$NO2, NO3=data$NO3, HCO3=data$HCO3)
    ### convert from mmol/L molequivalents into meq in percent
    data_meq_perc = meq_mmol_percent_conversion(Mg=data_meq_mmol$Mg, Ca=data_meq_mmol$Ca, K=data_meq_mmol$K, Na=data_meq_mmol$Na, NH4=data_meq_mmol$NH4, Cl=data_meq_mmol$Cl, Fl=data_meq_mmol$Fl, SO4=data_meq_mmol$SO4, NO2=data_meq_mmol$NO2, NO3=data_meq_mmol$NO3, HCO3=data_meq_mmol$HCO3)
    #transform the data into piper based coordinates
    piper_data <- transform_piper_data(Ca=data_meq_perc$Ca, Mg = data_meq_perc$Mg, Cl=data_meq_perc$Cl, SO4= data_meq_perc$SO4, name=data$WaterType)

    # Now points can be added like...
    ggplot_piper() + geom_point(aes(x,y), data=piper_data)

  })


#   observe({
#     # limit the scherfestigkeit to max the half size of druckfestigkeit
#     updateSliderInput(session, "tf", value = input$tf,
#                       min = 0, max = input$tp/2)
#   })

}


################## UI ##################
ui <- fluidPage(
  fluidRow(
    column(3,
           h4("Select Cation Concentrations"),
           numericInput("Mg", "Mg [mg/l or mol/l]:", 10, step=0.01),
           numericInput("Ca", "Ca [mg/l or mol/l]:", 10, step=0.01),
           numericInput("K", "K [mg/l or mol/l]:", 10, step=0.01),
           numericInput("Na", "Na [mg/l or mol/l]:", 10, step=0.01),
           numericInput("NH4", "NH4 [mg/l or mol/l]:", 10, step=0.01)
           ),
    column(3,
           h4("Select Anion Concentrations"),
           numericInput("Cl", "Cl [mg/l or mol/l]:", 10, step=0.01),
           numericInput("SO4", "SO4 [mg/l or mol/l]:", 10, step=0.01),
           numericInput("NO2", "NO2 [mg/l or mol/l]:", 10, step=0.01),
           numericInput("NO3", "NO3 [mg/l or mol/l]:", 10, step=0.01),
           numericInput("HCO3", "HCO3 [mg/l or mol/l]:", 10, step=0.01),
           numericInput("Fl", "Fl [mg/L or mol/L]:",0.048, step=0.01)
           ),
      column(6,plotOutput("distPlot"))
    )
  )

shinyApp(ui = ui, server = server)
