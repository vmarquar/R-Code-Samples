# # # TODO # # #
# - Excel Anbindung mit LFU Konformen Daten
# - Grundwasserstand
# - Konsistenzbänder: "halbfest bis fest" unterstüzung implementieren
# - Konsistenzbänder: "weich" und "breeig" implementieren
# - Vertikale X-Axen-Labels zentrieren
# - Exakte Maßstäbe implementieren für A4, A3, A2
# - Schichtbezeichnungen und Striche überprüfen und auf Maßstäbe anpassen
# - Presets für eine Bohrung und für mehrere Bohrungen
# load_colors: weitere Farben über CSV Datei hinzuladen

require(ggplot2)
require(readxl)
require(grid)
library(data.table)
require(geohydvm)


# Conveniance functions to display the correct size in mm for my plots and to print in DIN A Formats
size_in_pt <- function(size_in_mm){
  # The unit of the size param in ggplot is always point. To display the correct size in mm use:
  # aes(size=size_in_pt(size_in_mm=5))
  return(size_in_mm * 1/0.352777778)
}

saveDINA <- function(filename="tmp.pdf", plot, size="A3",orientation="landscape"){
  heightDIN <- c("A0"=841, "A1"=594, "A2"=420, "A3"=297, "A4"=210) # landscape default
  widthDIN <- c("A0"=1189, "A1"=841, "A2"=594, "A3"=420, "A4"=297) # landscape default
  height <- as.numeric(heightDIN[size])*0.03937008 # inch-mm factor
  width <- as.numeric(widthDIN[size])*0.03937008 # inch-mm factor
  #ggsave(filename = filename, plot=plot, width= width, height = height)
  
  if(orientation == "landscape"){
    ggsave(filename = filename, plot=plot, width= width, height = height)
  } else {
    ggsave(filename = filename, plot=plot, width= height, height = width)
  }
}

# Create Colors and Dictionary mapping for several abbreviations and languages:
load_colors<-function(load_list=FALSE, list_path="", sheet="Farbtabelle"){
  humus_color = '#cea470'
  gravel_color = '#f3e03a'
  sand_color = '#e09637'
  silt_color = '#aba77d'
  clay_color = '#974b89'
  concrete_color = '#6f6f6f'
  color_dictionary = c("q"=gravel_color, gravel = gravel_color,sand = sand_color, silt = silt_color, clay = clay_color,
                       Kies = gravel_color, Sand = sand_color, Schluff = silt_color, Ton = clay_color,
                       G = gravel_color, S = sand_color, U = silt_color, "T" = clay_color, Humus = humus_color,
                       Feinkies = gravel_color, "Feinsand"=sand_color, "Auffüllung"="white",
                       "Beton"="grey", Grobschluff =silt_color, "Künstlicher Feststoff"="black", "Kalk (locker)"='#e9edd5',
                       "Künstliches Lockermaterial"="white", "Material nicht bekannt"="white", "Mittelkies" = gravel_color,
                       "Mittelsand"=sand_color, "Sedimentäres Lockergestein o.ä."="grey", "sonstiger Bohrprobenverlust"="grey",
                       "Ton bis Schluff"=clay_color,
                       "ku"="#af8963"	,"ku/km"="#af8963"	,"mm"="#79869d"	,"mo2"="#a4bad7"	,"mu"="#79869d","s?"="#d69365"	,"so"="#dda77b"	,"Stö"="black")
  if(load_list){
    # load additional key-colorvalue pairs from csv file
    # append to color_dictionary
    # loaded_colors <-new c("code"="color")
    #loaded_colors <- c("test"="#ffffff")
    loaded_colors <- read_excel(path = list_path,sheet = sheet)
    #color_codes <- as.character(loaded_colors[1]) # color codes
    #rgb_values <- as.character(loaded_colors[2]) #rgb values
    #return(loaded_colors)
    color_dictionary <- c(color_dictionary,as.vector(loaded_colors,mode = "character"))
    
  }
  return(color_dictionary)
}

create_profile <- function(df=df, color_dictionary = color_dictionary, plot_type="blank blank_mit_gw mit_schichten mit_schichten_konsistenzen mit_schichten_konsistenzen_lagerung", aspect_ratio=1.0, drill_width=5, only_first=TRUE){
  # ANLEITUNG ZUR LEICHTEN MANUELLEN VERBESSERUNG:
  # 1. Breite der Bohrungen mit dem width-parameter geom_col verändern
  # 2. Bohrungs-IDS mit dem Element axix.text.x um 90° drehen (oder eben horizontal lassen)
  # 3. Die scharze Umrandung kann in geom_col(color="black") gesetzt werden
  # 4. hjust und vjust können textankerpunkt angeben. (normal: 0.5 also zentriert)
  # 5. die Länge der Striche und Position der Texte kann mit df$x-0.15 variiert werden
  # 6. Das Seiten-zu-Höhenverhältnis kann unter theme(aspect.ratio=1) verändert werden.
  # POSSIBLE PLOT_TYPES = blank, mit_schichten, mit_schichten_konsistenzen, mit_schichten_konsistenzen_lagerung
  if(plot_type == "blank blank_mit_gw mit_schichten mit_schichten_konsistenzen mit_schichten_konsistenzen_lagerung"){
    stop("Please choose one of the following plot types: blank, blank_mit_gw, mit_schichten, mit_schichten_konsistenzen, mit_schichten_konsistenzen_lagerung")
  }
  
  # Plot only first item
  if(only_first==TRUE){
    df <- df[df$Bohr.ID == df$Bohr.ID[1], ]
  }
  
  # Create an x value from unique BohrungsID to seperate plots (when there is no specific x supplied)
  if(!("x" %in% colnames(df) | "X" %in% colnames(df))){
    df <- transform(df,x=as.numeric(factor(df$Bohr.ID)));
  }
  if(("X" %in% colnames(df))){
    colnames(df)[which(names(df) == "X")] <- "x"
  }
  
  
  # Berechne Anzahl der Schichten pro Bohrung
  df$ix <- as.numeric(ave(df$Bohr.ID, df$Bohr.ID, FUN = function(x) seq_along(x))) # See Stackoverflow for Details...
  
  # Berechne Endtiefe und Anfangstiefe mit Offset:
  setDT(df)[ , `:=`(start = start <- c(Offset[1] + c(0, cumsum(head(Schichtmaechtigkeit, -1)))),
                    end = start + Schichtmaechtigkeit), by = Bohr.ID]

    #### I) BLANKER PLOT ####
  # Plot borehole profiles without labels
  drilling <- ggplot(data = df, aes(x = x, xend = x, y = start, yend = end, color = Petrographie, group = ix)) +
    theme_minimal() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.text.x=element_text(angle=90, hjust = 0), # Preset1: 90° verkippt, Text untenbündig; dreht BohrungsID um 90°, hjust = 0: bündig nach unten
          #axis.text.x=element_text(angle=0, hjust = 0.5), # Preset2: horizontaler Text, mittig zentriert; dreht BohrungsID um 90°, hjust = 0: bündig nach unten
          axis.text.x.top = element_text(vjust=0.5),
          aspect.ratio=aspect_ratio) +
    geom_segment(aes(x=(x), xend=(x), y=start-0.05, yend=end+0.05), size=drill_width+1, color="black") + # schwarze Umrahmung
    geom_segment(size = drill_width) + # eigentliches Bohrprofil
    geom_segment(size = drill_width, color = "black", aes(y=start-0.05, yend=start, x = x, xend = x)) + # schwarze Zwischenstriche
    scale_y_reverse(expand = c(0, 0), name ="Tiefe [m]") +
    scale_x_continuous(position = "top", breaks = df$x, labels=paste(df$Bohr.ID,"\n",df$x,"m [TM]","\n",df$Ansatzhoehe, "m [Höh]"), name="") +
    scale_color_manual(values = color_dictionary)
  
  #### II) PLOT MIT SCHICHTBEZEICHNUNGEN UND TIEFENANGABE ####
  drilling1 <- drilling +
    
    # Bohrgutbeschreibung rechts des Bohrprofils
    annotate("text",x = df$x+0.05, y=df$end-df$Schichtmaechtigkeit/2, label = df$Gesteinsansprache.DIN.4022, group=df$ix,hjust=0, vjust=0.5, size=3) +
    annotate("segment", x = df$x, y=df$end,xend = df$x+0.1, yend = df$end, color="black") +
    
    # Höhenangaben links des Bohrprofils
    annotate("text",x = df$x-0.05, y=df$end, label = paste(df$Untergrenze, "m"), group=df$ix,hjust=0, vjust=-0.25, size=3) +
    annotate("segment", x = df$x, y=df$end,xend = df$x-0.1, yend = df$end, color="black")
  
  ### IIb) PLOT MIT GW STAND
  drilling1b <- drilling + 
    
    # Höhenangaben links des Bohrprofils
    #TODO: nur 1x pro Bohrung (derzeit buggy mit jeder Schicht pro Bohrung)
    annotate("text",x = df$x-1.25, y=df$GW+df$Offset, label = paste("GW:",df$GW, "m"), group=df$ix, hjust=0, vjust=-0.25, size=3, color ="blue") +
    annotate("segment", x = df$x-(drill_width*2), y=df$GW+df$Offset,xend = df$x+(drill_width*15), yend = df$GW+df$Offset, color="blue")
    
  
  
  
  
  #### III) PLOT MIT SCHICHTBEZEICHNUNGEN UND TIEFENANGABE UND KONSISTENZEN####
  # Generiere subsets für Konsistenzen und Lagerungsbeschriftung
  df_kfe <- df[df$`Zustand und Festigkeit` == "fest", ];df_kfe <- df_kfe[complete.cases(df_kfe[ , 6]),] # lösche NA-Reihen
  df_khf <- df[df$`Zustand und Festigkeit` == "halbfest", ];  df_khf <- df_khf[complete.cases(df_khf[ , 6]),] # lösche NA-Reihen
  df_kstf <- df[df$`Zustand und Festigkeit` == "steif", ];  df_kstf <- df_kstf[complete.cases(df_kstf[ , 6]),] # lösche NA-Reihen
  df_kwh <- df[df$`Zustand und Festigkeit` == "weich", ];  df_kwh <- df_kwh[complete.cases(df_kwh[ , 6]),] # lösche NA-Reihen
  df_kbr <- df[df$`Zustand und Festigkeit` == "breiig", ];  df_kbr <- df_kbr[complete.cases(df_kbr[ , 6]),] # lösche NA-Reihen
  
  # Füge die Konistenzbänder hinzu: (derzeit wird BIS nicht unterstützt)
  
  if(!(is.data.frame(df_kfe) && nrow(df_kfe)==0)){
    drilling2 <- drilling1 +
      geom_segment(data = df_kfe,aes(x=df_kfe$x+.11, xend=df_kfe$x+.11,
                                     y=df_kfe$start, yend=df_kfe$end), linetype=1, lwd=0.4) + # fest Teil 1 (erster Strich)
      geom_segment(data = df_kfe,aes(x=df_kfe$x+.13, xend=df_kfe$x+.13,
                                     y=df_kfe$start, yend=df_kfe$end), linetype=1, lwd=0.4) # fest Teil 2 (zweiter Strich)
  }
  if(!(is.data.frame(df_khf) && nrow(df_khf)==0)){
    drilling2 <- drilling1 +
      geom_segment(data = df_khf,aes(x=df_khf$x+.11, xend=df_khf$x+.11,
                                     y=df_khf$start, yend=df_khf$end), linetype=1, lwd=0.4) # halbfest
  }
  if(!(is.data.frame(df_kstf) && nrow(df_kstf)==0)){
    drilling2 <- drilling1 +
      geom_segment(data = df_kstf,aes(x=df_kstf$x+.11, xend=df_kstf$x+.11,
                                      y=df_kstf$start, yend=df_kstf$end), linetype=5, lwd=0.4) # steif
  }
  if(!(is.data.frame(df_kwh) && nrow(df_kwh)==0)){
    drilling2 <- drilling1 +
      geom_segment(data = df_kwh,aes(x=df_kwh$x+.11, xend=df_kwh$x+.11,
                                     y=df_kwh$start, yend=df_kwh$end), linetype=1, lwd=0.4, color="red")# weich
  }
  if(!(is.data.frame(df_kbr) && nrow(df_kbr)==0)){
    drilling2 <- drilling1 +
      geom_segment(data = df_kbr,aes(x=df_kbr$x+.11, xend=df_kbr$x+.11,
                                     y=df_kbr$start, yend=df_kbr$end), linetype=2, lwd=0.4, color ="red") # breiig
  }
  
  # #### IV) PLOT MIT SCHICHTBEZEICHNUNGEN UND TIEFENANGABE UND KONSISTENZEN UND LAGERUNGSDICHTEN####
  #     df_ksd <- df[df$`Zustand und Festigkeit` == "sehr dicht", ];df_ksd <- df_ksd[complete.cases(df_ksd[ , 6]),] # lösche NA-Reihen
  #     df_kd <- df[df$`Zustand und Festigkeit` == "dicht", ];  df_kd <- df_kd[complete.cases(df_kd[ , 6]),] # lösche NA-Reihen
  #     df_kmd <- df[df$`Zustand und Festigkeit` == "mitteldicht", ];  df_kmd <- df_kmd[complete.cases(df_kmd[ , 6]),] # lösche NA-Reihen
  #     df_kl <- df[df$`Zustand und Festigkeit` == "locker", ];  df_kl <- df_kl[complete.cases(df_kl[ , 6]),] # lösche NA-Reihen
  #     df_ksl <- df[df$`Zustand und Festigkeit` == "sehr locker", ];  df_ksl <- df_ksl[complete.cases(df_ksl[ , 6]),] # lösche NA-Reihen
  #
  #     # Füge die Lagerungsdichten hinzu (derzeit wird "bis" nicht unterstützt):
  #     if(!(is.data.frame(df_ksd) && nrow(df_ksd)==0)){
  #       drilling3 <- drilling2 +
  #     geom_segment(data = df_ksd,aes(x=df_ksd$x+.11, xend=df_ksd$x+.11,
  #                                  y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=3, lwd=0.4) + # sehr dicht 1. Teil
  #     geom_segment(data = df_ksd,aes(x=df_ksd$x+.13, xend=df_ksd$x+.13,
  #                                  y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=1, lwd=0.4) + # sehr dicht 2. Teil
  #     geom_segment(data = df_ksd,aes(x=df_ksd$x+.15, xend=df_ksd$x+.15,
  #                                  y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=3, lwd=0.4)   # sehr dicht 3. Teil
  #     }
  #     if(!(is.data.frame(df_kd) && nrow(df_kd)==0)){
  #       drilling3 <- drilling2 +
  #     geom_segment(data = df_kd,aes(x=df_kd$x+.11, xend=df_kd$x+.11,
  #                                  y=df_kd$Obergrenze, yend=df_kd$Untergrenze), linetype=1, lwd=0.4) + # dicht 1. Teil
  #     geom_segment(data = df_kd,aes(x=df_kd$x+.13, xend=df_kd$x+.13,
  #                                  y=df_kd$Obergrenze, yend=df_kd$Untergrenze), linetype=3, lwd=0.4)   # dicht 2. Teil
  #     }
  #     if(!(is.data.frame(df_kmd) && nrow(df_kmd)==0)){
  #       drilling3 <- drilling2 +
  #     geom_segment(data = df_kmd,aes(x=df_kmd$x+.11, xend=df_kmd$x+.11,
  #                                  y=df_kmd$Obergrenze, yend=df_kmd$Untergrenze), linetype=3, lwd=0.4) + # mitteldicht 1. Teil
  #     geom_segment(data = df_kmd,aes(x=df_kmd$x+.11, xend=df_kmd$x+.11,
  #                                  y=df_kmd$Obergrenze, yend=df_kmd$Untergrenze), linetype=3, lwd=0.4)   # mitteldicht 2. Teil
  #     }
  #     if(!(is.data.frame(df_kl) && nrow(df_kl)==0)){
  #       drilling3 <- drilling2 +
  #     geom_segment(data = df_kl,aes(x=df_kl$x+.11, xend=df_kl$x+.11,
  #                                  y=df_kl$Obergrenze, yend=df_kl$Untergrenze), linetype=3, lwd=0.4)  # locker
  #     }
  #     if(!(is.data.frame(df_ksl) && nrow(df_ksl)==0)){
  #       drilling3 <- drilling2 +
  #     geom_segment(data = df_ksl,aes(x=df_ksl$x+.11, xend=df_ksl$x+.11,
  #                                  y=df_ksl$Obergrenze, yend=df_ksl$Untergrenze), linetype=3, lwd=0.4) # sehr locker
  #     }
  
  # Abfrage der Plottype Ausgabe:
  if(plot_type == "blank"){
    return(drilling)
  }
  if(plot_type == "mit_schichten"){
    return(drilling1)
  }
  if (plot_type == "blank_mit_gw"){
    return(drilling1b)
  }
  if(plot_type == "mit_schichten_konsistenzen"){
    return(drilling2)
  }
  if(plot_type == "mit_schichten_konsistenzen_lagerung"){
    return(drilling3)
  }
  
}

# --- MAIN FUNCTION --- #

#Read Input Data (needs to have all drilling names in a column)
#Mandatory Fields: Petrographie = Ton, Schluff, etc.; Bohr.ID = Bohrungsid; Schichtmaechtigkeit = Schichtmächtigkeit in meter
excel_path <- "/Volumes/Fallstudie/Tunnel Bebenroth/Unterlagen Gutachten - Tunnel Bebenroth/Bohrungen/Zusammenfassung_Bohrungen.xlsx"
df <- xls_to_dataframe(path = excel_path,
                      sheet = "Bohrdaten",range='A4:P33',header=TRUE) #L34 sind genau 3 Bohrungen

#df <- read_excel("sampledata/Bohrlochdaten.xlsx", sheet = "Rohdaten-Teil2-1", range = "A1:O369") # mit Header=TRUE

if(("X" %in% colnames(df))){
  colnames(df)[which(names(df) == "X")] <- "x"
}

names(df)[4] <- "Schichtmaechtigkeit"
names(df)[5] <- "Petrographie"
names(df)[1] <- "Bohr.ID"
names(df)[3] <- "Untergrenze"
names(df)[2] <- "Obergrenze"
names(df)[6]<- "Gesteinsansprache.DIN.4022"
names(df)[9]<- "Ansatzhoehe"
names(df)[16]<- "Offset"
names(df)[8]<- "X" #Tunnelmeter
names(df)[7]<- "GW"

# column 16 = Offset
color_dictionary <- load_colors(load_list = FALSE,list_path = excel_path)
setwd("/Users/Valentin/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/Ingenieur-und-Hydro/3-1 Ingenieurgeologische Fallstudie/3-HAUPTGUTACHTEN/Anlage 1 - Tunnelprofil")
# create_profile(df=df, color_dictionary = color_dictionary, "blank", aspect_ratio = 0.5, only_first=TRUE)
#create_profile(df=df,color_dictionary = color_dictionary,"mit_schichten", aspect_ratio = 1, only_first = TRUE)
create_profile(df=df, "mit_schichten", color_dictionary = color_dictionary, aspect_ratio = 0.5, only_first = TRUE)

#ggsave(file="test.pdf", plot=drilling, width=10, height=10)
#saveDINA(filename = "Bohrungen_05-achse.pdf", plot=create_profile(df=df, "blank_mit_gw", color_dictionary = color_dictionary, aspect_ratio = 0.5, only_first = FALSE), size = "A1")


