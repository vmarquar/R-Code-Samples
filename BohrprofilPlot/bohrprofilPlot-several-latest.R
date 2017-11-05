# # # TODO # # #
# - Excel Anbindung mit LFU Konformen Daten
# - Grundwasserstand
# - Platzhalter für die vertikale verschiebung der einzelnen Bohrungen
#     - zunächst Höhenmittelwert berechnen
#     - dann für jede Bohr-ID die Abweichung dazu berechnen
#     - den Platzhalter (die Abweichung) zur mittleren Höhe als extra Reihe hinzufügen (mit ID 0)
# - Konsistenzbänder: "halbfest bis fest" unterstüzung implementieren
# - Konsistenzbänder: "weich" und "breeig" implementieren
# load_colors: weitere Farben über CSV Datei hinzuladen

library(ggplot2)
library(geohydvm)
library(readxl)
library(svglite)
require(grid)

# Create Colors and Dictionary mapping for several abbreviations and languages:
load_colors<-function(load_list=FALSE){
  humus_color = '#cea470'
  gravel_color = '#f3e03a'
  sand_color = '#e09637'
  silt_color = '#aba77d'
  clay_color = '#974b89'
  concrete_color = '#6f6f6f'
  color_dictionary = c(gravel = gravel_color,sand = sand_color, silt = silt_color, clay = clay_color,
                       Kies = gravel_color, Sand = sand_color, Schluff = silt_color, Ton = clay_color,
                       G = gravel_color, S = sand_color, U = silt_color, "T" = clay_color, Humus = humus_color,
                       Feinkies = gravel_color, "Feinsand"=sand_color, "Auffüllung"="white",
                       "Beton"="grey", Grobschluff =silt_color, "Künstlicher Feststoff"="black", "Kalk (locker)"='#e9edd5',
                       "Künstliches Lockermaterial"="white", "Material nicht bekannt"="white", "Mittelkies" = gravel_color,
                       "Mittelsand"=sand_color, "Sedimentäres Lockergestein o.ä."="grey", "sonstiger Bohrprobenverlust"="grey",
                       "Ton bis Schluff"=clay_color)
  if(load_list){
    # load additional key-colorvalue pairs from csv file
    # append to color_dictionary
  }
  return(color_dictionary)
}

color_dictionary <- load_colors()


# Read Input Data (needs to have all drilling names in a column)
# Mandatory Fields: Petrographie = Ton, Schluff, etc.; Bohr.ID = Bohrungsid; Schichtmaechtigkeit = Schichtmächtigkeit in meter
df <- xls_to_dataframe(path = '/Users/Valentin/Desktop/Bohrlochdaten/Bohrlochdaten.xlsx',
                       sheet = "Rohdaten-Teil1",range='A1:M114',header=TRUE) #L34 sind genau 3 Bohrungen

names(df)[12] <- "Schichtmaechtigkeit"
names(df)[3] <- "Petrographie"
names(df)[11] <- "Bohr.ID"
names(df)[2] <- "Untergrenze"
names(df)[1] <- "Obergrenze"
names(df)[4]<- "Gesteinsansprache.DIN.4022"

create_profile <- function(plot_type="blank mit_schichten mit_schichten_konsistenzen mit_schichten_konsistenzen_lagerung", aspect_ratio=1.0){
  # ANLEITUNG ZUR LEICHTEN MANUELLEN VERBESSERUNG:
  # 1. Breite der Bohrungen mit dem width-parameter geom_col verändern
  # 2. Bohrungs-IDS mit dem Element axix.text.x um 90° drehen (oder eben horizontal lassen)
  # 3. Die scharze Umrandung kann in geom_col(color="black") gesetzt werden
  # 4. hjust und vjust können textankerpunkt angeben. (normal: 0.5 also zentriert)
  # 5. die Länge der Striche und Position der Texte kann mit df$x-0.15 variiert werden
  # 6. Das Seiten-zu-Höhenverhältnis kann unter theme(aspect.ratio=1) verändert werden.
  # POSSIBLE PLOT_TYPES = blank, mit_schichten, mit_schichten_konsistenzen, mit_schichten_konsistenzen_lagerung

  # Create an x value from unique BohrungsID to seperate plots (when there is no specific x supplied)
  if(!"x" %in% colnames(df)){
    df <- transform(df,x=as.numeric(factor(df$Bohr.ID)));
    }
  # Berechne Anzahl der Schichten pro Bohrung
  df$ix <- as.numeric(ave(df$Bohr.ID, df$Bohr.ID, FUN = function(x) seq_along(x))) # See Stackoverflow for Details...

#### I) BLANKER PLOT ####
  # Plot borehole profiles without labels
  drilling <- ggplot(data = df, aes(x = x, y = Schichtmaechtigkeit, group = ix, fill = Petrographie)) +
    theme_minimal() + # theme_minimal()
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          #axis.text.x=element_text(angle=-90), # dreht BohrungsID um 90°
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(),
          aspect.ratio=aspect_ratio) +
    geom_col(position = position_stack(reverse = TRUE), width= .15,color="black") +
    scale_y_reverse(expand = c(0, 0), name ="Tiefe [m]") +
    scale_x_continuous(position = "top", breaks = df$x, labels=paste(df$Bohr.ID,"\n",df$x,"[GK4]"), name="") +
    scale_fill_manual(values = color_dictionary)

#### II) PLOT MIT SCHICHTBEZEICHNUNGEN UND TIEFENANGABE ####
    drilling1 <- drilling +

      # Bohrgutbeschreibung rechts des Bohrprofils
      annotate("text",x = df$x+0.16, y=df$Untergrenze-df$Schichtmaechtigkeit/2, label = df$Gesteinsansprache.DIN.4022, group=df$ix,hjust=0, vjust=0.5, size=3) +
      annotate("segment", x = df$x, y=df$Untergrenze,xend = df$x+0.5, yend = df$Untergrenze, color="black") +

      # Höhenangaben links des Bohrprofils
      annotate("text",x = df$x-0.15, y=df$Untergrenze, label = paste(df$Untergrenze, "m"), group=df$ix,hjust=1, vjust=-0.25, size=3) +
      annotate("segment", x = df$x, y=df$Untergrenze,xend = df$x-0.25, yend = df$Untergrenze, color="black")

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
                                     y=df_kfe$Obergrenze, yend=df_kfe$Untergrenze), linetype=1, lwd=0.4) + # fest Teil 1 (erster Strich)
          geom_segment(data = df_kfe,aes(x=df_kfe$x+.13, xend=df_kfe$x+.13,
                                       y=df_kfe$Obergrenze, yend=df_kfe$Untergrenze), linetype=1, lwd=0.4) # fest Teil 2 (zweiter Strich)
      }
      if(!(is.data.frame(df_khf) && nrow(df_khf)==0)){
        drilling2 <- drilling1 +
          geom_segment(data = df_khf,aes(x=df_khf$x+.11, xend=df_khf$x+.11,
                                   y=df_khf$Obergrenze, yend=df_khf$Untergrenze), linetype=1, lwd=0.4) # halbfest
      }
      if(!(is.data.frame(df_kstf) && nrow(df_kstf)==0)){
        drilling2 <- drilling1 +
          geom_segment(data = df_kstf,aes(x=df_kstf$x+.11, xend=df_kstf$x+.11,
                                   y=df_kstf$Obergrenze, yend=df_kstf$Untergrenze), linetype=5, lwd=0.4) # steif
      }
      if(!(is.data.frame(df_kwh) && nrow(df_kwh)==0)){
        drilling2 <- drilling1 +
          geom_segment(data = df_kwh,aes(x=df_kwh$x+.11, xend=df_kwh$x+.11,
                                   y=df_kwh$Obergrenze, yend=df_kwh$Untergrenze), linetype=1, lwd=0.4, color="red")# weich
      }
      if(!(is.data.frame(df_kbr) && nrow(df_kbr)==0)){
        drilling2 <- drilling1 +
          geom_segment(data = df_kbr,aes(x=df_kbr$x+.11, xend=df_kbr$x+.11,
                                   y=df_kbr$Obergrenze, yend=df_kbr$Untergrenze), linetype=2, lwd=0.4, color ="red") # breiig
      }

#### IV) PLOT MIT SCHICHTBEZEICHNUNGEN UND TIEFENANGABE UND KONSISTENZEN UND LAGERUNGSDICHTEN####
    df_ksd <- df[df$`Zustand und Festigkeit` == "sehr dicht", ];df_ksd <- df_ksd[complete.cases(df_ksd[ , 6]),] # lösche NA-Reihen
    df_kd <- df[df$`Zustand und Festigkeit` == "dicht", ];  df_kd <- df_kd[complete.cases(df_kd[ , 6]),] # lösche NA-Reihen
    df_kmd <- df[df$`Zustand und Festigkeit` == "mitteldicht", ];  df_kmd <- df_kmd[complete.cases(df_kmd[ , 6]),] # lösche NA-Reihen
    df_kl <- df[df$`Zustand und Festigkeit` == "locker", ];  df_kl <- df_kl[complete.cases(df_kl[ , 6]),] # lösche NA-Reihen
    df_ksl <- df[df$`Zustand und Festigkeit` == "sehr locker", ];  df_ksl <- df_ksl[complete.cases(df_ksl[ , 6]),] # lösche NA-Reihen

    # Füge die Lagerungsdichten hinzu (derzeit wird "bis" nicht unterstützt):
    if(!(is.data.frame(df_ksd) && nrow(df_ksd)==0)){
      drilling3 <- drilling2 +
    geom_segment(data = df_ksd,aes(x=df_ksd$x+.11, xend=df_ksd$x+.11,
                                 y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=3, lwd=0.4) + # sehr dicht 1. Teil
    geom_segment(data = df_ksd,aes(x=df_ksd$x+.13, xend=df_ksd$x+.13,
                                 y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=1, lwd=0.4) + # sehr dicht 2. Teil
    geom_segment(data = df_ksd,aes(x=df_ksd$x+.15, xend=df_ksd$x+.15,
                                 y=df_ksd$Obergrenze, yend=df_ksd$Untergrenze), linetype=3, lwd=0.4)   # sehr dicht 3. Teil
    }
    if(!(is.data.frame(df_kd) && nrow(df_kd)==0)){
      drilling3 <- drilling2 +
    geom_segment(data = df_kd,aes(x=df_kd$x+.11, xend=df_kd$x+.11,
                                 y=df_kd$Obergrenze, yend=df_kd$Untergrenze), linetype=1, lwd=0.4) + # dicht 1. Teil
    geom_segment(data = df_kd,aes(x=df_kd$x+.13, xend=df_kd$x+.13,
                                 y=df_kd$Obergrenze, yend=df_kd$Untergrenze), linetype=3, lwd=0.4)   # dicht 2. Teil
    }
    if(!(is.data.frame(df_kmd) && nrow(df_kmd)==0)){
      drilling3 <- drilling2 +
    geom_segment(data = df_kmd,aes(x=df_kmd$x+.11, xend=df_kmd$x+.11,
                                 y=df_kmd$Obergrenze, yend=df_kmd$Untergrenze), linetype=3, lwd=0.4) + # mitteldicht 1. Teil
    geom_segment(data = df_kmd,aes(x=df_kmd$x+.11, xend=df_kmd$x+.11,
                                 y=df_kmd$Obergrenze, yend=df_kmd$Untergrenze), linetype=3, lwd=0.4)   # mitteldicht 2. Teil
    }
    if(!(is.data.frame(df_kl) && nrow(df_kl)==0)){
      drilling3 <- drilling2 +
    geom_segment(data = df_kl,aes(x=df_kl$x+.11, xend=df_kl$x+.11,
                                 y=df_kl$Obergrenze, yend=df_kl$Untergrenze), linetype=3, lwd=0.4)  # locker
    }
    if(!(is.data.frame(df_ksl) && nrow(df_ksl)==0)){
      drilling3 <- drilling2 +
    geom_segment(data = df_ksl,aes(x=df_ksl$x+.11, xend=df_ksl$x+.11,
                                 y=df_ksl$Obergrenze, yend=df_ksl$Untergrenze), linetype=3, lwd=0.4) # sehr locker
    }

  # Abfrage der Plottype Ausgabe:
  if(plot_type == "blank"){
    return(drilling)
  }
  if(plot_type == "mit_schichten"){
    return(drilling1)
  }
  if(plot_type == "mit_schichten_konsistenzen"){
    return(drilling2)
  }
  if(plot_type == "mit_schichten_konsistenzen_lagerung"){
    return(drilling3)
  }

}




#ggsave(file="test.pdf", plot=drilling, width=10, height=10)
