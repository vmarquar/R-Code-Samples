# Import Corporate Design of TUM
#Blautöne:
TUMBlau <- rgb(0/255,101/255,189/255) # Pantone 300
TUMBlauDunkel <- rgb(0/255,82/255,147/255) # Pantone 301
TUMBlauHell <- rgb(152/255,198/255,234/255) # Pantone 283
TUMBlauMittel <- rgb(100/255,160/255,200/255) # Pantone 542

#Hervorhebung:
TUMElfenbein <- rgb(218/255,215/255,203/255) # Pantone 7527 -Elfenbein #DAD7CB
TUMGruen <- rgb(162/255,173/255,0/255) # Pantone 383 - Grün
TUMOrange <- rgb(227/255,114/255,34/255) # Pantone 158 - Orange
TUMGrau <-rgb(153/255,153/255,153/255) # Grau 60%
TUMSchwarz <- '#000000'

# Greyscale (Black & White) Colors:
bw_colors <- c(TUMSchwarz, TUMGrau, TUMElfenbein, 'white')

# Zuerst Primärfarben:
primary_colors <- c(TUMBlau, TUMSchwarz, 'white')

# Komplexe Diagramme (Blaue Color Palette)
# Sekundärfarben:
secondary_colors <- c(TUMBlauDunkel,TUMBlauMittel,TUMBlauHell)

# Bei Weiterer Komplexität oder zusätzlichen Markierungen:
# Tertiärfarben:
tertiary_colors <- c(TUMOrange,TUMGruen,TUMGrau)

# bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# # Scatter plot
# sp + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))