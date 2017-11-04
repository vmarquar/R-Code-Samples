library(ggplot2)
gravel_color = '#f3e03a' #rgb(243	224	58)
sand_color = '#e09637'
silt_color = '#aba77d'
clay_color = '#974b89'
color_dictionary = c("gravel" = gravel_color,"sand" = sand_color, "silt" = silt_color, "clay" = clay_color, "Kies" = gravel_color, "Sand" = sand_color, "Schluff" = silt_color, "Ton" = clay_color, "G" = gravel_color, "S" = sand_color, "U" = silt_color, "T" = clay_color)
color_dictionary = data.table(gravel = gravel_color,sand = sand_color, silt = silt_color, clay = clay_color, Kies = gravel_color, Sand = sand_color, Schluff = silt_color, Ton = clay_color, G = gravel_color, S = sand_color, U = silt_color, "T" = clay_color)

df <- data.frame(X = 0, layer.thickness = c(0.2,1,3,0.4,0.8,2),layer.depth = c(0.2,1.2,3.2,0.4,1.2,3.2), ID = c(1,2,3,4,5,6),Category=c("Ton","U","Sand","Kies","Ton","T"),BSCategory=c("Drilling1","Drilling1","Drilling1","Drilling2","Drilling2","Drilling2"))
df$ix <- ave(df$BSCategory, df$BSCategory, FUN = function(x) rev(seq_along(x)))


# Calculate aesthetics
breaks_fun <- function(layer.thickness) seq(0, ceiling(max(sum(df$layer.thickness))))

ggplot(data = df, aes(x = BSCategory, y = layer.thickness, group = ix, fill = Category)) +
  theme_minimal() + # theme_minimal()
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_col(width= .25) +
  scale_y_reverse(name ="Tiefe [m]") +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = color_dictionary)
  #scale_y_continuous(name = "Tiefe [m]", trans = 'reverse', breaks = breaks_fun)

  


