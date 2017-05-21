# Written by V.M. on 07.05.2017
# Some standard ternary plots using ggtern + convencience functions to normalize the data, cite package like this:
# citation('ggtern')
# gist -u 2547744bd340fe40a7d7aa683d47d0f8 createTernaryPlots.R 

#install.packages("ggtern")
library(ggtern)

ternaryBW <- function(df=NULL,left_lab='x',right_lab='y',top_lab='z',addDensity=FALSE){
  if(is.null(df)){
    print('Using standard Feldspar data as no external source was defined')
    data('Feldspar')
    df <- Feldspar
    left<-Feldspar$Ab
    left_lab='Ab'
    top<-Feldspar$An
    top_lab='An'
    right<-Feldspar$Or
    right_lab='Or'
  } else {
    left<-df$left
    top<-df$top
    right<-df$right
  }

  ggtern(df,aes(left,top,right)) + 
    geom_point()  +
    theme_bw() + 
    labs(x=left_lab,z=right_lab,y=top_lab) + 
    if(addDensity){
      geom_density_tern(aes(color=..level..),bins=4)
    } else {
      NULL
    }
}

showThemeOverview <- function(){
  # Shows two facetted plot pages of all possible standard themes
  # Create a list of the theme suffixes
  themesOrg = c('gray','bw','linedraw','light','dark','minimal','classic','void')
  themesNew = c('custom','darker','rgbw','rgbg','tropical','matrix','bluelight','bluedark')
  
  #Iterate over all the suffixes, creating a list of plots
  plotThemes = function(themes){
    grobs = lapply(themes,function(x){
      thmName = sprintf("theme_%s",x)
      thm = do.call(thmName,args=list(base_size=9))
      df  = data.frame(label=thmName)
      ggtern(df) + facet_wrap(~label) + thm
    })
    grobs
  }
  #Arrange the Original Themes
  grid.arrange(grobs=plotThemes(themesOrg),top = "Collection of Themes (Original)")
  #New Themes
  grid.arrange(grobs=plotThemes(themesNew),top = "Collection of Themes (New Themes)")
}

normalizeData <- function(df,norm100=TRUE){
  # input a data.frame object with named columns, e.g. df <- data.frame(a=c(1,2),b=c(2,3))
  # if norm100 is TRUE, a base 100 [%] normalized dataframe will be retured.
  # Otherwise the data will be normalized to 1.0
  if(norm100){
    normFactor = 100
  } else {
    normFactor = 1.0
  }
  fracs <- data.frame()
  for(colNr in seq(df)){
    # normalizes each column and add data inplace, so all colnames stay the same
    df[colNr] = df[colNr]/sum(df[colNr]) * normFactor
  }
  return(df)
}

# 0. Normalize Data
# normalizeData(df,norm100=TRUE)

# 1. Create basic bw plot:
ternaryBW(data.frame(left=c(0.1,0.2),top=c(0.1,0.4),right=c(0.3,0.2)),left_lab='x',right_lab='y',top_lab='z')

# 2. Create bw plot /w density lines:
ternaryBW(data.frame(left=c(0.1,0.2),top=c(0.1,0.4),right=c(0.3,0.2)),left_lab='x',right_lab='y',top_lab='z',addDensity = TRUE)


