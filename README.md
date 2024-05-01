Package `shinyFactory` is an helper to construct fast shiny frames, easy to complete.
Install shinyFactory with remotes::install_github('Julien-Bousquet/shinyFactory')
So first install `remotes` package :

  install.packages('remotes')

And then install `shinyFactory` : 

  remotes::install_github('Julien-Bousquet/shinyFactory') ; 
  
  library(shinyFactory)

`shinyFactory` possess only one function : `makeApp()`. For example :

makeApp(input=c('numeric', 'slider'))

construct the frame of a shinyApp with a numeric input and a slider. 

Please read the help written for you :

?makeApp( )

  
