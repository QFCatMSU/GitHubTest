{
  rm(list=ls());                         # clear Console Window
  options(show.error.locations = TRUE);  # show line numbers on error
  library(package=ggplot2);              # include all GGPlot2 functions

  source("scripts/ggplotTemplate.r");
  ## How about if you moved ggplotTemplate.r to the main project folder
  weatherData = read.csv(file="data/weatherData.csv");
  ## How about if you moved weatherData.csv to the templates folder inside the scripts folder
  
  #### Labels for the facets of the plot
  windLabels = c(Low = "Light Winds",
                 Medium = "Medium Winds",
                 High = "Heavy Winds");

  
  #### Boxplots of Change in Temperature vs. Wind Direction 
  #           at different wind speeds
  thePlot = ggplot(data=weatherData) +
    geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), 
                 na.rm=TRUE,
<<<<<<< HEAD
                 color=c("blue", rep("grey10", 3),
=======
                 color=c("purple", rep("black", 3),
>>>>>>> 9e7547a2cbe8a2b44407db362cbd03f71c4dfdf6
                         "green", rep("black", 3),
                         "orange", rep("black", 3)),
                 fill=c(rep(NA, 8), rep("red", 3), NA)) +
    facet_grid(facets=.~factor(windSpeedLevel,
                               levels=c("Low", "Medium", "High")),
               labeller=as_labeller(windLabels)) +
    scale_x_discrete(limits=c("North", "East", "South", "West")) +
    labs(title = "Change in Temperature vs. Wind Direction",
         subtitle = "Lansing, Michigan: 2016",
         x = "Wind Direction",
         y = "Degrees (Fahrenheit)");
  
  thePlot = thePlot + GGP_template;
  plot(thePlot);
}  