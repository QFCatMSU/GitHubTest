  rm(list=ls());                         # clear Environment Window
  options(show.error.locations = TRUE);  # show line numbers on error
  
  # In this script functions from the ggplot2 and tidyr packages will be used.  
  # If these packages aren't installed, run the following 2 lines of code
  # install.packages("ggplot2")
  # install.packages("tidyr")
  library(ggplot2);                      # include ggplot package
  library(tidyr);                        # include tidyr package
  
  # Read in the largemouth bass and bluegill diet data
  LMB = read.csv(file="LMB_diet.csv", header=TRUE);
  BLG = read.csv(file="BLG_diet.csv", header=TRUE);
  
  
  # Gather converts the data set into a long format to make the data easier to graph
  LMB_long = gather(LMB, Diet, Proportion, DI1:DI28, factor_key=TRUE);
  BLG_long = gather(BLG, Diet, Proportion, DI1:DI28, factor_key=TRUE);
  
  # Note: in the bubble plots below, 
  #     size = ifelse(Proportion==0, NA, Proportion) 
  #     is an inelegant way to say that values of 0 should not be plotted
 
  # Bubble plot showing diet composition of (volume proportions) largemouth bass
  p1=ggplot(data=LMB_long, 
            mapping=aes(x=ID, y=Diet,
                        size=ifelse(Proportion==0, NA, Proportion)))+
    labs(title= "Largemouth Bass Diet Composition")+
    geom_point() +
    theme_classic() +               # theme without grid lines or background
    theme(legend.position="none");  # get rid of legend
  plot(p1);
  
  # Bubble plot showing diet composition of (volume proportions) of bluegill
  p2=ggplot(data=BLG_long, 
            mapping=aes(x=ID, y=Diet, 
                        size=ifelse(Proportion==0, NA, Proportion)))+
    labs(title= "Bluegill Diet Composition")+
    geom_point() +
    theme_classic() +               # theme without grid lines or background
    theme(legend.position="none");  # get rid of legend
  plot(p2);
  
  # Calculate the mean proportions for each diet item across all fish in the samples
  LMB_mean= apply(LMB[,3:30], MARGIN=2, mean)
  BLG_mean= apply(BLG[,3:30], MARGIN=2, mean)
  
  # Calculates the Schoener diet overlap value between largemouth bass and
  #   bluegill for the original data
  Overlap= 1-0.5*sum(abs(LMB_mean-BLG_mean))

  #### Create a function for calculating the Schoner overlap index ###
  SchoenerOverlap = function(group1=NULL, group2=NULL) 
  {
    # Check to be sure data for both groups has been specified
    if(is.null(group1) | is.null(group2)) 
    {
      stop("Data for both groups must be specified");
    }
    group1Mean = apply(group1, MARGIN=2, FUN=mean);
    group2Mean = apply(group2, MARGIN=2, FUN=mean);
    overlap = 1-0.5*sum(abs(group1Mean - group2Mean));
    return(overlap);
  }
  
  example1 = SchoenerOverlap(group1=LMB[,3:30], group2=BLG[,3:30])
    
  #### Create the jackknife function for the Schoener overlap index based
  #      on description in Smith (1985)
  dietoverlapJK = function(group1=NULL, group2=NULL, alpha=0.05)
  {
    # Calculate diet overlap from the original sample
    overlap = SchoenerOverlap(group1, group2);
    
    # Count number of observations in each group
    numObsG1 = nrow(group1);
    numObsG2 = nrow(group2);
    
    # Create vectors to hold the pseudo-values for the groups
    group1PVS = numeric(numObsG1);
    group2PVS = numeric(numObsG2);
    
    ## Calculate the pseuodo-values by deleting each stomach sample from
    #  each group separately and recalculating overlap
    for(j in 1:numObsG1)
    {
      group1PVS[j] = (numObsG1 - 0.5) * overlap - (numObsG1 - 1) *
        SchoenerOverlap(group1[-j,], group2);
    }
    for(j in 1:numObsG2)
    {
      group2PVS[j] = (numObsG2 - 0.5) * overlap - 
                     (numObsG2 - 1) * SchoenerOverlap(group1, group2[-j,]);
    }
    
    # Calculate the jackknife diet overlap value from the pseudo-values
    overlapJK = mean(group1PVS) + mean(group2PVS);
    
    # Calculate the variance and standard error for the jackknife
    # diet overlap value
    group1Var = 1 / (numObsG1 -1) * sum((group1PVS - mean(group1PVS))^2);
    group2Var = 1 / (numObsG2 -1) * sum((group2PVS - mean(group2PVS))^2);
    overlapVar = (group1Var / numObsG1) + (group2Var / numObsG2);
    overlapSE = sqrt(overlapVar);
    
    # Calculate a 100%*(1-alpha) confidence interval for diet overlap value 
    #   between largemouth bass and bluegill
    overlapCI = overlapJK + 
                c(-1,1) * 
                qt(1-alpha/2,df=numObsG1+numObsG2-2) * 
                overlapSE;
    
    # Create a list to hold the results
    results = list(overlapOrig = overlap,
                   overlapJK = overlapJK,
                   overlapJK_SE = overlapSE,
                   overlapCI = overlapCI);
    return(results);
  }
  
  example2 = dietoverlapJK(group1=LMB[,3:30], group2=BLG[,3:30], alpha=0.05);
