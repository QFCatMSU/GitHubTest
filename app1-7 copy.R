{
  rm(list=ls());  options(show.error.locations = TRUE);

  score = readline("Enter your score: ");
  score = as.numeric(score);

  if(score > 90) 
{
  cat("A");
}
  else if(score > 80) # score between 80-90
{
  cat("B");
}
  else if(score > 70) # score between 70-80
{ 
  cat("C");
}
  else if(score > 60) # score between 60-70
{ 
  cat("D");
}
  else  # all above conditions were FALSE
{                 
  cat("E");
}
}
