# Function Package for linear regression

library(tidyverse)

visualize_lm_in_one_fig <- function(lm_model ){
  
  pred_string <-names(lm_model$model)[-1]
  return_list <- list(length(pred_string))
  

  for(i in 1:length(pred_string)){
    q <- effect_plot(lm_model, pred = !!(pred_string[i]), interval = TRUE, plot.points = TRUE)
    return_list[i] <- list(q)
  }
  
  nCol <- floor(sqrt(length(pred_string)))
  do.call("grid.arrange", c(return_list, ncol=nCol))
  rm(q)
  rm(list = c("i","pred_string","return_list"))
}

visualize_lm_4_presentation <- function(lm_model ){
  
  pred_string <-names(lm_model$model)[-1]
  return_list <- list(length(pred_string))
  
  
  for(i in 1:length(pred_string)){
    q <- effect_plot(lm_model, pred = !!(pred_string[i]), interval = TRUE, plot.points = TRUE)
    return_list[i] <- list(q)
  }
  
  rm(q)
  
  for(i in 1:floor(length(pred_string)/4)){
    if(i*4 <= length(pred_string)){
      print_list <- return_list[ (i*4-3) : (i*4)  ]
    } else {
      print_list <- return_list[ (i*4-3) : length(pred_string) ]
    }
    do.call("grid.arrange", c(print_list, ncol=2, nrow = 2))
  }
  rm(list = c("i","pred_string","print_list","return_list"))
}  