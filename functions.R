hyp1 <- function(clark_good_sum, clark_bad_sum, total=0){
  
  coefs <- rep(0, 9999) 
  for (i in 1:9999){
    
    means     <- sample(0:100, 1, replace=TRUE)  # For H0 --> Same mean
    if (total!=3){
      total_good  <- clark_good_sum$total
      total_bad   <- clark_bad_sum$total
    }
    else{
      total_good  <- total
      total_bad   <- total
    }
    good_list <- as.integer(rnorm(total_good, 
                                  mean=means, 
                                  sd=clark_good_sum$sd))
    bad_list  <- as.integer(rnorm(total_bad, 
                                  mean=means, 
                                  sd=clark_bad_sum$sd)) 
    dataframe <- tibble::tibble(language = c(rep("adger-good", 
                                                 total_good), 
                                             rep("adger-bad",
                                                 total_bad)),
                                rating = c(good_list, bad_list))
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  return(data.frame(coef=coefs))
}

hyp2 <- function(clark_good_sum, clark_bad_sum, total=0){
  coefs <- rep(0, 9999) 
  for (i in 1:9999){
    mean_good <- sample(0:100, 1, replace=TRUE)  
    mean_bad  <- mean_good + (clark_good_sum$mean - clark_bad_sum$mean)
    if (total!=3){
      total_good  <- clark_good_sum$total
      total_bad   <- clark_bad_sum$total
    }
    else{
      total_good  <- total
      total_bad   <- total
    }
    good_list <- as.integer(rnorm(total_good, 
                                  mean=mean_good, 
                                  sd=clark_good_sum$sd))
    bad_list  <- as.integer(rnorm(total_bad, 
                                  mean=mean_bad, 
                                  sd=clark_bad_sum$sd))
    dataframe <- tibble::tibble(language = c(rep("adger-good", 
                                                 total_good), 
                                             rep("adger-bad",
                                                 total_bad)),
                                rating = c(good_list, bad_list))
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  return(data.frame(coef = coefs))
}

hyp1_2 <- function(clark_good_sum, clark_bad_sum, total=0){
  
  coefs <- rep(0, 9999) 
  print("IN")
  for (i in 1:9999){
    means         <- sample(0:100, 1, replace=TRUE)  # For H0 --> Same mean
    if (total!=3){
      total_good  <- clark_good_sum$total
      total_bad   <- clark_bad_sum$total
    }
    else{
      total_good  <- total
      total_bad   <- total
    }
    # GOOD
    good_list          <- as.integer(rnorm(total_good, mean=means, sd=clark_good_sum$sd))
    # REPLACEMENTS --> values between 0 and 100
    good_list_replaced <- replace(good_list, good_list < 0, 0)
    good_list_replaced <- replace(good_list_replaced, good_list_replaced > 100, 100)
    # BAD
    bad_list           <- as.integer(rnorm(total_bad, mean=means, sd=clark_bad_sum$sd))
    # REPLACEMENTS --> values between 0 and 100
    bad_list_replaced  <- replace(bad_list, bad_list < 0, 0)
    bad_list_replaced  <- replace(bad_list_replaced, bad_list_replaced > 100, 100)
    dataframe <- tibble::tibble(language = c(rep("adger-good", total_good), 
                                             rep("adger-bad", total_bad)),
                                rating = c(good_list_replaced, bad_list_replaced))
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  return(data.frame(coef = coefs))
}

hyp2_2 <- function(clark_good_sum, clark_bad_sum, total=0){
  
  coefs <- rep(0, 9999) 
  for (i in 1:9999){
    mean_good <- sample(0:100, 1, replace=TRUE)  
    mean_bad  <- mean_good + (clark_good_sum$mean - clark_bad_sum$mean)
    if (total!=3){
      total_good  <- clark_good_sum$total
      total_bad   <- clark_bad_sum$total
    }
    else{
      total_good  <- total
      total_bad   <- total
    }
    # GOOD
    good_list          <- as.integer(rnorm(total_good, 
                                           mean=mean_good, 
                                           sd=clark_good_sum$sd))
    # REPLACEMENTS --> values between 0 and 100
    good_list_replaced <- replace(good_list, good_list < 0, 0)
    good_list_replaced <- replace(good_list_replaced, good_list_replaced > 100, 100)
    # BAD
    bad_list           <- as.integer(rnorm(total_bad, 
                                           mean=mean_bad, 
                                           sd=clark_bad_sum$sd))
    # REPLACEMENTS --> values between 0 and 100
    bad_list_replaced  <- replace(bad_list, bad_list < 0, 0)
    bad_list_replaced  <- replace(bad_list_replaced, bad_list_replaced > 100, 100)
    dataframe <- tibble::tibble(language = c(rep("adger-good", 
                                                 total_good), 
                                             rep("adger-bad",
                                                 total_bad)),
                                rating = c(good_list_replaced, bad_list_replaced))
    
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  return(data.frame(coef = coefs))
  
}