# Mahad function (from careless package)
mahad <- function(x, plot = TRUE, flag = FALSE, confidence = 0.99, na.rm = TRUE) {
  if(na.rm == FALSE) {
    if(any(is.na(x)) == TRUE) {stop("Some values are NA. Mahalanobis distance was not computed.
                                      Use na.rm = TRUE to use available cases.", call. = FALSE)}
  }
  
  #Subfunction "mahad" (from psych package)
  outlier <- 
    function(x,plot=TRUE,bad=5,na.rm=TRUE,xlab,ylab,...) {
      if(missing(xlab)) xlab <- expression("Quantiles of " * ~chi ^2)
      if(missing(ylab)) ylab <- expression("Mahalanobis " * D^2)
      rn <- rownames(x)
      nvar <- ncol(x)
      n.obs <- nrow(x)
      if(!is.matrix(x)) x <- as.matrix(x)
      nvar <- ncol(x)
      Sx <- cov(x,use="pairwise")
      Sx.inv <- solve(Sx)
      # Mx <- colMeans(x,na.rm=na.rm)
      # x <- sweep(x,2,Mx)
      #x <- t(scale(t(x),scale=FALSE))
      x <- scale(x,scale=FALSE)
      D2 <- t(apply(x,1,function(xx) colSums(xx * Sx.inv,na.rm=TRUE)))
      D2 <- rowSums(D2*x,na.rm=TRUE)
      names(D2) <- rn
      
      if(plot) {
        Chi2 <- qchisq(ppoints(n.obs), df =  nvar)
        qqplot(Chi2, D2,
               main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                                   " vs. quantiles of" * ~ chi[nvar]^2),xlab=xlab,ylab=ylab,...)
        abline(0, 1, col = 'gray')
        worst <- order(D2,decreasing=TRUE)
        text(Chi2[n.obs:(n.obs-bad+1)],D2[worst[1:bad]],names(D2)[worst[1:bad]],pos=3,...)
      }
      return(D2)
    }
  
  #remove rows with all NA and issue warning
  complete.na <- apply(x, 1, function(y) { all(is.na(y)) } )
  if(any(complete.na)) {
    warning("Some cases contain only NA values. The Mahalanobis distance will be calculated using available cases.",
            call. = FALSE) }
  x_filtered <- x[!complete.na,]
  
  maha_data <- as.numeric(outlier(x_filtered, plot, bad = 0, na.rm = na.rm))
  d_sq <- rep_len(NA, nrow(x_filtered))
  d_sq[!complete.na] <- maha_data
  
  if(flag == TRUE) {
    cut <- stats::qchisq(confidence, ncol(x))
    flagged <- (d_sq > cut)
    return(data.frame(d_sq = d_sq, flagged = flagged))
  }
  else{ return(d_sq) }
}

# IRV function (from careless package)
irv <- function(x, na.rm = TRUE, split = FALSE, num.split = 3) {
  out <- apply(x, 1, stats::sd, na.rm = na.rm)
  
  if(split == TRUE) {
    chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
    split_x <- apply(x, 1, chunk, num.split)
    out_split <- t(replicate(nrow(x), rep(NA, num.split)))
    colnames(out_split) <- paste0("irv",1:num.split)
    for(k in 1:nrow(out_split)) {
      split_x_single <- split_x[[k]]
      out_split[k,] <- unlist(lapply(split_x_single, stats::sd, na.rm = na.rm), use.names = FALSE)
    }
    out_split <- data.frame(out, out_split)
    colnames(out_split)[1] <- "irvTotal"
    return(out_split)} else { #split subsection end
      return(out)
    }
}

# Longstring function (from careless package)
longstring <- function(x, avg=FALSE) {
  
  # subfunction that calculates the length of consecutive identical responses
  rle_string <- function(x) {
    rle_list <- rle(x)
    longstr <- max(rle_list$lengths)
    avgstr <- mean(rle_list$lengths)
    return(cbind(longstr, avgstr))
  }
  
  # apply the subfunctions to each row (case, subject)
  output <- apply(x, 1, rle_string)
  output <- data.frame(t(output))
  colnames(output) <- (c('longstr','avgstr'))
  
  if(avg == TRUE) {
    return(output)
  } else {
    return(output[,'longstr'])
  }
}

#Longstring number of times 7 or more consecutive identical responses (modified from longstring in careless package)
longstring_n_times <- function(x, avg=FALSE, threshold=6) {
  
  # subfunction that calculates the length of consecutive identical responses
  rle_string <- function(x) {
    rle_list <- rle(x)
    longstr <- max(rle_list$lengths)
    avgstr <- mean(rle_list$lengths)
    count_longstr <- sum(rle_list$lengths > threshold)
    return(cbind(longstr, avgstr, count_longstr))
  }
  
  # apply the subfunctions to each row (case, subject)
  output <- apply(x, 1, rle_string)
  output <- data.frame(t(output))
  colnames(output) <- c('longstr','avgstr', 'count_longstr')
  
  if(avg == TRUE) {
    return(output)
  } else {
    return(output[, c('longstr', 'count_longstr')])
  }
}

# Psych synonyms function (from careless package)
psychsyn <- function(x, critval=.60, anto=FALSE, diag=FALSE, resample_na=TRUE) {
  x <- as.matrix(x)
  item_pairs <- get_item_pairs(x, critval, anto)
  
  synonyms <- apply(x,1,syn_for_one, item_pairs, resample_na)
  synonyms_df <- as.data.frame(aperm(synonyms))
  colnames(synonyms_df) <- c("numPairs", "cor")
  
  if(diag==TRUE) { return(synonyms_df) }
  else { return(synonyms_df$cor) }
}

# Helper function that identifies psychometric synonyms in a given dataset
get_item_pairs <- function(x, critval=.60, anto=FALSE) {
  critval <- abs(critval) #Dummy Proofing
  
  correlations <- stats::cor(x, use = "pairwise.complete.obs")
  correlations[upper.tri(correlations, diag=TRUE)] <- NA
  correlations <- as.data.frame(as.table(correlations))
  
  # Identifying item pairs differs depending on whether the user wants
  # Psychometric Synonyms or Psychometric Antonyms
  if(anto==FALSE) {
    item_pair_names <- correlations[which(correlations$Freq > critval, arr.ind=TRUE),c(1,2)]
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Synonyms found.")
    }
  }
  else if(anto==TRUE) {
    item_pair_names <- correlations[which(correlations$Freq < -critval, arr.ind=TRUE),c(1,2)]
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Antonyms found.")
    }
  }
  
  matches <- item_pair_names
  return(matches)
}

# Helper function to calculate the within person correlation for a single individual
syn_for_one <- function(x, item_pairs, resample_na) {
  item_pairs_omit_na <- which(!(is.na(x[item_pairs[,1]]) | is.na(x[item_pairs[,2]])))
  sum_item_pairs <- length(item_pairs_omit_na)
  #only execute if more than two item pairs
  if(sum_item_pairs > 2) {
    itemvalues <- cbind(as.numeric(x[as.numeric(item_pairs[,1])]), as.numeric(x[as.numeric(item_pairs[,2])]))
    
    # helper that calculates within-person correlation
    psychsyn_cor <- function(x) {
      suppressWarnings(stats::cor(x, use = "pairwise.complete.obs", method = "pearson")[1,2])
    }
    
    # if resample_na == TRUE, re-calculate psychsyn should a result return NA
    if(resample_na == TRUE) {
      counter <- 1
      synvalue <- psychsyn_cor(itemvalues)
      while(counter <= 10 & is.na(synvalue)) {
        itemvalues <- t(apply(itemvalues, 1, sample, 2, replace = F))
        synvalue <- psychsyn_cor(itemvalues)
        counter = counter+1
      }
    } else {
      synvalue <- psychsyn_cor(itemvalues) # executes if resample_na == FALSE
    }
    
  } else {synvalue <- NA} # executes if insufficient item pairs
  
  return(c(sum_item_pairs, synvalue))
}