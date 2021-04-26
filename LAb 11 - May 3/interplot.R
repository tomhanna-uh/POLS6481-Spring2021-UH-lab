interplot.default <- function(m, var1, var2, plot = TRUE, steps = NULL, 
                              ci = .95, hist = FALSE, var2_dt = NA, point = FALSE, sims = 5000, xmin = NA, 
                              xmax = NA, ercolor = NA, esize = 0.5, ralpha = 0.5, rfill = "grey70", 
                              ...) {
  set.seed(324)
  
  m.class <- class(m)
  m.sims <- arm::sim(m, sims)
  
  
  ### For factor base terms###
  factor_v1 <- factor_v2 <- FALSE
  
  if (is.factor(eval(parse(text = paste0("m$model$", var1)))) & is.factor(eval(parse(text = paste0("m$model$", 
                                                                                                   var2))))) 
    stop("The function does not support interactions between two factors.")
  
  
  if (is.factor(eval(parse(text = paste0("m$model$", var1))))) {
    var1_bk <- var1
    var1 <- paste0(var1, eval(parse(text = paste0("m$xlevel$", var1))))
    factor_v1 <- TRUE
    ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                             ":", var1)[-1])
    
    # the first category is censored to avoid multicolinarity
    for (i in seq(var12)) {
      if (!var12[i] %in% names(m$coef)) 
        var12[i] <- paste0(var1, ":", var2)[-1][i]
      if (!var12[i] %in% names(m$coef)) 
        stop(paste("Model does not include the interaction of", 
                   var1, "and", var2, "."))
    }
    
  } else if (is.factor(eval(parse(text = paste0("m$model$", var2))))) {
    var2_bk <- var2
    var2 <- paste0(var2, eval(parse(text = paste0("m$xlevel$", var2))))
    factor_v2 <- TRUE
    ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                             ":", var1)[-1])
    
    # the first category is censored to avoid multicolinarity
    for (i in seq(var12)) {
      if (!var12[i] %in% names(m$coef)) 
        var12[i] <- paste0(var1, ":", var2)[-1][i]
      if (!var12[i] %in% names(m$coef)) 
        stop(paste("Model does not include the interaction of", 
                   var1, "and", var2, "."))
    }
    
  } else {
    ifelse(var1 == var2, var12 <- paste0("I(", var1, "^2)"), var12 <- paste0(var2, 
                                                                             ":", var1))
    
    # the first category is censored to avoid multicolinarity
    for (i in seq(var12)) {
      if (!var12[i] %in% names(m$coef)) 
        var12[i] <- paste0(var1, ":", var2)[i]
      if (!var12[i] %in% names(m$coef)) 
        stop(paste("Model does not include the interaction of", 
                   var1, "and", var2, "."))
    }
  }
  
  ################### 
  
  
  if (factor_v2) {
    xmin <- 0
    xmax <- 1
    steps <- 2
  } else {
    if (is.na(xmin)) 
      xmin <- min(m$model[var2], na.rm = T)
    if (is.na(xmax)) 
      xmax <- max(m$model[var2], na.rm = T)
    
    if (is.null(steps)) {
      steps <- eval(parse(text = paste0("length(unique(na.omit(m$model$", 
                                        var2, ")))")))
    }
    
    
    if (steps > 100) 
      steps <- 100  # avoid redundant calculation
  }
  
  coef <- data.frame(fake = seq(xmin, xmax, length.out = steps), coef1 = NA, 
                     ub = NA, lb = NA)
  coef_df <- data.frame(fake = numeric(0), coef1 = numeric(0), ub = numeric(0), 
                        lb = numeric(0), model = character(0))
  
  if (factor_v1) {
    for (j in 1:(length(eval(parse(text = paste0("m$xlevel$", var1_bk)))) - 
                 1)) {
      # only n - 1 interactions; one category is avoided against
      # multicolinarity
      
      for (i in 1:steps) {
        coef$coef1[i] <- mean(m.sims@coef[, match(var1[j + 1], 
                                                  names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                       names(m$coef))])
        coef$ub[i] <- quantile(m.sims@coef[, match(var1[j + 1], 
                                                   names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                        names(m$coef))], 1 - (1 - ci) / 2)
        coef$lb[i] <- quantile(m.sims@coef[, match(var1[j + 1], 
                                                   names(m$coef))] + coef$fake[i] * m.sims@coef[, match(var12[j], 
                                                                                                        names(m$coef))], (1 - ci) / 2)
      }
      
      if (plot == TRUE) {
        coef$value <- var1[j + 1]
        coef_df <- rbind(coef_df, coef)
        if (hist == TRUE) {
          if (is.na(var2_dt)) {
            var2_dt <- eval(parse(text = paste0("m$model$", var2)))
          } else {
            var2_dt <- var2_dt
          }
        }
      } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
      }
    }
    coef_df$value <- as.factor(coef_df$value)
    interplot.plot(m = coef_df, hist = hist, var2_dt = var2_dt, steps = steps, 
                   point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                   rfill = rfill, ...) + facet_grid(. ~ value)
    
  } else if (factor_v2) {
    for (j in 1:(length(eval(parse(text = paste0("m$xlevel$", var2_bk)))) - 
                 1)) {
      # only n - 1 interactions; one category is avoided against
      # multicolinarity
      
      for (i in 1:steps) {
        coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + 
                                coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))])
        coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                                 coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], 
                               1 - (1 - ci) / 2)
        coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                                 coef$fake[i] * m.sims@coef[, match(var12[j], names(m$coef))], 
                               (1 - ci) / 2)
      }
      
      if (plot == TRUE) {
        coef$value <- var2[j + 1]
        coef_df <- rbind(coef_df, coef)
        if (hist == TRUE) {
          if (is.na(var2_dt)) {
            var2_dt <- eval(parse(text = paste0("m$model$", var2)))
          } else {
            var2_dt <- var2_dt
          }
        }
      } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
      }
    }
    coef_df$value <- as.factor(coef_df$value)
    interplot.plot(m = coef_df, hist = hist, steps = steps, var2_dt = var2_dt, 
                   point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                   rfill = rfill, ...) + facet_grid(. ~ value)
    
    
  } else {
    ## Correct marginal effect for quadratic terms
    multiplier <- if (var1 == var2) 
      2 else 1
    
    for (i in 1:steps) {
      coef$coef1[i] <- mean(m.sims@coef[, match(var1, names(m$coef))] + 
                              multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                              names(m$coef))])
      coef$ub[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                               multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                               names(m$coef))], 1 - (1 - ci) / 2)
      coef$lb[i] <- quantile(m.sims@coef[, match(var1, names(m$coef))] + 
                               multiplier * coef$fake[i] * m.sims@coef[, match(var12, 
                                                                               names(m$coef))], (1 - ci) / 2)
    }
    
    if (plot == TRUE) {
      if (hist == TRUE) {
        if (is.na(var2_dt)) {
          var2_dt <- eval(parse(text = paste0("m$model$", var2)))
        } else {
          var2_dt <- var2_dt
        }
      }
      interplot.plot(m = coef, steps = steps, hist = hist, var2_dt = var2_dt, 
                     point = point, ercolor = ercolor, esize = esize, ralpha = ralpha, 
                     rfill = rfill, ...)
    } else {
      names(coef) <- c(var2, "coef", "ub", "lb")
      return(coef)
    }
    
  }
  
}

interplot <- function(m, var1, var2, plot = TRUE, steps = NULL, ci = .95, 
                      hist = FALSE, var2_dt = NA, point = FALSE, sims = 5000, 
                      xmin = NA, xmax = NA, ercolor = NA, esize = 0.5, 
                      ralpha = 0.5, rfill = "grey70", ...) {
  if (class(m)[1] == "list") {
    if (class(m[[1]])[1] == "lmerMod") {
      class(m) <- "mlmmi"
    }
    if (class(m[[1]])[1] == "glmerMod") {
      class(m) <- "gmlmmi"
    }
    if (class(m[[1]])[1] == "lm") {
      class(m) <- "lmmi"
    }
    if (class(m[[1]])[1] == "glm") {
      class(m) <- "glmmi"
    }
  }
  
  if (class(m)[1] == "data.frame") 
    class(m) <- "plot"
  
  
  UseMethod("interplot", m)
}

interplot.plot <- function(m, var1 = NULL, var2 = NULL, plot = TRUE, steps = NULL, ci = .95, hist = FALSE, var2_dt = NULL, point = FALSE, sims = 5000, xmin = NA, xmax = NA, ercolor = NA, esize = 0.5, ralpha = 0.5, rfill = "grey70", ...) {
  if(is.null(steps)) steps <- nrow(m)
  levels <- sort(unique(m$fake))
  ymin <- ymax <- vector() # to deal with the "no visible binding for global variable" issue
  xdiff <- vector() # to deal with the "no visible binding for global variable" issue
  
  
  if (hist == FALSE) {
    if (steps < 10 | point == T) {
      if (is.na(ercolor)) 
      {
        ercolor <- "black"
      }  # ensure whisker can be drawn
      coef.plot <- ggplot(m, aes_string(x = "fake", y = "coef1")) + geom_point(...) + geom_errorbar(aes_string(ymin = "lb", 
                                                                                                               ymax = "ub"), width = 0, color = ercolor, size = esize) + scale_x_continuous(breaks = levels) + 
        ylab(NULL) + xlab(NULL)
    } else {
      coef.plot <- ggplot(m, aes_string(x = "fake", y = "coef1")) + geom_line(...) + geom_ribbon(aes_string(ymin = "lb", 
                                                                                                            ymax = "ub"), alpha = ralpha, color = ercolor, fill = rfill) + ylab(NULL) + xlab(NULL)
    }
    return(coef.plot)
  } else {
    if (point == T) {
      if (is.na(ercolor)) 
      {
        ercolor <- "black"
      }  # ensure whisker can be drawn
      
      yrange <- c(m$ub, m$lb, var2_dt)
      maxdiff <- (max(yrange) - min(yrange))
      
      break_var2 <- steps + 1
      if (break_var2 >= 100) 
        break_var2 <- 100
      hist.out <- hist(var2_dt, breaks = seq(min(var2_dt), max(var2_dt), l = break_var2), plot = FALSE)
      
      n.hist <- length(hist.out$mids)
      
      if (steps <10) {dist <- (hist.out$mids[2] - hist.out$mids[1])/3
      } else {dist <- hist.out$mids[2] - hist.out$mids[1]}
      hist.max <- max(hist.out$counts)
      
      if (steps <10) {
        histX <- data.frame(ymin = rep(min(yrange) - maxdiff/5, n.hist),
                            ymax = hist.out$counts/hist.max * maxdiff/5 + min(yrange) - maxdiff/5, 
                            xmin = sort(unique(var2_dt)) - dist/2, 
                            xmax = sort(unique(var2_dt)) + dist/2)
      } else {
        histX <- data.frame(ymin = rep(min(yrange) - maxdiff/5, n.hist), 
                            ymax = hist.out$counts/hist.max * maxdiff/5 + min(yrange) - maxdiff/5, 
                            xmin = hist.out$mids - dist/2, 
                            xmax = hist.out$mids + dist/2)
      } 
      #when up to 10, the sort(unique(var2_dt)) - dist/2 leads to problemtic histogram
      
      
      if (steps <10) {
        histX_sub <- histX
      } else {
        histX_sub <- mutate(histX, xdiff = xmax - xmin, xmax = xmax - xdiff/2)
      }
      
      coef.plot <- ggplot()
      coef.plot <- coef.plot + geom_rect(data = histX, aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                                                           ymax = ymax), colour = "gray50", alpha = 0, size = 0.5)  #histgram
      
      coef.plot <- coef.plot +
        geom_errorbar(data = m, aes_string(x = "fake", ymin = "lb", ymax = "ub"), width = 0, 
                      color = ercolor, size = esize) + scale_x_continuous(breaks = levels) + ylab(NULL) + 
        xlab(NULL) + geom_point(data = m, aes_string(x = "fake", y = "coef1")) 
      
    } else {
      
      yrange <- c(m$ub, m$lb)
      
      maxdiff <- (max(yrange) - min(yrange))
      
      break_var2 <- length(unique(var2_dt))
      if (break_var2 >= 100) 
        break_var2 <- 100
      hist.out <- hist(var2_dt, breaks = break_var2, plot = FALSE)
      
      n.hist <- length(hist.out$mids)
      dist <- hist.out$mids[2] - hist.out$mids[1]
      hist.max <- max(hist.out$counts)
      
      histX <- data.frame(ymin = rep(min(yrange) - maxdiff/5, n.hist), ymax = hist.out$counts/hist.max * 
                            maxdiff/5 + min(yrange) - maxdiff/5, xmin = hist.out$mids - dist/2, xmax = hist.out$mids + 
                            dist/2)
      
      
      
      # interplot.plot(m = coef, var1 = 'cyl', var2 = 'wt')
      
      coef.plot <- ggplot()
      coef.plot <- coef.plot + geom_rect(data = histX, aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                                                           ymax = ymax), colour = "gray50", alpha = 0, size = 0.5)
      
      
      coef.plot <- coef.plot + geom_line(data = m, aes_string(x = "fake", y = "coef1")) + 
        geom_ribbon(data = m, aes_string(x = "fake", ymin = "lb", ymax = "ub"), alpha = ralpha, 
                    color = ercolor, fill = rfill) + ylab(NULL) + xlab(NULL)
    }
    return(coef.plot)
  }
  
}