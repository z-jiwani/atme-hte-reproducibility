
# Add max.depth to SL.ranger
# Don't need new pred wrapper as this will re-use pred.SL.ranger
SL.ranger.1 <-
  function(Y, X, newX, family,
           obsWeights,
           num.trees = 500,
           mtry = floor(sqrt(ncol(X))),
           write.forest = TRUE,
           probability = family$family == "binomial",
           min.node.size = ifelse(family$family == "gaussian", 5, 1),
           max.depth = NULL,
           replace = TRUE,
           sample.fraction = ifelse(replace, 1, 0.632),
           num.threads = 1,
           verbose = T,
           ...) {
    # need write.forest = TRUE for predict method
    SuperLearner:::.SL.require("ranger")
    
    if (family$family == "binomial") {
      Y = as.factor(Y)
    }
    
    # Ranger does not seem to work with X as a matrix, so we explicitly convert to
    # data.frame rather than cbind. newX can remain as-is though.
    if (is.matrix(X)) {
      X = data.frame(X)
    }
    
    # Use _Y as our outcome variable name to avoid a possible conflict with a
    # variable in X named "Y".
    fit <- ranger::ranger(`_Y` ~ ., data = cbind("_Y" = Y, X),
                          num.trees = num.trees,
                          mtry = mtry,
                          min.node.size = min.node.size,
                          max.depth = max.depth,
                          replace = replace,
                          sample.fraction = sample.fraction,
                          case.weights = obsWeights,
                          write.forest = write.forest,
                          probability = probability,
                          num.threads = num.threads,
                          verbose = verbose)
    
    pred <- predict(fit, data = newX)$predictions
    
    # For binomial family $predictions is a two-column matrix.
    if (family$family == "binomial") {
      # P(Y = 1 | X) for binomial.
      pred = pred[, "1"]
    }
    
    fit <- list(object = fit, verbose = verbose)
    class(fit) <- c("SL.ranger")
    out <- list(pred = pred, fit = fit)
    return(out)
  }
