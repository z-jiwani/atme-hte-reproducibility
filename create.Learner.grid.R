###########################################################################
####### Change create.Learner built-in function to include tunegrid 
####### This is useful where one of the parameters (num.trees in this case)
####### is different for each row of the grid
###########################################################################
create.Learner.grid = function(base_learner, params = list(), tune = list(), 
                               tunegrid = data.frame(),
                               env = parent.frame(), name_prefix = base_learner,
                               detailed_names = F, verbose = F) {
  if (length(tunegrid) > 0) {
    tuneGrid = tunegrid
    names = rep("",nrow(tuneGrid))
    max_runs = nrow(tuneGrid)
  }
  else if (length(tune) > 0) {
    tuneGrid = expand.grid(tune, stringsAsFactors = FALSE)
    names = rep("", nrow(tuneGrid))
    max_runs = nrow(tuneGrid)
  } else {
    # Run once if no tuneGrid is defined, otherwise run once per grid row.
    max_runs = 1
    tuneGrid = NULL
    names = c()
  }
  
  for (i in seq(max_runs)) {
    
    name = paste(name_prefix, i, sep="_")
    
    if (length(tuneGrid) > 0) {
      # Specify drop=F in case tuneGrid is a single-column dataframe.
      g = tuneGrid[i, , drop=F]
      
      # Separate with "_" because some hyperparameters could be floats with a period.
      if (detailed_names) {
        name = do.call(paste, c(list(name_prefix), g, list(sep="_")))
      }
    } else {
      g = c()
    }
    
    names[i] = name
    
    # Create the custom learner function. This approach allows us to not specify
    # some of the learner arguments. and have the function use its own defaults.
    # Or we can set those arguments to "NULL".
    fn_params = ""
    all_params = c(as.list(g), params)
    for (name_i in names(all_params)) {
      val = all_params[[name_i]]
      # Ignore a parameter if it's set to a real NULL or the string "NULL".
      # May need to tweak this if someone really needs to pass a NULL for some reason.
      if (!is.null(val) && val != "NULL") {
        # Add quotes around val if it is a string rather than numeric.
        if (class(val) == "character") {
          val = paste0('"', val, '"')
        }
        fn_params = paste0(fn_params, ", ", name_i, "=", val)
      }
    }
    
    fn = paste0(name, " <- function(...) ", base_learner, "(...", fn_params, ")")
    if (verbose) {
      cat(fn, "\n")
    }
    eval(parse(text = fn), envir = env)
  }
  results = list(grid = tuneGrid, names = names, base_learner = base_learner,
                 params = params)
  invisible(results)
}
