## Apply a rate to a cash flow.  Also calculate the derivative, for
## use with Newton's method.
fvFlow <- function(cashFlow, rate) {

    nYears <- length(cashFlow$year);
    ## Return a modified cashFlow array with a couple of new columns
    ## for the calculation.
    return(cashFlow %>%
           mutate(reverseYear=nYears-row_number(),
                  rr=rate^reverseYear,
                  rp=reverseYear * rate^(reverseYear - 1),
                  ## The future value of this money at the end of the term.
                  fv=flow * (rate^(reverseYear)),
                  ## The derivative at this point.
                  fvp=flow * reverseYear * (rate^(reverseYear - 1))));
}

## Takes a step in the iteration that is Newton's method.
newtonStep <- function(cashFlow, rateGuess, futureVal, verbose=FALSE) {
    ## Calculate the future value at the guess
    fvf <- fvFlow(cashFlow, rateGuess);

    ## Calculate the value...
    fv <- fvf %>% select(fv) %>% sum();
    ## ... and the derivative.
    fvp <- fvf %>% select(fvp) %>% sum();

    if (verbose)
        cat("Future val:", fv, "slope:",  fvp,
            "target:", futureVal, "rate", rateGuess, "\n");

    nextStep <- rateGuess - (futureVal - fv)/(-fvp);

    return(nextStep);
}


## Given a first guess, uses Newton's method to find the rate
## necessary to make the given cash flow come out to the future value,
## at the end of the time described by the cash flow tibble.  Note
## that the rate returned should be a number greater than zero, with a
## value of 1 indicating 0%.
findRate <- function(cashFlow, futureVal=0, flowName="flow",
                     firstGuess=1.5, maxIter=100, tolerance=0.001,
                     maxRecurse=10, verbose=FALSE) {

    ## If we've had to recurse a lot, we probably aren't going to find
    ## a solution.
    if (maxRecurse <= 0) return(NA);

    ## Standardize the cash flow data.
    if (!("flow" %in% names(cashFlow))) {
        cashFlow <- cashFlow %>%
            rename_with(function(x) { ifelse(x == flowName, "flow", x) }) %>%
            select(year, flow);
    }

    ## Check to see if this is empty or pathological.  We have to have
    ## at least one entry above zero and one entry below.
    bounds <- cashFlow %>% summarize(max=max(flow), min=min(flow));
    if (is.na(bounds$min) || is.na(bounds$max)) {
        print(as.data.frame(cashFlow));
        stop("There are NAs in the cash flow.\n");
    }
    if (bounds$max * bounds$min >= 0) {
        if (verbose) cat("Need positive and negative values.",
                         "Have min:", bounds$min, ", max:", bounds$max,".\n");
        return(NA);
    }

    if (verbose) cat("flowName:", flowName, "\n");

    currentGuess <- firstGuess;
    if (verbose) cat("First guess =", currentGuess, "\n");

    oldGuess <- currentGuess;
    iterCount <- 0;
    for (i in 1:maxIter) {
        iterCount <- iterCount + 1;
        if (verbose) cat(i, "> ");
        currentGuess <- newtonStep(cashFlow, oldGuess, futureVal, verbose);

        ## Zero is often a degenerate solution and if we're headed
        ## there (or if we're in negative territory) let's try going
        ## somewhere else instead.
        if (currentGuess < tolerance) {
            if (verbose)
                cat("current guess: ", currentGuess,
                    ", restarting at: ", 1.3 * firstGuess,
                    ", recurse: ", maxRecurse, "\n", sep="");
            currentGuess <- findRate(cashFlow,
                                     futureVal=futureVal,
                                     firstGuess=1.3 * firstGuess,
                                     maxIter=maxIter,
                                     tolerance=tolerance,
                                     maxRecurse=maxRecurse - 1,
                                     verbose=verbose);
            break;
        }

        if (tolerance > abs(1 - (currentGuess / oldGuess))) break;
        oldGuess <- currentGuess;
    }

    if (iterCount >= maxIter) {
        cat("MaxIter limit hit (", maxIter, ") NA returned.\n");
        print(as.data.frame(cashFlow))
        return(NA);
    }

    return(currentGuess);
}


