## An attempt to estimate the CAR for the Kentucky CERS system.
## See https://www.kyret.ky.gov/Publications/Actuarial%20Valuations/2022%20CERS%20Actuarial%20Valuation.pdf
##
##
## Pull in the CAR calculation apparatus.
source("../../src/car.r", chdir=TRUE);
source("../../validate/validate.r", chdir=TRUE);

## System-specific, from the KY CERS 2022 val report.
doesMemberSeparate <- function(age, sex, service, status="active", tier="1",
                               mortClass="General", verbose=FALSE) {
    ## If this is not currently an active employee, get out.
    if (status != "active") return(status);

    if (verbose) cat("Separate? age:", age, "service:", service, "status:", status,
                     "tier:", tier, "mortClass:", mortClass);

    ## Percentages from p.67 of the val report.
    if (mortClass == "General") {
        thresholds <- c(20.00, 15.58, 12.48, 10.66, 9.37,
                        8.37, 7.56, 6.87, 6.27, 5.74,
                        5.27, 4.84, 4.45, 4.09, 3.76,
                        3.45, 3.16, 2.89, 2.64, 2.39,
                        2.16, 1.94, 1.74, 1.54, 1.35);
    } else if (mortClass == "Safety") {
        thresholds <- c(20.00, 9.11, 7.24, 6.14, 5.37,
                        4.76, 4.27, 3.85, 3.49, 3.18,
                        2.89, 2.63, 2.40, 2.18, 1.98,
                        1.80, 1.62, 1.46, 1.30, 1.16,
                        0.00, 0.00, 0.00, 0.00, 0.00);
    }

    ## Roll the dice.
    if (service < 1) service <- 1;
    if (service > 25) service <- 25;
    if (runif(1) < thresholds[service]/100.0) status <- "separated";

    if (verbose) cat(" threshold:", threshold, "new status:", status, "\n");

    return(status);
}



doesMemberRetire <- function(age, sex, service, status="active", year=2022,
                             tier="A", mortClass="General", verbose=FALSE) {
    ## If already retired, disabled, or dead, get out.
    ## If already retired, disabled, or dead, get out.
    if (status %in% c("retired", "retired/survivor", "deceased", "disabled/accident",
                      "disabled/ordinary")) return(status);
    
    if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                     ", tier: ", tier, " begin: ", status, "..",
                     sep="");

    if (!(mortClass %in% c("General", "Safety"))) {
        stop(paste0("What mortClass is this: ", mortClass));
    }

    if (!(tier %in% c("1", "2", "3"))) {
        stop(paste0("No such tier: ", tier));
    }

    ## These are from the table on page 61 of the 2022 val report
    generalRetirementMale <- c(35.0, 35.0, 35.0, 35.0, 35.0, 35.0,
                               rep(30.0, 25), 100.0);
    generalRetirementFemale <- c(rep(27.0, 18), 40.0, 35.0, 30.0, 30.0,
                                 rep(27.0, 9), 100.0);
    
    generalEarlyRetirementMale <- c(rep(15.0, 3), rep(4.0, 7));
    generalEarlyRetirementFemale <- c(16.0, 18.0, 20.0, 9.0, 8.0, rep(5.0, 5));

    safetyRetireTierOne <- c(rep(17.0, 15), 30.0, 22.5, 18.0, 21.0, 24.0, 30.0,
                             33.0, 36.0, 39.0, 39.0);
    safetyRetireTierTwo <- c(21.6, 24.0, 26.4, 28.8, 31.2, 31.2);
    safetyRetireTierThree <- c(rep(16.0, 5), 100.0);
    

    threshold <- 0;

    if (mortClass == "General") {
        
        if (tier == "1")  {
            ## General retirement provisions seem to depend on age, less on
            ## service, though they have a 5-year vesting requirement.
            if (service < 5) {
                if (verbose) cat("-> not even vested.\n");
                return(status);
            }

            ## Check eligibility.
            if ((age >= 65) || (service >= 27)) {
            
                if (sex == "M") {
                    threshold <- generalRetirementMale[min(max(age - 43, 1), 32)];
                } else {
                    threshold <- generalRetirementFemale[min(max(age - 43, 1), 32)];
                }

            } else if ((service >= 25) || ((age >= 55) && (service >= 5))) {
                ## Check for early retirement.
                if (sex == "M") {
                    threshold <-
                        generalEarlyRetirementMale[min(max(age - 54, 1), 10)];
                } else {
                    threshold <-
                        generalEarlyRetirementFemale[min(max(age - 54, 1), 10)];
                }
            }
        } else if ((tier == "2") || (tier == "3")) {
            
            ## General retirement provisions seem to depend on age, less on
            ## service, though they have a 5-year vesting requirement.
            if (service < 5) {
                if (verbose) cat("-> not even vested.\n");
                return(status);
            }

            ## Check eligibility.
            if (((age >= 65) && (service >= 5)) ||
                ((age >= 57) && (age + service > 87))) {
            
                if (sex == "M") {
                    threshold <- generalRetirementMale[min(max(age - 43, 1), 32)];
                } else {
                    threshold <- generalRetirementFemale[min(max(age - 43, 1), 32)];
                }

            } else if ((tier == "2") && ((age >= 60) && (service >= 10))) {
                ## Check for early retirement. (No such for tier 3.)
                if (sex == "M") {
                    threshold <-
                        generalEarlyRetirementMale[min(max(age - 54, 1), 10)];
                } else {
                    threshold <-
                        generalEarlyRetirementFemale[min(max(age - 54, 1), 10)];
                }
            }
        }
    } else { ## mortClass == Safety
            
        if (tier == 1) {
            if (age >= 62) {
                if (age < 65) {
                    threshold <- safetyRetireTierOne[min(max(service - 4, 1), 25)];
                } else {
                    threshold <- 1.0;
                }
            } else if (((age >= 55) && (service >= 1)) ||
                       (service > 20) ||
                       ((age >= 50) && (service >= 15))) {   # (Early retirement.)
                threshold <- safetyRetireTierOne[min(max(service - 4, 1), 25)];
            }
        } else if (tier == 2) {
            if (age >= 60) {
                if (service < 5) {
                    threshold <- safetyRetireTierTwo[min(max(service - 24, 1), 6)];
                } else {
                    threshold <- 1.0;
                }
            } else if ((service >= 25) ||
                       ((age >= 50) && (service >= 15))) {   # (Early retirement.)
                threshold <- safetyRetireTierTwo[min(max(service - 24, 1), 6)];
            }
        } else if (tier == 3) {
            if (age >= 60) {
                if (service < 5) {
                    threshold <- safetyRetireTierThree[min(max(service - 24, 1), 6)];
                } else {
                    threshold <- 1.0;
                }
            } else if (service >= 25) {
                threshold <- safetyRetireTierThree[min(max(service - 24, 1), 6)];
            }
        }
    }

    ## If we're not even vested, do nothing and return.
    if (threshold == 0) {
        if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                         "-> not even vested.\n", sep="");
        return(status);
    } else {
    
        ## Roll the dice.
        if (runif(1) < threshold/100.0) status <- "retired";
    
        if (verbose) cat("result: ", status, "\n", sep="");

        return(status);
    }
}


doesMemberDisableAccident <- function(age, sex, service, status,
                                      mortClass="General", tier="A",
                                      verbose=FALSE) {
    ## If already retired, disabled, or dead, get out.
    if (status %in% c("retired", "retired/survivor", "deceased",
                      "disabled/accident", "disabled/ordinary")) return(status);

    if (sex == "M") {
        if (age < 35) threshold <- 0.0003
        else if (age < 65) 
            threshold <- c(0.0004, 0.0004, 0.0005, 0.0006, 0.0007,
                           0.0008, 0.0009, 0.0010, 0.0012, 0.0014,
                           0.0016, 0.0018, 0.0021, 0.0024, 0.0028,
                           0.0033, 0.0039, 0.0046, 0.0053, 0.0061,
                           0.0069, 0.0077, 0.0086, 0.0095, 0.0105,
                           0.0115, 0.0126, 0.0138, 0.0151, 0.0164)[age - 34]
        else threshold <- 0.0;
    } else {
        if (age < 28) threshold <- 0.0003
        else if (age < 30) threshold <- 0.0004
        else if (age < 65)
            threshold <- c(0.0004, 0.0005, 0.0005, 0.0006, 0.0006,
                           0.0007, 0.0008, 0.0009, 0.0010, 0.0012,
                           0.0013, 0.0015, 0.0017, 0.0019, 0.0022,
                           0.0024, 0.0027, 0.0030, 0.0033, 0.0036,
                           0.0040, 0.0044, 0.0049, 0.0054, 0.0059,
                           0.0064, 0.0069, 0.0074, 0.0080, 0.0085,
                           0.0090, 0.0096, 0.0101, 0.0105, 0.0109)[age - 29]
        else threshold <- 0.0;
    }

    ## Roll the dice.
    if (runif(1) < threshold) status <- "disabled/accident";

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

## This is empty because ACC does not distinguish between kinds of
## disability.  We are using disabled/ordinary as a synonym for
## retired-because-of-disability, since disabled/accident seems not to
## imply actually retired in the ACC system. See above.
doesMemberDisableOrdinary <- function(age, sex, service, status,
                                      mortClass="General", tier="A",
                                      verbose=FALSE) {
    return(status);
}


projectSalaryDelta <- function(year, age, salary, service=1, tier="A",
                               mortClass="General", verbose=verbose) {

    ## Appendix table I
    if (service < 3) delta <- 1.065
    else if (service < 5) delta <- 1.06
    else if (service < 6) delta <- 1.05
    else if (service < 12) delta <- 1.045
    else delta <- 1.0425;

    return(delta);
}


doesMemberHaveSurvivor <- function(age, sex, status, survivor,
                                   tier=tier, mortClass=mortClass,
                                   verbose=verbose) {

    if (verbose) cat("Member: age:", age, "sex:", sex, "status:", status, "\n")
    
    ## We only want to do this once. And we probably want to do better
    ## (less retro?) choices of sex and age in the specialized versions.
    if ((status == "retired") && (survivor$status == "")) {
        if (verbose) cat("Creating a survivor...");
        survivor$status <- "retired/survivor";

        if (sex == "M") {
            survivor$age <- age - 2;
        } else {
            survivor$age <- age + 2;
        }
        
        survivor$sex <- ifelse(sex=="M", "F", "M"); 
    }

    if (verbose) cat("Survivor is age:", survivor$age, "sex:", survivor$sex,
                     "status:", survivor$status, "\n");

    return(survivor);
}




## ACC val report, page 34.
projectBasePension <- function(salaryHistory, mortClass="General", tier="A",
                               verbose=FALSE) {    
    
    ## Find first entry that is *not* active or separated. (Or d/a for ACC.)
    retirementType <- first(salaryHistory$status[!salaryHistory$status %in%
                                                 c("active", "separated",
                                                   "disabled/accident")]);

    if (verbose) cat("Retirement type:", retirementType);
    
    ## Find last entry before retirement.
    preRetirementStatus <- salaryHistory %>%
        filter(status %in% c("active", "separated", "disabled/accident")) %>%
        group_by(status) %>%
        dplyr::summarize(year=last(year),
                         age=last(age),
                         service=last(service));
    ## Might be multiple rows, we want the last one.
    preRetirementStatus <- preRetirementStatus[dim(preRetirementStatus)[1],];

    year <- preRetirementStatus[["year"]];
    age <- preRetirementStatus[["age"]];
    service <- preRetirementStatus[["service"]];
    status <- preRetirementStatus[["status"]];
        
    if (verbose) cat(" -> year:", year, "age:", age,
                     "service:", service, "status:", status, "\n");
    
    ## Find the average of the top three-year period. Note that this is different
    ## from the top three years, but it will do for the model, since we don't
    ## use it to generate any pathological cases.
    s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 10);
    threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
    avgSalary <- max(threeYears) / 3.0;

    if (grepl("retired", retirementType)) {
        if (verbose) cat("Calculating pension for regular retirement: ")
        if ((status == "active") || (status == "separated")) {
            basePension <- max(0.0185 * avgSalary * min(service, 32) +
                0.0025 * max(0, service - 32) * avgSalary, 240 * 12);

            ## is this an early retirement?
            if ((mortClass == "General") && (age <= 62)) {
                basePension <- basePension * (1 - 0.00333 * 12 * (62 - age));
            } else if ((mortClass == "Safety") && (age <= 60)) {
                basePension <- basePension * (1 - 0.00333 * 12 * (60 - age));
            }
        }       

    } else {
        ## Not a retirement. Return zero.
        basePension <- 0;
    }

    return(basePension);
}

## Apply the base pension to the first retirement year, then roll it
## forward according to the COLA for that tier and year.
projectPensionPayments <- function(salaryHistory, basePension, tier="A",
                                   mortClass=mortClass, verbose=FALSE) {

    retireYear <- salaryHistory %>%
        filter(status %in% c("retired", "disabled/ordinary")) %>%
        dplyr::summarize(retireYear=min(year)) %>% as.numeric();

    ## Assumptions: No COLAs in ACC after 2018.
    salaryHistory <- salaryHistory %>%
        mutate(pension=ifelse((status %in% c("retired",
                                             "disabled/ordinary",
                                             "disabled/accident")),
                              basePension,
                       ifelse(survivorStatus == "retired/survivor",
                              basePension,
                              0)));

    return(salaryHistory);
}

    

projectPension <- function(salaryHistory, tier="A", mortClass="General",
                           verbose=FALSE) {

    basePension <- projectBasePension(salaryHistory, tier=tier,
                                      mortClass=mortClass, verbose=verbose);

    if (verbose) cat("Base pension: ", round(basePension), "\n", sep="");
    
    salaryHistory <- projectPensionPayments(salaryHistory, basePension, tier=tier,
                                            mortClass=mortClass, verbose=verbose);

    return(salaryHistory);
}

## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share, do not include amortization
## payments.)
projectPremiums <- function(salaryHistory, tier="A", mortClass="General",
                            verbose=FALSE) {

    premiumPerPayroll <- .145;

    if (verbose) cat("Running projectPremiums from acc.r, tier:", tier,
                     "premium % payroll:", premiumPerPayroll * 100, "%\n");
    
    return(salaryHistory %>%
           mutate(premium = salary * premiumPerPayroll))
}


############################################################################
############################################################################
## The above is all about specifying the benefits. Now we model the
## actual population and then run the model.
accModel <- function(verbose=FALSE) {

    xlfile <- "../../acc/data/ACC-scratch.xlsx";
    
    if (verbose) cat("building model", date(), "from", xlfile, "\n");

    accDemo <- read_excel(xlfile, sheet="demographics", range="A1:J101",
                          col_types=c(rep("numeric", 8), rep("text", 2)));

    accModel <- memberList();
    for (i in 1:dim(accDemo)[1]) {
        accModel <- genEmployees(accDemo$N[i],
                                 ageRange=c(accDemo$minAge[i], accDemo$maxAge[i]),
                                 servRange=c(accDemo$minService[i], accDemo$maxService[i]),
                                 avgSalary=accDemo$avgSalary[i],
                                 sex=list(M=accDemo$M[i], F=accDemo$F[i]),
                                 class=accDemo$mortClass[i],
                                 tier=accDemo$tier[i],
                                 currentYear=2020,
                                 members=accModel,
                                 verbose=verbose)
    }
    
    if (verbose) cat("finished building model", date(), "\n");
    
    return(accModel);
}

##accModelOutputLg <- runModel(accModel, N=75, verbose=TRUE, reallyVerbose=FALSE,
##                           audit=TRUE);

