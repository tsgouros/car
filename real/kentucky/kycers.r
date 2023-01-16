## An attempt to estimate the CAR for the Kentucky CERS system.
## See https://www.kyret.ky.gov/Publications/Actuarial%20Valuations/2022%20CERS%20Actuarial%20Valuation.pdf
##
##
## Pull in the CAR calculation apparatus.
source("../../src/car.r", chdir=TRUE);
source("../../validate/validate.r", chdir=TRUE);

validateInputsKY <- function(age=20, ageRange=c(20,120),
                           sex="M", sexRange=c("M","F"),
                           service=0, serviceRange=c(0,100),
                           status="active",
                           statusRange=c("active", "separated", "retired",
                                         "retired/survivor", "deceased",
                                         "disabled/accident", "disabled/ordinary"),
                           mortClass="General",
                           mortClassRange=c("General","Safety"),
                           tier="1", tierRange=c("1", "2", "3"),
                           verbose=FALSE) {
    return(validateInputs(age=age, ageRange=ageRange,
                          sex=sex, sexRange=sexRange,
                          service=service, serviceRange=serviceRange,
                          status=status, statusRange=statusRange,
                          mortClass=mortClass, mortClassRange=mortClassRange,
                          tier=tier, tierRange=tierRange,
                          verbose=verbose));
}


## System-specific, from the KY CERS 2022 val report.
doesMemberSeparate <- function(age, sex, service, status="active", tier="1",
                               mortClass="General", verbose=FALSE) {

    if (verbose) cat("doesMemberSeparate: ");
    validateInputsKY(age=age, sex=sex, service=service, status=status, tier=tier,
                     mortClass=mortClass, verbose=verbose);

    ## If this is not currently an active employee, get out.
    if (!checkStatus(status, acceptable=c("active"))) return(status);

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

    if (verbose) cat("doesMemberRetire: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

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


doesMemberBecomeDisabled <- function(age, sex, service, status,
                                     mortClass="General", tier="A",
                                     verbose=FALSE) {

    if (verbose) cat("doesMemberDisableOrdinary: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

    ## The KY system has one table for disability probabilities, and then
    ## assumes a fixed percentage of them are accidental, with that fixed
    ## percentage being a function of the mortClass.
    ageClass <- min(floor((age-10)/10), 5);

    ## Numbers from table on page 62
    if (mortClass == "General") {
        threshold <- c(0.04, 0.06, 0.14, 0.39, 1.02)[ageClass];
    } else if (mortClass == "Safety") {
        threshold <- c(0.07, 0.12, 0.26, 0.73, 1.90)[ageClass]
    }
    
    ## Roll dice
    if (runif(1) < threshold/100) status <- "disabled/ordinary";

    ## Roll again to see if it is actually an accidental disability.  See page
    ## 63 of 2022 val report.
    if (status == "disabled/ordinary") {
        if (mortClass == "General") {
            threshold <- 2.0;
        } else if (mortClass == "Safety") {
            threshold <- 50.0;
        }

        if (runif(1) < threshold/100) status <- "disabled/accident";
    }
    
    return(status);
}

projectSalaryDelta <- function(year, age, salary, service=1, tier="A",
                               mortClass="General", verbose=verbose) {

    ## val report page 60
    generalDelta <- c(1.1030, 1.0730, 1.0630, 1.0480, 1.0455, 1.0455, 1.0430,
                      1.0430, 1.0405, 1.0405, 1.0380, 1.0380, 1.0355, 1.0355,
                      1.0355, 1.0330);
    safetyDelta <-  c(1.1905, 1.0755, 1.0555, 1.0480, 1.0455, 1.0455, 1.0455,
                      1.0455, 1.0405, 1.0405, 1.0355, 1.0355, 1.0355, 1.0355,
                      1.0355, 1.0355);

    if (mortClass == "General") {
        delta <- generalDelta[ min( service, 15 ) + 1 ];
    } else if (mortClass == "Safety") {
        delta <- safetyDelta[ min( service, 15 ) + 1 ];
    }

    return(delta);
}

doesMemberHaveSurvivor <- function(age, sex, status, survivor,
                                   tier=tier, mortClass=mortClass,
                                   verbose=verbose) {

    validateInputsKY(age=age, sex=sex, service=service, status=status, tier=tier,
                     mortClass=mortClass, verbose=verbose);

    ## If already retired with a survivor, or dead or disabled, get out.
    if (checkStatus(status, acceptable=c("retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);
    
    ## We only want to do this once. And we probably want to do better
    ## (less retro?) choices of sex and age in the specialized versions.
    if ((status == "retired") && (survivor$status == "")) {
        if (verbose) cat("Creating a survivor...");
        survivor$status <- "retired/survivor";

        if (sex == "M") {
            survivor$age <- age - round(rnorm(1, mean=3, sd=1));
        } else {
            survivor$age <- age + round(rnorm(1, mean=3, sd=1));
        }
        
        survivor$sex <- ifelse(sex=="M", "F", "M"); 
    }

    if (verbose) cat("Survivor is age:", survivor$age, "sex:", survivor$sex,
                     "status:", survivor$status, "\n");

    return(survivor);
}


## val report, page 72ff.
## Note that this function is for calculating benefits, not determining
## eligibility.
projectBasePension <- function(salaryHistory, retirementType, retireYear,
                               retireAge, retireService, retireStatus,
                               mortClass="General", tier="A",
                               verbose=FALSE) {    

    if (verbose) cat("projectBasePension: ");
    validateInputsKY(age=retireAge, service=retireService, status=retireStatus,
                     tier=tier, mortClass=mortClass, verbose=verbose);

    ## Find the average of the top five-year period. Note that this is different
    ## from the top five years, but it will do for the model, since we don't
    ## use it to generate any pathological cases.
    s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 10);
    fiveYears <- c(s, 0, 0, 0, 0) + c(0, s, 0, 0, 0) + c(0, 0, s, 0, 0) +
        c(0, 0, 0, s, 0) + c(0, 0, 0, 0, s);
    avgSalaryFive <- max(fiveYears) / 5.0;

    ## Turns out we also need the 3-year average for some calculations.
    threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
    avgSalaryThree <- max(threeYears) / 3.0;

    ## And also the final salary for some others. Wow.
    finalSalary <- tail(s,1)

    if (tier == 1) {
        if (mortClass == "General") {
            if (grepl("retired", retirementType)) {
                if (verbose) cat("Calculating regular General, 1 benefit: ")
                if ((retireStatus == "active") || (retireStatus == "separated")) {
                    basePension <- 0.022 * avgSalaryFive * retireService;
                }
            } else if (grepl("disabled", retirementType)) {
                if (verbose) cat("Calculating disability General, 1 benefit: ")
                if ((retireService == 25) || (retireService == 26)) retireService <- 27;
                if (retireService < 25) retireService <- retireService + min(65 - retireAge, retireService);

                basePension <- 0.022 * avgSalaryFive * retireService;
            }
        } else if (mortClass == "Safety") {
            if (grepl("retired", retirementType)) {
                if (verbose) cat("Calculating regular Safety, 1 benefit: ")
                if ((retireStatus == "active") || (retireStatus == "separated")) {
                    basePension <- 0.025 * avgSalaryThree * retireService;
                }
            } else if (grepl("disabled", retirementType)) {
                if (verbose) cat("Calculating disability Safety, 1 benefit: ")
                if (retireService < 20) retireService <- retireService + min(55 - retireAge, retireService);

                basePension <- 0.025 * avgSalaryThree * retireService;
            }
        }
    } else if (tier == 2) {
        if (mortClass == "General") {
            if (verbose) cat("Calculating regular General, 2 benefit: ")
            if ((retireStatus == "active") || (retireStatus == "separated")) {
                if (retireService <= 10) {
                    benefitMultiplier <- 0.011;
                } else if (retireService <= 20) {
                    benefitMultiplier <- 0.013;
                } else if (retireService <= 26) {
                    benefitMultiplier <- 0.015;
                } else {
                    benefitMultiplier <- 0.0175;
                }

                basePension <- benefitMultiplier * avgSalaryFive * retireService;

                if (retireService > 30) {
                    basePension <-
                        basePension + 0.0025 * avgSalaryFive * (retireService - 30);
                }

                if (grepl("disabled", retirementType)) {
                    if (verbose) cat("Calculating disability General, 2 benefit: ")
                    if ((retirementType == "disabled/ordinary") && (retireService < 5)) {
                        basePension <- 0;
                    } else {
                        basePension <- max(0.2 * finalSalary, basePension);
                    }
                }
            } else {
                stop("Why is this not active or separated?", retireStatus);
            }
        } else if (mortClass == "Safety") {
            if (verbose) cat("Calculating regular Safety, 2 benefit: ")
            if ((retireStatus == "active") || (retireStatus == "separated")) {
                if (retireService <= 10) {
                    benefitMultiplier <- 0.013;
                } else if (retireService <= 20) {
                    benefitMultiplier <- 0.015;
                } else if (retireService <= 25) {
                    benefitMultiplier <- 0.0225;
                } else {
                    benefitMultiplier <- 0.025;
                }

                basePension <- benefitMultiplier * avgSalaryThree * retireService;

                if (grepl("disabled", retirementType)) {
                    if (verbose) cat("Calculating disability General, 2 benefit: ")
                    if ((retirementType == "disabled/ordinary") && (retireService < 5)) {
                        basePension <- 0;
                    } else {
                        basePension <- max(0.25 * finalSalary, basePension);
                    }
                }
            } else {
                stop("Why is this not active or separated?", retireStatus);
            }
        }
    } else if (tier == 3) {
        if (verbose) cat("Calculating annuity amount tier 3: ");
          
        ## This is the weird one. First task is to estimate the size of the
        ## member's personal "account". Use this number for the actual accrual rate
        ## enjoyed by a member. (Sponsor guarantees 4%, assumes 6.25%, offers 3/4 of
        ## the difference between guaranteed and real.)
        actualTierThreeAccrual <- 1.05;
        
        acctSum <- salaryHistory %>%
            filter(year <= retireYear) %>%
            mutate(acct=premium * actualTierThreeAccrual^(retireYear - year)) %>%
            select(acct) %>% sum();

        ## We are going to hack together an approximation for the annuity,
        ## using these "annuity factors" published at
        ## https://www.kyret.ky.gov/Members/Tier-3/Pages/Benefit-Calculation.aspx
        ## Someone who retires at 65yo/$50k should have around $228k saved,
        ## which would provide an income of around $17k. We use these other
        ## factors to fudge a number for other ages. Each one starts at age 43.
        generalAnnuityFactor <- c(203.182020, 201.601850, 199.945982, 198.226985,
                                  196.446939, 194.610459, 192.722340, 190.791488,
                                  188.829604, 186.783399, 184.650636, 182.426379,
                                  180.106856, 177.687428, 175.163644, 172.542464,
                                  169.925609, 167.346525, 164.764198, 162.142797,
                                  159.451715, 156.665325, 153.762907, 150.728886,
                                  147.552258, 144.226735, 140.750536, 137.125678,
                                  133.358503, 129.458446, 125.438014, 121.312562,
                                  117.098779, 112.815417, 108.482166, 104.119047,
                                  99.746061, 95.382974);
        safetyAnnuityFactor <- c(200.709276, 199.022455, 197.256200, 195.424407,
                                 193.528832, 191.574284, 189.565335, 187.509206,
                                 185.416934, 183.239644, 180.975153, 178.618594,
                                 176.167668, 173.617409, 170.963698, 168.224667,
                                 165.545982, 162.899727, 160.244237, 157.543528,
                                 154.767311, 151.890423, 148.893196, 145.761198,
                                 142.484927, 139.059905, 135.486120, 131.767698,
                                 127.912922, 123.933232, 119.843128, 115.659498,
                                 111.400832, 107.087137, 102.739047);

        if (mortClass == "General") {
            annuityFactor <- generalAnnuityFactor[min(retireAge, 80) - 42];
            annuityFactor <- 0.075 * 153.762907 / annuityFactor;
        } else if (mortClass == "Safety") {
            annuityFactor <- generalAnnuityFactor[min(retireAge, 77) - 42];
            annuityFactor <- 0.075 * 148.893196 / annuityFactor;
        }
        ## Don't really know if this is the right way to use these, but it
        ## seems ok enough for the moment.

        ## Now use the annuityFactor to estimate a base pension amount.
        basePension <- acctSum * annuityFactor

        if (grepl("disabled", retirementType)) {
            if (verbose) cat("Calculating disability tier 3 benefit: ")
            basePension <- max(0.25 * finalSalary, basePension);
        }          
    }
    
    return(basePension);
}

## Apply the base pension to the first retirement year, then roll it
## forward according to the COLA for that tier and year.
projectPensionPayments <- function(salaryHistory, basePension, retirementType,
                                   retireYear, retireAge, retireService,
                                   retireStatus, tier="A",
                                   mortClass=mortClass, verbose=FALSE) {

    if (verbose) cat("projectPensionPayments: ");
    validateInputsKY(age=retireAge, service=retireService, status=retireStatus,
                     tier=tier, mortClass=mortClass, verbose=verbose);

    ## Assume zero COLA.
    salaryHistory <- salaryHistory %>%
        mutate(pension=ifelse((status %in% c("retired",
                                             "disabled/ordinary",
                                             "disabled/accident")),
                              basePension,
                              0));
    ## We are not taking survivors into account, but we need to.
                       ## ifelse(survivorStatus == "retired/survivor",
                       ##        basePension,
                       ##        0)));

    return(salaryHistory);
}

    

projectPension <- function(salaryHistory, tier="A", mortClass="General",
                           verbose=FALSE) {

    if (verbose) cat("projectPension: ");
    validateInputsKY(tier=tier, mortClass=mortClass, verbose=verbose);
    
    ## Find last entry before retirement.
    preRetirementStatus <- salaryHistory %>%
        filter(status %in% c("active", "separated")) %>%
        group_by(status) %>%
        dplyr::summarize(year=last(year),
                         age=last(age),
                         service=last(service));
    ## Might be multiple rows, we want the last one.
    preRetirementStatus <- preRetirementStatus[dim(preRetirementStatus)[1],];

    retireYear <- preRetirementStatus[["year"]];
    retireAge <- preRetirementStatus[["age"]];
    retireService <- preRetirementStatus[["service"]];
    retireStatus <- preRetirementStatus[["status"]];
        
    if (verbose) cat(" -> year:", retireYear, "age:", retireAge,
                     "service:", retireService, "status:", retireStatus, "\n");
    
    ## Now find first entry that is *not* active or separated.
    retirementType <- first(salaryHistory$status[!salaryHistory$status %in%
                                                 c("active", "separated")]);

    ## Is this an early retirement? "retired/early" is not a status that will
    ## ever show up in a salaryHistory, but it's useful here to set out the
    ## pension payments for tiers 1 and 2.
    if (retirementType == "retired") {
        if (tier == "1") {
            if (mortClass == "General") {
                if ((retireAge < 65) && (retireService < 27)) {
                    retirementType <- "retired/early";
                }
            } else if (mortClass == "Safety") {
                if ((retireAge < 55) && (retireService < 20)) {
                    retirementType <- "retired/early";
                }
            }
        } else if (tier == "2") {
            if (mortClass == "General") {
                if ((retireAge < 65) && ((retireAge + retireService) < 87)) {
                    retirementType <- "retired/early";
                }
            } else if (mortClass == "Safety") {
                if ((retireAge < 60) && (retireService < 25)) {
                    retirementType <- "retired/early";
                }
            }
        }
    }
    
    if (verbose) cat("Retirement type:", retirementType);
    
    basePension <- projectBasePension(salaryHistory, retirementType, retireYear,
                                      retireAge, retireService, retireStatus,
                                      tier=tier, mortClass=mortClass,
                                      verbose=verbose);

    if (verbose) cat("Base pension: ", round(basePension), "\n", sep="");
    
    salaryHistory <- projectPensionPayments(salaryHistory, basePension,
                                            retirementType, retireYear, retireAge,
                                            retireService, retireStatus, tier=tier,
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

