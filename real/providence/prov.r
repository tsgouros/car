## An attempt to estimate the CAR for the Providence system, using the
## 1996 valuation report. Using 1996 because it's the earliest I can
## find that has explicit mention of the 5% and 6% COLAs. They seem to
## have been in place earlier, but the val reports are vague about
## them and reference a court order from 1991.
##
## Note that the system-wide stuff (e.g. COLAs) is in the code itself,
## while the subject-specific stuff is generally in the argument
## lists.
##
## Pull in the CAR calculation apparatus.
source("car.r")

## System-specific, from the 1996 val report. Members belong to Class
## A or Class B, use the "tier" variable for this.

doesMemberSeparate <- function(age, service, status, tier="A",
                               mortClass="General", verbose=FALSE) {
    ## If this is not currently an active employee, get out.
    if (status != "active") return(status);

    if (tier == "A") {
        ## Page 11, "Withdrawal and deferred retirement column"
        rates <- c(.1413, .1206, .0644, .0473, .0389, .0272, .0174, .0101);
        if (age < 54) {
            threshold <- rates[floor((age-15)/5)];
        } else if (age >= 54) {
            threshold <- rates[8];
        } else {
            threshold <- 0;
        }
    } else if (tier == "B") {
        ## Page 12, "Withdrawal and deferred disability" . I think "disability"
        ## is a typo here because it doesn't make much sense as written.
        rates <- c(.0258, .0183, .0104, .0046, .0029, .0024);
        if (age < 50) {
            threshold <- rates[floor((age-15)/5)];
        } else {
            threshold <- rates[6];
        }
    }

    ## Roll the dice.
    if (runif(1) < threshold) status <- "separated";
        
    return(status);
}

doesMemberRetire <- function(age, service, status, tier="A",
                             mortClass="General", verbose=FALSE) {
    ## If already retired, disabled, or dead, get out.
    if (status %in% c("retired", "retired/survivor", "deceased",
                      "disabled/accident", "disabled/ordinary")) return(status);

    ## The service years on input refer to years that have begun, but
    ## not necessarily completed.  We want completed years.  This is
    ## related to the model's approximation that events happen on the
    ## transition from one year to the next, as opposed to the real
    ## world, where events happen whenever they feel like it.
    completedYears <- service - 1;

    ## If we're not even vested, do nothing and return.
    if (completedYears < 10) {
        if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                         "-> not even vested.\n", sep="");
        return(status);
    }
    
    if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                     ", tier: ", tier, " begin: ", status, "..",
                     sep="");

    ## Cannot find a number for this, so we estimate.
    if (status == "separated") {
        if (age < 55) {
            threshold <- 0.0;
        } else if (age < 65) {
            threshold  <- 0.1;
        } else {
            threshold <- 1.0;
        }
    } else if (status == "active") {
        if (tier == "A") {
            ## Page 11, "Retirement" column.
            if ((age >= 45) && (age < 75)) {
                rates <- c(.0671, .0925, .0859, .1138, .1229, .1741, .2500, .2500);
                threshold <- rates[floor((age-40)/5)];
            } else {
                threshold <- 0.0;
            }

            ## If you're not actually eligible, then never mind.
            if ((completedYears < 25) && (age < 55)) threshold <- 0.0;
        
        } else if (tier == "B") {
            if ((age < 65) && (age >= 40)) {
                ## Page 12, "Retirement column"
                rates <- c(.07403, .07599, .08004, .08860, .10238, .2500, .2500);
                threshold <- rates[floor(age - 35)/5];
            } else {
                threshold <- 1.0;
            }

            ## If you're not actually eligible, then never mind.
            if ((completedYears < 20) && (age < 55)) threshold <- 0.0;
        } else {
            stop(paste("what tier is this?", tier));
        }
    } else {
        stop(paste("what status is this?", status));
    }
    
    if (verbose) cat("threshold: ", threshold, "..", sep="");
    
    ## Roll the dice.
    if (runif(1) < threshold) status <- "retired";

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

doesMemberDisableAccident <- function(age, sex, service, status,
                                      mortClass="General", tier="A",
                                      verbose=FALSE) {
    ## If already retired, disabled, or dead, get out.
    if (status %in% c("retired", "retired/survivor", "deceased",
                      "disabled/accident", "disabled/ordinary")) return(status);

    if (tier=="A") {
        ## Page 11, "Disability/Accident" column
        rates <- c(.0002, .0003, .0003, .0005, .0008, .0012, .0018, .0026, .0037,
                   .0042, .0062, .0068, .0068);
        if (age < 59) {
            threshold <- rates[ floor((age-15)/5) ];
        } else if (age == 59) {
            threshold <- rates[9];
        } else if ((age >= 60) || (age < 64)) {
            threshold <- rates[10];
        } else if (age == 64) {
            threshold <- rates[11];
        } else {
            threshold <- rates[12];
        }
    } else if (tier == "B") {
        ## Page 12, "Disability/Accidental" column
        rates <- c(.0010, .0015, .0023, .0030, .0045, .0080, .0138, .0198, .0258);

        if (age < 59) {
            threshold <- rates[ floor((age - 15)/5) ];
        } else {
            threshold <- rates[9];
        }
    }
    ## Roll the dice.
    if (runif(1) < threshold) status <- "disabled/accident";

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

doesMemberDisableOrdinary <- function(age, sex, service, status,
                                      mortClass="General", tier="A",
                                      verbose=FALSE) {
    ## If already retired, disabled, or dead, get out.
    if (status %in% c("retired", "retired/survivor", "deceased",
                      "disabled/accident", "disabled/ordinary")) return(status);

    if (tier=="A") {
        ## Page 11, "Disability/Ordinary" column
        rates <- c(.0005, .0006, .0006, .0010, .0016, .0025, .0037, .0051, .0074,
                   .0083, .0125, .0136, .0136);
        if (age < 59) {
            threshold <- rates[ floor((age-15)/5) ];
        } else if (age == 59) {
            threshold <- rates[9];
        } else if ((age >= 60) || (age < 64)) {
            threshold <- rates[10];
        } else if (age == 64) {
            threshold <- rates[11];
        } else {
            threshold <- rates[12];
        }
    } else if (tier == "B") {
        ## Page 12, "Disability/Ordinary" column
        rates <- c(.0003, .0005, .0007, .0010, .0014, .0026, .0044);

        if (age < 50) {
            threshold <- rates[ floor((age - 15)/5) ];
        } else {
            threshold <- rates[7];
        }
    }
    ## Roll the dice.
    if (runif(1) < threshold) status <- "disabled/ordinary";

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

projectSalaryDelta <- function(year, age, salary, service=1, tier="A",
                               mortClass="General", verbose=FALSE) {
    ## Page 12, "Salary Increases"
    if ((year > 1995) & (year < 2000)) {
        return(1.0);
    } else {
        return(1.05);
    }
}


## For Providence system, see 1996 val report, p15ff.
projectBasePension <- function(salaryHistory, tier="A", mortClass="General",
                               verbose=FALSE) {    
    
    ## Find first entry that is *not* active or separated.
    retirementType <- first(salaryHistory$status[!salaryHistory$status %in%
                                                 c("active", "separated")]);

    if (verbose) cat("Retirement type:", retirementType);
    
    ## Find last entry before retirement.
    preRetirementStatus <- salaryHistory %>%
        filter(status %in% c("active", "separated")) %>%
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
    s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);
    threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
    avgSalary <- max(threeYears) / 3.0;

    if (grepl("retired", retirementType)) {
        if (verbose) cat("Calculating pension for regular retirement: ")
        if (status == "active") {
            if (tier == "A") {
                if ((age >= 55) || (service >=25)) {
                    basePension <- min(20, service) * (avgSalary/40) +
                        max((service - 20), 0) * (avgSalary/50);
                } else {
                    basePension <- 0;
                }
                if (verbose) cat(basePension, "\n");
            } else if (tier == "B") {
                if ((age >= 55) || (service >=20)) {
                    basePension <- min(20, service) * (avgSalary/40) +
                        max(min((service - 20), 12), 0) * (avgSalary/50);
                } else {
                    basePension <- 0;
                }
                if (verbose) cat(basePension, "\n");
            }
        } else if (status == "separated") {
            if (tier == "A") {
                if ((age >= 55) && (service >=10)) {
                    basePension <- min(20, service) * (avgSalary/40) +
                        max((service - 20), 0) * (avgSalary/50);
                } else {
                    basePension <- 0;
                }
            } else if (tier == "B") {
                if ((age >= 55) && (service >=20)) {
                    basePension <- min(20, service) * (avgSalary/40) +
                        max(min((service - 20), 12), 0) * (avgSalary/50);
                } else {
                    basePension <- 0;
                }
            }
        }
    } else if (retirementType == "disabled/accidental") {
        basePension <- 0;
        
    } else if (retirementType == "disabled/ordinary") {
        basePension <- 0;

    } else {
        ## Not a retirement. Return zero.
        basePension <- 0;
    }

    return(basePension);
}

## Apply the base pension to the first retirement year, then roll it
## forward according to the COLA for that tier and year.
projectPensionPayments <- function(salaryHistory, basePension, tier="A",
                                   mortClass="General", verbose=FALSE) {

    retireYear <- salaryHistory %>%
        filter(status %in% c("retired","disabled/accident", "disabled/ordinary")) %>%
        dplyr::summarize(retireYear=min(year)) %>% as.numeric();

    if (tier=="A") {
        if (retireYear < 2060) {
            if (verbose) cat("Tier A, retired before 1994, 3% cola.\n")
            salaryHistory <- salaryHistory %>%
                mutate(pension=ifelse((status=="retired"),
                                      basePension * (1.03)^(year - retireYear),
                                      0));
        } else {
            if (verbose) cat("Tier A, retired after 1990, no cola.\n");
            salaryHistory <- salaryHistory %>%
                mutate(pension=ifelse((status=="retired"),
                                      basePension,
                                      0));
        }

    } else if (tier == "B") {
        if (retireYear < 1990) {
            if (verbose) cat("Tier B, retired before 1990, 5% cola.\n")
            salaryHistory <- salaryHistory %>%
                mutate(pension=ifelse((status=="retired"),
                                      basePension * (1.05)^(year - retireYear),
                                      0));
        } else if (retireYear < 2060) {
            if (verbose) cat("Tier B, retired after 1990, before 1994, 6% cola.\n")
            salaryHistory <- salaryHistory %>%
                mutate(pension=ifelse((status=="retired"),
                                      basePension * (1.06)^(year - retireYear),
                                      0));
        } else {
            ## This one is a "simple" COLA. Of all the stupid ideas.
            if (verbose) cat("Tier B, retired after 1994, 3% simple cola.\n")
            salaryHistory <- salaryHistory %>%
                mutate(pension=ifelse((status=="retired"),
                                      basePension + (0.03 * min(basePension, 10000)) * (year - retireYear),
                                      0));
        }
    }
        
    return(salaryHistory);
}

    

projectPension <- function(salaryHistory, tier="A", mortClass="General",
                           verbose=FALSE) {

    basePension <- projectBasePension(salaryHistory, tier, verbose=verbose);

    if (verbose) cat("Base pension: ", round(basePension), "\n", sep="");
    
    salaryHistory <- projectPensionPayments(salaryHistory, basePension, tier,
                                            verbose=verbose);

    return(salaryHistory);
}


## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share, do not include amortization
## payments.)
projectPremiums <- function(salaryHistory, tier="A", mortClass="General",
                            verbose=FALSE) {

    if (tier == "A") {
        premiumPerPayroll <- .081;
    } else if (tier == "B") {
        premiumPerPayroll <- .0923;
    }

    if (verbose) cat("Running projectPremiums from prov.r, tier:", tier,
                     "premium % payroll:", premiumPerPayroll * 100, "%\n");
    
    return(salaryHistory %>%
           mutate(premium = salary * premiumPerPayroll))
}


############################################################################
############################################################################
## The above is all about specifying the benefits. Now we model the
## actual population and then run the model.
provModel <- function(verbose=FALSE) {

    if (verbose) cat("building model", date(), "\n");

    provModel <- memberList();
    
### Class B

###  25    23      39
###    781828 1397634
    provModel <- genEmployees(23, ageRange=c(25,29), servRange=c(1,4), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=781828/23, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(39, ageRange=c(25,29), servRange=c(5,9), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=1397634/39, sex="M",
                              members=provModel, verbose=verbose);

### 30     49     100      58
###   1691552 3603168 2065150
    provModel <- genEmployees(49, ageRange=c(30,34), servRange=c(1,4), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=1691552/49, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(49, ageRange=c(30,34), servRange=c(5,9), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=3603168/100, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(58, ageRange=c(30,34), servRange=c(10,14), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=2065150/58, sex="M",
                              members=provModel, verbose=verbose);

### 35      9      68     142      27
###    306780 2432812 5104394 1038303
    provModel <- genEmployees(9, ageRange=c(35,39), servRange=c(1,4), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=306780/9, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(68, ageRange=c(35,39), servRange=c(5,9), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=2432812/68, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(142, ageRange=c(35,39), servRange=c(10,14), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=5104394/142, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(27, ageRange=c(35,39), servRange=c(15,19), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=2065150/27, sex="M",
                              members=provModel, verbose=verbose);

### 40     4      20      61      95      23       3
###   132944  720056 2172440 3725088  957248  128919
    provModel <- genEmployees(4, ageRange=c(40,44), servRange=c(1,4), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=132944/4, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(20, ageRange=c(40,44), servRange=c(5,9), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=720056/20, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(61, ageRange=c(40,44), servRange=c(10,14), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=2172440/61, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(95, ageRange=c(40,44), servRange=c(15,19), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=3725088/95, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(23, ageRange=c(40,44), servRange=c(20,24), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=957248/23, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(3, ageRange=c(40,44), servRange=c(25,29), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=128919/3, sex="M",
                              members=provModel, verbose=verbose);

### 45     0       4      14      37      39      26       2
###        0  149859  528407 1438345 1601299 1141586   85546
    provModel <- genEmployees(4, ageRange=c(45,49), servRange=c(5,9), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=149859/4, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(14, ageRange=c(45,49), servRange=c(10,14), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=528407/14, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(37, ageRange=c(45,49), servRange=c(15,19), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=1438345/37, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(39, ageRange=c(45,49), servRange=c(20,24), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=1601299/39, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(26, ageRange=c(45,49), servRange=c(25,29), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=1141586/26, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(2, ageRange=c(45,49), servRange=c(30,34), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=85546/2, sex="M",
                              members=provModel, verbose=verbose);

### 50     0       0       2       6       4      22      18
###        0       0   70720  224604  159724  990264  774249
    provModel <- genEmployees(2, ageRange=c(50,54), servRange=c(10,14), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=70720/2, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(6, ageRange=c(50,54), servRange=c(15,19), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=224604/6, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(4, ageRange=c(50,54), servRange=c(20,24), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=159724/4, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(22, ageRange=c(50,54), servRange=c(25,29), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=990264/22, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(18, ageRange=c(50,54), servRange=c(30,34), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=774249/18, sex="M",
                              members=provModel, verbose=verbose);
    
### 55     0       0       0       0       1       1      10       4
###        0       0       0       0   25052   34650  465142  189434
    provModel <- genEmployees(1, ageRange=c(55,59), servRange=c(20,24), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=25052, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(1, ageRange=c(55,59), servRange=c(25,29), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=34650, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(10, ageRange=c(55,59), servRange=c(30,34), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=465142/10, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(4, ageRange=c(55,59), servRange=c(35,39), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=189434/4, sex="M",
                              members=provModel, verbose=verbose);

### 60     0       0       0       1       0       0       1
###        0       0       0   34650       0       0   39751
    provModel <- genEmployees(1, ageRange=c(60,65), servRange=c(15,19), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=34650, sex="M",
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(1, ageRange=c(60,65), servRange=c(30,34), tier="B",
                              class="Safety", currentYear=1989,
                              avgSalary=39751, sex="M",
                              members=provModel, verbose=verbose);

### age
### 20     19       1
###    275530   19869
    provModel <- genEmployees(19, ageRange=c(20,24), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=275530/19, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(1, ageRange=c(20,24), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=19869, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 25     75      58       3
###   1376392 1313876   63195
    provModel <- genEmployees(75, ageRange=c(25,29), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1376392/75, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(58, ageRange=c(25,29), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1313876/58, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(75, ageRange=c(25,29), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=63195/3, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 30     72     103      29
###   1558188 2614164  789943
    provModel <- genEmployees(72, ageRange=c(30,34), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1558188/72, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(103, ageRange=c(30,34), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=2614164/103, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(29, ageRange=c(30,34), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=789943/29, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 35     57     110      54      20       2
###   1125860 2721561 1640651  598454   56167
    provModel <- genEmployees(57, ageRange=c(35,39), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1125860/57, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(110, ageRange=c(35,39), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=2721561/110, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(54, ageRange=c(35,39), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1640651/54, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(20, ageRange=c(35,39), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=598454/20, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(57, ageRange=c(35,39), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=56167/2, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 40     67     112      54      42      60       7
###   1411295 2692796 1511539 1177987 1762562  254241
    provModel <- genEmployees(67, ageRange=c(40,44), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1411295/67, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(112, ageRange=c(40,44), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=2692796/112, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(54, ageRange=c(40,44), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1511539/54, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(42, ageRange=c(40,44), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1177987/42, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(60, ageRange=c(40,44), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1762562/60, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(7, ageRange=c(40,44), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=254241/7, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 45     47      96      53      37      55      59       3
###    937651 2159847 1460528 1124354 1579871 2052419   95374
    provModel <- genEmployees(47, ageRange=c(45,49), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=937651/47, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(96, ageRange=c(45,49), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=2159847/96, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(53, ageRange=c(45,49), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1460528/53, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(37, ageRange=c(45,49), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1124354/37, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(55, ageRange=c(45,49), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1579871/55, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(59, ageRange=c(45,49), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=2052419/59, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(3, ageRange=c(45,49), servRange=c(30,34), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=95374/3, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 50     33      56      49      35      40      50      13
###    748785 1469105 1323777 1040890 1272271 1615005  412891
    provModel <- genEmployees(33, ageRange=c(50,54), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=748785/33, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(56, ageRange=c(50,54), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1469105/56, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(49, ageRange=c(50,54), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1323777/49, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(35, ageRange=c(50,54), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1040890/35, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(40, ageRange=c(50,54), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1272271/40, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(50, ageRange=c(50,54), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1615005/50, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(13, ageRange=c(50,54), servRange=c(30,34), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=412891/13, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 55     24      47      38      16      24      21      12       8
###    441472 1040055 1089742  414661  648085  581532  277194  251443
    provModel <- genEmployees(24, ageRange=c(55,59), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=441472/24, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(47, ageRange=c(55,59), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1040055/47, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(38, ageRange=c(55,59), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=1089742/38, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(16, ageRange=c(55,59), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=414661/16, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(24, ageRange=c(55,59), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=648085/24, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(21, ageRange=c(55,59), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=581532/21, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(12, ageRange=c(55,59), servRange=c(30,34), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=277194/12, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(8, ageRange=c(55,59), servRange=c(35,39), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=251443/8, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 60      8      35      19      15      16      12       6       2
###    155684  727315  529204  421908  474367  329278  199060   50222
    provModel <- genEmployees(8, ageRange=c(60,62), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=155684/8, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(35, ageRange=c(60,62), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=727315/35, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(19, ageRange=c(60,62), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=5292004/19, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(15, ageRange=c(60,62), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=421908/15, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(16, ageRange=c(60,62), servRange=c(20,24), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=474367/16, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(12, ageRange=c(60,62), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=329278/12, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(6, ageRange=c(60,62), servRange=c(30,34), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=199060/6, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(2, ageRange=c(60,62), servRange=c(35,39), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=50222/2, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 63      3       9       6       2       0       5       1
###     67838  171130  185148   38507       0  131904   25222
    provModel <- genEmployees(3, ageRange=c(63,65), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=67838/3, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(9, ageRange=c(63,65), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=171130/9, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(6, ageRange=c(63,65), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=185148/6, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(2, ageRange=c(63,65), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=38507/2, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(5, ageRange=c(63,65), servRange=c(25,29), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=131904/5, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(1, ageRange=c(63,65), servRange=c(30,34), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=25222, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 66      0       3       3       1       0       0       1
###         0   53577   71741   28221       0       0   26175
### 67      0       6       1       0       1       0       ...      1
###         0  229450   25325       0   15758       0       ...  24329
### 68      0       3       2       3       0       0       0
###         0  125147   48285   77840       0       0       0
### 69      1       0       0       0       1       0       1
###     15758       0       0       0   47027       0   29222
### 70      0       2       2       0       0       0       0
###         0   57292   85852       0       0       0       0
### sum     1      14       8       4       2       0       2
###     15758  465466  231203  106061   62785       0   55397
    provModel <- genEmployees(1, ageRange=c(66,70), servRange=c(1,4), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=15758, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(14, ageRange=c(66,70), servRange=c(5,9), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=465466/14, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(8, ageRange=c(66,70), servRange=c(10,14), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=213203/8, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(4, ageRange=c(66,70), servRange=c(15,19), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=106061/4, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(2, ageRange=c(66,70), servRange=c(15,20), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=62785/2, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(2, ageRange=c(66,70), servRange=c(21,30), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=55397/2, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

### 71      0       2       0       0       0       1       0       0       1
###         0   39295       0       0       0   15713       0       0   29222
### 72      0       2       0       1       0       0       0       0       0
###         0   30817       0   25000       0       0       0       0       0
### 73      0       1       0       1       0       0       0      ...        1
###         0   12638       0   15713       0       0       0      ...    29977
### 74      0       1       0       1       0       0       0       0       0
###         0   35541       0   31602       0       0       0       0       0
### 75      0       0       1       0       0       0       0       0       0
###         0       0   24008       0       0       0       0       0       0
### 76      0       2       0       0       0       0       0       0       0
###         0   38990       0       0       0       0       0       0       0
### 78      0       0       0       0       0       0       0      ...        1
###         0       0       0       0       0       0       0      ...    33601
### 79      0       2       0       0       0       0       0       0       0
###         0   35446       0       0       0       0       0       0       0
### 80      0       2       0       0       0       0       0       0       0
###         0   58377       0       0       0       0       0       0       0
### 82      0       1       0       0       0       0       0       0       0
###         0   36749       0       0       0       0       0       0       0
### sum     0      13       1       3       0       1       0      ...        3
###         0  287853   24008   72315       0   15713       0      ...    92800
    provModel <- genEmployees(13, ageRange=c(71,80), servRange=c(2,10), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=311861/14, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(3, ageRange=c(71,80), servRange=c(11,20), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=72315/3, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);
    provModel <- genEmployees(4, ageRange=c(71,80), servRange=c(2,10), tier="A",
                              class="General", currentYear=1989,
                              avgSalary=108513/4, sex=list(M=0.58, F=0.42),
                              members=provModel, verbose=verbose);

    
    if (verbose) cat("finished building model", date(), "\n");
    
    return(provModel);
}

provModelOutput <- runModel(provModel, N=1, verbose=TRUE, reallyVerbose=FALSE,
                            audit=TRUE);

