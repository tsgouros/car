library(tidyverse,warn.conflicts=FALSE)

## This requires the newton.r and mortality.r functions to be loaded first.
## To load from remote directories, do this:
##
##   setwd("../../src/")
##   source("car.r")
##   setwd("../real/kentucky")
##
## We do not make this a library because many of the functions included here
## are meant to be overridden.
source("newton.r")
source("mortality.r")

validateInputs <- function(age=20, ageRange=c(20,120),
                           sex="M", sexRange=c("M","F"),
                           service=0, serviceRange=c(0,100),
                           status="active",
                           statusRange=c("active", "separated", "retired",
                                         "retired/survivor", "deceased",
                                         "disabled/accident", "disabled/ordinary"),
                           mortClass="General",
                           mortClassRange=c("General","Safety","Teacher"),
                           tier="1", tierRange=c("1", "2", "3"),
                           sysName="this",
                           sysClass="", sysClassRange=c(""),
                           verbose=FALSE) {


    if (verbose) cat("age:", age, ", sex:", sex, ", status:", status,
                     ", service:", service, ", mortClass:", mortClass,
                     ", tier:", tier, ", sys:", sysName, "(", sysClass, ")\n",
                     sep="");
    
    if (!is.numeric(age))
        return(paste0("Age is not numeric: ", age))
    if ((age < ageRange[1]) || (age > ageRange[2]))
        return(paste0("Bad age: ", age));

    if (!is.numeric(service))
        return(paste0("Service is not numeric: ", service))
    if ((service < serviceRange[1]) || (service > serviceRange[2]))
        return(paste0("Bad service: ", service));

    if (!(sex %in% sexRange))
        return(paste0("Bad sex: ", sex));
    
    if (!(status %in% statusRange))
        return(paste0("Bad status: ", status));
    
    if (!(mortClass %in% mortClassRange))
        return(paste0("Bad mortClass: ", mortClass));
    
    if (!(tier %in% tierRange))
        return(paste0("Bad tier: ", tier));

    if (!(sysClass %in% sysClassRange))
        return(paste0("Bad sysClass: ", sysClass));

    ## If we're here, all is good.
    return("");
}    

checkStatus <- function(status,
                        acceptable=c("active", "separated", "retired",
                                     "retired/survivor", "deceased",
                                     "disabled/accident", "disabled/ordinary")) {
    return(status %in% acceptable);
}


################### BEGIN SYSTEM SPECIFIC DEFINITIONS ####################

## Following is a set of functions that describe plan provisions and actuarial
## assumptions for the pension plan under scrutiny here.  Plans are wildly
## disparate in their benefit provisions, and not so terribly consistent in
## their actuarial assumptions, either. So we model these differences with
## functions, which create no real limit on the complexity.

## We provide default functions for each of these provisions, but they should
## each be overridden by a plan-specific version.  Note that the
## "doesMemberDie" function is over in mortality.r, largely due to the
## widespread use of the same published mortality tables.

## Most of the arguments are self-explanatory. The sysName and sysClass
## arguments may be useful for multi-employer systems, cf Arizona PSPRS,
## where there are 200+ systems and they are classified by the county in the
## assumption data.
##
## The 'service' variable should refer to *completed* years of service. That
## is, it should begin at zero.
##
## Provides an estimate of the probability of separation, given the age, class,
## and service years of the employee.
doesMemberSeparate <- function(age, sex, service, status="active", tier="1",
                               mortClass="General", sysName="this", sysClass="",
                               verbose=FALSE) {
    cat("Running default doesMemberSeparate.\n");
    if (verbose) cat("doesMemberSeparate: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);
    
    ## If this is not currently an active employee, get out.
    if (!checkStatus(status, acceptable=c("active"))) return(status);

    rates <- c(0.070, 0.045, 0.037, 0.030, 0.025,
               0.017, 0.017, 0.017, 0.017, 0.015,
               0.011, 0.007, 0.007, 0.007, 0.006,
               0.005, 0.005, 0.004, 0.004, 0.004);

    service <- min(service + 1, 20);
    if (runif(1) < rates[service]) status <- "separated";

    return(status);
}

## Provides an estimate of the probability of retirement, given the age, class,
## and service years of the employee.
doesMemberRetire <- function(age, sex, service, status="active", tier="1",
                             mortClass="General", sysName="this", sysClass="",
                             verbose=FALSE) {
    cat("Running default doesMemberRetire.\n");

    if (verbose) cat("doesMemberRetire: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

    completedYears <- service;

    if ((age >= 62) && (completedYears >= 15)) {
        if ( ((age == 62) && (runif(1) > 0.4)) ||
             (((age > 62) && (age < 70)) && (runif(1) > 0.5)) ||
             (age >= 70) ) {
            status <- "retired";
        }
    } else if (completedYears >= 20) {
            rates <- c(0.14, 0.14, 0.07, 0.07, 0.07,
                       0.22, 0.26, 0.19, 0.32, 0.30,
                       0.30, 0.30, 0.55, 0.55, 1.00);

            completedYears <- min(completedYears, 34);
            ## Roll the dice.
            if (runif(1) < rates[completedYears - 19]) status <- "retired";
    }

    return(status);
}

doesMemberBecomeDisabled <- function(age, sex, service, status, tier="1", 
                                     mortClass="General", sysName="this",
                                     sysClass="", verbose=FALSE) {
    cat("Running default doesMemberBecomeDisabled.\n");

    if (verbose) cat("doesMemberBeceomDisabled: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

    status <- doesMemberDisableOrdinary(age, sex, service, status, tier=tier,
                                        mortClass=mortClass, sysName=sysName,
                                        sysClass=sysClass, verbose=verbose);

    status <- doesMemberDisableAccident(age, sex, service, status, tier=tier,
                                        mortClass=mortClass, sysName=sysName,
                                        sysClass=sysClass, verbose=verbose);

    return(status);
}
    
## We distinguish between a member becoming disabled ("ordinary") and becoming
## disabled on the job ("accident").  The default versions are nops, so you can
## decide whether to specialize the accident, ordinary, or combined disability
## probability function.
doesMemberDisableOrdinary <- function(age, sex, service, status, tier="1",
                                      mortClass="General", sysName="this",
                                      sysClass="", verbose=FALSE) {
    cat("Running default doesMemberDisableOrdinary.\n");

    if (verbose) cat("doesMemberDisableOrdinary: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);

    return(status);
}

## See "Ordinary" above.
doesMemberDisableAccident <- function(age, sex, service, status, tier="1",
                                      mortClass="General", sysName="this",
                                      sysClass="", verbose=FALSE) {
    cat("Running default doesMemberDisableAccident.\n");

    if (verbose) cat("doesMemberDisableAccident: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);

    return(status);
}

doesMemberHaveSurvivor <- function(age, sex, status, service, survivor,
                                   tier="1", mortClass="General",
                                   sysName="this", sysClass="", 
                                   verbose=verbose) {

    cat("Running default doesMemberHaveSurvivor\n");

    if (verbose) cat("doesMemberHaveSurvivor: ");
    validateInputs(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);
    
    ## We only want to do this once. And we probably want to do better
    ## (less retro?) choices of sex and age in the specialized versions.
    if ((status == "retired") && (survivor$status == "")) {
        if (verbose) cat("Creating a survivor...");
        survivor$status <- "retired/survivor";
        survivor$age <- age - 5;
        survivor$sex <- ifelse(sex=="M", "F", "M"); 
    }

    if (verbose) cat("Survivor is age:", survivor$age, "sex:", survivor$sex,
                     "status:", survivor$status, "\n");

    return(survivor);
}

## Defines the function 'doesMemberDie' using the pubs 2010 mortality
## tables in the mortalityTables subdirectory.
source("mortality.r")

## The assumed salary increment, from the table of merit increases in
## each valuation report.  Tier refers to any kind of subdivision
## among the members.
projectSalaryDelta <- function(year, age, salary, service=1, tier="1", 
                               mortClass="General", sysName="this", sysClass="",
                               verbose=FALSE) {
    cat("Running default projectSalaryDelta.\n");

    if (age < 25) {
        delta <- 1.075;
    } else if ((age >= 25) && (age < 30)) {
        delta <- 1.0735;
    } else if ((age >= 30) && (age < 35)) {
        delta <- 1.0674;
    } else if ((age >= 35) && (age < 40)) {
        delta <- 1.0556;
    } else if ((age >= 40) && (age < 45)) {
        delta <- 1.0446;
    } else if ((age >= 45) && (age < 50)) {
        delta <- 1.0374;
    } else if (age >= 50) {
        delta <- 1.035;
    }

    if (verbose) cat("  year:", year, "age:", age, "service:", service,
                     "tier:", tier, "mortClass:", mortClass,
                     "\n  delta:", delta, "\n");
    
    return(delta);
}

projectStartingPension <- function(year, birthYear, retireYear, salary, 
                                   tier="1", mortClass="General",
                                   sysName="this", sysClass="",
                                   verbose=FALSE) {
    cat("Running default projectStartingPension.\n");

    if (verbose)
        cat("  as of year", year, 
            "\n  projecting pension for member born in", year,
            "\n  and retiring in", retireYear,
            "\n  tier:", tier, "mortClass:", mortClass);

    startingPension <- max(salary) * 0.55;

    if (verbose) cat("\n  result=", startingPension, "\n");
    
    return(startingPension);
}

projectPension <- function(salaryHistory, tier="1", mortClass="General",
                           cola=1.02, sysName="this", sysClass="",
                           verbose=FALSE) {
    cat("Running default projectPension.\n");

    ## If this person never retired, send them away without a pension.
    if (!("retired" %in% salaryHistory$status))
        return(salaryHistory %>% mutate(pension = 0));

    startingPension <-
        projectStartingPension(
            year=min(salaryHistory$year[salaryHistory$status=="retired"]),
            birthYear=min(salaryHistory$year) - min(salaryHistory$age),
            retireYear=min(salaryHistory$year[salaryHistory$status=="retired"]),
            salary=salaryHistory$salary,
            tier=tier, mortClass=mortClass, sysName=sysName, sysClass=sysClass,
            verbose=verbose);

    retireYear <- salaryHistory %>%
        filter(status=="retired") %>%
        summarize(retireYear=min(year)) %>%
        as.numeric()

    salaryHistory <- salaryHistory %>%
        mutate(pension={if ("survivorStatus" %in% names(.))
                            ifelse(status == "retired",
                                   startingPension * cola^(year - retireYear),
                            ifelse((survivorStatus == "retired/survivor"),
                                   0.67 * startingPension * cola^(year - retireYear),
                                   0))
                        else
                            ifelse(status == "retired",
                                   startingPension * cola^(year - retireYear),
                                   0)})

    return(salaryHistory);
}

## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share.)
projectPremiums <- function(salaryHistory, tier="A", mortClass="General",
                            sysName="this", sysClass="",
                            verbose=FALSE) {
    cat("Running default projectPremiums.\n");

    return(salaryHistory %>%
           mutate(premium = salary * .25))
}

################### END SYSTEM SPECIFIC DEFINITIONS #############

## For a given year, uses an age, years of service, and salary
## history, to project a typical career forward to separation or
## retirement, and backward to the initial hire.  Returns a tibble
## with salary figures for each working year, and a status column for
## active, separated, or retired.
##
## Can also input a salaryHistory tibble, with year, age,
## service, (annual) salary, and status columns.  This is assumed to
## be a partial record, and the function will use the assumptions and
## mortality tables to fill out the career of this person.
projectCareer <- function(year=0, age=0, service=0, salary=0,
                          salaryHistory=NA, sex="M",
                          mortClass="General", tier="1",
                          sysName="this", sysClass="",
                          verbose=FALSE) {

    ## Test if the salaryHistory data frame is empty.
    if (is.null(dim(salaryHistory))) {

        ## If so we just have a single year to project from.
        career <- projectCareerFromOneYear(year, age, service, salary,
                                           sex=sex, mortClass=mortClass,
                                           tier=tier, sysName=sysName,
                                           sysClass=sysClass, verbose=verbose);
    } else {

        ## We have a few years to project from.
        career <- projectCareerFromRecord(salaryHistory, sex=sex,
                                          mortClass=mortClass, tier=tier,
                                          sysName=sysName, sysClass=sysClass,
                                          verbose=verbose)
    }

    return(career);
}

## Given data from an individual year, use the salary increase
## assumptions to work backward to the year of hire.  We assume you
## are starting from an active year.
simulateCareerBackward <- function(year, age, service, salary,
                                   sex="M", status="active",
                                   mortClass="General",
                                   tier="1", sysName="this", sysClass="",
                                   verbose=FALSE) {

    if (verbose) cat("\nIn", year, "--simulating career backward for--\n",
                     "age:", age, "service:", service, "salary:", salary,
                     "status:", status, "mortClass:", mortClass, "tier:", tier,
                     "\n");

    salaries <- c(salary);
    ages <- c(age);
    services <- c(service);
    statuses <- c("active");
    fromData <- c(FALSE);
    years <- c(year);

    ## March backward to the year of initial hire. We only want to
    ## generate records for previous years, so if the input has
    ## service=0, there is nothing to do here.
    if (service > 0) {
        for (iyear in seq(from=year - 1, to=year - service)) {
            if (verbose) cat("calculating for year: ", iyear, ", age: ",
                             age - (year - iyear), ", service: ",
                             service - (year - iyear), "\n", sep=""); 
            ages <- c(ages, age - (year - iyear));
            services <- c(services, service - (year - iyear));
            salaries <-
                c(salaries,
                  tail(salaries, 1)/projectSalaryDelta(iyear,
                                                       age - (year - iyear),
                                                       salary,
                                                       service=service,
                                                       tier=tier,
                                                       mortClass=mortClass,
                                                       sysName=sysName,
                                                       sysClass=sysClass,
                                                       verbose=verbose));
            statuses <- c(statuses, "active");
            years <- c(years, iyear);
            fromData <- c(fromData, FALSE);
        }
    }

    ## That first year was probably not a complete year.  Roll some
    ## dice and pick a random fraction of the year.
    salaries[length(salaries)] <- runif(1) * salaries[length(salaries)];

    ## Reverse the data so the years are in forward order.  Leave off
    ## the last one because that's just the original (input) year.
    ord <- head(order(years), -1);

    return(tibble(year =    years[ord],
                  age =     ages[ord],
                  service = services[ord],
                  salary =  salaries[ord],
                  fromData= fromData[ord],
                  status =  statuses[ord]));
}

simulateCareerForward <- function(year, age, service, salary,
                                  sex="M", status="active",
                                  mortClass="General", tier="1",
                                  sysName="this", sysClass="",
                                  verbose=FALSE) {

    if (verbose) cat("\nIn", year, "--simulating career forward for--\n",
                     "age:", age, "service:", service, "salary:", salary,
                     "status:", status, "mortClass:", mortClass, "tier:", tier,
                     "\n");

    salaries <- c(salary);
    ages <- c(age);
    services <- c(service);
    statuses <- c(as.character(status));

    ## We keep track of the current status of a potential survivor with the
    ## 'survivor' list. We accumulate that potential survivor's age and status,
    ## in case it later seems appropriate to append that to the salaryHistory.
    survivor <- list(status="", age=0, sex="");
    survivorStatuses <- c();
    survivorAges <- c();
    
    fromData <- c(TRUE);  ## The first year is from data, the rest are sims.
    years <- c(year);

    ## Now march forward through a simulated career.  Stop when you
    ## hit "deceased."
    currentStatus <- status;
    currentService <- service + 1;
   
    iyear <- year + 1;
    ## The loop is supposed to terminate when retiree and survivor are
    ## deceased. This limit is here just in case.
    while(iyear < (year + (150 - age))) {

        testAge <- age - (year - iyear);

        if (verbose) cat (">>>", iyear, ": At age: ", testAge,
                          ", service: ", currentService,
                          ", start as: ", currentStatus, "\n", sep="");

        ## Test for transitions.

        currentStatus <-
            doesMemberDie(testAge, sex, currentStatus,
                          mortClass=mortClass, verbose=verbose);

        currentStatus <-
            doesMemberBecomeDisabled(testAge, sex, currentService,
                                     currentStatus, tier=tier,
                                     mortClass=mortClass, sysName=sysName,
                                     sysClass=sysClass, verbose=verbose);

        currentStatus <-
            doesMemberSeparate(testAge, sex, currentService, currentStatus,
                               tier=tier, mortClass=mortClass, sysName=sysName,
                               sysClass=sysClass, verbose=verbose);

        currentStatus <-
            doesMemberRetire(testAge, sex, currentService, currentStatus,
                             tier=tier, mortClass=mortClass, sysName=sysName,
                             sysClass=sysClass, verbose=verbose);

        ## Check for a survivor? Note that we (might) create a survivor here,
        ## but they won't be relevant to the pension amount in the salary
        ## history until the currentStatus is "deceased", below.
        if (currentStatus == "retired")
            survivor <-
                doesMemberHaveSurvivor(testAge, sex, currentStatus, currentService,
                                       survivor,
                                       tier=tier, mortClass=mortClass,
                                       sysName=sysName, sysClass=sysClass,
                                       verbose=verbose);

        ## Is there a survivor receiving benefits? Start accumulating his or
        ## her data, and check to see if he or she dies.
        if (survivor$status %in% c("retired/survivor", "deceased")) {
            survivor$status <- doesMemberDie(survivor$age, survivor$sex,
                                             survivor$status, verbose=verbose);
            survivorAges <- c(survivorAges, survivor$age);
            survivorStatuses <- c(survivorStatuses, survivor$status);
        }

        if (verbose) cat ("<<< end as:", currentStatus, "\n", sep="");

        salaries <-
            c(salaries,
              ifelse(currentStatus == "active",
                     tail(salaries, 1) * projectSalaryDelta(iyear,
                                                            age-(year-iyear),
                                                            salary,
                                                            service=service,
                                                            tier=tier,
                                                            sysName=sysName,
                                                            sysClass=sysClass,
                                                            verbose=verbose),
                     0));
        ages <- c(ages, testAge);
        services <- c(services, currentService);
        statuses <- c(statuses, currentStatus);
        years <- c(years, iyear);
        fromData <- c(fromData, FALSE);

        ## If the member is dead and there is no survivor, break.
        if ((currentStatus == "deceased") &&
            (survivor$status != "retired/survivor")) break;

        ## Add a service year if still active.
        if (currentStatus == "active") currentService <- currentService + 1;

        ## If we're carrying a survivor along, age him or her one year.
        survivor$age <- survivor$age + 1;

        iyear <- iyear + 1;
    }

    if (length(survivorStatuses) > 0) {
        output <- tibble(year=years,
                         salary=salaries,
                         age=ages,
                         service=services,
                         fromData=fromData,
                         status=statuses,
                         survivorAge=c(rep(0, length(ages) - length(survivorAges)),
                                       survivorAges),
                         survivorSex=rep(survivor$sex, length(ages)),
                         survivorStatus=c(rep(0, length(ages) - length(survivorAges)),
                                          survivorStatuses));
    } else {
        output <- tibble(year=years,
                         salary=salaries,
                         age=ages,
                         service=services,
                         fromData=fromData,
                         status=statuses,
                         survivorAge=c(rep(0, length(ages))),
                         survivorSex=rep("", length(ages)),
                         survivorStatus=c(rep(0, length(ages))))
    }
    
    return(output);
}

## Given a few years of salary, project the rest of a member's career
## and life.  The salaryHistory arg is a tibble with year, salary,
## age, service, and status columns.
projectCareerFromRecord <- function(salaryHistory, sex="M",
                                    mortClass="General", tier="1",
                                    sysName="this", sysClass="", 
                                    verbose=FALSE) {

    backward <- simulateCareerBackward(head(salaryHistory$year, 1),
                                       head(salaryHistory$age, 1),
                                       head(salaryHistory$service, 1),
                                       head(salaryHistory$salary, 1),
                                       sex=sex,
                                       status=head(as.character(salaryHistory$status), 1),
                                       mortClass=mortClass, tier=tier,
                                       sysName=sysName, sysClass=sysClass,
                                       verbose=verbose);

    forward <- simulateCareerForward(tail(salaryHistory$year, 1),
                                     tail(salaryHistory$age, 1),
                                     tail(salaryHistory$service, 1),
                                     tail(salaryHistory$salary, 1),
                                     sex=sex,
                                     status=tail(as.character(salaryHistory$status), 1),
                                     mortClass=mortClass, tier=tier,
                                     sysName=sysName, sysClass=sysClass,
                                     verbose=verbose);

    totalNYears <- length(backward$year) + length(salaryHistory$year) +
        length(forward$year) -1;
    totalYears  <- c(backward$year, forward$year);

    return(tibble(year=c(backward$year,
                         salaryHistory$year,
                         tail(forward$year, -1)),
                  age=c(backward$age,
                        salaryHistory$age,
                        tail(forward$age, -1)),
                  sex=rep(sex, totalNYears),
                  mortClass=rep(mortClass, totalNYears),
                  tier=rep(tier, totalNYears),
                  service=c(backward$service,
                            salaryHistory$service,
                            tail(forward$service, -1)),
                  hireYear=rep(min(totalYears), totalNYears),
                  salary=c(backward$salary,
                           salaryHistory$salary,
                           tail(forward$salary, -1)),
                  fromData=c(backward$fromData,
                             rep(TRUE, length(salaryHistory$salary)),
                             tail(forward$fromData, -1)),
                  premium = c(rep(0, length(backward$salary)),
                              salaryHistory$premium,
                              rep(0, length(forward$salary) - 1)),
                  status=factor(c(backward$status,
                                  as.character(salaryHistory$status),
                                  tail(forward$status, -1))),
                  survivorAge=c(rep(0, length(backward$status)),
                                forward$survivorAge),
                  survivorSex=c(rep(0, length(backward$status)),
                                forward$survivorSex),
                  survivorStatus=c(rep(0, length(backward$status)),
                                   forward$survivorStatus)));

}


## Given a single year's record, project what a career might look
## like.  We assume that the status is 'active' for the given year.
projectCareerFromOneYear <- function(year, age, service, salary, sex="M",
                                     mortClass="General", tier="1",
                                     sysName="this", sysClass="", 
                                     verbose=FALSE) {

    backward <- simulateCareerBackward(year, age, service, salary,
                                       sex=sex, status="active",
                                       mortClass=mortClass, tier=tier,
                                       sysName=sysName, sysClass=sysClass,
                                       verbose=verbose)

    forward <- simulateCareerForward(year, age, service, salary,
                                     sex=sex, status="active",
                                     mortClass=mortClass, tier=tier,
                                     sysName=sysName, sysClass=sysClass,
                                     verbose=verbose)

    totalYears <- c(backward$year, forward$year);
    totalNYears <- length(totalYears);

    return(tibble(year=c(backward$year, forward$year),
                  age=c(backward$age, forward$age),
                  sex=rep(sex, totalNYears),
                  mortClass=rep(mortClass, totalNYears),
                  tier=rep(tier, totalNYears),
                  service=c(backward$service, forward$service),
                  hireYear=rep(min(totalYears), totalNYears),
                  salary=c(backward$salary, forward$salary),
                  fromData=c(backward$fromData, forward$fromData),
                  premium=rep(0, length(backward$salary) +
                                 length(forward$salary)),
                  status=factor(c(backward$status, forward$status)),
                  survivorAge=c(rep(0, length(backward$status)),
                                forward$survivorAge),
                  survivorSex=c(rep(0, length(backward$status)),
                                forward$survivorSex),
                  survivorStatus=c(rep(0, length(backward$status)),
                                   forward$survivorStatus)));
}

## Here's an object for a member, initialized for some specific year.
## The inputs are ages and years of service because that's what is
## published in the pension report tables.  The mortClass arg
## references the mortality tables (General, Safety, Teacher) and the
## tier argument is a string that can be used in whatever way is
## appropriate to reflect different classes of retirement benefits and
## salaries among plan members.
##
## The point of this function is to use the data given for some specific year
## and individual, and generate a cash flow for that individual's career and
## retirement. (This is the salaryHistory data frame.)
##
## The 'tier' argument can be either a string indicating the tier, or
## a function that looks like tier(year,age,hireYear,service).
member <- function(age=0, service=0, salary=0,
                   id="none", salaryHistory=NA,
                   currentYear=2018, birthYear=0,
                   hireYear=0, sepYear=0, retireYear=0,
                   sex="M", mortClass="General", tier="1",
                   status="active", note="", cola=1.02,
                   sysName="this", sysClass="", 
                   verbose=FALSE) {

    if (verbose) {
        if (is.function(tier)) { tierVal <- "TBD";} else { tierVal <- tier;}
        cat("Creating a member with birthYear:", birthYear,
            ", age: ", age, ", sex: ", sex, ", mortClass: ", mortClass,
            "\nhireYear: ", hireYear, ", sepYear: ", sepYear,
            ", retireYear: ", retireYear, ", tier: ", tierVal, "\n", sep="");
    }
    ## The possible status codes as of this telling are: active,
    ##   separated, retired, retired/survivor, deceased,
    ##   disabled/accident, and disabled/ordinary.

    ## Set up the facts of this member's life.
    if (is.null(dim(salaryHistory))) {
        ## If all we have is the single year's information, work with
        ## that.

        if ((birthYear == 0) && (age != 0)) {
            birthYear <- currentYear - age;
        } else {
            age <- currentYear - birthYear ;
        }
        if (birthYear == currentYear)
            stop("Must specify an age or a birth year.\n");

        if (hireYear == 0) {
            hireYear <- currentYear - service;
        } else {
            service <- currentYear - hireYear;
        }

        ## Manage the selection of a tier.
        if (is.function(tier)) {
            tierVal <-
                do.call(tier, alist(year=currentYear, age=age,
                                    hireYear=hireYear, service=service));
        } else {
            tierVal <- tier;
        }
        
        ## Generate an entire career's worth of salary history from
        ## the single-year snapshot. This also outlines the retirement
        ## years, though the pension itself is determined later.
        salaryHistory <- projectCareer(year=currentYear, age=age,
                                       service=service, salary=salary,
                                       sex=sex, mortClass=mortClass,
                                       tier=tierVal, sysName=sysName,
                                       sysClass=sysClass, verbose=verbose);
    } else {
        ## If we're here, we already have some fraction of a member's
        ## salary history to work with.
        currentYear <- head(salaryHistory$year, 1);
        age <- head(salaryHistory$age, 1);
        service <- head(salaryHistory$service, 1);

        birthYear <- currentYear - age;
        hireYear <- currentYear - service;

        ## Generate the rest of a career's worth of salary history
        ## from the history we've been given.
        salaryHistory <- projectCareer(salaryHistory=salaryHistory, age=age,
                                       service=service, sex=sex,
                                       mortClass=mortClass, tier=tierVal,
                                       sysName=sysName, sysClass=sysClass,
                                       verbose=verbose);
    }

    ## Add the premiums paid into the system.
    salaryHistory <- projectPremiums(salaryHistory, tier=tierVal,
                                     mortClass=mortClass,
                                     sysName=sysName, sysClass=sysClass,
                                     verbose=verbose);

    ## If this member gets to retire, estimate pension.
    if ("retired" %in% salaryHistory$status) {
        salaryHistory <- projectPension(salaryHistory, tier=tierVal,
                                        mortClass=mortClass,
                                        sysName=sysName, sysClass=sysClass,
                                        cola=cola, verbose=verbose);
        retireYear <- as.numeric(salaryHistory %>%
            filter(status=="retired") %>% summarize(retireYear=min(year)));
    } else {
        retireYear <- NA;
    }

    if ("separated" %in% salaryHistory$status) {
        sepYear <- as.numeric(salaryHistory %>%
            filter(status=="separated") %>% summarize(sepYear=min(year)));
    } else {
        sepYear <- NA;
    }

    ## Estimate CAR for this employee.
    if ("retired" %in% salaryHistory$status) {
        car <- findRate(salaryHistory %>% mutate(netFlow = premium - pension),
                        flowName="netFlow", maxIter=200, verbose=verbose);
    } else {
        car <- NA;
    }

    ## If no ID was provided, generate a random six-hex-digit id number.
    if (id == "none") {
        id <- format(as.hexmode(round(runif(1) * 16777216)),width=6);
    }

    ## Return everything in a list.
    out <- list(id=id,
                birthYear=birthYear,
                hireYear=hireYear,
                sepYear=sepYear,
                retireYear=retireYear,
                sex=sex,
                mortClass=mortClass,
                tier=tierVal,
                sysName=sysName,
                sysClass=sysClass,
                car=car,
                note=note,
                salaryHistory=salaryHistory);
    attr(out, "class") <- "member";

    return(out);
}


format.member <- function(m, ...) {
    out <- paste0("birthYear: ", m$birthYear,
                  ", deathYear: ", max(m$salaryHistory$year),
                  " sex: ", m$sex,
                  "\n     hireYear: ", m$hireYear,
                  ", sepYear: ", m$sepYear,
                  ", retireYear: ", m$retireYear,
                  "\n     mortality class: ", m$mortClass,
                  ", tier: ", m$tier,
                  ", sys: ", m$sysName, "(", m$sysClass, ")");

    ## The last row of the salary history is always zero, and not so
    ## interesting.
    career <- m$salaryHistory %>%
        group_by(status) %>%
        summarize(startYear=first(year), startSalary=first(salary),
                  endYear=last(year), endSalary=last(salary)) %>%
        filter(status == "active");

    out <- paste0(out, "\n",
                  "     salaryHistory: (", career$startYear[1], ", ",
                  format(career$startSalary[1], digits=5, big.mark=","), ") ",
                  " -> (", career$endYear[1], ", ",
                  format(career$endSalary[1], digits=5, big.mark=","), ")");

    if (!is.na(m$retireYear)) {
        retirement <- m$salaryHistory %>%
            group_by(status) %>%
            summarize(startYear=first(year), startPension=first(pension),
                      endYear=last(year), endPension=last(pension)) %>%
            filter(status == "retired");
        out <- paste0(out, "\n",
                  "     pension:       (", retirement$startYear[1], ", ",
                  format(retirement$startPension[1], digits=5, big.mark=","), ") ",
                  " -> (", retirement$endYear[1], ", ",
                  format(retirement$endPension[1], digits=5, big.mark=","), ")");
    }

    if (m$note != "") {
        out <- paste0(out, "\n", "     note: ", m$note);
    }

    out <- paste0(out, "\n",
                  "     car: ", format(m$car, digits=4));

    return(out);
}

print.member <- function(m, ...) {
    cat("id: ", m$id, ", ", format(m), "\n", sep="");
}

## Generate a latex output for making a detailed report for a member. Useful for
## doing QC on the model members.
latex.member <- function(m, ...) {
    out <- paste0("\\begin{minipage}[t]{0.49\\linewidth}",
                  "\\begin{tabular}{llll} \\\\\n",
                  "id: & ", m$id, " & sex: & ", m$sex, "\\\\\n",
                  "birthYear: & ", m$birthYear, 
                  " & deathYear: & ", max(m$salaryHistory$year), "\\\\\n",
                  "hireYear: & ", m$hireYear,
                  " & sepYear: & ", m$sepYear, "\\\\\n",
                  "retireYear: & ", m$retireYear, "& & \\\\\n",
                  "mortality class: & ", m$mortClass,
                  " & tier: & ", m$tier, "\\\\\n",
                  "car: & ",
                  ifelse(is.na(m$car), "NA",
                         paste0(format((m$car-1)*100, digits=4),"\\%")),
                  " & & \\\\\n",
                  "\\end{tabular}\n\n");

    
    if (m$note != "") {
        out <- paste0(out, "\n", "     note: ", m$note);
    }

    ## Did this person ever get a pension?
    getsPension <- FALSE;
    if ("pension" %in% names(m$salaryHistory)) getsPension <- TRUE;
    
    out <- paste0(out, "{\\fontsize{7pt}{8pt}\\selectfont{",
                  ifelse(getsPension,
                         "\\begin{tabular}{rrrrrrc} \\\\",
                         "\\begin{tabular}{rrrrrc} \\\\"),
                  "\\textbf{year}&\\textbf{age}&\\textbf{service}&",
                  "\\textbf{salary}&\\textbf{premium}&",
                  ifelse(getsPension, "\\textbf{pension}&", ""),
                  "\\textbf{status} \\\\ \\hline\n");
    
    for (i in 1:length(m$salaryHistory$salary)) {
                                                    
        out <- paste0(out,
                      m$salaryHistory$year[i], " & ",
                      m$salaryHistory$age[i], " & ",
                      m$salaryHistory$service[i], " & ")
        
        if (m$salaryHistory$fromData[i]) {
            out <- paste0(out,
                          "\\textbf{",
                          format(round(m$salaryHistory$salary[i]),
                                 digits=15, big.mark=","), "} & ");
        } else {
            out <- paste0(out,
                          format(round(m$salaryHistory$salary[i]),
                                 digits=15, big.mark=","), " & ");
        }

        out <- paste0(out, 
                      format(round(m$salaryHistory$premium[i]),
                             digits=15, big.mark=","), " & ",
                      ifelse(getsPension,
                             paste0(format(round(m$salaryHistory$pension[i]),
                                           digits=15, big.mark=","), " & "), ""),
                      m$salaryHistory$status[i], " \\\\\n ");
    }
    out <-
        paste0(out,
               ifelse(getsPension,
                      "\\multicolumn{7}{l}{(Bold font implies from data.)}\\\\\n",
                      "\\multicolumn{6}{l}{(Bold font implies from data.)}\\\\\n"),
                  "\\end{tabular}}}\n",
                  "\\end{minipage}\n");
                      
    return(out);
}

## Defines a class of 'memberList' for convenience.
memberList <- function(members=c()) {
    out <- list();

    if (length(members) > 0) {
        for (m in members) {
            out[[m$id]] <- m;
        }
    }

    attr(out, "class") <- "memberList";
    return(out);
}

## We want a second 'print' for memberLists that presents a summary. Tried
## 'summary.memberList' but summary sort of does something different, and packs
## its output into a vector. I just want a quick summary of a huge list. So we
## define a generic function for these objects.
synopsize <- function(object, ...) UseMethod("synopsize");
synopsize.default <- function(object, ...) print(object)

synopsize.memberList <- function(ml, ...) {

    sumList <- sapply(ml, function(x) { c(x$birthYear, x$hireYear, x$retireYear,
                                          last(x$salaryHistory$year), x$sepYear,
                                          x$sex, x$tier, x$mortClass)},
                      simplify="array") %>%
        t();

    sumTbl <- tibble(members=rownames(sumList),
                     birthYear=as.numeric(sumList[,1]),
                     hireYear=as.numeric(sumList[,2]),
                     retireYear=as.numeric(sumList[,3]),
                     deathYear=as.numeric(sumList[,4]),
                     sepYear=as.numeric(sumList[,5]),
                     sex=as.factor(sumList[,6]),
                     tier=as.factor(sumList[,7]),
                     mortClass=as.factor(sumList[,8]))

    summ <- sumTbl %>% dplyr::summarize(minBirthYear=min(birthYear, na.rm=TRUE),
                                        maxBirthYear=max(birthYear, na.rm=TRUE),
                                        minHireYear=min(hireYear, na.rm=TRUE),
                                        maxHireYear=max(hireYear, na.rm=TRUE),
                                        minRetireYear=min(retireYear, na.rm=TRUE),
                                        maxRetireYear=max(retireYear, na.rm=TRUE),
                                        minDeathYear=min(deathYear, na.rm=TRUE),
                                        maxDeathYear=max(deathYear, na.rm=TRUE))
    
    out <- list(birthYears=list(min=summ$minBirthYear, max=summ$maxBirthYear),
                hireYears=list(min=summ$minHireYear, max=summ$maxHireYear),
                retireYears=list(min=summ$minRetireYear, max=summ$maxRetireYear),
                deathYears=list(min=summ$minDeathYear, max=summ$maxDeathYear),
                sexes=summary(sumTbl$sex),
                tiers=summary(sumTbl$tier),
                mortClasses=summary(sumTbl$mortClass));

    cat(length(ml), " members\n",
        "  birthYears: ", out$birthYears$min, "--", out$birthYears$max,
        ", hireYears: ", out$hireYears$min, "--", out$hireYears$max, "\n",
        "  retireYears: ", out$retireYears$min, "--", out$retireYears$max,
        ", deathYears: ", out$deathYears$min, "--", out$deathYears$max,
        "\n  Sexes: ", sep="");
    for (i in 1:length(out$sexes))
        cat(names(out$sexes)[i], ": ", out$sexes[i],
            " (", format(100*out$sexes[i]/sum(out$sexes), digits=3),
            "%) ", sep="");
    cat("\n  Tiers: ");
    for (i in 1:length(out$tiers)) cat(names(out$tiers)[i], ": ", out$tiers[i],
            " (", format(100*out$tiers[i]/sum(out$tiers), digits=3),
            "%) ", sep="");
    cat("\n  Mortality Classes: ");
    for (i in 1:length(out$mortClasses)) cat(names(out$mortClasses)[i], ": ",
            " (", format(100*out$mortClasses[i]/sum(out$mortClasses), digits=3),
            "%) ", sep="");
    cat("\n");
}

format.memberList <- function(ml, ...) {
    out <- "";
    for (member in ml) {
        out <- paste0(out, "[[", member$id, "]]\n     ",
                      format(member), "\n");
    }
    return(substr(out, 1, nchar(out) - 1));
}

print.memberList <- function(ml, ...) {
    cat(format(ml), "\n");
}

latex.memberList <- function(ml, ...) {
    out <- paste0("\\documentclass[10pt]{article}\n",
                  "\\usepackage[margin=0.5in]{geometry}\n",
                  "\\usepackage{mathptmx}\n",
                  "\\begin{document}\n\n")

    i <- 1;
    for (member in ml) {
        out <- paste0(out, latex.member(member));
        if ((i %% 2) == 0) out <- paste0(out, "\n");
        i <- i+1;
    }

    out <- paste0(out, "\\end{document}")
    return(out);
}

# Then a 'snapshot' function to create an employee matrix for a given
# year and from a series of those, we can create the 'P' matrix above.


## Generate N new active employees with the given ranges, and append
## them to the input list of members.
genEmployees <- function (N=1, ageRange=c(20,25), ageLimits=c(20,75),
                          servRange=c(0,5), avgSalary=75000, sdSalary=5000,
                          members=memberList(),
                          sex="M", tier="1", currentYear=2022,
                          mortClass="General", status="active",
                          sysName="this", sysClass="", 
                          cola=1.02, verbose=FALSE) {

    if (N < 1) return(members);

    if (verbose) {
        if (is.function(tier)) { tierVal <- "TBD";} else { tierVal <- tier;}
        cat("Creating", N, "members in", currentYear, "\n",
            "avgSalary:", avgSalary, "age range:", ageRange[1], "-",
            ageRange[2], "service:", servRange[1], "-", servRange[2],
            "tier:", tierVal, "class:", mortClass, "status:", status, "\n");
    }
    
    ## Choose some ages, make sure they are within bounds.
    ages <- sapply(round(runif(N)*(ageRange[2] - ageRange[1])) + ageRange[1],
                   function(x) { max( min( x, ageLimits[2]), ageLimits[1]) });

    ## Set an upper bound on service here so we don't have 26 year olds with 9
    ## years of service.
    servs <- pmin(round(runif(N)*(servRange[2] - servRange[1])) + servRange[1],
                  ages - ageLimits[1]);

    salaries <- rnorm(N, mean=avgSalary, sd=sdSalary);

    ## The sex arg can be a list like (M=0.4, F=0.6) indicating proportions of
    ## the population. We assume the components add up to 1 and are only M and
    ## F. Will change this when mortality tables change it.
    if (is.list(sex)) {
        sex <- ifelse(runif(N) < sex$M, "M", "F");
    } else {
        sex <- rep(sex, N);
    }

    for (i in 1:N) {
        m <- member(age=ages[i], service=servs[i], salary=salaries[i],
                    sex=sex[i], tier=tier, mortClass=mortClass, sysName=sysName,
                    sysClass=sysClass, status=status,
                    currentYear=currentYear, cola=cola, verbose=verbose);

        members[[m$id]] <- m;
    }

    return(members);
}

## Make a tibble from a memberList.  The sampler is a function that takes a
## member object and returns TRUE if it should be included in the output
## tibble.
##
## e.g. function(m) { ifelse(m$tier == "1", TRUE, FALSE) }
##
makeTbl <- function(memberList, sampler=function(m) {TRUE} ) {

    out <- tibble();

    for (member in memberList) {
        if (sampler(member))
            out <- rbind(out,
                         tibble(id=c(member$id),
                                hireYear=c(member$hireYear),
                                sepYear=c(member$sepYear),
                                retireYear=c(member$retireYear),
                                maxSalary=c(max(member$salaryHistory$salary)),
                                car=c(member$car),
                                tier=c(member$tier),
                                birthYear=c(member$birthYear),
                                deathYear=c(max(member$salaryHistory$year))));
    }

    return(out);
}

## Every year, premiums are collected for everyone.  Imagine a big
## matrix of premium payments, where each row represents a year of
## premiums (P) and pension payments (R), and each column is all the
## actives who will retire in a class.
##
## year
##   1   P11   P12   P13   P14   P15 ...
##   2   R21   P22   P23   P24   P25 ...
##   3   R31   R32   P33   P34   P35 ...
##   4   R41   R42   R43   P44   P45 ...
##   5   R51   R52   R53   R54   P55 ...
##   6     .     .     .     .    .  ...
##   7     .     .     .     .    .  ...
##   8     .     .     .     .    .  ...
##
## The "1" class (first column) retires at the end of year 1, so
## receives its pension benefits in year 2.  The 2 class retires at
## the end of year 2, and so on.  We refer to this below as the master
## cash flow matrix.
##
##

## Build the master cash flow matrix.  This involves grouping retirees
## by retirement date and aggregating them.
buildMasterCashFlow <- function(memberTbl, members, verbose=FALSE) {

    ## Our master cash flow will begin at the earliest hire date and
    ## end at the latest death date.
    startYear <- min(memberTbl$hireYear);
    endYear <- max(memberTbl$deathYear);

    ## Get all the retireYears, in order.
    retireYears <- unique(memberTbl$retireYear)
    retireYears <- retireYears[order(retireYears, na.last=NA)];

    if (length(retireYears) == 0) stop(" Nobody retired!\n");

    nYears <- endYear - startYear + 1;

    if (verbose) cat("Starting at", startYear, "ending at", endYear,
                     "n =", nYears, "\n");

    ## Initialize output tbl.
    out <- tibble(year=startYear:endYear);

    ## Loop through all the potential retirement classes, even if
    ## they're empty.
    for (retireClass in min(retireYears):max(retireYears)) {

        if (verbose) cat("Considering retirement class", retireClass, "\n");

        ## Initialize an empty row to hold the cash flow from this class.
        collectiveCashFlow <- rep(0, nYears);

        if (retireClass %in% retireYears) {

            classMemberIDs <- memberTbl %>%
                filter(retireYear == retireClass) %>%
                select(id);

            if (verbose) print(classMemberIDs);

            ## Now add the cash flow from each member of that retirement
            ## class to the collective cash flow.
            for (id in classMemberIDs$id) {
                if (verbose) cat("adding", id, format(members[[id]]), "\n");

                for (iyear in members[[id]]$salaryHistory$year) {
                    yearsFlow <- members[[id]]$salaryHistory %>%
                        filter(year == iyear) %>%
                        mutate(flow = premium - pension) %>%
                        select(flow) %>% as.numeric();
                    collectiveCashFlow[1 + iyear - startYear] <-
                        collectiveCashFlow[1 + iyear - startYear] + yearsFlow;
                }
            }
        }

        ## Add the column for this class.
        out <- cbind(out, tibble(flow=collectiveCashFlow) %>%
                          rename_with(function(x) {
                              ifelse(x=="flow",
                                     paste0('R',retireClass), x)}));
    }

    out <- out %>% mutate(sum=rowSums(across(where(is.double)))) ;

    return(tibble(out));
}


## Given a function to construct a model population of plan members,
## this function will run that model and compute the CAR for its members.
runModelOnce <- function(modelConstructionFunction,
                         sampler=function(m) {TRUE},
                         verbose=FALSE) {

    model <- modelConstructionFunction(verbose=verbose);

    if (verbose) cat("model: Constructed a model with", length(model),
                     "members.\n");

    ## Make a summary table of all the members.
    modelTbl <- makeTbl(model, sampler=sampler);

    ## Build the master cash flow matrix. 
    modelMCF <- buildMasterCashFlow(modelTbl, model, verbose=verbose);

    if (verbose) cat("model: Retirement classes:", dim(modelMCF)[2] - 2,
                     "from", colnames(modelMCF)[2],
                     "to", head(tail(colnames(modelMCF),2),1), "\n");

    ## Compute the CAR for the overall results.
    modelCAR <- findRate(modelMCF, flowName="sum", maxIter=200,
                         verbose=verbose);

    if (verbose) cat("model: CAR estimate:", modelCAR, "\n");

    ## Record the aggregate CAR under the year 1000 because why not.
    if (!is.na(modelCAR)) {
        modelOut <- tibble(ryear = c(1000),
                           car = c(modelCAR - 1.0));
    } else {
        ## This is a bad scene. Record everything and get out.
        write.csv(modelTbl, file="dumpMemberTable.csv");
        write.csv(modelMCF, file="dumpMasterCashFlow.csv");
        stop("Something seriously wrong, check out dump files.");
    }

    ## We are also interested in calculating the CAR for each
    ## retirement class. Note that there are two extra columns in the
    ## master cash flow matrix, for the year and for the row sums.  So
    ## subtract two to get the number of retirement classes.
    minRetireYear <- min(modelTbl$retireYear, na.rm=TRUE);
    for (i in 1:(dim(modelMCF)[2] - 2)) {
        newYear <- minRetireYear + i - 1;
        newRate <- findRate(modelMCF, flowName=paste0("R", newYear),
                            maxIter=200);

        ## If no error, record the rate for this retirement class.
        if (!is.na(newRate)) {
            modelOut <- rbind(modelOut,
                              tibble(ryear=c(newYear), car=c(newRate - 1.0)));
        }
    }

    out <- list(model=model,
                modelTbl=modelTbl,
                modelMCF=modelMCF,
                modelOut=modelOut);
    attr(out, "class") <- "modelOutput";

    return(out);
}

print.modelOutput <- function(mo, ...) {
    cat("model:\n");
    synopsize(mo$model);
}
        


runModel <- function(modelConstructionFunction, N=1,
                     sampler=function(m) {TRUE},
                     verbose=FALSE,
                     reallyVerbose=FALSE,
                     audit=FALSE) {
    if (verbose) cat("Starting run on:", date(),"\n");

    ## Prepare the main output, just a record of years and CAR estimates.
    modelOut <- tibble(ryear=c(), car=c());

    ## We will also collect the output from each model run, for audit purposes.
    modelColl <- list();

    for (i in 1:N) {
        if (verbose) cat("  ", date(), ": model run number", i, "...");

        M <- runModelOnce(modelConstructionFunction, sampler=sampler,
                          verbose=reallyVerbose)

        modelOut <- rbind(modelOut, M$modelOut);
        modelColl[[i]] <- M;

        if (verbose) cat("runModel: ", date(), ">", i, ": CAR =",
                         as.numeric(M$modelOut %>%
                                    filter(ryear == 1000) %>%
                                    select(car)), "\n", sep="");
    }

    if (verbose) cat("Ending at:", date(),"\n");

    if (audit) {
        ## Include the model and audit data in the return list.
        return(list(output=modelOut, audit=modelColl));
    } else {
        return(list(output=modelOut));
    }
}


## Some useful output routines.
library(ggplot2)

plotModelOut <- function(modelOut, xlimits=c(2020,2065), ylimits=c(0,0.1)) {

    modelOutSummary <-
        modelOut %>%
        group_by(ryear) %>%
        summarize(car=mean(car),N=n());

    modelOutAvg <- modelOutSummary %>%
        filter(ryear == 1000) %>% select(car) %>% as.numeric();

    plotOut <- ggplot(modelOutSummary %>% filter(ryear > 1900)) +
        geom_point(aes(x=ryear, y=car, color=N)) +
        xlim(xlimits) +
        ylim(ylimits) +
        geom_hline(yintercept=modelOutAvg, color="red") +
        labs(x="retirement class", y="CAR");

    return(plotOut);
}

altPlotModelOut <- function(modelOut,
                            xlimits=c(2020,2065), ylimits=c(-0.05,0.1),
                            system="") {

    modelOutSummary <-
        modelOut %>%
        group_by(ryear) %>%
        summarize(car=mean(car),N=n());

    modelOutAvg <- modelOutSummary %>%
        filter(ryear == 1000) %>% select(car) %>% as.numeric();

    modelOutAug <- modelOut %>%
        group_by(ryear) %>%
        mutate(N=length(car),delta=car-mean(car)) %>%
        filter(abs(delta) < 0.09)

    plotOut <- ggplot(modelOutAug %>% filter(ryear > 1900)) +
        geom_point(aes(x=ryear, y=car, color=abs(delta))) +
        ylim(ylimits) +
        xlim(xlimits) +
        scale_colour_gradientn(
            colours = c("#ff0000","#aa3333","#665555","#777777",
                        "#888888","#aaaaaa","#cccccc","#eeeeee"),
            values = c(0.0, 0.01, 0.04, 0.09, 0.13, 0.35, 0.60, 0.80, 1.0))+
        geom_hline(yintercept=modelOutAvg, color="blue") +
        annotate("text", label=sprintf("%4.1f%%",modelOutAvg*100),
                 x=xlimits[2], y=modelOutAvg, color="blue",
                 vjust=-0.3,hjust=0.5) +
        theme(legend.position="NONE") +
        labs(x=paste("Retirement class", system), y="CAR");

    return(plotOut);
}

plotModelOutNoLim <- function(modelOut) {

    modelOutSummary <-
        modelOut %>%
        group_by(ryear) %>%
        summarize(car=mean(car),N=n());

    modelOutAvg <- modelOutSummary %>%
        filter(ryear == 1000) %>% select(car) %>% as.numeric();

    plotOut <- ggplot(modelOutSummary %>% filter(ryear > 1900)) +
        geom_point(aes(x=ryear, y=car, color=N)) +
        geom_hline(yintercept=modelOutAvg, color="red") +
        labs(x="retirement class", y="CAR");

    return(plotOut);
}


