## Pull in the CAR calculation apparatus.
source("../../src/car.r", chdir=TRUE);

## System-specific information.
##
## This example uses valuation data from the Queen Creek (AZ) Fire
## Department, 4/21
##
## These functions (doIseparate and doIretire) give the probability of
## separation or retirement, given the age and service years of the
## employee.
doesMemberSeparate <- function(age, sex, service, status, tier="1",
                               mortClass="General", sysName="this", sysClass="",
                               verbose=FALSE) {
    ## If this is not currently an active employee, get out.
    if (status != "active") return(status);

    rates <- c(0.070, 0.045, 0.037, 0.030, 0.025,
               0.017, 0.017, 0.017, 0.017, 0.015,
               0.011, 0.007, 0.007, 0.007, 0.006,
               0.005, 0.005, 0.004, 0.004, 0.004);

    service <- min(service, 20);
    if (runif(1) < rates[service]) status <- "separated";

    return(status);
}

doesMemberRetire <- function(age, sex, service, status, tier="1",
                             mortClass="General", sysName="this", sysClass="",
                             verbose=FALSE) {
    ## If already retired, get out.
    if ((status == "retired") || (status == "deceased")) return(status);

    ## The service years on input refer to years that have begun, but
    ## not necessarily completed.  We want completed years.  This is
    ## related to the model's approximation that events happen on the
    ## transition from one year to the next, as opposed to the real
    ## world, where events happen whenever they feel like it.
    completedYears <- service - 1;
    if (completedYears < 10) return(status);

    if (verbose) cat(" --retire? age: ", age, ", service: ", service,
                     ", tier: ", tier, " begin: ", status, "...",
                     sep="");

    if (tier == "1") {

        if ((age >= 62) && (completedYears <= 20)) {
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
    } else if (tier == "2") {
        if ((age >= 53) && (completedYears >= 15)) {
            rates <- c(0.22, 0.26, 0.19, 0.32, 0.30,
                       0.30, 0.30, 0.55, 0.55, 0.55,
                       0.55, 1.00);
            if (runif(1) < rates[min(age - 52, 12)]) status <- "retired";
        }
    } else if (tier == "3") {
        if ((age >= 55) && (completedYears >= 15)) {
            rates <- c(0.19, 0.32, 0.30, 0.30, 0.30,
                       0.55, 0.55, 0.55, 0.55, 1.00);
            if (runif(1) < rates[min(age - 54, 10)]) status <- "retired";
        }
    } else if (tier == "0") {
        ## There are "tier 0" people in QC Fire.  They are already
        ## retired, and receiving a pension.  There are employer
        ## contributions happening on their behalf, but they are otherwise
        ## irrelevant to the investigation of the CAR.  We age them and
        ## retire them, and will try to avoid counting them later.
        if (age > 65) status <- "retired";
    }

    if (verbose) cat("result: ", status, "\n", sep="");

    return(status);
}

doesMemberBecomeDisabled <- function(age, sex, service, status,
                                     mortClass="General", tier="1",
                                     sysName="this", sysClass="", verbose=FALSE) {
    ## If already retired or disabled, don't change anything and get out.
    if ((status == "retired") || (status == "deceased") ||
        (status == "disabled") ) return(status);

    ## These are rates for ages 20-25, 25-30, 30-35, etc
    rates <- c(0.0003, 0.0003, 0.0004, 0.0009, 0.0017, 0.0017, 0.0043, 0.01);

    ## Select the appropriate rate.
    irate <- min(length(rates), ceiling((age - 20)/5));

    ## Roll the dice.
    if (runif(1) < rates[irate]) status <- "disabled";

    return(status);
}

## The assumed salary increment, from the table of merit increases in
## each valuation report.
projectSalaryDelta <- function(year, age, salary, service=1, tier="1",
                               mortClass="General", sysName="this", sysClass="",
                               verbose=FALSE) {

    if (age < 25) {
        out <- 1.075;
    } else if ((age >= 25) && (age < 30)) {
        out <- 1.0735;
    } else if ((age >= 30) && (age < 35)) {
        out <- 1.0674;
    } else if ((age >= 35) && (age < 40)) {
        out <- 1.0556;
    } else if ((age >= 40) && (age < 45)) {
        out <- 1.0446;
    } else if ((age >= 45) && (age < 50)) {
        out <- 1.0374;
    } else if (age >= 50) {
        out <- 1.035;
    }

    ## Tier 3 salaries are limited.  ??
##    if (tier == "3") {
##        iyear <- max(year, 2020);
##        limit <- 110000 * 1.02^(year - 2020);
##        out <- min(out, (out - limit) / limit);
##    }

    return(out);
}

projectPension <- function(salaryHistory, tier="1", mortClass="General",
                           cola=1.0175, sysName="this", sysClass="",
                           verbose=FALSE) {

    service <- sum(salaryHistory$salary > 0);

    ## Calculate the base salary from which to calculate the pension.
    if (tier == "1") {
        # Find the maximum 3-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);
        threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
        avgSalary <- max(threeYears) / 3.0;

        startingPension <- 0.5 * avgSalary;

        if ((service >= 15) && (service < 20)) {
            startingPension <- startingPension * (1 - ((20 - service) * 0.04));
        } else if (service >= 25) {
            startingPension <- startingPension * (1 + ((service - 20) * 0.025));
        } else { ## 20-25
            startingPension <- startingPension * (1 + ((service - 20) * 0.02));
        }

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == "2") {
        # Find the maximum 5-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);

        fiveYears <-
            c(s, 0, 0, 0, 0) +
            c(0, s, 0, 0, 0) +
            c(0, 0, s, 0, 0) +
            c(0, 0, 0, s, 0) +
            c(0, 0, 0, 0, s);
        avgSalary <- max(fiveYears) / 5.0;

        if ((service >= 15) && (service < 17)) {
            benefitMultiplier <- 0.015;
        } else if ((service >= 17) && (service < 19)) {
            benefitMultiplier <- 0.0175;
        } else if ((service >= 19) && (service < 22)) {
            benefitMultiplier <- 0.02;
        } else if ((service >= 22) && (service < 25)) {
            benefitMultiplier <- 0.0225;
        } else if (service >= 25) {
            benefitMultiplier <- 0.025;
        } else { print(as.data.frame(salaryHistory)); cat("tier:", tier, "service:", service, "\n");}

        startingPension <- avgSalary * (service * benefitMultiplier);

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == "3") {
        # Find the maximum 5-year period.
        s <- tail(salaryHistory %>%
                  mutate(X=pmin(salary, 110000 * (1.02^(pmax(0, year-2020))))) %>%
                  filter(X > 0) %>%
                  select(X) %>%
                  unlist(use.names=FALSE), 15);

        fiveYears <-
            c(s, 0, 0, 0, 0) +
            c(0, s, 0, 0, 0) +
            c(0, 0, s, 0, 0) +
            c(0, 0, 0, s, 0) +
            c(0, 0, 0, 0, s);
        avgSalary <- max(fiveYears) / 5.0;

        if ((service >= 15) && (service < 17)) {
            benefitMultiplier <- 0.015;
        } else if ((service >= 17) && (service < 19)) {
            benefitMultiplier <- 0.0175;
        } else if ((service >= 19) && (service < 22)) {
            benefitMultiplier <- 0.02;
        } else if ((service >= 22) && (service < 25)) {
            benefitMultiplier <- 0.0225;
        } else if (service >= 25) {
            benefitMultiplier <- 0.025;
        } else {
            ## This is an error condition, and a fault will follow.
            print(as.data.frame(salaryHistory));
            cat("tier:", tier, "service:", service, "\n");
        }

        startingPension <- avgSalary * (service * benefitMultiplier);

        startingPension <- min(0.8 * avgSalary, startingPension);
    } else if (tier == "0") {
        ## The tier 0 people can retire again but they don't get
        ## another pension.
        startingPension <- 0.0;
    }

    cola <- 1.0175;

    ## If this person never retired, send them away without a pension.
    if (!("retired" %in% salaryHistory$status))
        return(salaryHistory %>% mutate(pension = 0));

    retireYear <- as.numeric(salaryHistory %>%
                             filter(status=="retired") %>%
                             summarize(retireYear=min(year)));

    return(salaryHistory %>%
           mutate(pension = ifelse(status == "retired",
                                   startingPension * cola^(year - retireYear),
                                   0)));

}

## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share.)
qcPremiumRate <- tibble(year = c(2008,2009,
                                 2010,2011,2012,2013,2014,
                                 2015,2016,2017,2018,2019,
                                 2020,2021),
                        premiumRate=c(0.186,0.159,
                                      0.154,0.152,0.155,0.242,0.264,
                                      0.208,0.216,0.145,0.126,0.129,
                                      0.118,0.120));

projectPremiums <- function(salaryHistory, tier="A", mortClass="General",
                            sysName="this", sysClass="", verbose=FALSE) {    
    return(salaryHistory %>%
           left_join(qcPremiumRate,by="year") %>%
           mutate(premiumRate=ifelse(is.na(premiumRate),.2265,premiumRate),
                  premium = ifelse(premium==0, salary * premiumRate, premium)));
}


if (TRUE) {
cat("starting one two three", date(), "\n");
## Let's model the Queen Creek fire department.  This data is from the
## valuation report, the member population table.  This function
## produces a list of members, and the function itself can be fed to
## the runModel functions in car.r.
## File: 247 2021_PSPRS-Valuation-QueenCreekFireDept.pdf
## Page: 23 of 44 (PDF page number)
qcModel <- function(verbose=FALSE) {
    ## Ages 20-24
    qcFire <- genEmployees(1, ageRange=c(20,24), servRange=c(0,4), tier="2",
                           avgSalary=78545, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(20,24), servRange=c(0,4), tier="3",
                           avgSalary=57174, verbose=verbose);

    ## Ages 25-29  Note that in the document, pay values are averaged across
    ## service classes, which is totally unhelpful. We have attempted to back
    ## out average values from the numbers of members and the average values from
    ## previous lines.
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(0,4), tier="1",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(5,9), tier="1",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(25,29), servRange=c(0,4), tier="2",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(25,29), servRange=c(0,4), tier="3",
                           avgSalary=61951, members=qcFire, verbose=verbose);

    ## Ages 30-34  Assume salary for service range 5-9 is 1.3x 0-4.
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(0,4), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(30,34), servRange=c(0,4), tier="2",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="2",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(30,34), servRange=c(0,4), tier="3",
                           avgSalary=64261, members=qcFire, verbose=verbose);

    ## Ages 35-39   Assume salary for service range 10-14 is 1.7x 0-4.
    ##              Assume salary for service range 15-19 is 2x 0-4.
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(0,4), tier="2",
                           avgSalary=72266, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="1",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="2",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(10,14), tier="1",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(10,14), tier="2",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(15,19), tier="1",
                           avgSalary=144533, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(35,39), servRange=c(0,4), tier="3",
                           avgSalary=71214, members=qcFire, verbose=verbose);

    ## Ages 40-44
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="2",
                           avgSalary=73960, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="1",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="2",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(6, ageRange=c(40,44), servRange=c(10,14), tier="1",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(10,14), tier="2",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="3",
                           avgSalary=63883, members=qcFire, verbose=verbose);

    ## Ages 45-49
    qcFire <- genEmployees(4, ageRange=c(45,49), servRange=c(10,14), tier="1",
                           avgSalary=137705, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(15,19), tier="1",
                           avgSalary=162007, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(0,4), tier="3",
                           avgSalary=77123, members=qcFire, verbose=verbose);

    ## Ages 50-54
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(5,9), tier="2",
                           avgSalary=100591, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(5, ageRange=c(50,54), servRange=c(10,14), tier="1",
                           avgSalary=131543, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(15,19), tier="1",
                           avgSalary=154756, members=qcFire, verbose=verbose);

    return(qcFire);
}

qcModelOutput <- runModel(qcModel, N=100, verbose=TRUE, reallyVerbose=FALSE);
qcModelPlot <- altPlotModelOut(qcModelOutput)
ggsave("../../report/images/qc-from-model.png",
       device="png", width=6.5, height=6.5, units="in")
cat("done with all", date(), "\n");

## Now we're going to model the same but force everyone into tier 1 or three.
cat("starting Tier One", date(), "\n");
qcModel <- function(verbose=FALSE) {
    ## Ages 20-24
    qcFire <- genEmployees(1, ageRange=c(20,24), servRange=c(0,4), tier="1",
                           avgSalary=78545, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(20,24), servRange=c(0,4), tier="1",
                           avgSalary=57174, verbose=verbose);

    ## Ages 25-29  Note that in the document, pay values are averaged across
    ## service classes, which is totally unhelpful. We have attempted to back
    ## out average values from the numbers of members and the average values from
    ## previous lines.
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(0,4), tier="1",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(5,9), tier="1",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(25,29), servRange=c(0,4), tier="1",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(25,29), servRange=c(0,4), tier="1",
                           avgSalary=61951, members=qcFire, verbose=verbose);

    ## Ages 30-34  Assume salary for service range 5-9 is 1.3x 0-4.
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(0,4), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(30,34), servRange=c(0,4), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="1",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(30,34), servRange=c(0,4), tier="1",
                           avgSalary=64261, members=qcFire, verbose=verbose);

    ## Ages 35-39   Assume salary for service range 10-14 is 1.7x 0-4.
    ##              Assume salary for service range 15-19 is 2x 0-4.
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(0,4), tier="1",
                           avgSalary=72266, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="1",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="1",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(10,14), tier="1",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(10,14), tier="1",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(15,19), tier="1",
                           avgSalary=144533, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(35,39), servRange=c(0,4), tier="1",
                           avgSalary=71214, members=qcFire, verbose=verbose);

    ## Ages 40-44
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="1",
                           avgSalary=73960, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="1",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="1",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(6, ageRange=c(40,44), servRange=c(10,14), tier="1",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(10,14), tier="1",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="1",
                           avgSalary=63883, members=qcFire, verbose=verbose);

    ## Ages 45-49
    qcFire <- genEmployees(4, ageRange=c(45,49), servRange=c(10,14), tier="1",
                           avgSalary=137705, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(15,19), tier="1",
                           avgSalary=162007, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(0,4), tier="1",
                           avgSalary=77123, members=qcFire, verbose=verbose);

    ## Ages 50-54
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(5,9), tier="1",
                           avgSalary=100591, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(5, ageRange=c(50,54), servRange=c(10,14), tier="1",
                           avgSalary=131543, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(15,19), tier="1",
                           avgSalary=154756, members=qcFire, verbose=verbose);

    return(qcFire);
}

qcModelOutputTierOne <- runModel(qcModel, N=100, verbose=TRUE, reallyVerbose=FALSE);
qcModelPlotTierOne <- altPlotModelOut(qcModelOutputTierOne)
ggsave("../../report/images/qc-from-model-tier-one.png",
       plot=qcModelPlotTierOne,
       device="png", width=6.5, height=6.5, units="in")

cat("starting Tier Three\n");
qcModel <- function(verbose=FALSE) {
    ## Ages 20-24
    qcFire <- genEmployees(1, ageRange=c(20,24), servRange=c(0,4), tier="3",
                           avgSalary=78545, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(20,24), servRange=c(0,4), tier="3",
                           avgSalary=57174, verbose=verbose);

    ## Ages 25-29  Note that in the document, pay values are averaged across
    ## service classes, which is totally unhelpful. We have attempted to back
    ## out average values from the numbers of members and the average values from
    ## previous lines.
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(0,4), tier="3",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(25,29), servRange=c(5,9), tier="3",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(25,29), servRange=c(0,4), tier="3",
                           avgSalary=77994, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(25,29), servRange=c(0,4), tier="3",
                           avgSalary=61951, members=qcFire, verbose=verbose);

    ## Ages 30-34  Assume salary for service range 5-9 is 1.3x 0-4.
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(0,4), tier="3",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(30,34), servRange=c(0,4), tier="3",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="3",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(30,34), servRange=c(5,9), tier="3",
                           avgSalary=83872, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(7, ageRange=c(30,34), servRange=c(0,4), tier="3",
                           avgSalary=64261, members=qcFire, verbose=verbose);

    ## Ages 35-39   Assume salary for service range 10-14 is 1.7x 0-4.
    ##              Assume salary for service range 15-19 is 2x 0-4.
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(0,4), tier="3",
                           avgSalary=72266, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="3",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(35,39), servRange=c(5,9), tier="3",
                           avgSalary=93946, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(3, ageRange=c(35,39), servRange=c(10,14), tier="3",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(10,14), tier="3",
                           avgSalary=122853, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(35,39), servRange=c(15,19), tier="3",
                           avgSalary=144533, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(4, ageRange=c(35,39), servRange=c(0,4), tier="3",
                           avgSalary=71214, members=qcFire, verbose=verbose);

    ## Ages 40-44
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="3",
                           avgSalary=73960, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="3",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(40,44), servRange=c(5,9), tier="3",
                           avgSalary=96148, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(6, ageRange=c(40,44), servRange=c(10,14), tier="3",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(10,14), tier="3",
                           avgSalary=125733, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(2, ageRange=c(40,44), servRange=c(0,4), tier="3",
                           avgSalary=63883, members=qcFire, verbose=verbose);

    ## Ages 45-49
    qcFire <- genEmployees(4, ageRange=c(45,49), servRange=c(10,14), tier="3",
                           avgSalary=137705, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(15,19), tier="3",
                           avgSalary=162007, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(45,49), servRange=c(0,4), tier="3",
                           avgSalary=77123, members=qcFire, verbose=verbose);

    ## Ages 50-54
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(5,9), tier="3",
                           avgSalary=100591, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(5, ageRange=c(50,54), servRange=c(10,14), tier="3",
                           avgSalary=131543, members=qcFire, verbose=verbose);
    qcFire <- genEmployees(1, ageRange=c(50,54), servRange=c(15,19), tier="3",
                           avgSalary=154756, members=qcFire, verbose=verbose);

    return(qcFire);
}

qcModelOutputTierThree <- runModel(qcModel, N=100, verbose=TRUE, reallyVerbose=FALSE);
qcModelPlotTierThree <- altPlotModelOut(qcModelOutputTierThree)
ggsave("../../report/images/qc-from-model-tier-three.png",
       plot=qcModelPlotTierThree,
       device="png", width=6.5, height=6.5, units="in")
}
