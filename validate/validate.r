##
## This file contains some functions and facilities for validating the
## system-specific provisions of the employment contract that contribute to
## the CAR.  Obviously it is not possible to make a priori validation
## functions for arbitrary benefit and employment provisions, but it is
## feasible to produce tables of outcomes, for example, that can be compared
## with the actuarial assumptions of probability. Or it is feasible to present
## a career's worth of salary information and present the base pension amount
## for evaluation of its plausibility.
##

## This produces a tibble with probabilities of separation at different ages,
## presumably more accurate with larger Ns. Really just meant to make a table
## you can compare to the separation probabilities in the actuarial
## assumptions. You get N samples for each age.
##
validateDoesMemberSeparate <- function(N, tier="A", mortClass="General",
                                       verbose=FALSE) {

    out <- tibble(age=1:65, service=age-20, Nsep=0, Ntot=0);

    for (age in 21:65) {
        for (i in 1:N) {
            newStatus <- doesMemberSeparate(age, sex="M", service=age-20,
                                            status="active", tier=tier,
                                            mortClass=mortClass, verbose=verbose)

            out[age, "Ntot"] <- out[age, "Ntot"] + 1;
            
            if (newStatus == "separated") out[age, "Nsep"] <- out[age, "Nsep"] + 1;
        }
    }

    return(out %>% filter(age>20) %>% mutate(prob=Nsep/ifelse(Ntot==0,1,Ntot)));

}

## These are also suitable for A and B versions, i.e. not just the annual
## probabilities, but also the probability of anyone becoming disabled over
## the course of a career. That might make a better plausibility test.

validateDoesMemberDisableAccident <- function(N, sex, mortClass="General",
                                              tier="A", verbose=FALSE) {
    out <- tibble(age=1:65, Nacc=0, Ntot=0);

    for (age in 20:65) {
        for (i in 1:N) {
            newStatus <- doesMemberDisableAccident(age, sex, 5, "active",
                                                   mortClass=mortClass, tier=tier,
                                                   verbose=verbose)

            out[age, "Ntot"] <- out[age, "Ntot"] + 1;
            
            if (newStatus == "disabled/accident")
                out[age, "Nacc"] <- out[age, "Nacc"] + 1;
        }
    }

    return(out %>% filter(age>=20) %>% mutate(prob=Nacc/ifelse(Ntot==0,1,Ntot)));
}

validateDoesMemberDisableOrdinary <- function(N, sex, mortClass="General",
                                              tier="A", verbose=FALSE) {
    out <- tibble(age=1:65, Nord=0, Ntot=0);

    for (age in 20:65) {
        for (i in 1:N) {
            newStatus <- doesMemberDisableOrdinary(age, sex, 5, "active",
                                                   mortClass=mortClass, tier=tier,
                                                   verbose=verbose)

            out[age, "Ntot"] <- out[age, "Ntot"] + 1;
            
            if (newStatus == "disabled/ordinary")
                out[age, "Nord"] <- out[age, "Nord"] + 1;
        }
    }

    return(out %>% filter(age>=20) %>% mutate(prob=Nord/ifelse(Ntot==0,1,Ntot)));
}

validateDoesMemberDie <- function(N, sex, mortClass="General",
                                  tier="A", verbose=FALSE) {
}

## Calculate a pension. Uses data in validation-data, but you can use
## adjYear and adjRetire to adjust the year and retirement year of the
## two time series.
validateProjectPension <- function(status="active", adjYear=0, adjRetire=0,
                                   tier="A", verbose=FALSE) {

    if (status == "active") {
        if (verbose) cat("active -> retired..adjYear:", adjYear,
                         "adjRetire:", adjRetire, "\n");
        salaryHistory <- read.csv("validation-data/prov-validate-2.csv")  %>%
            mutate(year=year + adjYear,
                   pension=0);
        if (adjRetire > 0) {
            salaryHistory[22:(22 + adjRetire), "status"] <- "active";
        } else if (adjRetire > 0) {
            salaryHistory[(22 + adjRetire):22, "status"] <- "active";
        }
    } else if (status == "separated") {
        if (verbose) cat("separated -> retired..adjYear:", adjYear,
                         "adjRetire:", adjRetire, "\n");
        salaryHistory <- read.csv("validation-data/prov-validate.csv")  %>%
            mutate(year=year + adjYear,
                   pension=0);
        if (adjRetire > 0) {
            salaryHistory[31:(31 + adjRetire), "status"] <- "separated";
        } else if (adjRetire > 0) {
            salaryHistory[(31 + adjRetire):31, "status"] <- "separated";
        }
    } else {
        return(NA);
    }

    salaryHistory <- projectPension(salaryHistory, tier=tier, verbose=verbose) %>%
        mutate(cola=ifelse(lag(pension)==0,
                           0,
                           (pension - lag(pension))/lag(pension)));

    return(salaryHistory);
}

displayPensionValidation <- function(salaryHistory) {}


validateProjectPremiums <- function() {}

validateProjectSalaryDelta <- function(startYear=1990, endYear=2030,
                                       startAge=25, tier="A", verbose=FALSE) {
    out <- tibble(year=startYear:endYear) %>%
        mutate(age=startAge + year - startYear,
               service=1 + year - startYear);

    salary <- c(1000.0);
    for (i in 2:length(out$year)) {
        salary <- c(salary, salary[i - 1] * projectSalaryDelta(out$year[i - 1],
                                                               out$age[i - 1],
                                                               salary[i - 1],
                                                               out$service[i - 1],
                                                               tier=tier));
    }
    out$salary <- salary;
    out <- out %>% mutate(delta=(salary-lag(salary))/lag(salary));

    return(out);
}

## Checks out retirement decisions based on a range of service tenure values.
## Result is a 2D table with a row for each age range and a column for service
## ranges. Note that we are validating not only the probability of choosing to
## retire, but also the function's evaluation of eligibility. So there should
## be a lot of zeros.
validateDoesMemberRetireA <- function(N, status="active", tier="A", verbose=FALSE) {
  
    out <- tibble(age=1:80,
                  R00t05=0, N00t05=0,
                  R05t10=0, N05t10=0,
                  R10t15=0, N10t15=0,
                  R15t20=0, N15t20=0,
                  R20t25=0, N20t25=0,
                  R25t30=0, N25t30=0,
                  R30t35=0, N30t35=0);

    out <- tibble(age=c(), service=c(), oldStatus=c(), newStatus=c())
    for (age in 40:75) {
        ## Generate a selection of service years for each age.
        data <- tibble(age=rep(age, N),
                           service=round(0.5 + runif(N) * min(34.5, age-21)));

        ## Run doesMemberRetire for each one. Note that there is a more
        ## tidyverse way to do this, but there's something screwy about using
        ## verbose for debugging in that case.
        data$newStatus <- apply(data, 1, function(x, status, tier, verbose) {
            doesMemberRetire(x["age"], x["service"], status, tier, verbose)
        }, status=status, tier=tier, verbose=verbose)

        ## This is here because if we include it in the original definition of
        ## data, then apply assumes all the x args to function are char. (?!)
        data$oldStatus <- rep(status, N);
        
        out <- out %>% rbind(data);
    }

    out <- out %>%
        mutate(gs=ceiling(service/5)) %>%
        group_by(age,gs) %>%
        dplyr::summarize(ret=sum(newStatus=="retired"), tot=n(),.groups="drop") %>%
        group_by(age) %>%
        dplyr::summarize(R00t05=sum(ret[gs==1]), N00t05=sum(tot[gs==1]),
                         R05t10=sum(ret[gs==2]), N05t10=sum(tot[gs==2]),
                         R10t15=sum(ret[gs==3]), N10t15=sum(tot[gs==3]),
                         R15t20=sum(ret[gs==4]), N15t20=sum(tot[gs==4]),
                         R20t25=sum(ret[gs==5]), N20t25=sum(tot[gs==5]),
                         R25t30=sum(ret[gs==6]), N25t30=sum(tot[gs==6]),
                         R30t35=sum(ret[gs==7]), N30t35=sum(tot[gs==7]));
    return(out);
}

## Convenience function for the output of validateDoesMemberRetireA.
displayRetirementValidation <- function(valOutput) {

    out <- valOutput %>%
        mutate(s00t05=sprintf("%5.3f|%d", ifelse(N00t05==0,0,R00t05/N00t05), N00t05),
               s05t10=sprintf("%5.3f|%d", ifelse(N05t10==0,0,R05t10/N05t10), N05t10),
               s10t15=sprintf("%5.3f|%d", ifelse(N10t15==0,0,R10t15/N10t15), N10t15),
               s15t20=sprintf("%5.3f|%d", ifelse(N15t20==0,0,R15t20/N15t20), N15t20),
               s20t25=sprintf("%5.3f|%d", ifelse(N20t25==0,0,R20t25/N20t25), N20t25),
               s25t30=sprintf("%5.3f|%d", ifelse(N25t30==0,0,R25t30/N25t30), N25t30),
               s30t35=sprintf("%5.3f|%d", ifelse(N30t35==0,0,R30t35/N30t35), N30t35)) %>%
        select(age, s00t05, s05t10, s10t15, s15t20, s20t25, s25t30, s30t35);

    return(out);
}

## A longitudinal approach to the same question. This function generates
## individuals and follows them by advancing their age and service until they
## retire, and returns some ratios about that.
##
## Try: validateDoesMemberRetireB(1000, tier="B") %>% ggplot()  +
##                 geom_point(aes(x=age,y=retirements))
validateDoesMemberRetireB <- function(N, status="active", tier="A", verbose=FALSE) {

    out <- tibble(age=40:80, retirements=0);
    
    for (i in 1:N) {
        ## Create an individual with the given status and tier.
        age <- 40 - 1;
        service <- min(age - 21, round(rnorm(1, mean=10, sd=5))) - 1;
        curStatus <- status;

        if (verbose) cat(" Member age:", age, "service:", service,
                         "status:", curStatus, "\n");

        while((curStatus != "retired") & (age <= 80)) {
            ## Apparently not this year, advance age and service.
            age <- age + 1;
            service <- service + 1;

            curStatus <- doesMemberRetire(age, service, curStatus, tier, verbose);
            if (verbose) cat(" Member age:", age, "service:", service,
                             "status:", curStatus, "\n");
        }

        if (curStatus == "retired") {
            out[age - 39, "retirements"] <- out[age - 39, "retirements"] + 1;
        }
    }

    return(out);
}
