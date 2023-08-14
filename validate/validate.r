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
                                       sysName="this", sysClass="",
                                       verbose=FALSE) {

    out <- tibble(age=1:65, service=age-20, Nsep=0, Ntot=0);

    for (age in 21:65) {
        for (i in 1:N) {
            newStatus <- doesMemberSeparate(age, sex="M", service=age-20,
                                            status="active", tier=tier,
                                            mortClass=mortClass,
                                            sysName=sysName, sysClass=sysClass,
                                            verbose=verbose)

            out[age, "Ntot"] <- out[age, "Ntot"] + 1;
            
            if (newStatus == "separated") out[age, "Nsep"] <- out[age, "Nsep"] + 1;
        }
    }

    return(out %>% filter(age>20) %>% mutate(prob=Nsep/ifelse(Ntot==0,1,Ntot)));

}

## These are also suitable for A and B versions, i.e. not just the annual
## probabilities, but also the probability of anyone becoming disabled over
## the course of a career. That might make a better plausibility test.
validateDoesMemberBecomeDisabledA <- function(N, sex="M", status="active",
                                              tier="A", mortClass="General",
                                              sysName="this", sysClass="",
                                              verbose=FALSE) {
  
    out <- tibble(age=c(), service=c(), oldStatus=c(), newStatus=c())
    for (age in 20:69) {
        ## Generate a selection of service years for each age.
        data <- tibble(age=rep(age, N));

        ## Run doesMemberBecomeDisabled for each one. Note that there is a more
        ## tidyverse-ish way to do this, but there's something screwy about using
        ## verbose for debugging in that case.
        data$newStatus <- apply(data, 1, function(x) {
            doesMemberBecomeDisabled(x["age"], sex, 1, status=status,
                                     tier=tier, mortClass=mortClass,
                                     sysName=sysName, sysClass=sysClass,
                                     verbose=verbose)
        })

        ## This is here because if we include it in the original definition of
        ## data, then apply assumes all the x args to function are char. (?!)
        data$oldStatus <- rep(status, N);
        
        out <- out %>% rbind(data);
    }

    out <- out %>%
        mutate(ageBracket=floor((age - 15)/5)) %>%
        group_by(ageBracket) %>%
        dplyr::summarize(ordinary=sum(newStatus=="disabled/ordinary"),
                         accident=sum(newStatus=="disabled/accident"),
                         total=n()) %>%
        mutate(bracket=sprintf("%d to %d", 15 + 5*ageBracket,15 + 5*(ageBracket+1)),
               pct=100*(ordinary + accident)/total) %>%
        select(bracket, ordinary, accident, total, pct);

    return(out);
}

validateDoesMemberDisableAccident <- function(N, sex,
                                              tier="A", mortClass="General",
                                              sysName="this", sysClass="",
                                              verbose=FALSE) {
    out <- tibble(age=1:65, Nacc=0, Ntot=0);

    for (age in 20:65) {
        for (i in 1:N) {
            newStatus <-
                doesMemberDisableAccident(age, sex, 5, "active",
                                          mortClass=mortClass, tier=tier,
                                          sysName=sysName, sysClass=sysClass,
                                          verbose=verbose)

            out[age, "Ntot"] <- out[age, "Ntot"] + 1;
            
            if (newStatus == "disabled/accident")
                out[age, "Nacc"] <- out[age, "Nacc"] + 1;
        }
    }

    return(out %>% filter(age>=20) %>% mutate(prob=Nacc/ifelse(Ntot==0,1,Ntot)));
}

validateDoesMemberDisableOrdinary <- function(N, sex,
                                              tier="A", mortClass="General",
                                              sysName="this", sysClass="",
                                              verbose=FALSE) {
    out <- tibble(age=1:65, Nord=0, Ntot=0);

    for (age in 20:65) {
        for (i in 1:N) {
            newStatus <-
                doesMemberDisableOrdinary(age, sex, 5, "active",
                                          mortClass=mortClass, tier=tier,
                                          sysName=sysName, sysClass=sysClass,
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
##
## Try validateProjectPension(status="active", adjYear=0, adjRetire=0,
##                            mortClass="Safety", tier="1",
##                            sysClass="mariPol") %>%
##     displayPensionValidation()
## Also status="separated".
validateProjectPension <- function(status="active", adjYear=0, 
                                   mortClass="General", tier="A", cola=1.02,
                                   sysName="this", sysClass="",
                                   verbose=FALSE) {

    if (status == "active") {
        if (verbose) cat("active -> retired..adjYear:", adjYear,
                         "mortClass:", mortClass, "tier:", tier, "\n");
        salaryHistory <- read.csv("../../validate/data/salary-validate-2.csv")  %>%
            mutate(year=year + adjYear,
                   mortClass=!!mortClass,
                   tier=!!tier,
                   service=service-1,
                   pension=0);

    } else if (status == "separated") {
        if (verbose) cat("separated -> retired..adjYear:", adjYear,
                         "mortClass:", mortClass, "tier:", tier, "\n");
        salaryHistory <- read.csv("../../validate/data/salary-validate.csv")  %>%
            mutate(year=year + adjYear,
                   mortClass=!!mortClass,
                   tier=!!tier,
                   service=service-1,
                   pension=0);
    } else {
        return(NA);
    }

    salaryHistory <- projectPension(salaryHistory, tier=tier, mortClass=mortClass,
                                    cola=cola, sysName=sysName, sysClass=sysClass,
                                    verbose=verbose) %>%
        mutate(cola=ifelse(lag(pension)==0,
                           0,
                           (pension - lag(pension))/lag(pension)));

    return(salaryHistory);
}

## Try validateProjectPension(...) %>% displayPensionValidation().
displayPensionValidation <- function(salaryHistory) {
    return(salaryHistory %>%
           select(year, age, service, salary, premium, status, pension, cola));
}


validateProjectPremiums <- function(adjYear=0, tier="1", mortClass="General",
                            sysName="this", sysClass="", verbose=FALSE) {

    salaryHistory <- read.csv("../../validate/data/salary-validate-2.csv");
    return(projectPremiums(salaryHistory=salaryHistory, adjYear=adjYear, tier=tier,
                           mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                           verbose=verbose));
}

validateProjectSalaryDelta <- function(startYear=1990, endYear=2030,
                                       startAge=25, tier="A", mortClass="General",
                                       sysName=sysName, sysClass=sysClass,
                                       verbose=FALSE) {
    out <- tibble(year=startYear:endYear) %>%
        mutate(age=startAge + year - startYear,
               service=year - startYear);

    salary <- c(1000.0);
    for (i in 2:length(out$year)) {
        salary <- c(salary,
                    salary[i - 1] * projectSalaryDelta(out$year[i - 1],
                                                       out$age[i - 1],
                                                       salary[i - 1],
                                                       out$service[i - 1],
                                                       tier=tier,
                                                       mortClass=mortClass,
                                                       sysName=sysName,
                                                       sysClass=sysClass,
                                                       verbose=verbose));
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
validateDoesMemberRetireA <- function(N, sex="M", status="active",
                                      tier="A", mortClass="General",
                                      sysName="this", sysClass="",
                                      verbose=FALSE) {
  
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
        data$newStatus <- apply(data, 1, function(x) {
            doesMemberRetire(x["age"], sex, x["service"], status=status,
                             tier=tier, mortClass=mortClass,
                             sysName=sysName, sysClass=sysClass,
                             verbose=verbose)
        })

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

## Convenience function for the output of validateDoesMemberRetireA. What you get
## from this function is a list of service classes ("s00t05" means "service 0 years
## to five years") and for each one you get the % of members who retired and the
## number of trials, in order.  So "0.25|24" means we tried 24 times and six (0.25)
## of them retired. These probabilities should look roughly like the probabilities
## from the val reports.
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
## Try: validateDoesMemberRetireB(1000, tier="B") %>% ggplot()  + geom_point(aes(x=age,y=retirements))
validateDoesMemberRetireB <- function(N, sex="M", status="active",
                                      tier="A", mortClass="General",
                                      sysName="this", sysClass="",
                                      verbose=FALSE) {

    out <- tibble(age=40:80, retirements=0);
    
    for (i in 1:N) {
        ## Create an individual with the given status and tier.
        age <- 43;
        service <- max(0, min(age - 21, round(rnorm(1, mean=10, sd=5))));
        curStatus <- status;

        if (verbose) cat(" Member age:", age, "sex:", sex, "service:", service,
                         "status:", curStatus, "tier:", tier,
                         "mortClass:", mortClass, "\n");

        while((curStatus != "retired") & (age <= 80)) {
            ## Apparently not this year, advance age and service.
            age <- age + 1;
            service <- service + 1;

            curStatus <- doesMemberRetire(age, sex, service, status=curStatus,
                                          tier=tier, mortClass=mortClass,
                                          sysName=sysName, sysClass=sysClass,
                                          verbose=verbose);
            if (verbose) cat(" Member age:", age, "service:", service,
                             "status:", curStatus, "\n");
        }

        if (curStatus == "retired") {
            out[age - 39, "retirements"] <- out[age - 39, "retirements"] + 1;
        }
    }

    return(out);
}

## Try: validateDoesMemberRetireB(1000, tier="B") %>% ggplot()  + geom_point(aes(x=age,y=disabled))
validateDoesMemberBecomeDisabledB <- function(N, sex="M", status="active",
                                              tier="A", mortClass="General",
                                              sysName="this", sysClass="",
                                              verbose=FALSE) {

    out <- tibble(age=20:70, disabled=0);
    
    for (i in 1:N) {
        ## Create an individual with the given status and tier.
        age <- 20;
        service <- 1;
        curStatus <- status;

        if (verbose) cat(" Member age:", age, "sex:", sex, "service:", service,
                         "status:", curStatus, "tier:", tier,
                         "mortClass:", mortClass, "\n");

        while((curStatus != "disabled/ordinary") &&
              (curStatus != "disabled/accident") && (age <= 70)) {
            ## Apparently not this year, advance age and service.
                  age <- age + 1;
                  service <- service + 1;

                  curStatus <-
                      doesMemberBecomeDisabled(age, sex, service, status=curStatus,
                                               tier=tier, mortClass=mortClass,
                                               sysName=sysName, sysClass=sysClass,
                                               verbose=verbose);
                  if (verbose) cat(" Member age:", age, "service:", service,
                                   "status:", curStatus, "\n");
              }

        if ((curStatus == "disabled/ordinary") || (curStatus == "disabled/accident")) {
            out[age - 19, "disabled"] <- out[age - 19, "disabled"] + 1;
        }
    }

    return(out);
}
