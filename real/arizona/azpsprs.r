## An attempt to estimate the CAR for various sub-systems in the Arizona
## PSPRS system.
## See https://misc.psprs.com/actuarials.aspx
##
##
## Pull in the CAR calculation apparatus.
source("../../src/car.r", chdir=TRUE);
source("../../validate/validate.r", chdir=TRUE);

## Read data provided by system. These are tables from the 2022 valuation
## reports, provided in spreadsheet form by the system actuary.
library(readxl)
retirementRateTableFile <- "../../../arizona/data/06.30.2022_Assumptions.xlsx";
popCountTableFile <- "../../../arizona/data/PSPRS_2022_CountSummaries.xlsx";
assumptionTableFile <- "../../../arizona/data/AssumpGrouping.xlsx";
sched2019file <- "../../../arizona/data/PSPRS-ER_Exhibits-2019_TS.xlsx";
sched2020file <- "../../../arizona/data/PSPRS-ER_Exhibits-2020_TS.xlsx";
sched2021file <- "../../../arizona/data/PSPRS_ErExhibits&PresentationStats_2021_TS.xlsx";
sched2022file <- "../../../arizona/data/PSPRS_ErExhibits&PresentationStats_2022_TS.xlsx";

colHeads <- c("s05","s10","s15","s20","s25","s30","s50","count","pay","avgpay");
popNames <- c(paste0("s20", colHeads),
              paste0("s25", colHeads),
              paste0("s30", colHeads),
              paste0("s35", colHeads),
              paste0("s40", colHeads),
              paste0("s45", colHeads),
              paste0("s50", colHeads),
              paste0("s55", colHeads),
              paste0("s60", colHeads),
              paste0("s65", colHeads),
              paste0("s99", colHeads),
              paste0("total", colHeads));

popCountNames <- read_excel(popCountTableFile,
                            sheet="CountSummaries", range="A3:B230",
                            col_names=c("sysNum", "sysName")) %>%
    mutate(sysNumCh=sprintf("%0.3d", sysNum)) %>%
    select(sysNum, sysNumCh, sysName);

popCountsT12 <- read_excel(popCountTableFile,
                           sheet="CountSummaries", range="DG3:HV230",
                           col_names=popNames);

popCountsT12 <- as_tibble(cbind(popCountNames, popCountsT12))

popCountsT3 <- read_excel(popCountTableFile,
                           sheet="CountSummaries", range="HW3:ML230",
                           col_names=popNames);

popCountsT3 <- as_tibble(cbind(popCountNames, popCountsT3))

disabilityRatesPSPRS <- read_excel(retirementRateTableFile,
                                   sheet="PSPRS", range="A8:G50",
                                   col_names=c("age", "mariFire", "otherFire",
                                               "pimaFire", "mariPol",
                                               "otherPol", "pimaPol"))
disabilityRatesPSPRS$age[1] <- "19";
disabilityRatesPSPRS$age[43] <- "61";
disabilityRatesPSPRS$age <- as.numeric(disabilityRatesPSPRS$age)

terminationRatesPSPRS <- read_excel(retirementRateTableFile,
                                   sheet="PSPRS", range="I8:O28",
                                   col_names=c("service", "mariFire",
                                               "otherFire",
                                               "pimaFire", "mariPol",
                                               "otherPol", "pimaPol"))
terminationRatesPSPRS$service[21] <- "20"
terminationRatesPSPRS$service <- as.numeric(terminationRatesPSPRS$service)

retirementRatesPSPRS.with20 <- read_excel(retirementRateTableFile,
                                          sheet="PSPRS", range="Q8:W22",
                                          col_names=c("service", "mariFire",
                                                      "otherFire",
                                                      "pimaFire", "mariPol",
                                                      "otherPol", "pimaPol"))
retirementRatesPSPRS.with20$service[15] <- "34"
retirementRatesPSPRS.with20$service <-
    as.numeric(retirementRatesPSPRS.with20$service)

retirementRatesPSPRS.wout20 <- read_excel(retirementRateTableFile,
                                          sheet="PSPRS", range="Q30:W38",
                                          col_names=c("age", "mariFire",
                                                      "otherFire",
                                                      "pimaFire", "mariPol",
                                                      "otherPol", "pimaPol"))
retirementRatesPSPRS.wout20$age[9] <- "70"
retirementRatesPSPRS.wout20$age <- as.numeric(retirementRatesPSPRS.wout20$age)

salaryIncreaseRatesPSPRS <- read_excel(retirementRateTableFile,
                                       sheet="PSPRS", range="AB8:AH41",
                                       col_names=c("age", "mariFire",
                                                   "otherFire",
                                                   "pimaFire", "mariPol",
                                                   "otherPol", "pimaPol"));
salaryIncreaseRatesPSPRS$age[34] <- "53";
salaryIncreaseRatesPSPRS$age <- as.numeric(salaryIncreaseRatesPSPRS$age);

countyRootList<- list("Maricopa"="mari","Pima"="pima","Other"="other");
typeList <- list("Police"="Pol", "Fire"="Fire")
assumptionKey <- read_excel(assumptionTableFile,
                            sheet="Sheet1", range="A3:C240",
                            col_names=c("sysNumName","county","type")) %>%
    group_by(sysNumName) %>%
    mutate(sysNumCh=str_split(sysNumName, " - ")[[1]][1],
           sysName=str_split(sysNumName, " - ")[[1]][2],
           sysNum=as.numeric(sysNumCh),
           snum=paste0("s", sysNumCh),
           sysType=paste0(countyRootList[[county]],typeList[[type]])) %>%
    ungroup() %>%
    select(sysNum, sysNumCh, snum, sysName, sysType, county, type);

contribT12.2019 <- readxl::read_excel(sched2019file,
                                      sheet="Tier12Contrib\ Pension",
                                      range="A3:H232",
                                      col_names=c("sysNum",
                                                  "sysNumCh",
                                                  "sysName",
                                                  "erNCpct",
                                                  "UALpmtPct",
                                                  "amortPeriod",
                                                  "calcERcont",
                                                  "reqERcont")) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT12.2020 <- readxl::read_excel(sched2020file,
                                      sheet="Tier12Contrib\ Pension",
                                      range="A3:H232",
                                      col_names=c("sysNum",
                                                  "sysNumCh",
                                                  "sysName",
                                                  "erNCpct",
                                                  "UALpmtPct",
                                                  "amortPeriod",
                                                  "calcERcont",
                                                  "reqERcont")) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT12.2021 <- readxl::read_excel(sched2021file,
                                      sheet="Tier12Contrib\ Pension",
                                      range="A3:H233",
                                      col_names=c("sysNum",
                                                  "sysNumCh",
                                                  "sysName",
                                                  "erNCpct",
                                                  "UALpmtPct",
                                                  "amortPeriod",
                                                  "calcERcont",
                                                  "reqERcont")) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT12.2022 <- readxl::read_excel(sched2022file,
                                      sheet="Tier12Contrib\ Pension",
                                      range="A4:H231",
                                      col_names=c("sysNum",
                                                  "sysNumCh",
                                                  "sysName",
                                                  "erNCpct",
                                                  "UALpmtPct",
                                                  "amortPeriod",
                                                  "calcERcont",
                                                  "reqERcont")) %>%
    mutate(snum=paste0("s", sysNumCh));

validateInputsAZ <- function(age=20, ageRange=c(20,120),
                           sex="M", sexRange=c("M","F"),
                           service=0, serviceRange=c(0,100),
                           status="active",
                           statusRange=c("active", "separated", "retired",
                                         "retired/survivor", "deceased",
                                         "disabled/accident", "disabled/ordinary"),
                           mortClass="Safety",
                           mortClassRange=c("General","Safety"),
                           tier="1", tierRange=c("1", "2", "3"),
                           sysName="this",
                           sysClass="mariPol",
                           sysClassRange=c("mariPol","pimaPol","otherPol",
                                           "mariFire","pimaFire","otherFire"),
                           verbose=FALSE) {
    out <- validateInputs(age=age, ageRange=ageRange,
                          sex=sex, sexRange=sexRange,
                          service=service, serviceRange=serviceRange,
                          status=status, statusRange=statusRange,
                          mortClass=mortClass, mortClassRange=mortClassRange,
                          tier=tier, tierRange=tierRange,
                          sysName=sysName,
                          sysClass=sysClass, sysClassRange=sysClassRange,
                          verbose=verbose);
    if (out != "") {
        stop(out);
    } else {
        return(out);
    }
}

doesMemberSeparate <- function(age, sex, service, status="active", tier="1",
                               mortClass="Safety", sysName="this", sysClass="",
                               verbose=FALSE) {

    if (verbose) cat("doesMemberSeparate: ");
    validateInputsAZ(age=age, sex=sex, service=service, status=status, tier=tier,
                     mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                     verbose=verbose);
    
    ## If this is not currently an active employee, get out.
    if (!checkStatus(status, acceptable=c("active"))) return(status);

    ## Choose probability, roll dice.
    serviceRow <- min(service, 21); ## Termination data is cardinal, not ordinal.
    if (runif(1) < terminationRatesPSPRS[serviceRow, sysClass])
        status <- "separated";

    return(status);
}

doesMemberRetire <- function(age, sex, service, status="active", tier="1",
                             mortClass="Safety", sysName="this", sysClass="",
                             verbose=FALSE) {

    if (verbose) cat("doesMemberRetire: ");
    validateInputsAZ(age=age, sex=sex, service=service, status=status, tier=tier,
                   mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                   verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

    if (service >= 20) {
        retirementProbability  <-
            retirementRatesPSPRS.with20[min(service, 34) - 19, sysClass];
    } else if (age >= 62) {
        retirementProbability <-
            retirementRatesPSPRS.wout20[min(age, 70) - 61, sysClass];
    } else {
        retirementProbability <- 0.0
    }

    if (runif(1) < retirementProbability) status <- "retired";

    return(status);
}

doesMemberBecomeDisabled <- function(age, sex, service, status, tier="1", 
                                     mortClass="Safety", sysName="this",
                                     sysClass="", verbose=FALSE) {

    if (verbose) cat("doesMemberBeceomDisabled: ");
    validateInputsAZ(age=age, sex=sex, service=service, status=status, tier=tier,
                     mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                     verbose=verbose);

    ## If already retired or dead or something, get out.
    if (checkStatus(status, acceptable=c("retired", "retired/survivor",
                                         "deceased", "disabled/accident",
                                         "disabled/ordinary"))) return(status);

    ## Select age, roll dice.
    if (age < 61) {
        disabilityProbability  <-
            disabilityRatesPSPRS[max(age-18, 1), sysClass];
    } else {
        disabilityProbability <- 0.0
    }

    if (runif(1) < disabilityProbability) status <- "disabled/accident";

    return(status);
}

doesMemberHaveSurvivor <- function(age, sex, status, service, survivor,
                                   tier="1", mortClass="Safety",
                                   sysName="this", sysClass="", 
                                   verbose=verbose) {

    if (verbose) cat("doesMemberHaveSurvivor: ");
    validateInputsAZ(age=age, sex=sex, service=service, status=status, tier=tier,
                     mortClass=mortClass, sysName=sysName, sysClass=sysClass,
                     verbose=verbose);

    ## This is essentially the default. Do we have any other information? XX
    
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


projectSalaryDelta <- function(year, age, salary, service=1, tier="1", 
                               mortClass="Safety", sysName="this", sysClass="",
                               verbose=FALSE) {

    if (verbose) cat("projectSalaryDelta: ");
    validateInputsAZ(age=age, tier=tier, mortClass=mortClass,
                     sysName=sysName, sysClass=sysClass, verbose=verbose);
    
    ## Select age
    delta <- 1 +
        as.numeric(salaryIncreaseRatesPSPRS[min(max(age, 20), 53) - 19, sysClass]);

    if (verbose) cat("  delta:", delta, "\n");
    
    return(delta);
}

projectPension <- function(salaryHistory, tier="1", mortClass="Safety",
                           cola=1.0175, sysName="this", sysClass="",
                           verbose=FALSE) {
    
    if (verbose) {
        cat("projectPension"); print((salaryHistory));
    }
    
    sex <- first(salaryHistory$sex);
    service <- sum(salaryHistory$salary > 0);

    ## Get the last status before retired.
    preRetireYear <- salaryHistory %>% filter(status=="retired") %>%
                        dplyr::summarize(retireYear=min(year)) %>%
                        as.numeric() - 1;
    preRetStatus <- salaryHistory %>%
        filter(year == preRetireYear) %>%
        select(status) %>%
        mutate_if(is.factor, as.character) %>%  ## <<-- !
        as.character() ;
    
    if (verbose) cat("projectPension: ");
    validateInputsAZ(age=min(salaryHistory$age),
                     sex=sex,
                     service=service,
                     status=preRetStatus,
                     tier=tier,
                     mortClass=mortClass,
                     sysName=sysName, sysClass=sysClass,
                     verbose=verbose);
    
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

    ## If this person never retired, send them away without a pension.
    if (!("retired" %in% salaryHistory$status))
        return(salaryHistory %>% mutate(pension = 0));

    retireYear <- as.numeric(salaryHistory %>%
                             filter(status=="retired") %>%
                             summarize(retireYear=min(year)));

    salaryHistory <- salaryHistory %>%
        mutate(pension = ifelse(status == "retired",
                                startingPension * cola^(year - retireYear),
                                0));

    ## Is this a DROP retirement? If so, just delete the last few
    ## years of contributions and reduce the starting pension.
    ## if ((tier == "1") & (runif(1) < 0.6)) {
    ##     ## Yes, a DROP.
    ##     if (verbose) cat("calculating pension as a DROP.\n")
    ##     salaryHistory <- projectDROP(salaryHistory, retireYear, preRetStatus,
    ##                                  service, tier=tier, mortClass=mortClass,
    ##                                  sysName=sysName, sysClass=sysClass,
    ##                                  verbose=verbose);
    ## }
    
    return(salaryHistory);
}

## This does not appear to be usefully correct.
projectDROP <- function(salaryHistory, retireYear, status, service, tier="1",
                        mortClass="General", sysName="this", sysClass="",
                        verbose=FALSE) {
    if (status != "active") return(salaryHistory);

    if (verbose) cat("projectDROP: doing DROP calculation for", retireYear,
                     "service:", service, "\n")
    ## DROP rules changes in 2022 to be 7 years instead of 5.
    if (retireYear >= 2022) {
        dropYears <- 7;
    } else {
        dropYears <- 5;
    }

    ## Compute the retirement age (when the real pension starts) and the
    ## service, also at retirement. This is for the purpose of undoing
    ## the last few years of salary increases.

    retireRow <- retireYear - min(salaryHistory$year) + 1;
    retireAge <- min(salaryHistory$age) + retireRow;
            
    ## Delete the last few years of salary (so the premiums
    ## will be zero, and reduce the pension amount.
    pensionDelta <- 1.0;
    for (year in 1:dropYears) {
        if (verbose) cat("projectDROP: Working on year", retireYear - year, "\n");
        pensionDelta <- pensionDelta *
            projectSalaryDelta(retireYear - year,
                               retireAge - year,
                               salaryHistory$salary[retireRow - year],
                               service=service - year,
                               tier=tier, mortClass=mortClass,
                               sysName=sysName, sysClass=sysClass,
                               verbose=verbose)
        salaryHistory$salary[retireRow - year] <- 0;
        salaryHistory$premium[retireRow - year] <- 0;
    }
    salaryHistory <- salaryHistory %>% mutate(pension = pension / pensionDelta);
    return(salaryHistory);
}


## Accepts a salary history tibble and adds a column for the estimated
## premiums paid into the system for this employee for each year.
## (Combined employer and employee share, do not include amortization
## payments.)
projectPremiums <- function(salaryHistory, adjYear=0, tier="1", mortClass="Safety",
                            sysName="this", sysClass="", verbose=FALSE) {

    if (verbose) cat("projectPremiums: tier:", tier, "mortClass:", mortClass,
                     "sysName:", sysName, "sysClass:", sysClass, "\n");

    ## Note the "!!" below, used because there is a "sysName" in contribT12.2019
    ## already. The excl points sort of 'unquote' the variable reference and point
    ## it to the environment rather than the data frame.
    salaryHistory <- salaryHistory %>% mutate(year=year + adjYear, ncRate=0.0);
    salaryHistory$ncRate[salaryHistory$year<=2019] <- .0765 + 
        contribT12.2019 %>% filter(sysNumCh==!!sysName) %>% select(erNCpct) %>%
        as.numeric();
    salaryHistory$ncRate[salaryHistory$year==2020] <- .0765 + 
        contribT12.2020 %>% filter(sysNumCh==!!sysName) %>% select(erNCpct) %>%
        as.numeric();
    salaryHistory$ncRate[salaryHistory$year==2021] <- .0765 + 
        contribT12.2021 %>% filter(sysNumCh==!!sysName) %>% select(erNCpct) %>%
        as.numeric();
    salaryHistory$ncRate[salaryHistory$year>=2022] <- .0765 + 
        contribT12.2022 %>% filter(sysNumCh==!!sysName) %>% select(erNCpct) %>%
        as.numeric();

    ## A rough estimate based on the system-wide rates.
    salaryHistory$ncRate[salaryHistory$year<=2019] <-
        .83 * salaryHistory$ncRate[salaryHistory$year==2019];
    
    return(salaryHistory %>%
           mutate(premium=salary * ncRate));
}


## The above is all about specifying the benefits. Now we model the
## actual population and then run the model.  This function uses data
## in spreadsheet form from the system to select a system and generate
## a model for it. The system name argument is actually the system
## number as a zero-padded integer.
psprsModel <- function(sysName, verbose=FALSE) {

    if (verbose) cat("building model:", date(), " for ", sysName, " (",
                     assumptionKey %>% filter(sysNumCh=="022") %>%
                     select(sysName) %>% as.character(), ")",
                     "\n", sep="");

    sysRow <- popCountsT12 %>% filter(sysNumCh==!!sysName);
    sysRowNames <- colnames(sysRow);
    sysClass <- assumptionKey %>% filter(sysNumCh==!!sysName) %>%
        select(sysType) %>% as.character();

    azModel <- memberList();
    
    for (i in 4:123) {

        ## > str_split("s25s30","s")
        ## [[1]]
        ## [1] ""   "25" "30"
        params <- str_split(sysRowNames[i], "s")[[1]];
        if (length(params) != 3) next;

        ageLim <- as.numeric(params[2]);
        if (ageLim == 99) {
            ageRange <- c(65, 75);
        } else {
            ageRange <- c(ageLim - 5, ageLim -1);
        }

        servLim <- as.numeric(params[3]);
        if (servLim == 50) {
            servRange <- c(30,45);
        } else {
            servRange <- c(servLim - 5, servLim - 1);
        }
        
        avgPay <- as.numeric(sysRow[, 3 + 10 * ceiling((i - 3)/10)]);
        targetCount <- as.numeric(sysRow[,i]);

        if (verbose) cat(">>", i, ":generating", targetCount,
                         "employees, ages", ageRange[1], "-",
                         ageRange[2], "service", servRange[1], "-",
                         servRange[2], "avgPay =", avgPay, "\n");
        azModel <- genEmployees(targetCount,
                                ageRange=ageRange, servRange=servRange,
                                avgSalary=avgPay,
                                sex=list(M=0.8, F=0.2),
                                tier="1",
                                mortClass="Safety",
                                currentYear=2022,
                                sysName=sysName, sysClass=sysClass,
                                cola=1.0185,
                                members=azModel,
                                verbose=verbose);
    }

    if (verbose) cat("finished building model", date(), "\n");
    
    return(azModel);
}

##systems <- c("022","021","029","030","007","002","003","010","012")
## systems <- c("020","023","024","025","026","027","028","031",
##              "032","033","034","035")

sys01 <- c("001","002","003","004","005","007","008","009","010");
sys02 <- c("011","012","013","014","015","016","017","018","020","021");
sys03 <- c("022","023","024","025","026","027","028","029","030","031");
sys04 <- c("032","033","034","035","036","037","038","039","040","041");
sys05 <- c("042","043","044","045","046","047","049","050","051","052");
sys06 <- c("053","054","055","056","058","059","060","061","062","064");
sys07 <- c("065","066","067","069","070","071","072","073","074","076");
sys08 <- c("077","078","079","080","081","083","085","086","087","088");
sys09 <- c("089","090","091","092","093","094","095","096","097","098");
sys10 <- c("100","101","102","103","104","105","106","107","108","109");
sys11 <- c("110","111","112","113","114","115","116","117","118","119");
sys12 <- c("120","121","122","123","124","125","126","127","128","129");
sys13 <- c("130","131","132","133","134","136","137","138","139","140");
sys14 <- c("142","143","144","145","146","147","148","149","150","151");
sys15 <- c("153","154","155","156","157","158","160","162","163","164");
sys16 <- c("165","166","167","168","169","170","171","172","173","174");
sys17 <- c("176","177","178","179","180","181","182","185","187","188");
sys18 <- c("190","192","193","194","195","196","197","198","199","200");
sys19 <- c("201","202","203","204","206","207","208","209","210","211");
sys20 <- c("212","213","214","215","216","217","221","222","223","224");
sys21 <- c("225","226","227","228","229","231","232","233","234","235");
sys22 <- c("236","237","238","239","241","242","243","244","245","246");
sys23 <- c("247","248","249","250","251","252","253","254","255","256");
sys24 <- c("257","258","259","261","262","263","264","265");

systems <- c("020")

for (s in c("007","008","009","010",sys02,sys03,sys04,sys05)) {
    sysName <- assumptionKey %>% filter(sysNumCh==s) %>% select(sysName) %>% as.character();
    sysOutput <- paste0("azModelOutput", s);
    cat(s, "-", sysName, "\n");
    eval(parse(text=paste0(sysOutput, "<-runModel(function(verbose) {psprsModel(\"", s, "\",verbose=verbose)}, N=50, audit=FALSE, reallyVerbose=FALSE, verbose=TRUE)"),"\n"));
    eval(parse(text=paste0("ggsave(\"images/modelPlot",s,".png\",plot=altPlotModelOut(",sysOutput,",system=sysName),width=5,height=5)"),"\n"));
    eval(parse(text=paste0("rm(",sysOutput,")"),"\n"));
}
