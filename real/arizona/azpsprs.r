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
#popCountTableFile <- "../../../arizona/data/PSPRS_2022_CountSummaries.xlsx";
popCountTableFile <- "../../../arizona/data/PSPRS_2023_CountSummaries.xlsx";
assumptionTableFile <- "../../../arizona/data/AssumpGrouping.xlsx";
sched2019file <- "../../../arizona/data/PSPRS-ER_Exhibits-2019_TS.xlsx";
sched2020file <- "../../../arizona/data/PSPRS-ER_Exhibits-2020_TS.xlsx";
sched2021file <- "../../../arizona/data/PSPRS_ErExhibits&PresentationStats_2021_TS.xlsx";
sched2022file <- "../../../arizona/data/PSPRS_ErExhibits&PresentationStats_2022_TS.xlsx";
sched2023file <- "../../../arizona/data/PSPRS_ErExhibits&PresentationStats_2023_TS.xlsx";

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

retirementRatesPSPRST1.with20 <- read_excel(retirementRateTableFile,
                                            sheet="PSPRS", range="Q8:W22",
                                            col_names=c("service", "mariFire",
                                                        "otherFire",
                                                        "pimaFire", "mariPol",
                                                        "otherPol", "pimaPol"))
retirementRatesPSPRST1.with20$service[15] <- "34"
retirementRatesPSPRST1.with20$service <-
    as.numeric(retirementRatesPSPRST1.with20$service)

retirementRatesPSPRST1.wout20 <- read_excel(retirementRateTableFile,
                                            sheet="PSPRS", range="Q30:W38",
                                            col_names=c("age", "mariFire",
                                                        "otherFire",
                                                        "pimaFire", "mariPol",
                                                        "otherPol", "pimaPol"))
retirementRatesPSPRST1.wout20$age[9] <- "70"
retirementRatesPSPRST1.wout20$age <- as.numeric(retirementRatesPSPRST1.wout20$age)

retirementRatesPSPRST2 <- read_excel(retirementRateTableFile,
                                     sheet="PSPRS", range="Y8:Z20",
                                     col_names=c("age", "rate"));
retirementRatesPSPRST2$age[1] <- "52";
retirementRatesPSPRST2$age[13] <- "64";
retirementRatesPSPRST2$age <- as.numeric(retirementRatesPSPRST2$age)


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
## Cheating here for a plan that joined PSPRS in 2021. Copying from ASU
## Campus Police, which seems to be about the same size.
contribT12.2019 <- rbind(contribT12.2019,
                         tibble(sysNum=265,sysNumCh="265",
                                sysName="Queen Creek Police Dept.",
                                erNCpct=0.1476,UALpmtPct=0.3072,
                                amortPeriod=17,calcERcont=0.4548,
                                reqERcont=0.4548,snum="s265"));
contribT12.2020 <- rbind(contribT12.2020,
                         tibble(sysNum=265,sysNumCh="265",
                                sysName="Queen Creek Police Dept.",
                                erNCpct=0.1386,UALpmtPct=0.328,
                                amortPeriod=16,calcERcont=0.4666,
                                reqERcont=0.4666,snum="s265"));


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
contribT12.2023 <- readxl::read_excel(sched2023file,
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

## Now do tier 3
contribT3.2019 <- readxl::read_excel(sched2019file,
                                     sheet="Tier3Contrib\ Pension",
                                     range="A4:H233",
                                     col_names=c("sysNum",
                                                 "sysNumCh",
                                                 "sysName",
                                                 "totNCpct",
                                                 "UALpmtPct",
                                                 "totalContPct",
                                                 "eeNCpct",
                                                 "erNCpct")) %>%
    filter(!is.na(sysNumCh)) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT3.2020 <- readxl::read_excel(sched2020file,
                                     sheet="Tier3Contrib\ Pension",
                                     range="A4:H233",
                                     col_names=c("sysNum",
                                                 "sysNumCh",
                                                 "sysName",
                                                 "totNCpct",
                                                 "UALpmtPct",
                                                 "totalContPct",
                                                 "eeNCpct",
                                                 "erNCpct")) %>%
    filter(!is.na(sysNumCh)) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT3.2021 <- readxl::read_excel(sched2021file,
                                     sheet="Tier3Contrib\ Pension",
                                     range="A4:H233",
                                     col_names=c("sysNum",
                                                 "sysNumCh",
                                                 "sysName",
                                                 "totNCpct",
                                                 "UALpmtPct",
                                                 "totalContPct",
                                                 "eeNCpct",
                                                 "erNCpct")) %>%
    filter(!is.na(sysNumCh)) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT3.2022 <- readxl::read_excel(sched2022file,
                                     sheet="Tier3Contrib\ Pension",
                                     range="A4:H23",
                                     col_names=c("sysNum",
                                                 "sysNumCh",
                                                 "sysName",
                                                 "totNCpct",
                                                 "UALpmtPct",
                                                 "totalContPct",
                                                 "eeNCpct",
                                                 "erNCpct")) %>%
    mutate(snum=paste0("s", sysNumCh));
contribT3.2023 <- readxl::read_excel(sched2023file,
                                     sheet="Tier3Contrib\ Pension",
                                     range="A4:H23",
                                     col_names=c("sysNum",
                                                 "sysNumCh",
                                                 "sysName",
                                                 "totNCpct",
                                                 "UALpmtPct",
                                                 "totalContPct",
                                                 "eeNCpct",
                                                 "erNCpct")) %>%
    mutate(snum=paste0("s", sysNumCh));


fundingT12.2023 <- readxl::read_excel(sched2023file,
                                         sheet="Tier12AL_AVA\ Pension",
                                         range="A4:K231",
                                         col_names=c("sysNum",
                                                     "sysNumCh",
                                                     "sysName",
                                                     "PVB","AAL","AVA",
                                                     "MV","UAL","UML",
                                                     "FPA","FPM")) %>%
    mutate(snum=paste0("s", sysNumCh));

fundingT3.2023 <- readxl::read_excel(sched2023file,
                                        sheet="Tier3AL_AVA\ Pension",
                                        range="A4:K23",
                                         col_names=c("sysNum",
                                                     "sysNumCh",
                                                     "sysName",
                                                     "PVB","AAL","AVA",
                                                     "MV","UAL","UML",
                                                     "FPA","FPM")) %>%
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

    if (tier == "1") {
        if (service >= 20) {
            retirementProbability  <-
                retirementRatesPSPRST1.with20[min(service, 34) - 19, sysClass];
        } else if ((age >= 62) && (service >= 15)) {
            retirementProbability <-
                retirementRatesPSPRST1.wout20[min(age, 70) - 61, sysClass];
        } else {
            retirementProbability <- 0.0
        }
    } else if (tier == "2") {
        if ((age >= 52) && (service >= 15)) {
            retirementProbability  <-
                retirementRatesPSPRST2[min(age, 64) - 51, "rate"];
        } else {
            retirementProbability <- 0.0;
        }
    } else if (tier == "3") {
        if ((age >= 55) && (service >= 15)) {
            retirementProbability  <-
                retirementRatesPSPRST2[min(age, 64) - 51, "rate"];
        } else {
            retirementProbability <- 0.0;
        }
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

    ## If already retired or dead or something, get out. Status "separated"
    ## doesn't count either.
    if (checkStatus(status,
                    acceptable=c("retired", "retired/survivor",
                                 "deceased", "disabled/accident",
                                 "separated", "disabled/ordinary")))
        return(status);

    ## Select age, roll dice.
    if (age < 61) {
        disabilityProbability  <-
            disabilityRatesPSPRS[max(age-18, 1), sysClass];
    } else {
        disabilityProbability <- 0.0
    }

    if (runif(1) < disabilityProbability) {
        if (runif(1) > 0.9) {
            status <- "disabled/ordinary";
        } else {
            status <- "disabled/accident";
        }
    }
    
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

## This is the "Average Monthly Benefit Compensation".
calcBaseSalary <- function(salaryHistory, tier) {

        ## Calculate the base salary from which to calculate the pension.
    if (tier == "1") {
        # Find the maximum 3-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);
        threeYears <- c(s, 0, 0) + c(0, s, 0) + c(0, 0, s);
        baseSalary <- max(threeYears) / 3.0;
    } else if (tier == "2") {
        # Find the maximum 5-year period.
        s <- tail(salaryHistory$salary[salaryHistory$salary > 0], 20);

        fiveYears <-
            c(s, 0, 0, 0, 0) +
            c(0, s, 0, 0, 0) +
            c(0, 0, s, 0, 0) +
            c(0, 0, 0, s, 0) +
            c(0, 0, 0, 0, s);
        baseSalary <- max(fiveYears) / 5.0;

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
        baseSalary <- max(fiveYears) / 5.0;
    }

    return(baseSalary);
}

calcPension <- function(baseSalary, tier, service) {

    ## We have a base salary. Now calculate the pension from the base salary.
    if (tier == "1") {

        startingPension <- 0.5 * baseSalary;
        
        if ((service >= 15) && (service < 20)) {
            startingPension <- startingPension * (1 - ((20 - service) * 0.04));
        } else if ((service >= 20) && (service < 25)) { ## 20-25
            startingPension <- startingPension * (1 + ((service - 20) * 0.02));
        } else if (service >= 25) {
            startingPension <- startingPension * (1 + ((service - 20) * 0.025));
        } else {
            cat("shouldn't be here\n");
        }
    } else if ((tier == "2") || (tier == "3")) {
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
            benefitMultiplier <- 1/service;

            cat("Unexpected service in tier 2\n");
            #print(as.data.frame(salaryHistory));
            cat("tier:", tier, "service:", service, "\n");
        }

        startingPension <- baseSalary * (service * benefitMultiplier);

    } else if (tier == "0") {
        ## The tier 0 people can retire again but they don't get
        ## another pension.
        startingPension <- 0.0;
    } else {
        cat("Don't know this tier: ", tier, "\n", sep="");
    }

    startingPension <- min(0.8 * baseSalary, startingPension);

    return(startingPension);
}

projectDisabilityPension <- function(salaryHistory, tier="1",
                                     mortClass="Safety", cola=1.0185,
                                     sysName="this", sysClass="",
                                     verbose=FALSE) {

    ## Disabilities in the line of duty get you the larger of 50% of
    ## Average Monthly Benefit Compensation and the monthly Normal
    ## Retirement pension that the member is entitled to receive if he
    ## or she retired immediately.
    sex <- first(salaryHistory$sex);
    service <- sum(salaryHistory$salary > 0);

    disabledStatuses <- c("disabled","disabled/accident","disabled/ordinary");

    ## Get the last status before retired.
    preDisabledYear <- salaryHistory %>%
        filter(status %in% disabledStatuses) %>%
        dplyr::summarize(retireYear=min(year)) %>%
        as.numeric() - 1;
    preDisabledStatus <- salaryHistory %>% filter(year == preDisabledYear) %>%
        select(status) %>%
        mutate_if(is.factor, as.character) %>%  ## <<-- !
        as.character() ;
    preDisabledAge <- salaryHistory %>% filter(year == preDisabledYear) %>%
        select(age) %>% as.numeric();
    
    if (verbose) cat("projectDisabilityPension: ");
    validateInputsAZ(age=min(salaryHistory$age),
                     sex=sex,
                     service=service,
                     status=preDisabledStatus,
                     tier=tier,
                     mortClass=mortClass,
                     sysName=sysName, sysClass=sysClass,
                     verbose=verbose);

    ## This is the "average monthly benefit compensation"
    baseSalary <- calcBaseSalary(salaryHistory, tier);
    disabilityPension <- 0.5 * baseSalary;

    retirementPension <- 0;

    ## Actuaries assume that 90% of disabilities are work-related. For
    ## the non-work related disabilities, it's just the standard
    ## retirement pension.
    if (runif(1) > 0.9) disabilityPension <- retirementPension;
    
    ## If this person is eligible for retirement, figure that out.
    if (tier == "1") {
        if ((service >= 20) | ((preDisabledAge >= 62) && (service >= 15))) {
            retirementPension <- calcPension(baseSalary, tier, service);
            disabilityPension <- max(disabilityPension, retirementPension);
        }
    } else if (tier == "2") {
        if ((preDisabledAge >= 52) && (service >= 15)) {
            retirementPension <- calcPension(baseSalary, tier, service);
            disabilityPension <- max(disabilityPension, retirementPension);
        }
    } else if (tier == "3") {
        if ((preDisabledAge >= 55) && (service >= 15)) {
            retirementPension <- calcPension(baseSalary, tier, service);
            disabilityPension <- max(disabilityPension, retirementPension);
        }
    }
    
    ## If this person was never disabled, send them away without a pension.
    if (!(Reduce("|", disabledStatuses %in% salaryHistory$status)))
        return(salaryHistory %>% mutate(pension = 0));

    retireYear <- as.numeric(salaryHistory %>%
                             filter(status %in% disabledStatuses) %>%
                             summarize(retireYear=min(year)));

    salaryHistory <- salaryHistory %>%
        mutate(pension = ifelse(status %in% disabledStatuses,
                                disabilityPension * cola^(year - retireYear),
                                0));

    return(salaryHistory);

}
    
projectRetirementPension <- function(salaryHistory, tier="1",
                                     mortClass="Safety", cola=1.0185,
                                     sysName="this", sysClass="",
                                     verbose=FALSE) {

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
    
    if (verbose) cat("projectRetirementPension: ");
    validateInputsAZ(age=min(salaryHistory$age),
                     sex=sex,
                     service=service,
                     status=preRetStatus,
                     tier=tier,
                     mortClass=mortClass,
                     sysName=sysName, sysClass=sysClass,
                     verbose=verbose);

    
    baseSalary <- calcBaseSalary(salaryHistory, tier);
    
    startingPension <- calcPension(baseSalary, tier, service);
    
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

projectPension <- function(salaryHistory, tier="1", mortClass="Safety",
                           cola=1.0185, sysName="this", sysClass="mariPol",
                           verbose=FALSE) {
    
    if (verbose) {
        cat("projectPension\n"); print(as.data.frame(salaryHistory));
    }
    
    ## Are we calculating a disability pension?
    if (Reduce("|", unique(salaryHistory$status) %in%
                    c("disabled","disabled/accident","disabled/ordinary"))) {
        salaryHistory <-
            projectDisabilityPension(salaryHistory,
                                     tier=tier, mortClass=mortClass,
                                     cola=cola, sysName=sysName,
                                     sysClass=sysClass, verbose=verbose);
    } else {
        ## Must be a retirement pension.
        salaryHistory <-
            projectRetirementPension(salaryHistory,
                                     tier=tier, mortClass=mortClass,
                                     cola=cola, sysName=sysName,
                                     sysClass=sysClass, verbose=verbose);
    }
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
projectPremiums <- function(salaryHistory, adjYear=0, tier="1",
                            mortClass="Safety", sysName="this",
                            sysClass="", verbose=FALSE) {
    ## Note that the sysName we refer to is actually sysNumCh.
    
    if (verbose) cat("projectPremiums: tier:", tier, "mortClass:", mortClass,
                     "sysName:", sysName, "sysClass:", sysClass, "\n");

    salaryHistory <- salaryHistory %>%
        mutate(year=year + adjYear, ncRate=0.0);
    
    if ((tier == "1") | (tier == "2")) {
        ## Note the "!!" below, used because there is a "sysName" in
        ## contribT12.2019 already. The excl points sort of 'unquote' the
        ## variable reference and point it to the environment rather than
        ## the data frame.
        salaryHistory$ncRate[salaryHistory$year<=2019] <- .0765 + 
            contribT12.2019 %>% filter(sysNumCh==!!sysName) %>%
            select(erNCpct) %>% as.numeric();
        salaryHistory$ncRate[salaryHistory$year==2020] <- .0765 + 
            contribT12.2020 %>% filter(sysNumCh==!!sysName) %>%
            select(erNCpct) %>% as.numeric();
        salaryHistory$ncRate[salaryHistory$year==2021] <- .0765 + 
            contribT12.2021 %>% filter(sysNumCh==!!sysName) %>%
            select(erNCpct) %>% as.numeric();
        salaryHistory$ncRate[salaryHistory$year==2022] <- .0765 + 
            contribT12.2022 %>% filter(sysNumCh==!!sysName) %>%
            select(erNCpct) %>% as.numeric();
        salaryHistory$ncRate[salaryHistory$year>=2023] <- .0765 + 
            contribT12.2023 %>% filter(sysNumCh==!!sysName) %>%
            select(erNCpct) %>% as.numeric();

    } else if (tier == "3") {

        if (sysName %in% contribT3.2019$sysNumCh) {
            salaryHistory$ncRate[salaryHistory$year<=2019] <-
                contribT3.2019 %>% filter(sysNumCh==!!sysName) %>%
                select(totNCpct) %>% as.numeric();
        } else {
            salaryHistory$ncRate[salaryHistory$year<=2019] <-
                contribT3.2019 %>% filter(sysName=="Risk Sharing") %>%
                select(totNCpct) %>% as.numeric();
        }
        if (sysName %in% contribT3.2020$sysNumCh) {
            salaryHistory$ncRate[salaryHistory$year==2020] <-
                contribT3.2020 %>% filter(sysNumCh==!!sysName) %>%
                select(totNCpct) %>% as.numeric();
        } else {
            salaryHistory$ncRate[salaryHistory$year==2020] <-
                contribT3.2020 %>% filter(sysName=="Risk Sharing") %>%
                select(totNCpct) %>% as.numeric();
        }
        if (sysName %in% contribT3.2021$sysNumCh) {
            salaryHistory$ncRate[salaryHistory$year==2021] <-
                contribT3.2021 %>% filter(sysNumCh==!!sysName) %>%
                select(totNCpct) %>% as.numeric();
        } else {
            salaryHistory$ncRate[salaryHistory$year==2021] <-
                contribT3.2021 %>% filter(sysName=="Risk Sharing") %>%
                select(totNCpct) %>% as.numeric();
        }
        if (sysName %in% contribT3.2022$sysNumCh) {
            salaryHistory$ncRate[salaryHistory$year==2022] <-
                contribT3.2022 %>% filter(sysNumCh==!!sysName) %>%
                select(totNCpct) %>% as.numeric();
        } else {
            salaryHistory$ncRate[salaryHistory$year==2022] <-
                contribT3.2022 %>% filter(sysName=="Risk Sharing") %>%
                select(totNCpct) %>% as.numeric();
        }
        if (sysName %in% contribT3.2023$sysNumCh) {
            salaryHistory$ncRate[salaryHistory$year>=2023] <-
                contribT3.2023 %>% filter(sysNumCh==!!sysName) %>%
                select(totNCpct) %>% as.numeric();
        } else {
            salaryHistory$ncRate[salaryHistory$year>=2023] <-
                contribT3.2023 %>% filter(sysName=="Risk Sharing") %>%
                select(totNCpct) %>% as.numeric();
        }
    
        
    }
    ## There might be tier 0, but they have no NC, so ncRate=0 is good.

    ## A rough estimate based on the system-wide rates.
    salaryHistory$ncRate[salaryHistory$year<2019] <-
        .83 * salaryHistory$ncRate[salaryHistory$year==2019];
    
    return(salaryHistory %>%
           mutate(premium=salary * ncRate));
}


## The above is all about specifying the benefits. Now we model the
## actual population and then run the model.  This function uses data
## in spreadsheet form from the system to select a system and generate
## a model for it. The system name argument is actually the system
## number as a zero-padded integer.
psprsModelT12 <- function(sysName, verbose=FALSE) {

    if (verbose) cat("building model:", date(), " for ", sysName, " (",
                     assumptionKey %>% filter(sysNumCh==!!sysName) %>%
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
                                ageRange=ageRange,
                                servRange=servRange,
                                avgSalary=avgPay,
                                sex=list(M=0.8, F=0.2),
                                tier=function(year,age,hireYear,service) {
                                    if (hireYear <= 2010) {
                                        return("1");
                                    } else {
                                        return("2");
                                    }
                                },                                        
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

## A tier 3 version. 
psprsModelT3 <- function(sysName, verbose=FALSE) {

    if (verbose) cat("building model:", date(), " for ", sysName, " (",
                     assumptionKey %>% filter(sysNumCh==!!sysName) %>%
                     select(sysName) %>% as.character(), ")",
                     "\n", sep="");

    sysRow <- popCountsT3 %>% filter(sysNumCh==!!sysName);
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
                                ageRange=ageRange,
                                servRange=servRange,
                                avgSalary=avgPay,
                                sex=list(M=0.8, F=0.2),
                                tier="3",                                 
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

## Original "results" table produced with:
## assumptionKey %>%
##   left_join(popCountsT12 %>%
##             mutate(NT12=totalcount) %>%
##             select(sysNum,NT12), by="sysNum") %>%
##   mutate(carT12=NA) %>%
##   left_join(popCountsT3 %>%
##             mutate(NT3=totalcount) %>%
##             select(sysNum,NT3), by="sysNum") %>%
##   mutate(carT3=NA) %>%
##   write.csv("results.csv",row.names=FALSE)

## This next step takes a few days. Commented out now.
#######################################################################
## results <- read.csv("results.csv",
##                     colClasses=c("integer",
##                                  rep("character",3),
##                                  rep("factor",3),
##                                  "integer","numeric",
##                                  "integer","numeric")) %>%
##     as_tibble()

## modelOutputsT12 <- list();
## modelOutputsT3 <- list();

## sys01 <- c("001","002","003","004","005","007","008","009","010");
## sys02 <- c("011","012","013","014","015","016","017","018","020","021");
## sys03 <- c("022","023","024","025","026","027","028","029","030","031");
## sys04 <- c("032","033","034","035","036","037","038","039","040","041");
## sys05 <- c("042","043","044","045","046","047","049","050","051","052");
## sys06 <- c("053","054","056","059","060","061","064");
## sys07 <- c("065","066","067","069","070","071","072","073","074","076");
## sys08 <- c("077","078","079","080","081","085","086","087","088");
## sys09 <- c("089","090","091","092","093","094","095","096","097","098");
## sys10 <- c("100","101","102","103","104","105","106","107","108","109");
## sys11 <- c("110","111","112","113","114","115","116","117","118","119");
## sys12 <- c("120","121","122","123","124","125","126","127","128","129");
## sys13 <- c("130","131","132","133","134","136","137","138","139","140");
## sys14 <- c("142","143","144","145","146","147","148","149","150","151");
## sys15 <- c("153","154","155","156","157","158","160","162","163","164");
## sys16 <- c("165","166","167","168","169","170","171","172","173","174");
## sys17 <- c("176","177","178","179","180","181","182","185","187","188");
## sys18 <- c("190","192","193","194","195","196","197","198","199","200");
## sys19 <- c("201","202","203","204","206","207","208","209","210","211");
## sys20 <- c("212","213","214","215","216","217","221","222","223","224");
## sys21 <- c("225","226","227","228","229","231","232","233","234","235");
## sys22 <- c("236","237","238","239","241","242","243","244","245","246");
## sys23 <- c("247","248","249","250","251","252","253","254","255","256");
## sys24 <- c("257","258","259","261","262","263","264","265");


## ##for (s in c(sys09,sys10,sys11,sys12)) {
## ##for (s in c("074",sys08,sys09)) {


## for (s in c("265")) {

##     sysName <- assumptionKey %>%
##         filter(sysNumCh==s) %>% select(sysName) %>% as.character();

##     ## Announce T12
##     cat(s, "-", sysName, "processing Tiers 1,2 \n");

##     doT12 <- TRUE;
##     if (is.na((results %>% filter(sysNumCh==s))$NT12==0)) {
##         cat("NA members\n");
##         doT12 <- FALSE;
##     } else if ((results %>% filter(sysNumCh==s))$NT12==0) {
##         cat("No members\n");
##         doT12 <- FALSE;
##     } else if (!is.na((results %>% filter(sysNumCh==s))$carT12)) {
##         cat("Already got this one!\n");
##         doT12 <- FALSE;
##     }

##     if (doT12) {
##         res <- try(modelOutputsT12[[s]] <-
##                        runModel(function(verbose) {
##                            psprsModelT12(s, verbose=verbose)
##                        },
##                        N=50, audit=FALSE, reallyVerbose=FALSE, verbose=TRUE));
##         if(class(res) == "try-error") {
##             cat("Error at:", s, "-", sysName, "\n");
##         } else {

##             ggsave(paste0("images/modelPlotT12-",s,".png"),
##                    plot=altPlotModelOut(modelOutputsT12[[s]]$output,
##                                         system=paste0(sysName, "/T1,T2")),
##                    width=5,height=5);
    
##             results[which(results$sysNumCh==s),"carT12"] <-
##                 modelOutputsT12[[s]]$output %>%
##                 group_by(ryear) %>%
##                 dplyr::summarize(car=mean(car,na.rm=TRUE)) %>%
##                 filter(ryear==1000) %>%
##                 select(car) %>% as.numeric();
    
##             write.csv(results,file="results.csv",row.names=FALSE);
##         }
##     }
##     ## Announce T3
##     cat(s, "-", sysName, "processing Tier 3 \n");

##     doT3 <- TRUE;
##     if (is.na((results %>% filter(sysNumCh==s))$NT3==0)) {
##         cat("NA members\n");
##         doT3 <- FALSE;
##     } else if ((results %>% filter(sysNumCh==s))$NT3==0) {
##         cat("No members\n");
##         doT3 <- FALSE;
##     } else if (!is.na((results %>% filter(sysNumCh==s))$carT3)) {
##         cat("Already got this one!\n");
##         doT3 <- FALSE;
##     }

##     if (doT3) {
##         res <- try(modelOutputsT3[[s]] <-
##                        runModel(function(verbose) {
##                            psprsModelT3(s, verbose=verbose)
##                        },
##                        N=50, audit=FALSE, reallyVerbose=FALSE, verbose=TRUE));
##         if(class(res) == "try-error") {
##             cat("Error at:", s, "-", sysName, "\n");
##         } else {

##             ggsave(paste0("images/modelPlotT3-",s,".png"),
##                    plot=altPlotModelOut(modelOutputsT3[[s]]$output,
##                                         system=paste0(sysName, "/T3")),
##                    width=5,height=5);
    
##             results[which(results$sysNumCh==s),"carT3"] <-
##                 modelOutputsT3[[s]]$output %>%
##                 group_by(ryear) %>%
##                 dplyr::summarize(car=mean(car,na.rm=TRUE)) %>%
##                 filter(ryear==1000) %>%
##                 select(car) %>% as.numeric();
    
##             write.csv(results,file="results.csv",row.names=FALSE);
##         }
##     }
## }
#######################################################################


avgT12 <- results %>%
    filter(!is.na(carT12)) %>%
    mutate(sT12=NT12*carT12) %>%
    select(NT12,sT12) %>%
    dplyr::summarize(NT12=sum(NT12,na.rm=TRUE),
                     sT12=sum(sT12,na.rm=TRUE)) %>%
    mutate(avgT12=sT12/NT12)

avgT3 <- results %>%
    filter(!is.na(carT3)) %>%
    mutate(sT3=NT3*carT3) %>%
    select(NT3,sT3) %>%
    dplyr::summarize(NT3=sum(NT3,na.rm=TRUE),
                     sT3=sum(sT3,na.rm=TRUE)) %>%
    mutate(avgT3=sT3/NT3)


enhancedResults <- results %>%
    left_join(fundingT3.2023 %>%
              select(sysNumCh,FPA),by="sysNumCh") %>%
    mutate(RP=ifelse(is.na(FPA),1,0));

carSummaryPlot <- enhancedResults %>%
    ggplot() +
    geom_point(aes(x=log10(NT12),y=carT12),color="red") +
    geom_point(aes(x=log10(NT3),y=carT3,color=RP)) + 
    geom_hline(yintercept=as.numeric(avgT12["avgT12"]), color="red") +
    annotate("text",
             label=sprintf("%5.1f%%", as.numeric(avgT12["avgT12"])*100),
             x=3.4, y=as.numeric(avgT12["avgT12"]), color="red",
             vjust=1.25,hjust=1) +
    geom_hline(yintercept=as.numeric(avgT3["avgT3"]), color="blue") +
    annotate("text",
             label=sprintf("%5.1f%%", as.numeric(avgT3["avgT3"])*100),
             x=3.4, y=as.numeric(avgT3["avgT3"]), color="blue",
             vjust=1.25,hjust=1) +
    theme(legend.position="NONE") +
    ylim(c(0.035,0.095)) +
    labs(x="Log number of members",
         y="CAR Tier 1,2 in red, Tier 3 in blue");

ggsave("images/carSummary.png", plot=carSummaryPlot,
       width=5, height=5);



carVfpaT3 <- results %>%
    left_join(fundingT3.2023 %>%
              select(sysNumCh,PVB,AAL,AVA,UAL,FPA),by="sysNumCh") %>%
    ## The only FPA==NA are the members of the risk-sharing pool.
    mutate(FPA=ifelse(is.na(FPA),1.0733616,FPA)) %>%
    ggplot() +
    geom_point(aes(x=carT3,y=FPA)) +
    ylim(c(0,1.5)) + xlim(c(0.035,0.095))

carVfpaT12 <- results %>%
    left_join(fundingT12.2023 %>%
              select(sysNumCh,PVB,AAL,AVA,UAL,FPA),by="sysNumCh") %>%
    ggplot() +
    geom_point(aes(x=carT12,y=FPA)) +
    ylim(c(0,1.5))

ggsave("images/carVfpaT3.png", plot=carVfpaT3,
       width=5, height=5);
ggsave("images/carVfpaT12.png", plot=carVfpaT12,
       width=5, height=5);
