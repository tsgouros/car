## Reads data from the pubs 2010 mortality tables and uses them in a
## function we can call to roll the dice for some particular plan
## member.
##
## See https://www.soa.org/resources/research-reports/2019/pub-2010-retirement-plans/
library(tidyverse)
library(readxl)

##
## We are pulling from the headcount-weighted tables.
##

## Pull from the "General" table.
pub2010headcountMortalityRatesGeneralAge <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubG.H-2010", range="B6:B108",
               col_names=c("age"), col_types=c("numeric"))
pub2010headcountMortalityRatesGeneralFemale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubG.H-2010", range="D6:G108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")
pub2010headcountMortalityRatesGeneralMale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubG.H-2010", range="I6:L108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")

pub2010headcountMortalityRatesGeneralMale <-
    tibble(cbind(pub2010headcountMortalityRatesGeneralAge,
                 pub2010headcountMortalityRatesGeneralMale));
pub2010headcountMortalityRatesGeneralFemale <-
    tibble(cbind(pub2010headcountMortalityRatesGeneralAge,
                 pub2010headcountMortalityRatesGeneralFemale));

## Pull the "Safety" date.
pub2010headcountMortalityRatesSafetyAge <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubS.H-2010", range="B6:B108",
               col_names=c("age"), col_types=c("numeric"))
pub2010headcountMortalityRatesSafetyFemale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubS.H-2010", range="D6:G108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")
pub2010headcountMortalityRatesSafetyMale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubS.H-2010", range="I6:L108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")

pub2010headcountMortalityRatesSafetyMale <-
    tibble(cbind(pub2010headcountMortalityRatesSafetyAge,
                 pub2010headcountMortalityRatesSafetyMale));
pub2010headcountMortalityRatesSafetyFemale <-
    tibble(cbind(pub2010headcountMortalityRatesSafetyAge,
                 pub2010headcountMortalityRatesSafetyFemale));

pub2010headcountMortalityRatesTeacherAge <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubT.H-2010", range="B6:B108",
               col_names=c("age"), col_types=c("numeric"))
pub2010headcountMortalityRatesTeacherFemale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubT.H-2010", range="D6:G108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")
pub2010headcountMortalityRatesTeacherMale <-
    read_excel("../mortalityTables/pub-2010-headcount-mort-rates.xlsx",
               sheet="PubT.H-2010", range="I6:L108",
               col_names=c("active", "retired", "disabled", "survivor"),
               col_types="numeric")

pub2010headcountMortalityRatesTeacherMale <-
    tibble(cbind(pub2010headcountMortalityRatesTeacherAge,
                 pub2010headcountMortalityRatesTeacherMale));
pub2010headcountMortalityRatesTeacherFemale <-
    tibble(cbind(pub2010headcountMortalityRatesTeacherAge,
                 pub2010headcountMortalityRatesTeacherFemale));

## Given a member's age, sex, and status, rolls the dice to decide if
## they kick the bucket this year or survive to live another year.
## Use the 'mortClass' arg to select which category of table to use.
## Omitting it gets you the general healthy table.
##
## The 'weight' arg selects if you want to use the headcount weighted
## or amount weighted table.  The memberSalary is there for using the
## amount weighting, though it's not implemented yet (as of 4/21).
doesMemberDie <- function(memberAge, memberSex, memberStatus,
                          mortClass="General", memberSalary=0,
                          weight="headcount", verbose=FALSE) {

    ## If the member is already dead, they can't be deader.
    if (memberStatus == "deceased") return(memberStatus);
    
    if (verbose) cat("doesMemberDie: ", mortClass,
                     "; ", memberStatus, " member (", memberSex,
                     "), aged ", memberAge, "...", sep="");

    ## First, find the appropriate number from the mortality tables.
    if (mortClass == "Safety") {
        if (memberSex == "F") {
            table <- pub2010headcountMortalityRatesSafetyFemale;
        } else {
            table <- pub2010headcountMortalityRatesSafetyMale;
        }
    } else if (mortClass == "Teacher") {
        if (memberSex == "F") {
            table <- pub2010headcountMortalityRatesTeacherFemale;
        } else {
            table <- pub2010headcountMortalityRatesTeacherMale;
        }
    } else { ## This is for mortClass == "General" and anything else.
        if (memberSex == "F") {
            table <- pub2010headcountMortalityRatesGeneralFemale;
        } else {
            table <- pub2010headcountMortalityRatesGeneralMale;
        }
    }

    ## We aren't really interested in former members who are
    ## separated, but never retire, but they play a role, so we age
    ## them and roll their dice, too.
    tempStatus <- memberStatus
    if (memberStatus == "separated") {
        if (memberAge < 80) {
            tempStatus <- "active";
        } else {
            tempStatus <- "retired";
        }
    }

    ## There are some improbable statuses that are actually sometimes seen,
    ## so limit the member ages to the ranges in the mortality tables.
    if (memberStatus == "active") memberAge <- max(min(memberAge, 80), 18);

    ## The mortality tables don't contemplate retirement before 50,
    ## but it does happen, so...
    if (memberStatus == "retired") memberAge <- min(max(memberAge, 55), 120);

    ## We can also handle survivor and disability retirements.
    if (memberStatus == "retired/survivor") {
        tempStatus <- "survivor";
        memberAge <- min(max(memberAge, 45), 120);
    }
    if (grepl("disabled", memberStatus)) tempStatus <- "disabled";

    ## Find the threshold for this member.
    threshold <- as.numeric(table %>%
                            filter(age == memberAge) %>%
                            select(contains(tempStatus)));

    if ((length(threshold) < 1) || is.null(threshold) || is.na(threshold)) {
        stop("\nerror establishing threshold, age=", memberAge,
            " status=", tempStatus, " threshold=", threshold);
    }

    ## Roll the dice.
    outStatus <- memberStatus;
    if (runif(1) < threshold) outStatus <- "deceased";

    if (verbose)
        cat(" threshold: ",threshold, " result: ", outStatus, "\n", sep="");

    return(outStatus);
}
