library(readxl)
library(dplyr)
source("car.r")
source("qc.r")


tmp <- read_excel("../../../../21/pension/pawg/Queen_Creek_Fire_2.xlsx", sheet="Queen_Creek_Fire");
qcMemberData <- tmp %>%
    mutate(year=as.numeric(substr(ppe_dt, 1, 4)),
           birth=as.numeric(substr(dob, 1, 4)),
           name=full_name) %>%
    group_by(full_name, year) %>%
    summarize(dob=first(birth),
              name=first(name),
              tier=as.character(first(tier_no)),
              status=last(employment_status),
              ee=sum(ee_amount, na.rm=TRUE),
              er=sum(er_amount, na.rm=TRUE),
              salary=sum(pensionable_salary,na.rm=TRUE),
              .groups="drop") %>%
    group_by(name) %>%
    summarize(year=year,
              birthYear=dob,
              tier=tier,
              status=status,
              hireYear=first(year),
              service=year - hireYear + 1,
              ## We have data through 5/2021.
              ee=ifelse(year==2021,ee*(12/5), ee),
              er=ifelse(year==2021,er*(12/5), er),
              salary=ifelse(year==2021,salary*(12/5), salary),
              .groups="drop")


##
## Takes a table of member data and converts it to a bunch of member
## objects, and returns them.  This is specific to the format in which
## we received the Queen Creek data.
##
genEmployeesFromData <- function(memberTbl, verbose=FALSE) {

    members = memberList();

    for (uname in unique(memberTbl$name)) {
        memberScalarData <- memberTbl %>%
            filter(name==uname) %>%
            group_by(name) %>%
            summarize(birthYear=first(birthYear),
                      hireYear=first(hireYear),
                      tier=as.character(first(tier)),
                      status=last(status))

        if (verbose) print(memberScalarData);

        memberStatus <- memberScalarData %>% select(status);

        if (memberStatus == "Refunded") {
            ## Skip this one.
            next;
        } else if (memberStatus == "KIA") {
            memberStatus <- "deceased";
        } else if (memberStatus == "Active") {
            memberStatus <- "active";
        } else if (memberStatus == "Quit/Terminated") {
            memberStatus <- "separated";
        } else if (memberStatus == "Resume Service") {
            memberStatus <- "active";
        } else if (memberStatus == "Retired") {
            memberStatus <- "retired";
        } else if (memberStatus == "Transferred") {
            ## Skip this one, too.
            next;      ## memberStatus <- "separated";
        }

        memberSalaryHistory <- memberTbl %>%
            filter(name==uname) %>%
            mutate(premium = ee + er,
                   age = year - birthYear,
                   status = ifelse(salary > 0, "active", status)) %>%
            select(year, salary, age, service, status, premium)

        if (verbose) print(memberSalaryHistory)

        m <- member(salaryHistory=memberSalaryHistory,
                    currentYear=2021,
                    birthYear = memberScalarData %>%
                        select(birthYear) %>% as.numeric(),
                    hireYear = memberScalarData %>%
                        select(hireYear) %>% as.numeric(),
                    tier = memberScalarData %>%
                        select(tier),
                    mortClass="Safety",
                    note=uname,
                    sex="M",
                    status = memberStatus,
                    verbose=verbose)

        members[[m$id]] <- m;

        if (verbose) print(m);
    }

    return(members);
}

##qcFireFromData <- genEmployeesFromData(qcMemberData, verbose=FALSE)
cat(date(),"\n");
qcModelOutputFromDataAlt <-
    runModel(function(verbose=FALSE) {
        genEmployeesFromData(qcMemberData, verbose=verbose)
    }, verbose=FALSE, N=5);
##qcModelPlotFromData <- plotModelOut(qcModelOutputFromData)
cat(date(), "done with all\n");

## qcModelOutputFromDataTierOne <-
##     runModel(function(verbose=FALSE) {
##         genEmployeesFromData(qcMemberData %>% filter(tier==1), verbose=verbose)
##     }, verbose=FALSE, N=250, sampler=function(m) { m$tier==1 });
## cat(date(), "done with one\n");

## qcModelOutputFromDataTierTwo <-
##     runModel(function(verbose=FALSE) {
##         genEmployeesFromData(qcMemberData %>% filter(tier==2), verbose=verbose)
##     }, verbose=FALSE, N=250, sampler=function(m) { m$tier==2 });
## cat(date(), "done with two\n");

## qcModelOutputFromDataTierThree <-
##     runModel(function(verbose=FALSE) {
##         genEmployeesFromData(qcMemberData %>% filter(tier==3), verbose=verbose)
##     }, verbose=FALSE, N=250, sampler=function(m) { m$tier==3 });
## cat(date(), "done with three\n");

cat(date(),"\n");
date()


dotplot.all.alt <- altPlotModelOut(qcModelOutputFromDataAlt)
ggsave("../../report/images/qc-from-data.png",
       plot=dotplot.all,
       device="png",
       width=5.5, height=4, units="in")

## dotplot.one <- plotModelOut(qcModelOutputFromDataTierOne)
## ggsave("car-tier-1.png",
##        plot=dotplot.one,
##        device="png",
##        width=5.5, height=4, units="in")

## dotplot.two <- plotModelOut(qcModelOutputFromDataTierTwo)
## ggsave("car-tier-2.png",
##        plot=dotplot.two,
##        device="png",
##        width=5.5, height=4, units="in")

## dotplot.three <- plotModelOut(qcModelOutputFromDataTierThree)
## ggsave("car-tier-3.png",
##        plot=dotplot.three,
##        device="png",
##        width=5.5, height=4, units="in")



