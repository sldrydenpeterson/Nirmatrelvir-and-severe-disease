library(tidyverse)
library(readxl)
library(broom)
library(janitor)
library(lubridate)
library(tableone)
library(flextable)
library(officer)
library(ggsci)
library(survival)
library(survminer)
library(gtsummary)
library(adjustedCurves)
library(riskRegression)
library(pammtools)
library(showtext)
font_add_google("Roboto", "Roboto")
fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}
`%!in%` = Negate(`%in%`)

enclave.clean<- enclave.total %>%
  
  #setup cohort (dataset pre-filtered, not required to run these)
  filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
  filter(!is.na(bmi)) %>% # include only those with weight in past 2yrs
  filter(age.at.covid >=50) %>%  # include those 50 and older
  filter(state == "Massachusetts" | state == "New Hampshire") %>%  #include those with positive recorded test Jan 21 or later, and residing in MA/NH
  filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+30) %>%  # remove those that received molnupiravir within 30d of covid diagnosis
  filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+30) %>%      # remove those that received sotrovimab within 30d of covid diagnosis
  filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+30) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
  filter(is.na(paxlovid_date) |  paxlovid_date <  covid_date+2) %>%      # remove those that received paxlovid more then 1 day after covid diagnosis
  filter(outpatient.remdesivir == 0) %>%
  filter(contraindicated != 1) %>%  # remove those with eGFR <30 and with contraindicated meds
  
  select(EMPI, seqn, MASS.cat, age.cat, vaxstatus.collapsed, vaxstatus, lastvaxgt20wks, race.eth,  highADI, outpatient.coviddx, paxlovid, paxlovid_date,
         covid_admit, covid_admit_death, covid_date, admit_date, death.4wks, death.2wks, death_date, sex, obesity.cat, MASS, prior_covid, prior_covid_date, eGFR, MASS.ic, MASS.dm, age.at.covid,
         CVD, pulm.dis, psych.non.unipolar, depr.anxiety, heme.malig, solid.malig, inflam.dis, studyperiod)


# Generate weights for outpatient diagnosis (diagnosis 2 cal days before hospitalization/death) of COVID


# check positivity assumption
enclave.clean %>% count(outpatient.coviddx, MASS.cat)
enclave.clean %>% count(outpatient.coviddx, age.cat)
enclave.clean %>% count(outpatient.coviddx, vaxstatus.collapsed)
enclave.clean %>% count(outpatient.coviddx, lastvaxgt20wks)
enclave.clean %>% count(outpatient.coviddx, race.eth)
enclave.clean %>% count(outpatient.coviddx, highADI)
enclave.clean %>% count(outpatient.coviddx, studyperiod)



# logistic model to generate denominator of IP weights, probability of outpatient covid diagnosis

denom.fit <- glm(
  outpatient.coviddx ~ as.factor(MASS.cat) + as.factor(age.cat) + as.factor(vaxstatus.collapsed)  
  + as.factor(lastvaxgt20wks) + as.factor(race.eth) + as.factor(highADI) + as.factor(studyperiod) ,
  family = binomial(),
  data = enclave.clean %>% mutate(race.eth = fct_relevel(race.eth, "White"))
)
summary(denom.fit)
tidy(denom.fit, conf.int = TRUE, exponentiate = TRUE)

#denominator
pd.outpt <- predict(denom.fit, type = "response")

# numerator of stabilized weights, overall probability of outpt diagnosis of covid
numer.fit <- glm(outpatient.coviddx ~ 1, family = binomial(), data =enclave.clean)
pn.outpt <- predict(numer.fit, type = "response")


# calculate stabilized weights
enclave.clean$sw.outpt <-
  if_else(enclave.clean$outpatient.coviddx == 0, ((1- pn.outpt) / (1-pd.outpt)),
          (pn.outpt / pd.outpt))

quantile(enclave.clean$sw.outpt, c(0.025, 0.5, 0.975))
mean(enclave.clean$sw.outpt)
max(enclave.clean$sw.outpt)
min(enclave.clean$sw.outpt)

# percentages of inpatient diangosis by race/eth
enclave.clean %>% 
  count(outpatient.coviddx, race.eth) %>%
  pivot_wider(names_from = outpatient.coviddx, values_from = n) %>%
  mutate(pct = `0`/ (`1` + `0`))

# percentages of inpatient diagnosis by ADI
enclave.clean %>% 
  count(outpatient.coviddx, highADI) %>%
  pivot_wider(names_from = outpatient.coviddx, values_from = n) %>%
  mutate(pct = `0`/ (`1` + `0`))




## calculate nirmatrelvir prescription weights
enclave.clean %>% filter(outpatient.coviddx == 0) %>% count()
enclave.clean.outptdx <- enclave.clean %>% filter(outpatient.coviddx == 1) #limit to those diagnosed as outpatients


# abstract statistics
# overall analyzed population
enclave.clean.outptdx %>% count()

#vax percent
enclave.clean.outptdx %>% count(vaxstatus.collapsed) %>%
  mutate(pct = n/sum(n))

#pax number
enclave.clean.outptdx %>% count(paxlovid)%>%
  mutate(pct = n/sum(n))

#endpoints
enclave.clean.outptdx %>% count(paxlovid, covid_admit_death)%>%
  group_by(paxlovid) %>%
  mutate(pct = n/sum(n))

# median age
median(enclave.clean.outptdx$age.at.covid)


#check positivity assumption
enclave.clean.outptdx %>% count(paxlovid, MASS.cat)
enclave.clean.outptdx %>% count(paxlovid, age.cat)
enclave.clean.outptdx %>% count(paxlovid, vaxstatus.collapsed)
enclave.clean.outptdx%>% count(paxlovid, lastvaxgt20wks)
enclave.clean.outptdx %>% count(paxlovid, race.eth)
enclave.clean.outptdx %>% count(paxlovid, highADI)
enclave.clean.outptdx %>% count(paxlovid, studyperiod)


# logistic model to generate denominator if IP weights, probability of paxlovid
denom.fit <- glm(
  paxlovid ~ as.factor(MASS.cat) + as.factor(age.cat) 
  + as.factor(vaxstatus.collapsed)  + as.factor(lastvaxgt20wks) + as.factor(race.eth) 
  + as.factor(highADI) + as.factor(studyperiod),
  family = binomial(),
  data = enclave.clean.outptdx %>% mutate(race.eth = fct_relevel(race.eth, "White")) )

tidy(denom.fit, conf.int = TRUE, exponentiate = TRUE)

# denominator of weights
pd.pax <- predict(denom.fit, type = "response")

# numerator of stabilized weights, overall probabilty of being prescribed paxlovid
numer.fit <- glm(paxlovid ~ 1, family = binomial(), data =enclave.clean.outptdx)
pn.pax <- predict(numer.fit, type = "response")

# calculate weights
enclave.clean.outptdx$sw.pax <-if_else(enclave.clean.outptdx$paxlovid == 0, ((1- pn.pax) / (1-pd.pax)),
                                       (pn.pax / pd.pax))

quantile(enclave.clean.outptdx$sw.pax, c(0.025, 0.5, 0.975))
mean(enclave.clean.outptdx$sw.pax)
max(enclave.clean.outptdx$sw.pax)
min(enclave.clean.outptdx$sw.pax)

## generate overall weights
enclave.clean.outptdx <- enclave.clean.outptdx %>%
  mutate(sw.pax = as.numeric(sw.pax),
         sw.outpt = as.numeric(sw.outpt),
         sw.overall = sw.pax * sw.outpt)

# diagnostics for weights
mean(enclave.clean.outptdx$sw.overall)
quantile(enclave.clean.outptdx$sw.overall, c(0, 0.01, 0.025, 0.05, .5, 0.95, 0.975, 0.99, 1))



# raw hospitalization/death rates
enclave.clean.outptdx %>%
  group_by(paxlovid) %>%
  summarize(
    total = n(),
    covid_admit_death = sum(covid_admit_death)) %>%
  ungroup() %>%
  mutate(percent.covid_admit_death = covid_admit_death*100/total)

# marginal structural model for primary endpoint
pax.msm <- geepack::geeglm(
  covid_admit_death ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)


# model for hosp alone
pax.msm.hosp <- geepack::geeglm(
  covid_admit ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm.hosp)
tidy(pax.msm.hosp, conf.int = TRUE, exponentiate = TRUE)

# model for death alone
pax.msm.death <- geepack::geeglm(
  death.4wks ~ paxlovid,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm.death)
tidy(pax.msm.death, conf.int = TRUE, exponentiate = TRUE)


# assessing whether model estimate stable to exclusion of
#influential observations (< 2.5% or >97.5% of sw.overall) [similar estimate and higher std.error that we using full dataset]
pax.msm.sens <- geepack::geeglm(
  covid_admit_death ~ paxlovid ,
  family = poisson(link=log),
  weights = sw.overall,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% filter(sw.overall>0.499 & sw.overall < 2.106))
summary(pax.msm.sens)
tidy(pax.msm.sens, conf.int = TRUE, exponentiate = TRUE)







