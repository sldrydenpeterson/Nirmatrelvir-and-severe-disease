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
  filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+1) %>%  # remove those that received molnupiravir within 2d of covid diagnosis
  filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+2) %>%      # remove those that received sotrovimab within 3d of covid diagnosis
  filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
  filter(is.na(remdesivir_date) |  remdesivir_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
  filter(contraindicated != 1) %>%  # remove those with eGFR <30 and with contraindicated meds
  
  select(EMPI, seqn, MASS.cat, age.cat, vaxstatus.collapsed, vaxstatus, lastvaxgt20wks, race.eth,  highADI, outpatient.coviddx, paxlovid, paxlovid_date,
         covid_admit, covid_admit_death, covid_date, admit_date, death.4wks, death.2wks, death_date, sex, obesity.cat, MASS, prior_covid, prior_covid_date, eGFR, MASS.ic, MASS.dm, age.at.covid,
         CVD, pulm.dis, psych.non.unipolar, depr.anxiety, heme.malig, solid.malig, inflam.dis, studyperiod, mAb.beyond2d:anytx.beyond2d)


## calculate nirmatrelvir prescription weights
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

# recieved therapy past 2d
enclave.clean.outptdx %>% count( anytx.beyond2d)%>%
  mutate(pct = n/sum(n))

enclave.clean.outptdx %>% count(pax.beyond2d)%>%
  mutate(pct = n/sum(n))

enclave.clean.outptdx %>% count(mAb.beyond2d)%>%
  mutate(pct = n/sum(n))

enclave.clean.outptdx %>% count(rdv.beyond2d)%>%
  mutate(pct = n/sum(n))

enclave.clean.outptdx %>% count(mpv.beyond2d)%>%
  mutate(pct = n/sum(n))


#check positivity assumption
enclave.clean.outptdx %>% count(paxlovid, MASS.cat)
enclave.clean.outptdx %>% count(paxlovid, age.cat)
enclave.clean.outptdx %>% count(paxlovid, vaxstatus)
enclave.clean.outptdx%>% count(paxlovid, lastvaxgt20wks)
enclave.clean.outptdx %>% count(paxlovid, race.eth)
enclave.clean.outptdx %>% count(paxlovid, highADI)
enclave.clean.outptdx %>% count(paxlovid, studyperiod)


# logistic model to generate denominator if IP weights, probability of paxlovid rx
denom.fit <- glm(
  paxlovid ~ as.factor(MASS.cat) + as.factor(age.cat) 
  + as.factor(vaxstatus)  + as.factor(lastvaxgt20wks) + as.factor(race.eth) 
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

quantile(enclave.clean.outptdx$sw.pax, c(0, 0.025, 0.5, 0.975, 1))
mean(enclave.clean.outptdx$sw.pax)


# model for primary endpoint
pax.msm <- geepack::geeglm(
  covid_admit_death ~ paxlovid,
  family = poisson(link=log),
  weights = sw.pax,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)


# model for hosp alone
pax.msm.hosp <- geepack::geeglm(
  covid_admit ~ paxlovid,
  family = poisson(link=log),
  weights = sw.pax,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx)
summary(pax.msm.hosp)
tidy(pax.msm.hosp, conf.int = TRUE, exponentiate = TRUE)

# model for death alone
pax.msm.death <- geepack::geeglm(
  death.4wks ~ paxlovid,
  family = poisson(link=log),
  weights = sw.pax,
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
  weights = sw.pax,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% filter(sw.pax>0.478 & sw.pax< 1.822))
summary(pax.msm.sens)
tidy(pax.msm.sens, conf.int = TRUE, exponentiate = TRUE)


# sensitivity analysis excluding patients who received therapy day 2 onwards
pax.msm <- geepack::geeglm(
  covid_admit_death ~ paxlovid,
  family = poisson(link=log),
  weights = sw.pax,
  id = seqn,
  corstr = "independence",
  data = enclave.clean.outptdx %>% filter(anytx.beyond2d == 0))
summary(pax.msm)
tidy(pax.msm, conf.int = TRUE, exponentiate = TRUE)


