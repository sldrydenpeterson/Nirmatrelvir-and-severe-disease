library(tidyverse)
library(readxl)
library(lubridate)

setwd("~/Dropbox (Partners HealthCare)/GitHub/Nirmatrelvir and severe disease")


enclave.pax<- tibble(read_excel("paxlovid_dataset_30aug2022_LOCKED.xlsx", sheet = "paxlovid_rebound_dataset_17aug2", guess_max =150000))

ADI.pax<- read_csv("ADI.pax.csv") %>% ungroup() 


enclave.total<- 
  left_join(
   enclave.pax,ADI.pax, by = "EMPI") %>%
  mutate(
    covid_date = date(covid_date),
    admit_date = date(admit_date),
    paxlovid_date = case_when(
      date(paxlovid_date) < as.Date("2022-01-21") ~ as.Date(NA),  # date prescribing became available at MGB, prior scripts when out-of-state or not filled
      date(paxlovid_date) >=  as.Date("2022-01-21") ~  date(paxlovid_date),
      TRUE ~ as.Date(NA)),
    paxlovid = case_when(
      date(paxlovid_date) < as.Date("2022-01-21") ~ 0,  # date prescribing became available at MGB, prior scripts when out-of-state or not filled
      date(paxlovid_date) >=  as.Date("2022-01-21") ~  1, 
      TRUE ~ 0),
    birth_date = date(birth_date),
    death_date = date(death_date),
    molnupiravir_date = date(molnupiravir),
    remdesivir_date = date(remdesivir),
    sotrovimab_date = date(sotrovimab),
    bebtelovimab_date = date(bebtelovimab),
    # evusheld_date = date(evusheld),
    age.at.covid = floor(as.numeric((covid_date - birth_date)/365.25)), #) %>% select(EMPI, covid_date, admit_date, paxlovid_date, birth_date, death_date, remdesivir_date:evusheld_date)
    
    covid_admit = case_when(
      !is.na(admit_date) & admit_date-covid_date <29 ~ 1,
      TRUE ~ 0),
    covid_admit_date = if_else(covid_admit == 1, admit_date, as.Date(NA)),
    outpatient.coviddx = case_when(
      covid_date >= admit_date-1 ~ 0,
      covid_date >= death_date-1 & !is.na(death_date) ~ 0,
      covid_date < admit_date-1 ~ 1,
      is.na(admit_date) ~ 1,
      is.na(death_date) ~ 1),
    death.4wks = case_when(
      death_date <= covid_date+28 ~ 1,
      death_date > covid_date+28 ~ 0,
      is.na(death_date) ~0
    ),
    death.2wks = case_when(
      death_date <= covid_date+14 ~ 1,
      death_date > covid_date+14 ~ 0,
      is.na(death_date) ~0
    ),
    covid_admit_death = case_when(
      !is.na(admit_date) & admit_date-covid_date <15 ~ 1,
      !is.na(death_date) & death_date-covid_date <29 ~ 1,
      TRUE ~ 0),
    
    eGFR = case_when(
      is.na(gfr) ~ as.numeric(65),
      gfr == "> 60" ~ as.numeric(61),
      gfr == ">60" ~ as.numeric(61),
      gfr == ">60.0" ~ as.numeric(61),
      gfr == "> 60.0" ~ as.numeric(61),
      gfr == "> 90" ~ as.numeric(91),
      gfr == ">90" ~ as.numeric(91),
      gfr == "> 100" ~ as.numeric(101),
      gfr == ">100" ~ as.numeric(101),
      gfr == "> 120" ~ as.numeric(121),
      gfr == ">120" ~ as.numeric(121),
      TRUE ~ as.numeric(gfr)      ),

    #22 is the the 80% percentile in the sample
    highADI = case_when(
      ADI_NATRANK >=19 ~ 1,
      ADI_NATRANK <19 ~ 0,
      is.na(ADI_NATRANK) ~ 0),
    
    race.eth = case_when(
      ethnicity == "Hispanic" ~ "Hispanic or Latinx",
      race == "Black" ~ "Black",
      race == "White" ~ "White",
      race == "Asian" ~ "Asian",
      race == "Other" ~ "Other or unavailable",
      race == "Two or More" ~ "Other or unavailable",
      race == "Unknown/Missing" ~ "Other or unavailable",
      race == "American Indian or Alaska Nati" ~ "Other or unavailable",
      race == "Native Hawaiian or Other Pacif" ~ "Other or unavailable",
      TRUE ~ "Other or unavailable"),
    
    covid_vaccine_dose_1 = date(covid_vaccine_dose_1),
    covid_vaccine_dose_1 = date(covid_vaccine_dose_1),
    covid_vaccine_dose_2 = date(covid_vaccine_dose_2),
    covid_vaccine_dose_3 = date(covid_vaccine_dose_3),
    covid_vaccine_dose_4 = date(covid_vaccine_dose_4),
    covid_vaccine_dose_5 = date(covid_vaccine_dose_5),
    covid_vaccine_dose_6 = date(covid_vaccine_dose_6),
    
    contraindicated = case_when(
      immunosuppression_type___cyclosporine == 1 ~1, 
      immunosuppression_type___tacrolimus == 1 ~1, 
      immunosuppression_type___everolimus == 1 ~1, 
      immunosuppression_type___sirolimus == 1 ~1, 
      immunosuppression_type___clopidogrel == 1 ~1, 
      immunosuppression_type___rivaroxaban == 1 ~1, 
      immunosuppression_type___amiodarone == 1 ~1, 
      immunosuppression_type___carbamazepine == 1 ~1, 
      immunosuppression_type___phenytoin == 1 ~1, 
      immunosuppression_type___ranolazine == 1 ~1, 
      eGFR < 30 ~ 1,
      TRUE ~ 0),
    
    recentvaxdt = case_when(
      !is.na(covid_vaccine_dose_6) & covid_vaccine_dose_6 + 14 < covid_date ~ covid_vaccine_dose_6,
      !is.na(covid_vaccine_dose_5) & covid_vaccine_dose_5 + 14< covid_date ~ covid_vaccine_dose_5,
      !is.na(covid_vaccine_dose_4) & covid_vaccine_dose_4 + 14< covid_date ~ covid_vaccine_dose_4,
      !is.na(covid_vaccine_dose_3) & covid_vaccine_dose_3 + 14< covid_date ~ covid_vaccine_dose_3,
      !is.na(covid_vaccine_dose_2) & covid_vaccine_dose_2 + 14< covid_date ~ covid_vaccine_dose_2,
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date ~ covid_vaccine_dose_1,
      TRUE ~ as.Date(NA)),
    
    vaxstatus = case_when(             #vax receipt 14d or more prior to infection date
      !is.na(covid_vaccine_dose_6) & covid_vaccine_dose_6 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_5) & covid_vaccine_dose_5 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_4) & covid_vaccine_dose_4 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_3) & covid_vaccine_dose_3 + 14< covid_date ~ "Vaccinated and boosted",
      !is.na(covid_vaccine_dose_2) & covid_vaccine_dose_2 + 14< covid_date ~ "Vaccinated",
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date & covid_vaccine_brand_1 == "JJ" ~ "Vaccinated",
      !is.na(covid_vaccine_dose_1) & covid_vaccine_dose_1 + 14< covid_date ~ "Partially vaccinated",
      TRUE ~"Unvaccinated"),
    daysfromvax = as.numeric(covid_date - recentvaxdt),
    lastvaxgt20wks = case_when(
      daysfromvax >140 ~ 1,
      is.na(daysfromvax) ~ 1, 
      daysfromvax < 141 ~0),
    
    age.at.covid = floor(as.numeric((covid_date - birth_date)/365.25)), 
    
    age.cat = cut(age.at.covid, c(17, 49, 64, 79, 150)),
    age.cat = fct_recode(age.cat,
                         "18 to 49" = "(17,49]", 
                         "50 to 64" = "(49,64]", 
                         "65 to 79"= "(64,79]", 
                         "80 and older"= "(79,150]" ),
    
    bmi = case_when(
      !is.na(bmi) & bmi >= 12 ~ bmi,
      (is.na(bmi) | bmi < 12) & !is.na(weight_kg) & sex == "Female" & age.at.covid > 16 ~ weight_kg/1.63^2,  #average height in MA per CDC
      (is.na(bmi) | bmi < 12) & !is.na(weight_kg) & sex == "Male" & age.at.covid > 16 ~ weight_kg/1.78^2,   #average height in MA per CDC
      TRUE ~ as.numeric(NA)    ),
    obesity.cat = 
      fct_relevel(
        case_when(
          is.na(bmi) ~ "BMI unavailable",
          bmi < 12 ~ "BMI unavailable",
          bmi < 25 ~ "BMI less than 25",
          bmi < 30 ~ "BMI 25 to 30",
          bmi <= 35 ~ "BMI 30 to 35",
          bmi > 35 ~ "BMI greater than 35"), 
        "BMI unavailable", "BMI less than 25", "BMI 25 to 30", "BMI 30 to 35"),
    
    HTN = medhx___htn,
    diabetes = medhx___dm,
    CKD = case_when(
      eGFR < 60 ~ 1, 
      TRUE ~ 0),
    CVD = case_when(
      medhx___phtn == 1 ~ 1,
      medhx___cad == 1 ~ 1,
      medhx___chf == 1 ~ 1,
      # medhx___congenital == 1 ~ 1,
      medhx___stroke == 1 ~ 1,
      TRUE ~ 0),
    pulm.dis = case_when(
      medhx___copd == 1 ~ 1,
      medhx___ipfild == 1 ~ 1, 
      medhx___cf == 1 ~ 1,
      medhx___bronchiectasis == 1 ~ 1, 
      TRUE ~ 0),
    asthma = case_when(
      medhx___asthma == 1 & pulm.dis == 0 ~ 1,
      TRUE ~ 0), 
    hiv = case_when(
      medhx___hiv == 1  ~ 1,
      TRUE ~ 0), 
    inflam.dis = case_when(
      medhx___ibd == 1 ~ 1,
      medhx___sle == 1 ~ 1,
      medhx___ra == 1 ~ 1,
      medhx___sjogrens == 1 ~ 1,
      medhx___psoriatic == 1 ~ 1,
      TRUE ~ 0),
    ms = case_when(
      medhx___ms == 1 ~ 1,
      TRUE ~ 0 ),
    solid.malig = `cancer___solid onc`,
    # case_when(
    # cancer___breast ==1 ~1,
    # cancer___lung ==1 ~1,
    # cancer___colon ==1 ~1,
    # cancer___gastric ==1 ~1,
    # cancer___brain ==1 ~1,
    # cancer___melanoma ==1 ~1,
    # cancer___renal ==1 ~1,
    # cancer___gu ==1 ~1,
    # cancer___metastatic ==1 ~1,
    # cancer___breast ==1 ~1,
    # TRUE ~ 0),
    heme.malig = `cancer___heme mal`,
    # case_when(
    # cancer___all == 1 ~1,
    # cancer___aml == 1 ~1,
    # cancer___cll == 1 ~1,
    # cancer___cml == 1 ~1,
    # cancer___hl == 1 ~1,
    # cancer___nhl == 1 ~1,
    # cancer___mm == 1 ~1,
    # TRUE ~ 0),
    psych.non.unipolar = case_when(
      psych___bipolar == 1 ~ 1, 
      psych___schizodo == 1 ~ 1, 
      psych___schizo == 1 ~ 1, 
      psych___ocd == 1 ~ 1, 
      TRUE ~ 0    ),
    depr.anxiety = case_when(
      psych___depression == 1 & psych.non.unipolar == 0 ~ 1, 
      psych___anxiety == 1 & psych.non.unipolar == 0  ~1, 
      TRUE ~ 0 ),
    current.pregnant = pregnant, 
    immunosupr.meds = case_when(
      immunosuppression_type___prednisone == 1 ~ 1, 
      immunosuppression_type___mycophenolate == 1 ~ 1, 
      immunosuppression_type___cyclosporine == 1 ~ 1, 
      immunosuppression_type___azathioprine == 1 ~ 1, 
      immunosuppression_type___tacrolimus == 1 ~ 1,
      immunosuppression_type___everolimus == 1 ~1, 
      immunosuppression_type___sirolimus == 1 ~1,
      immunosuppression_type___rituximab== 1 ~1,
      immunosuppression_type___obinutuzumab == 1 ~1,
      immunosuppression_type___infliximab == 1 ~1,
      immunosuppression_type___etanercept == 1 ~1,
      immunosuppression_type___adalimumab == 1 ~1,
      immunosuppression_type___pembrolizumab == 1 ~1,
      immunosuppression_type___nivolumab == 1 ~1,
      immunosuppression_type___ocrelizumab == 1 ~1,
      immunosuppression_type___sirolimus == 1 ~1,
      TRUE ~ 0    ),
    MASS.age = case_when(
      age.at.covid >=65 ~ 2,
      TRUE ~ 0),
    MASS.dm = case_when(
      diabetes == 1 ~2,
      TRUE ~0),
    MASS.ckd = case_when(
      CKD == 1 ~3,
      TRUE ~0),
    MASS.ic = case_when(
      immunosupr.meds == 1 ~3,
      solid.malig == 1 ~ 3, 
      heme.malig == 1 ~3,
      hiv == 1 ~ 3,
      TRUE ~0),
    MASS.pulm = case_when(
      pulm.dis == 1 & age.at.covid >=55 ~ 3,
      TRUE ~0),
    MASS.cvd = case_when(
      CVD == 1 & age.at.covid >=55 ~ 2,
      TRUE ~0),
    MASS.bmi = case_when(
      bmi >=35 ~ 2,
      TRUE ~0),
    MASS.htn = case_when(
      medhx___htn == 1 & age.at.covid >=55 ~ 1,
      TRUE ~0),
    MASS = MASS.age + MASS.dm + MASS.ckd + MASS.ic + MASS.pulm + MASS.cvd + MASS.bmi + MASS.htn,
    MASS.cat = fct_relevel(case_when(
      MASS >= 6 ~ "MASS 6 or greater",
      MASS >= 4 ~ "MASS 4 and 5",
      MASS < 4  ~ "MASS 3 or less"), "MASS 3 or less",  "MASS 4 and 5"),
    vaxstatus.collapsed = case_when(
      vaxstatus == "Vaccinated and boosted" ~ "Vaccinated",
      vaxstatus == "Vaccinated" ~ "Vaccinated",
      TRUE ~ "Not fully vaccinated"    ),
    seqn = row_number(),
    outpatient.remdesivir = case_when(
      remdesivir_date >= covid_date & remdesivir_date < covid_date+30 & remdesivir_date < admit_date ~ 1,
      remdesivir_date >= covid_date & remdesivir_date < covid_date+30 & is.na(admit_date) ~ 1,
      TRUE ~ 0),
    studyperiod = case_when(
      covid_date < as.Date("2022-05-01") ~ "Jan to Apr",
      TRUE ~ "May to Jul")
  )

