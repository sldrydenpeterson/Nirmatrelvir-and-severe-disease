library(tidyverse)
library(forestploter)

## Forest plot

## overall
overall<-
  
  left_join(
    
    cbind(
      tibble(predict
             (geepack::geeglm(
               covid_admit_death ~ paxlovid,
               family = poisson(link=log),
               weights = sw.overall,
               id = seqn,
               corstr = "independence",
               data = enclave.clean.outptdx),
               tribble(
                 ~paxlovid,
                 1, 
                 0), type = "response")) %>%
        clean_names() %>%
        mutate(pax = row_number(),
               pax = if_else(pax == 1, 1, 0)) %>%
        pivot_wider(names_from = pax, values_from = predict) %>%
        rename(incid_pax = `1`, incid_nopax = `0`) %>%
        mutate(riskdiff = incid_pax - incid_nopax,
               Subgroup = "All"),
      enclave.clean.outptdx %>%
        count(paxlovid) %>%
        pivot_wider(names_from = paxlovid, values_from = n)),
    
    
    geepack::geeglm(
      covid_admit_death ~ paxlovid,
      family = poisson(link=log),
      weights = sw.overall,
      id = seqn,
      corstr = "independence",
      data = enclave.clean.outptdx)  %>%
      tidy(conf.int = TRUE) %>% 
      filter(term == "paxlovid") %>%
      mutate(term = "All") %>%
      rename( Subgroup  = term),
    
    by = "Subgroup")

### age categories
age<-
  left_join(
    rbind(
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
                   filter(age.cat == "50 to 64")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "50 to 64"),
        enclave.clean.outptdx %>% 
          mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
          filter(age.cat == "50 to 64") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
                   filter(age.cat == "65 and older")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "65 and older"),
        enclave.clean.outptdx %>% 
          mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
          filter(age.cat == "65 and older") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n))),
    
    rbind(
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% 
            mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
            filter(age.cat == "50 to 64")) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "50 to 64") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% 
          mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
          filter(age.cat == "50 to 64") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "50 to 64") %>%
          select(Subgroup), by = "Subgroup"),
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% 
            mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
            filter(age.cat == "65 and older")) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "65 and older") %>%
          rename( Subgroup = term),
        enclave.clean.outptdx %>% 
          mutate(age.cat = fct_drop(fct_collapse(age.cat, "65 and older" = c("65 to 79", "80 and older")))) %>%
          filter(age.cat == "65 and older") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "65 and older") %>%
          select(Subgroup), by = "Subgroup")),
    
    by = "Subgroup") %>%
  add_row(Subgroup = "Age", .before = 1)


## vaccine categories
vaxstatus<- 
  left_join(
    
    rbind(
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   filter(vaxstatus.collapsed == "Not fully vaccinated")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Not fully vaccinated"),
        enclave.clean.outptdx %>% 
          filter(vaxstatus.collapsed == "Not fully vaccinated") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   filter(vaxstatus.collapsed == "Vaccinated")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Vaccinated"),
        enclave.clean.outptdx %>% 
          filter(vaxstatus.collapsed == "Vaccinated") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)) ),
    
    rbind(
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(vaxstatus.collapsed == "Not fully vaccinated" ) )%>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Not fully vaccinated") %>%
          rename( Subgroup = term),
        enclave.clean.outptdx %>% filter(vaxstatus.collapsed == "Not fully vaccinated" )%>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Not fully vaccinated") %>%
          select(Subgroup), by = "Subgroup"), 
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(vaxstatus.collapsed == "Vaccinated")) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Vaccinated") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>%  filter(vaxstatus.collapsed == "Vaccinated") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup =  "Vaccinated") %>%
          select(Subgroup), by = "Subgroup")
    ),
    by = "Subgroup") %>%
  add_row(Subgroup = "Vaccination status", .before = 1)

## vaccination timing
vaxtiming <-
  left_join(
    
    rbind(
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   filter(lastvaxgt20wks == 0)),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Last vaccine < 20 weeks prior"),
        enclave.clean.outptdx %>% 
          filter(lastvaxgt20wks == 0) %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   filter(lastvaxgt20wks == 1)),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Last vaccine > 20 weeks prior"),
        enclave.clean.outptdx %>% 
          filter(lastvaxgt20wks == 1) %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)) ),
    
    
    rbind(
      
      left_join(  
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(lastvaxgt20wks == 1)) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Last vaccine > 20 weeks prior") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% filter(lastvaxgt20wks == 1) %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Last vaccine > 20 weeks prior") %>%
          select(Subgroup), by = "Subgroup"),
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(lastvaxgt20wks == 0)) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Last vaccine < 20 weeks prior") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% filter(lastvaxgt20wks == 0) %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Last vaccine < 20 weeks prior") %>%
          select(Subgroup), by = "Subgroup")
    ),
    
    by = "Subgroup") %>%
  add_row(Subgroup = "Vaccination timing", .before = 1)

## MASS score
MASSscore <-
  left_join(
    
    rbind(
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater"))))%>% 
                   filter(MASS.cat == "MASS 3 or less")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "MASS 3 or less"),
        enclave.clean.outptdx %>% 
          mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater"))))%>% 
          filter(MASS.cat == "MASS 3 or less") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater"))))%>% 
                   filter(MASS.cat == "MASS 4 or greater")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "MASS 4 or greater"),
        enclave.clean.outptdx %>% 
          mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater"))))%>% 
          filter(MASS.cat == "MASS 4 or greater") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)) ),
    
    rbind(
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% 
            mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater")))) %>%
            filter(MASS.cat == "MASS 3 or less")) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "MASS 3 or less") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% 
          mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater")))) %>%
          filter(MASS.cat == "MASS 3 or less") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "MASS 3 or less",
                 rate = `1` / `0`) %>%
          select(Subgroup), by = "Subgroup"),
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% 
            mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater")))) %>%
            filter(MASS.cat == "MASS 4 or greater")) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "MASS 4 or greater") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% 
          mutate(MASS.cat = fct_drop(fct_collapse(MASS.cat, "MASS 4 or greater" = c("MASS 4 and 5", "MASS 6 or greater")))) %>%
          filter(MASS.cat == "MASS 4 or greater") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "MASS 4 or greater") %>%
          select(Subgroup), by = "Subgroup")
    ),
    by = "Subgroup") %>%
  add_row(Subgroup = "Comorbidity score", .before = 1)


## immunocompromise
immunocompromise <- 
  left_join(
    rbind(
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(MASS.ic = if_else(MASS.ic == 3, "Immunocompromise", "No immunocompromise")) %>%
                   filter(MASS.ic == "No immunocompromise")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "No immunocompromise"),
        enclave.clean.outptdx %>% 
          mutate(MASS.ic = if_else(MASS.ic == 3, "Immunocompromise", "No immunocompromise")) %>% 
          filter(MASS.ic == "No immunocompromise") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(MASS.ic = if_else(MASS.ic == 3, "Immunocompromise", "No immunocompromise")) %>%
                   filter(MASS.ic == "Immunocompromise")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Immunocompromise"),
        enclave.clean.outptdx %>% 
          mutate(MASS.ic = if_else(MASS.ic == 3, "Immunocompromise", "No immunocompromise")) %>% 
          filter(MASS.ic == "Immunocompromise") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)) ),
    
    
    rbind(
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(MASS.ic == 3)) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Immunocompromise") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% filter(MASS.ic == 0) %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Immunocompromise") %>%
          select(Subgroup), by = "Subgroup"),
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% filter(MASS.ic == 0)) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "No immunocompromise") %>%
          rename( Subgroup = term),
        enclave.clean.outptdx %>% filter(MASS.ic == 0) %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "No immunocompromise") %>%
          select(Subgroup), by = "Subgroup")
    ),
    
    by = "Subgroup") %>%
  add_row(Subgroup = "Immunocompromise status", .before = 1)




## obesity
obesity <- 
  left_join(
    rbind(
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(obesity.cat = fct_collapse(obesity.cat, 
                                                     "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                                     "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" )
                   )) %>%
                   filter(obesity.cat == "Non-obese (BMI < 30)")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Non-obese (BMI < 30)"),
        enclave.clean.outptdx %>% 
          mutate(obesity.cat = fct_collapse(obesity.cat, 
                                            "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                            "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
          filter(obesity.cat == "Non-obese (BMI < 30)") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)),
      
      cbind(
        tibble(predict
               (geepack::geeglm(
                 covid_admit_death ~ paxlovid,
                 family = poisson(link=log),
                 weights = sw.overall,
                 id = seqn,
                 corstr = "independence",
                 data =  enclave.clean.outptdx %>% 
                   mutate(obesity.cat = fct_collapse(obesity.cat, 
                                                     "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                                     "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
                   filter(obesity.cat == "Obese (BMI ≥ 30)")),
                 tribble(
                   ~paxlovid,
                   1, 
                   0), type = "response")) %>%
          clean_names() %>%
          mutate(pax = row_number(),
                 pax = if_else(pax == 1, 1, 0)) %>%
          pivot_wider(names_from = pax, values_from = predict) %>%
          rename(incid_pax = `1`, incid_nopax = `0`) %>%
          mutate(riskdiff = incid_pax - incid_nopax,
                 Subgroup = "Obese (BMI ≥ 30)"),
        enclave.clean.outptdx %>% 
          mutate(obesity.cat = fct_collapse(obesity.cat, 
                                            "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                            "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
          filter(obesity.cat == "Obese (BMI ≥ 30)") %>%
          count(paxlovid) %>%
          pivot_wider(names_from = paxlovid, values_from = n)) ) ,
    
    
    rbind(
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data =  enclave.clean.outptdx %>% 
            mutate(obesity.cat = fct_collapse(obesity.cat, 
                                              "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                              "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
            filter(obesity.cat == "Non-obese (BMI < 30)") ) %>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Non-obese (BMI < 30)") %>%
          rename( Subgroup  = term),
        enclave.clean.outptdx %>% 
          mutate(obesity.cat = fct_collapse(obesity.cat, 
                                            "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                            "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
          filter(obesity.cat == "Non-obese (BMI < 30)") %>%
          filter(paxlovid == 0) %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Non-obese (BMI < 30)") %>%
          select(Subgroup), by = "Subgroup"),
      
      left_join(
        geepack::geeglm(
          covid_admit_death ~ paxlovid,
          family = poisson(link=log),
          weights = sw.overall,
          id = seqn,
          corstr = "independence",
          data = enclave.clean.outptdx %>% 
            mutate(obesity.cat = fct_collapse(obesity.cat, 
                                              "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                              "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
            filter(obesity.cat == "Obese (BMI ≥ 30)") )%>%
          tidy(conf.int = TRUE) %>% 
          filter(term == "paxlovid") %>%
          mutate(term = "Obese (BMI ≥ 30)") %>%
          rename( Subgroup = term),
        enclave.clean.outptdx %>% 
          mutate(obesity.cat = fct_collapse(obesity.cat, 
                                            "Non-obese (BMI < 30)" = c("BMI less than 25", "BMI 25 to 30" ),
                                            "Obese (BMI ≥ 30)" = c("BMI 30 to 35", "BMI greater than 35" ) )) %>%
          filter(obesity.cat == "Obese (BMI ≥ 30)") %>%
          count(covid_admit_death) %>%
          pivot_wider(names_from = covid_admit_death, values_from = n) %>%
          mutate(Subgroup = "Obese (BMI ≥ 30)") %>%
          select(Subgroup), by = "Subgroup")),
    
    by = "Subgroup") %>%
  add_row(Subgroup = "Obesity status", .before = 1)





## construct data table for plot

forest.dt<- rbind(overall, age, vaxstatus, vaxtiming, MASSscore, obesity) %>%
  mutate(
    low = exp(as.numeric(estimate) - as.numeric(std.error)*1.96),
    high = exp(as.numeric(estimate) + as.numeric(std.error)*1.96),
    estimate = exp(as.numeric(estimate)),
    riskdiff = if_else(!is.na(riskdiff), paste0(formatC(round(riskdiff*100,2),2,format="f"), "%"), as.character(NA)),
    
    incid_pax = if_else(!is.na(incid_pax), formatC(round(incid_pax*100,2),2,format="f"), as.character(NA)),
    incid_pax = case_when(
      is.na(incid_pax) ~ "",
      TRUE ~ paste0(incid_pax, "%")),
    incid_nopax = if_else(!is.na(incid_nopax), formatC(round(incid_nopax*100,2),2,format="f"), as.character(NA)),
    incid_nopax = case_when(
      is.na(incid_nopax) ~ "",
      TRUE ~ paste0(incid_nopax, "%") 
    )) %>%
  rename("No nirmatrelvir" = `0`, 
         "Nirmatrelvir" = `1`) %>%
  select(Subgroup, "Nirmatrelvir", "No nirmatrelvir", incid_pax, incid_nopax, riskdiff, low, high, estimate,  std.error)

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
forest.dt$Subgroup <- ifelse(is.na(forest.dt$"No nirmatrelvir"), 
                             forest.dt$Subgroup,
                             paste0("    ", forest.dt$Subgroup))

# NA to blank or NA will be transformed to character.
forest.dt$`No nirmatrelvir` <- ifelse(is.na(forest.dt$`No nirmatrelvir`), "", forest.dt$`No nirmatrelvir`)
forest.dt$`Nirmatrelvir` <- ifelse(is.na(forest.dt$`Nirmatrelvir`), "", forest.dt$`Nirmatrelvir`)

# # create hi low

# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
forest.dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
forest.dt$`Relative\nrisk (95% CI)` <- ifelse(is.na(forest.dt$std.error), "",
                                              sprintf("%.2f (%.2f to %.2f)",
                                                      forest.dt$estimate, forest.dt$low, forest.dt$high))

## Add hosp rate

forest.dt <-forest.dt %>%
  mutate(riskdiff = if_else(is.na(riskdiff), "", riskdiff),
         `Hosp. or death among\ntreated / untreated           ` = if_else(incid_pax != "", paste0(incid_pax, " / ", incid_nopax),"")) %>%
  rename( `Risk\ndifference` = riskdiff ) %>%
  select(Subgroup,  `Nirmatrelvir`,`No nirmatrelvir`, `Hosp. or death among\ntreated / untreated           `, `Risk\ndifference`, estimate, std.error, low, high, ` `, `Relative\nrisk (95% CI)`) %>%
  mutate(`Nirmatrelvir` = 
           if_else(`Nirmatrelvir` == "", "",  prettyNum(`Nirmatrelvir`, big.mark =  ",")),
         `No nirmatrelvir` = 
           if_else(`No nirmatrelvir` == "", "", prettyNum(`No nirmatrelvir`, big.mark =  ","))) 

tm<- forest_theme(
  base_size = 12,
  base_family = "",
  ci_pch = 15,
  ci_col = "black",
  ci_lty = 1,
  ci_lwd = 1,
  ci_Theight = NULL,
  legend_name = "Group",
  legend_position = "right",
  legend_value = "",
  xaxis_lwd = 0.6,
  xaxis_cex = 1,
  refline_lwd = 1,
  refline_lty = "dashed",
  refline_col = "grey20",
  vertline_lwd = 1,
  vertline_lty = "solid",
  vertline_col = "grey20",
  summary_fill = "#4575b4",
  summary_col = "#4575b4",
  footnote_cex = 0.6,
  footnote_fontface = "plain",
  footnote_col = "black"
)


p <- forest(forest.dt[,c(1:5, 10:11)],
            est = forest.dt$estimate,
            lower = forest.dt$low, 
            upper = forest.dt$high,
            #sizes = forest.dt$std.err,
            ci_column = 6,
            ref_line = 1,
            vert_line = 0.579,
            arrow_lab = c("Nirmatrelvir better", "Nirmatrelvir poorer"),
            xlim = c(0, 1.7),
            ticks_at = c(0.1, 0.5, 1, 1.5),
            footnote = "",
            theme = tm)

# Print plot
plot(p)
ggsave("Nirmatrelvir subgroup Fig4.png", plot = p,
       width = 12, height = 6, dpi = 1000)


