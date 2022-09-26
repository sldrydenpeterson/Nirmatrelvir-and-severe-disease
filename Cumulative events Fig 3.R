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


# Figure 3, cumulative events

##Primary endpoint
#create survival object
survival <- enclave.clean.outptdx %>%
  mutate(
    censordt = case_when(
      !is.na(admit_date) & admit_date <= covid_date +14 ~ admit_date,
      !is.na(death_date) & death_date <= covid_date +28 ~ death_date,
      TRUE ~ covid_date +29 ),
    futime = as.numeric(pmax(.5,  censordt - covid_date)), 
    paxlovid.fct = fct_relevel(as.factor(
      case_when(
        paxlovid == 1 ~ "Nirmatrelvir",
        paxlovid == 0 ~ "No nirmatrelvir")), "No nirmatrelvir")
  )
cox_mod <- coxph(Surv(futime, covid_admit_death) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# calculate adjusted survival curves with survival object
adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="covid_admit_death",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100


# plot with confidence intervals
cum_primary<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Incidence (%)",
       title="A. Primary endpoint",
       subtitle = "Hospitalization within 14 days or death within 28 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_primary 



#secondary endpoint Death
cox_mod <- coxph(Surv(futime, death.4wks) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# use it to calculate adjusted survival curves

adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="death.4wks",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
#scale to per 1000
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100


# plot with confidence intervals
cum_deaths<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Deaths (%)",
       title="C. Deaths",
       subtitle = "Within 28 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_deaths


#secondary endpoint hospitalization

cox_mod <- coxph(Surv(futime, covid_admit) ~ paxlovid.fct,
                 data=survival, x=TRUE, weights = sw.overall)
tidy(cox_mod,conf.int = TRUE, exponentiate = TRUE)

# use it to calculate adjusted survival curves

adjsurv <- adjustedsurv(data=survival,
                        variable="paxlovid.fct",
                        ev_time="futime",
                        event="covid_admit",
                        method="direct",
                        outcome_model=cox_mod,
                        conf_int=TRUE)
#scale to per 1000
adjsurv$adjsurv$surv <- 1-(1-adjsurv$adjsurv$surv)*100
adjsurv$adjsurv$ci_lower <- 1-(1-adjsurv$adjsurv$ci_lower)*100
adjsurv$adjsurv$ci_upper <- 1-(1-adjsurv$adjsurv$ci_upper)*100

#limit to 14 days
adjsurv$adjsurv <-adjsurv$adjsurv %>% filter(time <= 14)


# plot with confidence intervals
cum_hosp<- plot(adjsurv, conf_int=TRUE, cif = TRUE) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1.5) ) +
  scale_x_continuous(breaks = scales::pretty_breaks(14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x="Days from COVID-19 Diagnosis", y="Adjusted Cumulative Hospitalizations (%)",
       title="B. Hospitalizations",
       subtitle = "Within 14 days") +
  scale_color_jama(alpha = 0.8) +
  scale_fill_jama(alpha = 0.3) +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.8))+
  guides(fill = "none")
cum_hosp


## construct figure
library(patchwork)
layout <- "
AAAAAAA
AAAAAAA
AAAAAAA
BBB#CCC
BBB#CCC"

cum_primary + cum_hosp + cum_deaths + 
  plot_layout(design = layout)

ggsave("Fig 3 cumulative incidence.png", width = 10, height = 12, dpi=1000)