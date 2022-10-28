library(tidyverse)

# CONSORT type flow diagram



data <- tibble(x= 1:200, y= 1:200)
head(data)

c <-  data %>% 
      ggplot(aes(x, y)) +
      scale_x_continuous(minor_breaks = seq(10, 200, 10)) +
      scale_y_continuous(minor_breaks = seq(10, 240, 10)) +
      theme_linedraw()
      c

# starting box
     start.total<- enclave.total %>%
     filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
     count()
c<- c +
  geom_rect(xmin = 50, xmax=150, ymin=220, ymax=230, color='black',
            fill='#DEDFE4', size=0.25) +
  annotate('text', x= 100, y=225,  size=2.5, hjust=0.5, vjust=0.5,
          label= str_wrap(paste0(prettyNum(start.total, big.mark =  ","), " Patients testing positive for SARS-CoV-2 infection (January 1 and July 17, 2022) were assessed for eligibility"),
          indent =0 , width =60, exdent = 0)) +
  geom_segment(
    x=100, xend=100, y=220, yend=200.5, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=100, xend=124.5, y=210, yend=210, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) 
  
 c

 # box of 50 and older
      age50.or.older<- enclave.total %>%    
        filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
        filter(age.at.covid >= 50) %>% count()
        
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=190, ymax=200, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=195,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum(age50.or.older, big.mark =  ","), " Patients 50 years and older"),
                            indent =0 , width =60, exdent = 0)) + 
   
   geom_rect(xmin = 125, xmax=182, ymin=205, ymax=215, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=210,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(start.total - age50.or.older, big.mark =  ","), " Patients younger than 50 years old"),
                            indent =0 , width = 30 , exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=190, yend=170.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   geom_segment(
     x=100, xend=124.5, y=180, yend=180, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) 
 
 c
 

 # box of MA/NH residents
residents <- enclave.total %>%    
  filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
  filter(age.at.covid >= 50) %>%
                    filter(state == "Massachusetts" | state == "New Hampshire") %>%
                      count()
 
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=160, ymax=170, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=165,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum(residents, big.mark =  ","), " Patients resident of Massachusetts or New Hampshire"),
                            indent =0 , width =60, exdent = 0))  + 
   
   geom_rect(xmin = 125, xmax=182, ymin=175, ymax=185, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=180,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(age50.or.older-residents, big.mark =  ","), " Patients residing outside of study area"),
                            indent =0 , width =38, exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=160, yend=140.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   geom_segment(
     x=100, xend=124.5, y=150, yend=150, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) 
 c
 
 # box of available weight
 avail.wt <- enclave.total %>% filter(age.at.covid >= 50) %>%
   filter(state == "Massachusetts" | state == "New Hampshire") %>%
   filter(!is.na(bmi)) %>%
   count()
 
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=130, ymax=140, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=135,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum(avail.wt, big.mark =  ","), " Patients with Mass General Brigham engagement within 2 years"),
                            indent =0 , width =60, exdent = 0))  + 
   
   geom_rect(xmin = 125, xmax=182, ymin=145, ymax=155, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=150,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(residents - avail.wt, big.mark =  ","), " Patients without weight measurement within 2 years"),
                            indent =0 , width =38, exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=130, yend=110.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   geom_segment(
     x=100, xend=124.5, y=120, yend=120, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) 
 c
 
 
 # box of other treatment
 outpatient.dx <- enclave.total %>%
   filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
   filter(age.at.covid >= 50) %>%
   filter(state == "Massachusetts" | state == "New Hampshire") %>%
   filter(!is.na(bmi)) %>%
   filter(outpatient.coviddx == 1) %>%
   # filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+1) %>%  # remove those that received molnupiravir within 2d of covid diagnosis
   # filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+2) %>%      # remove those that received sotrovimab within 3d of covid diagnosis
   # filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
   # filter(is.na(remdesivir_date) |  remdesivir_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis

   count()

 
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=100, ymax=110, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=105,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum( outpatient.dx, big.mark =  ","), " Patients diagnosed as outpatients"),
                            indent =0 , width =60, exdent = 0)) + 
   
   geom_rect(xmin = 125, xmax=182, ymin=115, ymax=125, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=120,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(avail.wt - outpatient.dx, big.mark =  ","), " Patients diagnosed within one calendar day of admission or death"),
                            indent =0 , width =38, exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=100, yend=80.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   geom_segment(
     x=100, xend=124.5, y=90, yend=90, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) 
 c

 
 # box of renal/ddi eligible outpatients
 eligible <- enclave.total %>%    
   filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
   filter(age.at.covid >= 50) %>%
   filter(state == "Massachusetts" | state == "New Hampshire") %>%
   filter(!is.na(bmi)) %>%
   filter(outpatient.coviddx == 1) %>%
   filter(contraindicated != 1) %>%
   count()
 
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=70, ymax=80, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=75,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum(eligible, big.mark =  ","), " Patients potentially eligible for nirmatrelvir plus ritonavir"),
                            indent =0 , width =60, exdent = 0)) + 
   
   geom_rect(xmin = 125, xmax=182, ymin=85, ymax=95, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=90,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(outpatient.dx - eligible, big.mark =  ","), " Patients with eGFR < 30 mL/min or receiving medications not advised to be coadministered"),
                            indent =0 , width =38, exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=70, yend=50.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   geom_segment(
     x=100, xend=124.5, y=60, yend=60, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) 
 c
 
 
 
 # box of eligible outpatients not receiving other therapy
 eligible.untreated <- enclave.total %>%    
   filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
   filter(age.at.covid >= 50) %>%
   filter(state == "Massachusetts" | state == "New Hampshire") %>%
   filter(!is.na(bmi)) %>%
   filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+1) %>%  # remove those that received molnupiravir within 2d of covid diagnosis
   filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+2) %>%      # remove those that received sotrovimab within 3d of covid diagnosis
   filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
   filter(is.na(remdesivir_date) |  remdesivir_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
   filter(contraindicated != 1) %>%
   filter(outpatient.coviddx == 1) %>%
   count()
 
 c<- c +
   geom_rect(xmin = 50, xmax=150, ymin=40, ymax=50, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 100, y=45,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum( eligible.untreated, big.mark =  ","), " Patients diagnosed as outpatients and eligible for nirmatrelvir plus ritonavir"),
                            indent =0 , width =60, exdent = 0)) + 
   
   geom_rect(xmin = 125, xmax=182, ymin=55, ymax=65, color='black',
             fill='white', size=0.25) +
   annotate('text', x= 127, y=60,  size=2.5, hjust=0, vjust=0.5,
            label= str_wrap(paste0(prettyNum(eligible -  eligible.untreated, big.mark =  ","), " Patients treated with anti-SARS-CoV-2 monoclonal antibodies, remdesivir, or molnupiravir within 1 calendar day of diagnosis"),
                            indent =0 , width =38, exdent = 0)) +
   geom_segment(
     x=100, xend=100, y=40, yend=30, 
     size=0.15, linejoin = "mitre", lineend = "butt") +
   
   geom_segment(
     x=60, xend=140, y=30, yend=30, 
     size=0.15, linejoin = "mitre", lineend = "butt") +
   
   geom_segment(
     x=60, xend=60, y=30, yend=20.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed")) +
   
   geom_segment(
     x=140, xend=140, y=30, yend=20.5, 
     size=0.15, linejoin = "mitre", lineend = "butt",
     arrow = arrow(length = unit(1, "mm"), type= "closed"))
 c
 

 # boxes of nirmatrelvir vs non-nirmatrelvir
 outpatient.pax <- enclave.total %>%    
   filter(covid_date >= as.Date("2022-01-01") & covid_date <= as.Date("2022-07-17")) %>%
   filter(age.at.covid >= 50) %>%
   filter(state == "Massachusetts" | state == "New Hampshire") %>%
   filter(!is.na(bmi)) %>%
   filter(is.na(molnupiravir_date) |  molnupiravir_date >  covid_date+1) %>%  # remove those that received molnupiravir within 2d of covid diagnosis
   filter(is.na(sotrovimab_date) |  sotrovimab_date >  covid_date+2) %>%      # remove those that received sotrovimab within 3d of covid diagnosis
   filter(is.na(bebtelovimab_date) |  bebtelovimab_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis
   filter(is.na(remdesivir_date) |  remdesivir_date >  covid_date+2) %>%      # remove those that received bebtelovimab within 30d of covid diagnosis   filter(contraindicated != 1) %>%
   filter(contraindicated != 1) %>%
   filter(outpatient.coviddx == 1) %>%
   group_by(paxlovid) %>%
   count()
 
 c<- c +
   geom_rect(xmin = 35, xmax=85, ymin=10, ymax=20, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 60, y=15,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum( outpatient.pax[2,2], big.mark =  ","), " Patients prescribed nirmatrelvir plus ritonavir"),
                            indent =0 , width = 30, exdent = 0)) + 
   
   geom_rect(xmin = 115, xmax=165, ymin=10, ymax=20, color='black',
             fill='#DEDFE4', size=0.25) +
   annotate('text', x= 140, y=15,  size=2.5, hjust=0.5, vjust=0.5,
            label= str_wrap(paste0(prettyNum( outpatient.pax[1,2], big.mark =  ","), " Patients not prescribed nirmatrelvir plus ritonavir"),
                            indent =0 , width =30, exdent = 0)) + 
   
   theme_void()
   

 c
 
 
 ggsave("Cohort diagram Fig 1.png", width = 8, height = 16, dpi = 1000, bg = "white")
 
 
 