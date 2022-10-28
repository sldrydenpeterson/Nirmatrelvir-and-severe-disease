
library(tidyverse)
library(readxl)
library(broom)
library(janitor)
library(lubridate)
library(tableone)
library(flextable)
library(officer)
library("cobalt")

fixzip <- function(x){ifelse(nchar(x)<5, paste0(0,x), x)}


enclave.clean.outptdx %>%
  count(paxlovid, highADI)

pax.tab1 <-data.frame( enclave.clean.outptdx %>%
  mutate(
    paxlovid = as.factor(if_else(paxlovid == 1, "Nirmatrelvir plus ritonavir", "No nirmatrelvir plus ritonavir")),
    paxlovid = fct_relevel(paxlovid, "Nirmatrelvir plus ritonavir"),
    vaxstatus = as.factor(vaxstatus),
    vaxstatus = fct_relevel(vaxstatus, "Vaccinated and boosted", "Vaccinated"),
    lastvaxgt20wks  = as.factor(lastvaxgt20wks),
   race.eth =as.factor(race.eth),
  highADI = fct_relevel(as.factor(highADI), "0"),
   MASS.ic = as.factor(if_else(MASS.ic == 3, 1, 0)),
    CVD = as.factor(CVD),
    diabetes= as.factor(if_else(MASS.dm == 2, 1, 0)),
    pulm.dis = as.factor(pulm.dis),
    inflam.dis = as.factor(inflam.dis),
    age.cat = as.factor(age.cat),
    sex = as.factor(sex),
    solid.malig = as.factor(solid.malig),
    heme.malig = as.factor(heme.malig),
    MASS.cat = as.factor(MASS.cat),
    psych.non.unipolar = as.factor(psych.non.unipolar),
    depr.anxiety = as.factor(depr.anxiety)
  ) %>% select(paxlovid, age.cat, sex, race.eth, highADI, vaxstatus, lastvaxgt20wks,    MASS.cat,   obesity.cat, MASS.ic, diabetes, CVD, pulm.dis, psych.non.unipolar, depr.anxiety,
                heme.malig, solid.malig, inflam.dis, sw.pax) %>% 
  rename(
    "Vaccination status" = vaxstatus, 
    "Male sex" = sex,
    "Last vaccine dose more than 20 weeks prior"= lastvaxgt20wks,
    "Increased neighborhood disadvantage (ADI)" = highADI,
    "Race and ethnicity" = race.eth,
    "Body mass index (BMI)" =  obesity.cat, 
    "Age" =  age.cat, 
    "Comorbidity score" =  MASS.cat, 
    "Immunocompromise" =  MASS.ic, 
    "Diabetes" =  diabetes, 
    "Heart disease or stroke" = CVD,
    "Pulmonary disease" = pulm.dis,
    "Rheumatologic or inflammatory bowel disease" = inflam.dis, 
    "Solid tumor malignancy" = solid.malig, 
    "Hematologic malignancy" = heme.malig, 
    "Depression and anxiety" = depr.anxiety, 
    "Bipolar, schizophrenia, and other disorders" = psych.non.unipolar
  )) 


# All variables excluding the group variable
allVars <- pax.tab1  %>% select(-('paxlovid')) %>% names()

# All categorical variables
catVars <- pax.tab1  %>% select(where(is.factor)) %>% 
  select(-('paxlovid')) %>% 
  names()


# All continuous variables
contVars <-  pax.tab1   %>% select(where(Negate(is.factor))) %>% 
  names()

# All continuous variables
contVars <-  pax.tab1  %>% select(where(Negate(is.factor))) %>% 
  names()

# cobalt
smd<-tibble((bal.tab(pax.tab1  %>% select(-c('paxlovid', sw.pax)), treat = pax.tab1$paxlovid, s.d.denom = "pooled", binary = "std", weights = pax.tab1$sw.pax, un = TRUE)[["Balance"]])  %>% 
              rownames_to_column(var = "Characteristic")) %>%
mutate(Characteristic = paste0("   ", sub('.*_', '', Characteristic)),
       Characteristic = case_when(
         Characteristic == "   Male" ~ "Male.sex (%)",
         Characteristic == "   Increased.neighborhood.disadvantage..ADI." ~ "Increased.neighborhood.disadvantage..ADI. (%)",
         Characteristic == "   Last.vaccine.dose.more.than.20.weeks.prior" ~ "Last.vaccine.dose.more.than.20.weeks.prior (%)",
         Characteristic == "   Immunocompromise" ~ "Immunocompromise (%)",
         Characteristic == "   Diabetes" ~ "Diabetes (%)",
         Characteristic == "   Heart.disease.or.stroke" ~ "Heart.disease.or.stroke (%)",
         Characteristic == "   Pulmonary.disease" ~ "Pulmonary.disease (%)",
         Characteristic == "   Bipolar..schizophrenia..and.other.disorders" ~ "Bipolar..schizophrenia..and.other.disorders (%)",
         Characteristic == "   Depression.and.anxiety" ~ "Depression.and.anxiety (%)",
         Characteristic == "   Hematologic.malignancy" ~ "Hematologic.malignancy (%)",
         Characteristic == "   Solid.tumor.malignancy" ~ "Solid.tumor.malignancy (%)",
         Characteristic == "   Rheumatologic.or.inflammatory.bowel.disease" ~ "Rheumatologic.or.inflammatory.bowel.disease (%)",
         TRUE ~ Characteristic),
       'SMD\n(Unweighted)' = round(-Diff.Un, 3),
       'SMD\n(Weighted)' = round(-Diff.Adj, 3)) %>%
  select(Characteristic,  'SMD\n(Unweighted)', 'SMD\n(Weighted)')


#table 1 object
pax.tab1 <- pax.tab1   %>% select(-sw.pax) %>%
  CreateTableOne(vars = allVars, 
     data = ., 
     factorVars = catVars,
     strata = 'paxlovid', 
     addOverall = F, 
     test = F)

pax.tab1 <- print(pax.tab1,  missing = F, explain = T, 
                     varLabels = T, printToggle = T, test = F, dropEqual = T, nonnormal = contVars, contDigits = 1,
                  trim=FALSE, scientific=F)

tab1_df <- left_join(
  as.data.frame(pax.tab1) %>% rownames_to_column(var = "Characteristic"),
  smd, by = "Characteristic") %>%
  mutate(
    Characteristic = str_replace_all(Characteristic, "\\.\\.\\.\\.", " (%)"),
    Characteristic = str_replace_all(Characteristic, "\\.\\.median\\.\\.IQR\\.\\.", " (median [IQR])"),
    Characteristic = str_replace_all(Characteristic, "X.", "  "),
    Characteristic = str_replace_all(Characteristic, "\\.", " "),
    Characteristic = if_else(Characteristic == "n", "No.", Characteristic))


## section to format n with rounding comma seperation 
pax.n<-str_split_fixed(tab1_df$`Nirmatrelvir plus ritonavir`, "\\(", 2)
pax.n<-as.data.frame(pax.n) %>%
  mutate(V1 = prettyNum(as.numeric(V1),big.mark=",",scientific=FALSE), 
         V1 = if_else(V1 == "NA", as.character(NA), V1),
         V2 = str_replace_all(V2, "\\)", ""),
         V2 = round(as.numeric(V2), 0),
         `Nirmatrelvir plus ritonavir` = case_when(
           is.na(V1) ~ as.character(NA),
           !is.na(V1) & is.na(V2) ~ V1, 
           !is.na(V1) & !is.na(V2) ~ paste0(V1, " (", V2, ")")))

nopax.n<-str_split_fixed(tab1_df$`No nirmatrelvir plus ritonavir`, "\\(", 2)
nopax.n<-as.data.frame(nopax.n) %>%
  mutate(V1 = prettyNum(as.numeric(V1),big.mark=",",scientific=FALSE), 
         V1 = if_else(V1 == "NA", as.character(NA), V1),
         V2 = str_replace_all(V2, "\\)", ""),
         V2 = round(as.numeric(V2), 0),
         `No nirmatrelvir plus ritonavir` = case_when(
           is.na(V1) ~ as.character(NA),
           !is.na(V1) & is.na(V2) ~ V1, 
           !is.na(V1) & !is.na(V2) ~ paste0(V1, " (", V2, ")")))

tab1_df$`Nirmatrelvir plus ritonavir` <- pax.n$`Nirmatrelvir plus ritonavir`
tab1_df$`No nirmatrelvir plus ritonavir` <- nopax.n$`No nirmatrelvir plus ritonavir`
##

#remove spaces in parenthetical
tab1_df[,2] = str_replace_all(tab1_df[,2], "\\( ", " (")
tab1_df[,3] = str_replace_all(tab1_df[,3], "\\( ", " (")
tab1_df




# # Set Table header
# header <- str_squish(str_remove("Table 1. Baseline characteristics of COVID-19 cases aged 50 and older (Jan 1 to July 17, 2022)", "\n"))
# 
# # # Set Table footer
# footer <- str_squish(str_remove("", "\n"))
# 
# # Set custom_tab() defaults
# tab1_defaults()
# 
# # Create the flextable object
# flextable_1 <- custom_tab(tab1_df, header)
# 
# 
# # Save as word .docx
# save_as_docx(flextable_1, path = "Nirmatrelvir Tab 1.docx", 
#              pr_section = 
#                prop_section(page_size = page_size(orient = "landscape"), 
#                             type = "continuous"))


### following IP weighting
pax.tab1.wt <-data.frame( enclave.clean.outptdx %>%
                         mutate(
                           paxlovid = as.factor(if_else(paxlovid == 1, "Nirmatrelvir plus ritonavir", "No nirmatrelvir plus ritonavir")),
                           paxlovid = fct_relevel(paxlovid, "Nirmatrelvir plus ritonavir"),
                           vaxstatus = as.factor(vaxstatus),
                           vaxstatus = fct_relevel(vaxstatus, "Vaccinated and boosted", "Vaccinated"),
                           lastvaxgt20wks  = as.factor(lastvaxgt20wks),
                           race.eth =as.factor(race.eth),
                           highADI = fct_relevel(as.factor(highADI), "0"),
                           MASS.ic = as.factor(if_else(MASS.ic == 3, 1, 0)),
                           CVD = as.factor(CVD),
                           diabetes= as.factor(if_else(MASS.dm == 2, 1, 0)),
                           pulm.dis = as.factor(pulm.dis),
                           inflam.dis = as.factor(inflam.dis),
                           age.cat = as.factor(age.cat),
                           sex = as.factor(sex),
                           solid.malig = as.factor(solid.malig),
                           heme.malig = as.factor(heme.malig),
                           MASS.cat = as.factor(MASS.cat),
                           psych.non.unipolar = as.factor(psych.non.unipolar),
                           depr.anxiety = as.factor(depr.anxiety)
                         ) %>% select(paxlovid, age.cat, sex, race.eth, highADI, vaxstatus, lastvaxgt20wks,    MASS.cat,   obesity.cat, MASS.ic, diabetes, CVD, pulm.dis, psych.non.unipolar, depr.anxiety,
                                      heme.malig, solid.malig, inflam.dis, sw.pax, EMPI) %>%
                         rename(
                           "Vaccination status" = vaxstatus,
                           "Male sex" = sex,
                           "Last vaccine dose more than 20 weeks prior"= lastvaxgt20wks,
                           "Increased neighborhood disadvantage (ADI)" = highADI,
                           "Race and ethnicity" = race.eth,
                           "Body mass index (BMI)" =  obesity.cat,
                           "Age" =  age.cat,
                           "Comorbidity score" =  MASS.cat,
                           "Immunocompromise" =  MASS.ic,
                           "Diabetes" =  diabetes,
                           "Heart disease or stroke" = CVD,
                           "Pulmonary disease" = pulm.dis,
                           "Rheumatologic or inflammatory bowel disease" = inflam.dis,
                           "Solid tumor malignancy" = solid.malig,
                           "Hematologic malignancy" = heme.malig,
                           "Depression and anxiety" = depr.anxiety,
                           "Bipolar, schizophrenia, and other disorders" = psych.non.unipolar
                         ))



# All variables excluding the group variable
allVars <- pax.tab1.wt  %>% select(-('paxlovid'), -('sw.pax'), -('EMPI')) %>% names()

# All categorical variables
catVars <- pax.tab1.wt  %>% select(where(is.factor)) %>%
  select(-('paxlovid')) %>%
  names()

# All continuous variables
contVars <-  pax.tab1.wt   %>% select(where(Negate(is.factor))) %>%
  names()

# All continuous variables
contVars <-  pax.tab1.wt  %>% select(where(Negate(is.factor))) %>%
  select(-('sw.pax'), -('EMPI')) %>%
  names()

library(survey)
pax.tab1.svy <- svydesign(id=~EMPI, strata=~paxlovid, weights=~sw.pax, data=pax.tab1.wt ,
          nest=TRUE)


#table 1 object
pax.tab1.wt <- pax.tab1.svy   %>%
  svyCreateTableOne(vars = allVars,
                 data = .,
                 factorVars = catVars,
                 strata = 'paxlovid',
                 addOverall = F,
                 test = F,
                 smd = TRUE
                 )

pax.tab1.wt <- print(pax.tab1.wt, smd = F, missing = F, explain = T,
                  varLabels = T, printToggle = T, test = F, dropEqual = T, nonnormal = contVars, contDigits = 2)

tab1_df.wt <- as.data.frame(pax.tab1.wt) %>% rownames_to_column(var = "Characteristic") %>%
  mutate(
    Characteristic = str_replace_all(Characteristic, "\\.\\.\\.\\.", " (%)"),
    Characteristic = str_replace_all(Characteristic, "\\.\\.median\\.\\.IQR\\.\\.", " (median [IQR])"),
    Characteristic = str_replace_all(Characteristic, "X.", "  "),
    Characteristic = str_replace_all(Characteristic, "\\.", " "),
    Characteristic = if_else(Characteristic == "n", "No.", Characteristic))

## section to format n with rounding comma seperation 
pax.n.wt<-str_split_fixed(tab1_df.wt$`Nirmatrelvir plus ritonavir`, "\\(", 2)
pax.n.wt<-as.data.frame(pax.n.wt) %>%
  mutate(V1 = prettyNum(round(as.numeric(V1),0),big.mark=",",scientific=FALSE), 
         V1 = if_else(V1 == "NA", as.character(NA), V1),
         V2 = str_replace_all(V2, "\\)", ""),
         V2 = round(as.numeric(V2), 0),
         `Nirmatrelvir plus ritonavir` = case_when(
           is.na(V1) ~ as.character(NA),
           !is.na(V1) & is.na(V2) ~ V1, 
           !is.na(V1) & !is.na(V2) ~ paste0(V1, " (", V2, ")")))

nopax.n.wt<-str_split_fixed(tab1_df.wt$`No nirmatrelvir plus ritonavir`, "\\(", 2)
nopax.n.wt<-as.data.frame(nopax.n.wt) %>%
  mutate(V1 = prettyNum(as.numeric(V1),big.mark=",",scientific=FALSE), 
         V1 = if_else(V1 == "NA", as.character(NA), V1),
         V2 = str_replace_all(V2, "\\)", ""),
         V2 = round(as.numeric(V2), 0),
         `No nirmatrelvir plus ritonavir` = case_when(
           is.na(V1) ~ as.character(NA),
           !is.na(V1) & is.na(V2) ~ V1, 
           !is.na(V1) & !is.na(V2) ~ paste0(V1, " (", V2, ")")))

tab1_df.wt$`Nirmatrelvir plus ritonavir` <- pax.n.wt$`Nirmatrelvir plus ritonavir`
tab1_df.wt$`No nirmatrelvir plus ritonavir` <- nopax.n.wt$`No nirmatrelvir plus ritonavir`
##

#remove spaces in parenthetical
tab1_df.wt[,2] = str_replace_all(tab1_df.wt[,2], "\\( ", " (")
tab1_df.wt[,3] = str_replace_all(tab1_df.wt[,3], "\\( ", " (")





# Table 1 formation, adapted from this site: https://michaeldismorr.netlify.app/post/publication-ready-tables-with-flextable-and-own-theme-in-r/
tab1_defaults <- function(){
  set_flextable_defaults(font.family = "Calibri", 
                         font.size = 10, 
                         border.color = "black")
}
custom_tab <- function(df, header, footer){
  flextable(df) %>% 
    add_header_lines(header) %>% 
    bold(i = 1, part = "header") %>% 
    hline_top(part = "header", 
              border = fp_border(color = "red", 
                                 width = 3, 
                                 style = "solid")) %>% 
    hline(i = 1, 
          part = "header", 
          border = fp_border(color = "black", 
                             width = 0.25, 
                             style = "solid")) %>% 
    hline_top(part = "body", 
              border = fp_border(color = "black", 
                                 width = 0.25, 
                                 style = "solid")) %>% 
    hline_bottom(part = "body", 
                 border = fp_border(color = "black", 
                                    width = 0.25, 
                                    style = "solid")) %>% 
    border_inner_h(part = "body", 
                   border = fp_border(color = "black", 
                                      width = 0.25, 
                                      style = "dotted")) %>% 
    autofit(part = "body") %>% 
    bg(part = "body", bg = "#f5f5f5") %>% 
    align(part = "all", align = "center") %>% 
    align(j = 1, part = "all", align = "left")
}

tab1_df_join <- left_join(
  tab1_df, tab1_df.wt, by= "Characteristic"
) %>% select(Characteristic:`No nirmatrelvir plus ritonavir.x`, `SMD\n(Unweighted)`, `Nirmatrelvir plus ritonavir.y`, `No nirmatrelvir plus ritonavir.y`, `SMD\n(Weighted)`)



# Set Table header
header <- str_squish(str_remove("Table 1. Baseline characteristics of COVID-19 cases aged 50 and older (Jan 1 to July 17, 2022) and weighted analytic cohort", "\n"))

# Set custom_tab() defaults
tab1_defaults()

# Create the flextable object
flextable_1 <- custom_tab(tab1_df_join, header, footer)


# Save as word .docx
save_as_docx(flextable_1, path = "Tab1 Cases and IPW cohort.docx",
             pr_section =
               prop_section(page_size = page_size(orient = "landscape"),
                            type = "continuous"))

