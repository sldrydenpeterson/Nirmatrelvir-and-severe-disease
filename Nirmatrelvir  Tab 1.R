
library(tidyverse)
library(readxl)
library(broom)
library(janitor)
library(lubridate)
library(tableone)
library(flextable)
library(officer)

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
                heme.malig, solid.malig, inflam.dis) %>% 
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

#table 1 object
pax.tab1 <- pax.tab1   %>%
  CreateTableOne(vars = allVars, 
     data = ., 
     factorVars = catVars,
     strata = 'paxlovid', 
     addOverall = F, 
     test = F)

pax.tab1 <- print(pax.tab1, smd = T, missing = F, explain = T, 
                     varLabels = T, printToggle = T, test = F, dropEqual = T, nonnormal = contVars, contDigits = 2)

tab1_df <- as.data.frame(pax.tab1) %>% rownames_to_column(var = "Characteristic") %>%
  mutate(
    Characteristic = str_replace_all(Characteristic, "\\.\\.\\.\\.", " (%)"),
    Characteristic = str_replace_all(Characteristic, "\\.\\.median\\.\\.IQR\\.\\.", " (median [IQR])"),
    Characteristic = str_replace_all(Characteristic, "X.", "  "),
    Characteristic = str_replace_all(Characteristic, "\\.", " "),
    Characteristic = if_else(Characteristic == "n", "No.", Characteristic))

#remove spaces in parenthetical
tab1_df[,2] = str_replace_all(tab1_df[,2], "\\( ", " (")
tab1_df[,3] = str_replace_all(tab1_df[,3], "\\( ", " (")



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



# Set Table header
header <- str_squish(str_remove("Table 1. Baseline characteristics of COVID-19 cases aged 50 and older (Jan 1 to July 17, 2022)", "\n"))

# # Set Table footer
footer <- str_squish(str_remove("", "\n"))

# Set custom_tab() defaults
tab1_defaults()

# Create the flextable object
flextable_1 <- custom_tab(tab1_df, header)


# Save as word .docx
save_as_docx(flextable_1, path = "Nirmatrelvir Tab 1.docx", 
             pr_section = 
               prop_section(page_size = page_size(orient = "portrait"), 
                            type = "continuous"))


# ### following IP weighting
# pax.tab1.wt <-data.frame( enclave.clean.outptdx %>%
#                          mutate(
#                            paxlovid = as.factor(if_else(paxlovid == 1, "Nirmatrelvir plus ritonavir", "No nirmatrelvir plus ritonavir")),
#                            paxlovid = fct_relevel(paxlovid, "Nirmatrelvir plus ritonavir"),
#                            vaxstatus = as.factor(vaxstatus),
#                            vaxstatus = fct_relevel(vaxstatus, "Vaccinated and boosted", "Vaccinated"),
#                            lastvaxgt20wks  = as.factor(lastvaxgt20wks),
#                            race.eth =as.factor(race.eth),
#                            highADI = fct_relevel(as.factor(highADI), "0"),
#                            MASS.ic = as.factor(if_else(MASS.ic == 3, 1, 0)),
#                            CVD = as.factor(CVD),
#                            diabetes= as.factor(if_else(MASS.dm == 2, 1, 0)),
#                            pulm.dis = as.factor(pulm.dis),
#                            inflam.dis = as.factor(inflam.dis),
#                            age.cat = as.factor(age.cat),
#                            sex = as.factor(sex),
#                            solid.malig = as.factor(solid.malig),
#                            heme.malig = as.factor(heme.malig),
#                            MASS.cat = as.factor(MASS.cat),
#                            psych.non.unipolar = as.factor(psych.non.unipolar),
#                            depr.anxiety = as.factor(depr.anxiety)
#                          ) %>% select(paxlovid, age.cat, sex, race.eth, highADI, vaxstatus.collapsed, lastvaxgt20wks,    MASS.cat,   obesity.cat, MASS.ic, diabetes, CVD, pulm.dis, psych.non.unipolar, depr.anxiety,
#                                       heme.malig, solid.malig, inflam.dis, sw.overall, EMPI) %>% 
#                          rename(
#                            "Vaccination status" = vaxstatus.collapsed, 
#                            "Male sex" = sex,
#                            "Last vaccine dose more than 20 weeks prior"= lastvaxgt20wks,
#                            "Increased neighborhood disadvantage (ADI)" = highADI,
#                            "Race and ethnicity" = race.eth,
#                            "Body mass index (BMI)" =  obesity.cat, 
#                            "Age" =  age.cat, 
#                            "Comorbidity score" =  MASS.cat, 
#                            "Immunocompromise" =  MASS.ic, 
#                            "Diabetes" =  diabetes, 
#                            "Heart disease or stroke" = CVD,
#                            "Pulmonary disease" = pulm.dis,
#                            "Rheumatologic or inflammatory bowel disease" = inflam.dis, 
#                            "Solid tumor malignancy" = solid.malig, 
#                            "Hematologic malignancy" = heme.malig, 
#                            "Depression and anxiety" = depr.anxiety, 
#                            "Bipolar, schizophrenia, and other disorders" = psych.non.unipolar
#                          )) 
# 
# 
# 
# # All variables excluding the group variable
# allVars <- pax.tab1.wt  %>% select(-('paxlovid'), -('sw.overall'), -('EMPI')) %>% names()
# 
# # All categorical variables
# catVars <- pax.tab1.wt  %>% select(where(is.factor)) %>% 
#   select(-('paxlovid')) %>% 
#   names()
# 
# # All continuous variables
# contVars <-  pax.tab1.wt   %>% select(where(Negate(is.factor))) %>% 
#   names()
# 
# # All continuous variables
# contVars <-  pax.tab1.wt  %>% select(where(Negate(is.factor))) %>% 
#   select(-('sw.overall'), -('EMPI')) %>%
#   names()
# 
# library(survey)
# pax.tab1.svy <- svydesign(id=~EMPI, strata=~paxlovid, weights=~sw.overall, data=pax.tab1.wt ,
#           nest=TRUE)
# 
# 
# #table 1 object
# pax.tab1.wt <- pax.tab1.svy   %>%
#   svyCreateTableOne(vars = allVars, 
#                  data = ., 
#                  factorVars = catVars,
#                  strata = 'paxlovid', 
#                  addOverall = F, 
#                  test = F,
#                  smd = TRUE
#                  )
# 
# pax.tab1.wt <- print(pax.tab1.wt, smd = T, missing = F, explain = T, 
#                   varLabels = T, printToggle = T, test = F, dropEqual = T, nonnormal = contVars, contDigits = 2)
# 
# tab1_df <- as.data.frame(pax.tab1.wt) %>% rownames_to_column(var = "Characteristic") %>%
#   mutate(
#     Characteristic = str_replace_all(Characteristic, "\\.\\.\\.\\.", " (%)"),
#     Characteristic = str_replace_all(Characteristic, "\\.\\.median\\.\\.IQR\\.\\.", " (median [IQR])"),
#     Characteristic = str_replace_all(Characteristic, "X.", "  "),
#     Characteristic = str_replace_all(Characteristic, "\\.", " "),
#     Characteristic = if_else(Characteristic == "n", "No.", Characteristic))
# 
# #remove spaces in parenthetical
# tab1_df[,2] = str_replace_all(tab1_df[,2], "\\( ", " (")
# tab1_df[,3] = str_replace_all(tab1_df[,3], "\\( ", " (")
# 
# 
# 
# 
# 
# 
# # Set Table header
# header <- str_squish(str_remove("IP weighted cohort of COVID-19 cases aged 50 and older (Jan 1 to July 17, 2022)", "\n"))
# 
# # Set custom_tab() defaults
# tab1_defaults()
# 
# # Create the flextable object
# flextable_1 <- custom_tab(tab1_df, header, footer)
# 
# 
# # Save as word .docx
# save_as_docx(flextable_1, path = "IPW cohort.docx", 
#              pr_section = 
#                prop_section(page_size = page_size(orient = "portrait"), 
#                             type = "continuous"))

