library(tidyverse)


epi.plots<- rbind(
  enclave.clean.outptdx %>% filter(!is.na(covid_date)) %>%
    mutate(date = covid_date,
           endpoint = "COVID-19") %>%
    select(date, endpoint) %>% arrange(date),
  enclave.clean.outptdx %>% filter(!is.na(paxlovid_date)) %>%
    mutate(date = paxlovid_date,
           endpoint = "Nirmatrelvir plus ritonavir prescription") %>%
    select(date, endpoint) %>% arrange(date),
  enclave.clean.outptdx %>%
    mutate(admit_date = pmin(admit_date, death_date, na.rm = TRUE)) %>%
    filter(!is.na(admit_date)) %>%
    mutate(date = admit_date,
           endpoint = "Hospital admission or death") %>%
    select(date, endpoint) %>% arrange(date)) %>%
  mutate(endpoint = as.factor(endpoint)) %>%
  mutate(endpoint = fct_relevel(endpoint, "COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death"))


#Figure 2
rollout<- epi.plots  %>%
  ggplot()+
  theme_classic()  + 
  theme(
    plot.title = element_text(size = rel(1.5)),
    axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)),
    legend.position = c(0.75, 0.8),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank()) +
  geom_bar(data = epi.plots %>% filter(endpoint == "COVID-19"), aes( x= date, fill = endpoint)) +
  geom_bar(data = epi.plots %>% filter(endpoint == "Nirmatrelvir plus ritonavir prescription"), aes( x= date, fill = endpoint)) +
  geom_bar(data = epi.plots %>% filter(endpoint == "Hospital admission or death"), aes( x= date, fill = endpoint)) +
  scale_fill_manual(values = c("#B2474599", "#00A1D599", "#000000"), name = "", breaks=c("COVID-19", "Nirmatrelvir plus ritonavir prescription", "Hospital admission or death")) +
  coord_cartesian(ylim = c(0,800), xlim= c(as.Date("2022-01-01"), as.Date("2022-08-13"))) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2022-08-13")),  labels=scales::label_date_short(),
               breaks = scales::pretty_breaks(8))+
  labs(x="Date", y="Number of Patients",
       title="",
       subtitle = "") 
rollout
ggsave("Fig2 Covid diagnosis treatment.pdf", width = 12, height =10)
ggsave("Fig2 Covid diagnosis treatment.png", width = 12, height =10, dpi = 1000)

