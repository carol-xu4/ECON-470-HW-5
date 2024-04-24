# Analysis Code

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

# SUMMARIZE THE DATA

# 1. Share of adult population with direct purchase health insurance over time
final.insurance1 = final.insurance %>%
    group_by(year) %>%
    summarize(total_pop = sum(adult_pop),
    total_ins_direct = sum(ins_direct))

final.insurance1 = final.insurance1 %>%
    mutate(share = total_ins_direct / total_pop)

ggplot(final.insurance1, aes(x = year, y = share)) +
    geom_line() +
    geom_point() +
    labs(title = "Share of Adult Population with Direct Purchase Health Insurance",
    x = "Year",
    y = "Proportion of Adult Population") +
    theme_minimal()
ggsave("Q1.png")

# 2. A decline in the purchase of direct health insurance from 2016 to 2017. This is likely due to the expansion of Medicaid coverage and reforms in the Affordable Care Act. These policy changes increased access to afforadable healthcare coverage, alternatives to direct insurance purchases.

# 3. Share of adult population with Medicaid over time
final.insurance3 = final.insurance %>%
    group_by(year) %>%
    summarize(total_pop = sum(adult_pop),
    total_medicaid = sum(ins_medicaid)) %>%
    mutate(share3 = total_medicaid / total_pop)

ggplot(final.insurance3, aes(x = year, y = share3)) +
    geom_line() +
    geom_point() +
    labs(title = "Share of Adult Population with Medicaid",
    x = "Year",
    y = "Proportion of Adult Population") +
    theme_minimal()
ggsave("Q3.png")

# 4. Share of uninsured over time, by states that expanded Medicaid in 2014 
medicaid_expansion = read.table("data/output/acs_medicaid.txt", header = TRUE, sep = "\t")

medicaid_expansion4 = medicaid_expansion %>%
    group_by(State, year) %>%
    summarize(
        adult_pop = first(adult_pop),
        uninsured = first(uninsured),
        expand_ever = first(expand_ever),
        date_adopted = first(date_adopted),
        expand_year = first(expand_year),
        expand = first(expand)
    ) %>%
    filter(is.na(expand_year) | expand_year == 2014)

uninsured_share = medicaid_expansion4 %>%
  group_by(State, year) %>%
  summarize(
    total_pop = sum(adult_pop),
    total_uninsured = sum(uninsured),
    expand_year = first(expand_year),
    share_uninsured = total_uninsured / total_pop)

uninsured_share = uninsured_share %>%
  mutate(expansion_status = ifelse(expand_year == 2014, "Expanded", "Not Expanded"))

grouped_data = uninsured_share %>%
  group_by(expansion_status, year) %>%
  summarize(average_share_uninsured = mean(share_uninsured))

ggplot(grouped_data, aes(x = year, y = average_share_uninsured, color = expansion_status)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Average Share of Uninsured", color = "Expansion Status", title = "Share of Uninsured Over Time by Medicaid Expansion Status") +
  theme_minimal()
ggsave("Q4.png")

# ESTIMATE ATES

# 5. Average percent of uninsured individuals in 2012 and 2015, by expansion and non-expansion states
medicaid_expansion5 = medicaid_expansion %>%
    group_by(State, year) %>%
    mutate(percent_uninsured = uninsured / adult_pop) %>%
    filter(year %in% c(2012, 2015))

average_uninsured = medicaid_expansion5 %>%
    group_by(year) %>%
    mutate(average_uninsured = mean(percent_uninsured))
    #     # average percent uninsured in 2012 and 2015 are 18.68% and 12.12%


# 6. Effect of Medicaid expansion on uninsurance rate using standard DD regression estimator
medicaid_expansion6 = medicaid_expansion %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(percent_uninsured = uninsured / adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

m.dd = lm(percent_uninsured ~ post + expand_ever + treat, data=medicaid_expansion6)
print(m.dd)

# 7. state and year fixed effects
install.packages("lfe")
library(lfe)

m.dd2 = felm(percent_uninsured ~ post + expand_ever + treat | State + year, data = medicaid_expansion6)
print(m.dd2)

# 8. State and year fixed effects on all states
medicaid_expansion8 = medicaid_expansion %>%
mutate(percent_uninsured = uninsured / adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

m.dd3 = felm(percent_uninsured ~ post + expand_ever + treat | State + year, data = medicaid_expansion8)
print(m.dd3)

# 9. Event study graph
install.packages("fixest")
library(fixest)

reg.dat = medicaid_expansion8 %>% 
  filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2014) | State + year,
                  cluster=~State,
                  data=reg.dat)

iplot(mod.twfe, 
      xlab = 'Time to treatment',
      main = 'Event study')

# 10. 
reg.dat2 = medicaid_expansion %>% 
  filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

mod.twfe2 <- feols(perc_unins~i(year, expand_ever, ref=2014) | State + year,
                  cluster=~State,
                  data=reg.dat)

iplot(mod.twfe2, 
      xlab = 'Time to treatment',
      main = 'Event study')
