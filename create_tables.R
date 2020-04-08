# create some .csv files that be used as tables
# formatted and knit together by index.Rmd

# Table 1. EPHI rates by industry
ephi %>% 
  inner_join(select(wa_claims, sector, sector_title), by = "sector") %>%
  arrange(sector_title) %>% 
  select(sector_title, ephi_rate) %>% 
  write_csv("table_ephi.csv")

# Table 2. WA Claims table
# create total numbers for WA
wa_claims_total_tbl <- wa_claims %>% 
  inner_join(filter(state_industry_emp, statefips == 53), by = "sector") %>% 
  summarize(emp_change = sum(ic_2020w11), emp_total = first(emp_total)) %>%
  mutate(wa_shock_industry = emp_change / emp_total) %>% 
  mutate(sector_title = "All industries") %>% 
  select(sector_title, emp_change, wa_shock_industry)
# combine industry & total numbers for WA
wa_claims_shocks <- wa_claims %>% 
  inner_join(wa_shocks, by = "sector") %>%
  rename(emp_change = ic_2020w11) %>% 
  mutate(wa_shock_industry = -wa_shock_industry) %>% 
  arrange(sector_title) %>% 
  bind_rows(wa_claims_total_tbl) %>% 
  left_join(ephi, by="sector") %>% 
  mutate(ephi_change = ephi_rate * emp_change) %>% 
  mutate(ephi_change = ifelse(is.na(ephi_change), sum(ephi_change, na.rm=TRUE), ephi_change)) %>% 
  select(sector_title, emp_change, ephi_change, wa_shock_industry)

write_csv(wa_claims_shocks, "table_wa.csv")


# Table 3. National industry-specific and overall changes
# First calculate all industry totals
us_total_tbl <- state_industry_shocks  %>% 
  summarize(emp_change = sum(emp_change), emp_total = sum(emp), ephi_change = sum(ephi_change)) %>% 
  mutate(shock_industry = emp_change / emp_total) %>% 
  select(-emp_total) %>% 
  mutate(sector_title = "All industries")
# industry-specific totals
us_claims <- state_industry_shocks %>% 
  group_by(sector_title) %>% 
  summarize(emp_change = sum(emp_change), ephi_change = sum(ephi_change), emp_total = sum(emp)) %>% 
  mutate(shock_industry = emp_change / emp_total) %>% 
  select(-emp_total) %>%
  arrange(sector_title) %>% 
  bind_rows(us_total_tbl) %>% 
  mutate_at(vars(-sector_title), ~.*-1)

write_csv(us_claims, "table_us.csv")


# Table 4 and Figure A: state-specific changes
state_summary_shocks <- state_industry_shocks %>% 
  group_by(statefips) %>% 
  summarize(state_name = first(state_name), emp = first(emp_total), emp_change = sum(emp_change), ephi_change = sum(ephi_change)) %>% 
  mutate(emp_change_pct = emp_change / emp) %>% 
  select(state_name, emp_change, ephi_change, emp_change_pct) %>% 
  mutate(state_name = ifelse(state_name == "U.S. Virgin Islands", "Virgin Islands", state_name)) %>% 
  mutate_at(vars(-state_name), ~.*-1)

write_csv(state_summary_shocks, "table_states.csv")

