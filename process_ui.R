# State IC data
# from https://www.dol.gov/ui/data.pdf
# grab state fips codes
state_claims <- tigris::fips_codes %>% 
  mutate(statefips = as.numeric(state_code)) %>% 
  group_by(statefips) %>% 
  summarize(state_name = first(state_name)) %>% 
  # join with State IC data
  inner_join(read_csv("us_initialclaims_week11_2020.csv"), by = "state_name") %>% 
  inner_join(read_csv("us_initialclaims_week12_2020.csv"), by = "state_name") %>% 
  mutate(state_ic_total = ic_2020w11_total + ic_2020w12_total) %>% 
  select(state_name, statefips, state_ic_total)

# UI data from WA. csv converted from
# https://esdorchardstorage.blob.core.windows.net/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Regional-reports/UI-Claims-Karen/2020%20claims/UI%20claims%20week%2011_2020.xlsx

# headline numbers for WA Week 11, as published by US DOL
# use this to scale total WA numbers publised by WA DOL
wa_ic_headline_2020w11 <- 133478

# WA Claims by industry, scaled
wa_claims <- read_csv("wa_initialclaims_week11_2020.csv") %>% 
  select(sector, sector_title, `2020WK11`) %>% 
  rename(ic_2020w11 = `2020WK11`) %>% 
  # drop totals and unclassified
  filter(!sector %in% c("INA/99", "Total")) %>% 
  # use combined manufacturing
  filter(!sector %in% c("31", "32", "33")) %>% 
  # use combined retail
  filter(!sector %in% c("44", "45")) %>% 
  # use combined transportation & warehousing
  filter(!sector %in% c("48", "49")) %>% 
  # SCALE WA INDUSTRY CLAIMS TO TOTAL TO HEADLINE NUMBER 
  mutate(ic_2020w11 = ic_2020w11 * wa_ic_headline_2020w11 / sum(ic_2020w11))

# WA shocks as share of emp, to apply to other states
wa_shocks <- state_industry_emp %>% 
  filter(statefips == 53) %>% 
  inner_join(wa_claims, by = "sector") %>% 
  mutate(wa_shock_industry = -ic_2020w11 / emp) %>% 
  mutate(wa_shock_total = -sum(ic_2020w11) / emp_total) %>% 
  select(sector, matches("shock"))

# Calculate stateXindustry-specific changes
state_industry_shocks <- state_industry_emp %>% 
  inner_join(state_claims, by = "statefips") %>% 
  inner_join(wa_shocks, by = "sector") %>% 
  group_by(statefips) %>% 
  mutate(state_shock_industry = wa_shock_industry * -state_ic_total / sum(wa_shock_industry * emp)) %>% 
  ungroup() %>% 
  mutate(emp_change = state_shock_industry * emp) %>% 
  inner_join(select(wa_claims, sector, sector_title), by = "sector") %>% 
  inner_join(ephi, by = "sector") %>% 
  mutate(ephi_change = emp_change * ephi_rate) %>% 
  select(statefips, state_name, sector, sector_title, emp, emp_total, state_shock_industry, emp_change, ephi_change)
