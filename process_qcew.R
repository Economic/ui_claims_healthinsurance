# Read in QCEW and calculate mean quarterly employment
# requires zipped "singlefile" csvs from https://www.bls.gov/cew/downloadable-data-files.htm
read_qcew <- function(x) {
  read_csv(paste0(x,"_qtrly_singlefile.zip")) %>% 
  # calculate quarterly emp using mean emp across months
  mutate(emp = (month1_emplvl + month2_emplvl + month3_emplvl)/3)
}

# Only data through 2019q3 is available
# For one year of data use 2018q4-2019q3
#
# allqcew20182019 <- bind_rows(read_qcew(2018), read_qcew(2019))
allqcew <- allqcew20182019 %>% 
  filter(year == 2019 | (year == 2018 & qtr == 4))

# Overall totals from highest level of aggregation
qcew_totals <- allqcew %>% 
  filter(agglvl_code == 50) %>% 
  # average annual overall employment
  group_by(area_fips) %>% 
  summarize(emp_total = mean(emp))

# Industry totals from aggregating across ownership
qcew_industry <-  allqcew %>% 
  filter(agglvl_code == 54) %>% 
  # average annual industry X owner employment
  group_by(area_fips, industry_code, own_code) %>% 
  summarize(emp = mean(emp)) %>% 
  # average annual industry employment
  group_by(area_fips, industry_code) %>% 
  summarize(emp = sum(emp))

# Final state-level data
state_industry_emp <- qcew_industry %>% 
  select(area_fips, industry_code, emp) %>% 
  # Re-scale industry-specific totals to sum to overall total
  # and in doing so also drop unclassified industries
  left_join(qcew_totals, by = c("area_fips")) %>% 
  filter(industry_code != 99) %>% 
  group_by(area_fips) %>% 
  mutate(emp = emp * emp_total / sum(emp)) %>% 
  ungroup() %>% 
  mutate(statefips = as.numeric(str_sub(area_fips, 1, 2))) %>% 
  rename(sector = industry_code) %>% 
  select(statefips, sector, emp, emp_total)
