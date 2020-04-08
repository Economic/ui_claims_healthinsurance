# Read in 2018 March CPS
# marchcps <- haven::read_dta("/data/cps/march/census/stata/cpsmarch_2018.dta.zip")
ephi <- marchcps %>% 
  # adult/af universe for health insurance q
  filter(prpertyp == 2 | prpertyp == 3) %>%
  # workers last year with industry info
  filter(industry > 0) %>% 
  # keep only private and public emp (drop self employed & without pay)
  filter(clwk == 1 | clwk == 2) %>% 
  # convert 2012 Census industry codes to NAICS sectors
  # using codebook https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/census-2012-final-code-list.xls
  mutate(
    sector = case_when(
      # Ag
      industry >= 170 & industry <= 290 ~ "11",
      # Mining
      industry >= 370 & industry <= 490 ~ "21",
      # Utilities
      industry >= 570 & industry <= 690 ~ "22", 
      # Construction
      industry == 770 ~ "23",
      # Manufacturing
      industry >= 1070 & industry <= 3990 ~ "31-33",
      # Wholesale Trade
      industry >= 4070 & industry <= 4590 ~ "42",
      # Retail Trade
      industry >= 4670 & industry <= 5790 ~ "44-45",
      # Transportation & Warehousing
      industry >= 6070 & industry <= 6390 ~ "48-49",
      # Information
      industry >= 6470 & industry <= 6780 ~ "51",
      # Finance & Insurance
      industry >= 6870 & industry <= 6990 ~ "52",
      # Real Estate & Rental & Leasing
      industry >= 7070 & industry <= 7190 ~ "53",
      # Professional, Scientific, & Technical Services
      industry >= 7270 & industry <= 7490 ~ "54",
      # Management of companies and enterprises
      industry == 7570 ~ "55",
      # Administrative & support & waste management services
      industry >= 7580 & industry <= 7790 ~ "56",
      # Educational Services
      industry >= 7860 & industry <= 7890 ~ "61",
      # Health Care and Social Assistance
      industry >= 7970 & industry <= 8470 ~ "62",
      # Arts, Entertainment, and Recreation
      industry >= 8560 & industry <= 8590 ~ "71",
      # Accommodation and Food Services
      industry >= 8660 & industry <= 8690 ~ "72",
      # Other Services, Except Public Administration
      industry >= 8770 & industry <= 9290 ~ "81",
      # Public Administration (include AF)
      industry >= 9370 & industry <= 9890 ~ "92"
    )
  ) %>% 
  mutate(ephi = ifelse(hi == 1, 1, 0)) %>% 
  group_by(sector) %>% 
  summarize(ephi_rate = weighted.mean(ephi, w = marsupwt)) 