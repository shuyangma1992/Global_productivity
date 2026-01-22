###################################################################################
###################################################################################
###################################################################################
#
# This will load database data from the file DBdata.RData. Put the data file in the
# working directory, then run the line at the bottom of the file.
#
###################################################################################
###################################################################################
###################################################################################
#
# The following objects are tables from RAM (data frames):
#
# --- metadata
#	Summarized metadata
# --- stock
#	General stock metadata
# --- assessment
#	General assessment metadata
# --- taxonomy
#	Taxonomic metadata
# --- management
#	Management authority metadata
# --- assessor
#	Stock assessor metadata
# --- assessmethod
#	Assessment method metadata
# --- area
#	Area metadata
# --- biometrics
#	Parameter data types with descriptions
# --- tsmetrics
#	Time series data types with descriptions
# --- timeseries
#	Full time series data listing
# --- bioparams
#	Full parameter data listing
# --- timeseries_values_views
#	Values by stock and year of common time series types
# --- timeseries_units_views
#	Units corresponding to values in timeseries_values_views
# --- timeseries_ids_views
#	Time series IDs corresponding to values in timeseries_values_views
# --- timeseries_assessments_views
#	Assessment IDs corresponding to values in timeseries_values_views
# --- timeseries_notes_views
#	Notes corresponding to values in timeseries_values_views
# --- timeseries_sources_views
#	Sources corresponding to values in timeseries_values_views
# --- timeseries_years_views
#	Year range corresponding to values in timeseries_values_views
# --- bioparams_values_views
#	Values by stock of common parameter types
# --- bioparams_units_views
#	Units corresponding to values in bioparams_values_views
# --- bioparams_ids_views
#	Parameter IDs corresponding to values in bioparams_values_views
# --- bioparams_assessments_views
#	Assessment IDs corresponding to values in bioparams_values_views
# --- bioparams_sources_views
#	Sources corresponding to values in bioparams_values_views
# --- bioparams_notes_views
#	Notes corresponding to values in bioparams_values_views
#
# ---------------------------------------------------------------------------------------------------
#
# There are also dataframes for the individual most-used time series:
#
# --- tb.data --- Total biomass data
# --- ssb.data --- Spawning stock biomass data
# --- tn.data --- Total abundance data
# --- r.data --- Recruits data
# --- tc.data --- Total catch data
# --- tl.data --- Total landings data
# --- recc.data --- Recreational catch data
# --- f.data --- Fishing mortality data (usually an instantaneous rate)
# --- er.data --- Exploitation rate data (usually an annual fraction harvested)
# --- divtb.data --- TB/TBmsy data
# --- divssb.data --- SSB/SSBmsy data
# --- divf.data --- F/Fmsy data
# --- diver.data --- ER/ERmsy data
# --- divbpref.data --- B/Bmsy pref data (B/Bmsy if available, otherwise B/Bmgt)
# --- divupref.data --- U/Umsy pref data (U/Umsy if available, otherwise U/Umgt)
# --- tbbest.data --- TBbest data (all in MT)
# --- tcbest.data --- TCbest data (all in MT)
# --- erbest.data --- ERbest data (usually an annual fraction harvested)
# --- divtb.mgt.data --- TB/TBmgt data
# --- divssb.mgt.data --- SSB/SSBmgt data
# --- divf.mgt.data --- F/Fmgt data
# --- diver.mgt.data --- ER/ERmgt data
# --- divbpref.mgt.data --- B/Bmgt pref data (B/Bmgt if available, otherwise B/Bmsy)
# --- divupref.mgt.data --- U/Umgt pref data (U/Umgt if available, otherwise U/Umsy)
# --- cpair.data --- Catch data that pairs with tac.data and/or cadv.data
# --- tac.data --- TAC data
# --- cadv.data --- Scientific advice for catch limit data
# --- survb.data --- Fishery-independent survey abundance data
# --- cpue.data --- CPUE data (fishery-dependent)
# --- effort.data --- Fishing effort data (fishery-dependent)
# --- divtn.data --- TN/TNmsy data
# --- divtn.mgt.data --- TN/TNmgt data
# --- cdivmeanc.data --- Catch/(mean catch) data
# --- cdivmsy.data --- Catch/MSY data
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into matrix/dataframe files for the model fits included version of the database.

library(tidyverse)
load("Data/DBdata[asmt][v4.61].RData")

# 1 Catch data ----------------------------------------------------
# including catch and landing
#--------------------------------------------------------------------------catch
tc.data_long <- tcbest.data %>%
  mutate(Year = rownames(tc.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "Catch") %>%
  drop_na()

# # change unit E00 to MT
# tc.data_long <- tc.data_long %>%
#   mutate(Catch = case_when(
#     stockid %in% filter(timeseries_units_views, TC == "E00")$stockid ~ Catch/1000,
#     .default = Catch
#   ))

# stock-specific adjustment
tc.data_long <- tc.data_long %>%
  mutate(Catch = case_when(
    stockid == "ATBTUNAWATL" & Year <= 1967 ~ Catch/1000,
    .default = Catch
  ))

# stocks with all catches equals to 0, change them to NA
catch <- NULL
for (i in unique(tc.data_long$stockid)) {
  # i="ACADRED2J3K"
  catch_cycle <- filter(tc.data_long, stockid == i)
  if (sum(catch_cycle$Catch, na.rm = T) == 0) {
    catch_cycle <- catch_cycle %>%
      mutate(Catch = na_if(Catch, 0))
  }
  catch <- bind_rows(catch, catch_cycle)
  print(i)
}

catch <- catch %>%
  drop_na()

#------------------------------------------------------------------------landing
tl.data_long <- tl.data %>%
  mutate(Year = rownames(tl.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "Landing") %>%
  drop_na()

# stocks with all landings equals to 0, change them to NA
landing <- NULL
for (i in unique(tl.data_long$stockid)) {
  # i="ACADRED2J3K"
  landing_cycle <- filter(tl.data_long, stockid == i)
  if (sum(landing_cycle$Landing, na.rm = T) == 0) {
    landing_cycle <- landing_cycle %>%
      mutate(Landing = na_if(Catch, 0))
  }
  landing <- bind_rows(landing, landing_cycle)
  print(i)
}

landing <- landing %>%
  drop_na()

# combine data
catch_data <- full_join(catch, landing) %>%
  arrange(stockid, Year)

summary(catch_data)
unique(catch_data$stockid) # 1051 stocks


# 2 CPUE data ----------------------------------------------------------
# including total biomass, total abundance and spawning stock biomass
# also cpue (fishery-dependent) and survey abundance (fishery-independent)
tb.data_long <- tbbest.data %>%
  mutate(Year = rownames(tb.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "Total_biomass") %>%
  drop_na()

tn.data_long <- tn.data %>%
  mutate(Year = rownames(tn.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "Total_abundance") %>%
  drop_na()

ssb.data_long <- ssb.data %>%
  mutate(Year = rownames(ssb.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "SSB") %>%
  drop_na()

cpue.data_long <- cpue.data %>%
  mutate(Year = rownames(cpue.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "CPUE") %>%
  drop_na()

survb.data_long <- survb.data %>%
  mutate(Year = rownames(survb.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "Survey_abundance") %>%
  drop_na()

# combine data
biomass_data <- full_join(tb.data_long, tn.data_long) %>%
  full_join(ssb.data_long) %>%
  full_join(cpue.data_long) %>%
  full_join(survb.data_long) %>%
  arrange(stockid, Year)

summary(biomass_data)
unique(biomass_data$stockid) # 1244 stocks


# 3 Combine data ----------------------------------------------------------
# time series data
ts_stocks <- left_join(catch_data, biomass_data)

# 4 Adjustment --------------------------------------------------------------
#-----------------------------------keep stocks with both catch and biomass data
stock_with_catch <- unique(catch_data$stockid)
stock_with_biomass <- unique(biomass_data$stockid)

ts_stocks <- ts_stocks %>%
  filter(stockid %in% intersect(stock_with_catch, stock_with_biomass))
unique(ts_stocks$stockid) # 871 stocks

#-------------------------------------delete stocks with data less than 15 years 
# ts_stocks_length_less_than_30_years <- ts_stocks %>%
#   group_by(stockid) %>%
#   summarise(length=n()) %>%
#   filter(length<30) #length <= 30, 392 stocks
#
# ts_stocks_length_less_than_20_years <- ts_stocks %>%
#   group_by(stockid) %>%
#   summarise(length=n()) %>%
#   filter(length<20) #length <= 20, 201 stocks

ts_stocks_length_less_than_15_years <- ts_stocks %>%
  group_by(stockid) %>%
  summarise(length = n()) %>%
  filter(length < 15) # length <= 15, 102 stocks

# ts_stocks_length_less_than_10_years <- ts_stocks %>%
#   group_by(stockid) %>%
#   summarise(length=n()) %>%
#   filter(length<10) #length <= 10, 54 stocks

ts_stocks <- ts_stocks %>%
  filter(!stockid %in% ts_stocks_length_less_than_15_years$stockid)
unique(ts_stocks$stockid) # 769 stocks

#--------------------------------------------------------------stock information
metadata_stock <- metadata %>%
  filter(stockid %in% unique(ts_stocks$stockid))

# combine data
data <- left_join(ts_stocks, metadata_stock)

#-------------------------------------------------delete stocks at a genus level
data <- data[!endsWith(data$scientificname, "spp"), ]
unique(data$stockid) # 744 stocks

#--------------------------delete stocks in FAO area 2, 4, and 5 (inland waters)
data <- data %>%
  filter(!primary_FAOarea %in% c(" 2", " 4", " 5"))
unique(data$stockid) # 743 stocks
unique(data$scientificname) # 292 species

#-----------------------------------------------B/Bmsy uesd for prior estimation
# B/Bmsy or B/Bmgt, used for estimting prior of K and psi.
divbpref.data_long <- divbpref.data %>%
  mutate(Year = rownames(divbpref.data)) %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "B/Bmsy") %>%
  drop_na()

data <- left_join(data, divbpref.data_long)

write_rds(data, file = "Data/stocks_data.rds")

# 5 Stock success information after JABBA fit -----------------------------------------------------
library(tidyverse)
stock_success <- read_rds("Data/stock_success_final.rds") %>%
  rename("stockid" = 1)

stock_success <- left_join(stock_success, as.data.frame(stock))
unique(stock_success$stockid) # 710 stocks
unique(stock_success$scientificname) # 285 species

write_rds(stock_success, "Data/stock_success_full_information_final.rds")



a <- read_rds("Data/stock_success_full_information_final.rds")

a <- filter(as_tibble(stock),stockid == 'STRMULLIIIa-IV-VIId')

c <- filter(timeseries, stockid == 'STRMULLIIIa-IV-VIId')

a <- filter(as_tibble(stock),stockid == 'ANCHMEDGSA29')

a <- filter(as_tibble(stock),stockid == 'JANCHOPJPN')

a <- filter(as_tibble(stock),stockid == 'HADVIIb-k')

a <- filter(as_tibble(stock),stockid == 'JCSQUIDJPNWR')
test <- filter(as_tibble(stock), scientificname == 'Gadus morhua')


b <- filter(as_tibble(stock),stockid == 'GTRIGGM')

b <- filter(as_tibble(stock),stockid == 'AMPL4T')

b <- filter(as_tibble(stock),stockid == 'PANDALGOM')

b <- filter(as_tibble(stock),stockid == 'RROCKLOBSTERSAUSNZ')

b <- filter(as_tibble(stock),stockid == 'GRSPROCKNCAL')






# 6 stock group information for Figure 1 productivity comparison -----------------------------------------------
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname, primary_FAOarea)
unique(stock_success$stockid) # 710 stocks
unique(stock_success$scientificname) # 28 species
unique(stock_success$primary_FAOarea) # 16 FAO areas

# load ram legacy database and find family/order/class in taxonomy 
# and fishery type/taxgroup in metadata
load("Data/DBdata[asmt][v4.61].RData")

a <- taxonomy %>%
  select(scientificname, family, ordername, classname)

b <- metadata %>%
  select(stockid, FisheryType, taxGroup)

# stock success group information data
stock_success_group <- left_join(stock_success, a) %>%
  left_join(b) %>%
  filter(!duplicated(stockid))

write_rds(stock_success_group, file = "Data/stock_success_group_information.rds")

# 99999999999productivity  ------------------------------------------------
a <- filter(as_tibble(stock), stockid == "STRMULLIIIa-IV-VIId")
a <- filter(as_tibble(stock), stockid == "ANCHMEDGSA29")
a <- filter(as_tibble(stock), stockid == "JANCHOPJPN")
a <- filter(as_tibble(stock), stockid == "HADVIIb-k")
a <- filter(as_tibble(stock), stockid == "JCSQUIDJPNWR")

a <- filter(as_tibble(stock), stockid == "GTRIGGM")
a <- filter(as_tibble(stock), stockid == "AMPL4T")
a <- filter(as_tibble(stock), stockid == "PANDALGOM")
a <- filter(as_tibble(stock), stockid == "RROCKLOBSTERSAUSNZ")
a <- filter(as_tibble(stock), stockid == "GRSPROCKNCAL")

a <- filter(as_tibble(stock), scientificname == "Reinhardtius hippoglossoides")
b <- select(as_tibble(tn.data), "GHALV-VI-XII-XIV")



# 999999999 winner loser information --------------------------------------
a <- filter(as_tibble(stock), stockid == "GURCH4RST")
a <- filter(as_tibble(stock), stockid == "PLAIC2432")
a <- filter(as_tibble(stock), stockid == "SCALL5ZMAB")
a <- filter(as_tibble(stock), stockid == "TUR2232")
a <- filter(as_tibble(stock), stockid == "PLAIC2123")



a <- filter(as_tibble(stock), stockid == "WHITNS-VIId")
a <- filter(as_tibble(stock), stockid == "WHITVIIbce-k")
a <- filter(as_tibble(stock), stockid == "WHITMEDGSA29")
a <- filter(as_tibble(stock), stockid == "WHITVIIa")
a <- filter(as_tibble(stock), stockid == "WHITIIIa")






