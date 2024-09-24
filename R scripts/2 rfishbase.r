library(tidyverse)
library(rfishbase)


# Derive stock information from fishbase and sealifebase ------------------
#read data
stock_data <- read_rds("Data/stocks_data.rds")

###############################step 1: derive data from fishbase and sealifebase
fishbase_information <- NULL
for (i in unique(stock_data$scientificname)) { #298 species
  # 
  # i <- "Sebastes fasciatus"
  # i <- "Theragra chalcogramma"
  # i <- "Chionoecetes opilio"
  # i <- "Gadus morhua"
  
  data_loop <- estimate(i) 
  data_loop$LastModified <- as.character(data_loop$LastModified)
  data_loop$scientificname <- i
  stock_resilience <- data.frame(Resilience=na.omit(stocks(i)$Resilience)) %>% 
    group_by(Resilience) %>% 
    summarise(count=n()) %>% 
    ungroup() %>% 
    arrange(desc(count)) 
  data_loop$Resilience <- stock_resilience[1,1]
  
  #if no such name in fishbase, try synonyms
  if (is.na(data_loop$Species[1])) { 
    
    synonyms_scientificname <- synonyms(i)
    data_loop <- estimate(synonyms_scientificname$Species)
    data_loop$LastModified <- as.character(data_loop$LastModified)
    data_loop$scientificname <- i
    stock_resilience <- data.frame(Resilience=na.omit(stocks(synonyms_scientificname$Species)$Resilience)) %>% 
      group_by(Resilience) %>% 
      summarise(count=n()) %>% 
      ungroup() %>% 
      arrange(desc(count)) 
    data_loop$Resilience <- stock_resilience[1,1]
    
  } 
  
  #if still no information in fishbase, try sealifebase
  if (is.na(data_loop$Species[1])) {
      
    data_loop <- estimate(i,server = "sealifebase")
    data_loop$scientificname <- i
    stock_resilience <- data.frame(Resilience=na.omit(stocks(i,server = "sealifebase")$Resilience)) %>% 
      group_by(Resilience) %>% 
      summarise(count=n()) %>% 
      ungroup() %>% 
      arrange(desc(count)) 
    data_loop$Resilience <- stock_resilience[1,1]
    
  } 
  
  #combine data
  fishbase_information <- bind_rows(fishbase_information,data_loop)
  print(i)
  
}

fishbase_information <- fishbase_information %>% 
  relocate(scientificname,Species,Resilience,everything())

unique(fishbase_information$scientificname)
unique(fishbase_information$Species)

####################################step 2: check species with multiple synonyms
species_with_multiple_synonyms <- fishbase_information %>% 
  filter(duplicated(scientificname))

#Dentex tumifrons corresponding to Evynnis tumifrons
filter(fishbase_information,scientificname=="Dentex tumifrons")
fishbase_information <- fishbase_information %>% 
  filter(!(Species %in% c("Dentex abei","Dentex hypselosomus","Dentex spariformis")))

#Chrysophrys auratus corresponding to Pagrus auratus
filter(fishbase_information,scientificname=="Chrysophrys auratus")
fishbase_information <- fishbase_information %>% 
  filter(!(Species %in% c("Sparus aurata")))

#Etrumeus teres corresponding to Etrumeus sadina
filter(fishbase_information,scientificname=="Etrumeus teres")
fishbase_information <- fishbase_information %>% 
  filter(!(Species %in% c("Etrumeus acuminatus","Etrumeus golanii","Etrumeus makiawa","Etrumeus micropus","Etrumeus whiteheadi","Etrumeus wongratanai")))

#Epinephelus niveatus corresponding to Hyporthodus niveatus
filter(fishbase_information,scientificname=="Epinephelus niveatus")
fishbase_information <- fishbase_information %>% 
  filter(!(Species %in% c("Hyporthodus niphobles")))

unique(fishbase_information$scientificname) #291 species
# write_rds(fishbase_information,file = "Data/fishbase_information.rds")

########################################step3: check species with no information
a <- stock_data %>% 
  select(scientificname) %>% 
  unique() 
a <- anti_join(a,fishbase_information)

#Farfantepenaeus aztecus corresponding to Penaeus aztecus 
data_loop <- estimate("Penaeus aztecus",server = "sealifebase")
data_loop$scientificname <- "Farfantepenaeus aztecus"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Penaeus aztecus",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#Cervimunida johni corresponding to Grimothea johni
data_loop <- estimate("Grimothea johni",server = "sealifebase")
data_loop$scientificname <- "Cervimunida johni"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Grimothea johni",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#Loligo reynaudii no information, delete
stock_data <- stock_data %>% 
  filter(!scientificname=="Loligo reynaudii")

#Loligo pealeii corresponding to Doryteuthis pealeii
data_loop <- estimate("Doryteuthis pealeii",server = "sealifebase")
data_loop$scientificname <- "Loligo pealeii"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Doryteuthis pealeii",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#Farfantepenaeus duorarum corresponding to Penaeus duorarum 
data_loop <- estimate("Penaeus duorarum",server = "sealifebase")
data_loop$scientificname <- "Farfantepenaeus duorarum"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Penaeus duorarum",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#Farfantepenaeus notialis corresponding to Penaeus notialis
data_loop <- estimate("Penaeus notialis",server = "sealifebase")
data_loop$scientificname <- "Farfantepenaeus notialis"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Penaeus notialis",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#Litopenaeus setiferus corresponding to Penaeus setiferus
data_loop <- estimate("Penaeus setiferus",server = "sealifebase")
data_loop$scientificname <- "Litopenaeus setiferus"
stock_resilience <- data.frame(Resilience=na.omit(stocks("Penaeus setiferus",server = "sealifebase")$Resilience)) %>% 
  group_by(Resilience) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) 
data_loop$Resilience <- stock_resilience[1,1]
fishbase_information <- bind_rows(fishbase_information,data_loop)

#297 species
unique(fishbase_information$scientificname)

#745 stocks Loligo reynaudii deleted
unique(stock_data$stockid)

#save data
write_rds(fishbase_information,file = "Data/fishbase_information.rds")
write_rds(stock_data,file = "Data/stocks_data.rds")












