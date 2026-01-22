library(tidyverse)
library(terra)
library(geojsonR)
library(ggsci)
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"),C=windowsFont("Arial"))
#theme
theme_set(theme_classic())
#plot color
# show_col(pal_npg()(10))
mypal <- pal_npg()(9)


#distribution
distribution <- read_rds("Data/stock_distribution.rds") %>% 
  filter(GRSF_uuid == "182f1148-4c60-3b13-a765-21b1b47ead6b")
str_js = distribution[1,2]
char_js = FROM_GeoJson(url_file_string = str_js)
crdref <- "+proj=longlat +datum=WGS84"
test_a <- vect(char_js$coordinates,type="polygons", crs=crdref)
plot(test_a,col="black")


# Macruronus novaezelandiae -----------------------------------------------
#Hoki Macruronus novaezelandiae
data <- read_rds("Data/stock_success_full_information_final.rds")

data <- data %>% 
  filter(scientificname == "Macruronus novaezelandiae")


productivity_characteristics %>% 
  filter(stockid %in% data$stockid)


# Gadus morhua ------------------------------------------------------------
#Gadus morhua
data <- read_rds("Data/stock_success_full_information_final.rds")

data <- data %>% 
  filter(scientificname == "Gadus morhua")

#iceland cod
cod_iceland <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "CODICE") %>% 
  select(1, 2, 10:1009)

cod_iceland_summary <- cod_iceland %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

cod_iceland_summary <- cod_iceland_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_cod_iceland <- ggplot(cod_iceland_summary) +
  ggtitle("Atlantic cod Iceland Grounds")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("Iceland_cod_projection.PDF",device = cairo_pdf,width = 5,height = 3)


#greenland cod east
cod_greenland_east <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "COD1F-XIV") %>% 
  select(1, 2, 10:1009)

cod_greenland_east_summary <- cod_greenland_east %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

cod_greenland_east_summary <- cod_greenland_east_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_cod_greenland_east <- ggplot(cod_greenland_east_summary) +
  ggtitle("Atlantic cod NAFO 1F and ICES 14")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("Greenland_east_cod_projection.PDF",device = cairo_pdf,width = 5,height = 3)


#greenland cod west
cod_greenland_west <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "COD1IN") %>% 
  select(1, 2, 10:1009)

cod_greenland_west_summary <- cod_greenland_west %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

cod_greenland_west_summary <- cod_greenland_west_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_cod_greenland_west <- ggplot(cod_greenland_west_summary) +
  ggtitle("Atlantic cod NAFO Subarea 1 (inshore)")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("Greenland_west_cod_projection.PDF",device = cairo_pdf,width = 5,height = 3)







# capelin Mallotus villosus --------------------------
data <- read_rds("Data/stock_success_full_information_final.rds")
data <- data %>% 
  filter(scientificname == "Mallotus villosus")

#capelin
capelin <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "CAPEIIa-V-XIV") %>% 
  select(1, 2, 10:1009)

capelin_summary <- capelin %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

capelin_summary <- capelin_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_capelin <- ggplot(capelin_summary) +
  ggtitle("Capelin ICES 2a-5-14")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("capelin_projection.PDF",device = cairo_pdf,width = 5,height = 3)


# beaked redfish Sebastes mentella --------------------------
data <- read_rds("Data/stock_success_full_information_final.rds")
data <- data %>% 
  filter(scientificname == "Sebastes mentella")

#beaked_redfish
beaked_redfish <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "REDDEEPDP-1-2-V-XII-XIV") %>% 
  select(1, 2, 10:1009)

beaked_redfish_summary <- beaked_redfish %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

beaked_redfish_summary <- beaked_redfish_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_beaked_redfish <- ggplot(beaked_redfish_summary) +
  ggtitle("Beaked redfish ICES 5-12-14 and NAFO Subareas 1-2 (deep)")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("beaked_redfish_projection.PDF",device = cairo_pdf,width = 5,height = 3)




# Golden redfish Sebastes norvegicus --------------------------
data <- read_rds("Data/stock_success_full_information_final.rds")
data <- data %>% 
  filter(scientificname == "Sebastes norvegicus")

#golden_redfish
golden_redfish <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds") %>% 
  filter(stockid == "GOLDREDV-VI-XII-XIV") %>% 
  select(1, 2, 10:1009)

golden_redfish_summary <- golden_redfish %>% 
  pivot_longer(-c(year, ssp), names_to = "number", values_to = "productivity")

golden_redfish_summary <- golden_redfish_summary %>% 
  group_by(year, ssp) %>% 
  summarise(median = median(productivity),
            q_25 = quantile(productivity, 0.25),
            q_75 = quantile(productivity, 0.75))

# plot
f_golden_redfish <- ggplot(golden_redfish_summary) +
  ggtitle("Golden redfish ICES 5-6-12-14")+
  geom_ribbon(aes(x = year, ymin = q_25, ymax = q_75, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = median, color = ssp)) +
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[2], "ssp585" = mypal[1])) +
  scale_x_continuous("Year", limits = c(2020,2100))+
  scale_y_continuous("Productivity")+
  theme(
    plot.title = element_text(family = "Arial", face = 2),
    plot.subtitle = element_text(family = "Arial", size = 6),
    axis.text.y = element_text(family = "Arial"),
    axis.text.x = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    legend.position = c(0.2,0.2)
  )

ggsave("golden_redfish_projection.PDF",device = cairo_pdf,width = 5,height = 3)
