library(tidyverse)
library(tidyterra)
library(readxl)
library(geojsonR)
library(terra)
library(rvest)
library(rnaturalearth)

# 1 Scrape stock distribution information from GSRF -----------------------
#stock full information
stock_information <- read_rds("Data/stock_success_full_information_final.rds")

stock_distribution <- NULL
for (i in stock_information$GRSF_uuid) {
  
  # i=stock_information$GRSF_uuid[1]
  if(i==""){next}
  
  #----------------------------------------------------------------made web link
  web_link <- paste0("https://ckan-grsf.d4science.org/dataset/",i)
  # browseURL(web_link)
  
  #----------------------------------------------------------scrape from the web
  #scrape from web
  a <- try(read_html(web_link))
  if(is.character(a)){next}
  
  #find selector
  css_selector <- "#content > div.row.wrapper > div > article > div > section.additional-info > div:nth-child(2) > table"
  
  #find what we need and change to a tibble
  b <- a %>%
    html_elements(css = css_selector) %>% 
    html_table()
  
  #find what we need, spatial term
  c <- as.character(filter(b[[1]],Field=="spatial")[2])
  
  #delete part we do not need
  d <- str_match(c,"\\s*(.*?)\\s*manage")[2]
  
  stock_distribution <- bind_rows(stock_distribution,
                                  data.frame(GRSF_uuid=i,geojson_str=d))
  print(i)
}

stock_distribution <- stock_distribution %>% 
  drop_na()
write_rds(stock_distribution,file="Data/stock_distribution.rds")

# #change it to geojson format
# char_js = FROM_GeoJson(url_file_string = d)
# 
# #change to shapefile
# crdref <- "+proj=longlat +datum=WGS84"
# test <- vect(char_js$coordinates,type="polygons", crs=crdref)
# 
# #plot
# plot(test)



# 2 Stock distribution area and centroid------------------------------------------------------
data <- read_rds("Data/stock_distribution.rds")

#ocean data
ocean <- ne_download(type="ocean",category = 'physical')
# plot(vect(ocean),col="black")
ocean <- crop(as.polygons(ext(c(-180,180,-85.60904,89.999999)), crs="+proj=longlat +datum=WGS84"),
              vect(ocean)) #change the ymax to 89.999999, if it is 90, some problems with the expanse function
plot(ocean,col="black")

stock_distribution_area_centroid <- NULL
for (i in 1:dim(data)[1]) {
  
  # i <- 257
  
  #change distribution code to vector
  uuid <- data[i,1]
  str_js = data[i,2]
  char_js = FROM_GeoJson(url_file_string = str_js)
  crdref <- "+proj=longlat +datum=WGS84"
  distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref) #vector
  plot(distribution_loop, col = "black")
  
  #crop only remain ocean area
  a <- try(distribution_ocean_loop <- crop(x=ocean,y=distribution_loop))
  
  if(is.character(a)) {
    
    next
    
  } else {
    
    # plot(distribution_ocean_loop, col = "black")
    
    #area
    area <- expanse(distribution_ocean_loop,unit="km")
    
    if(is_empty(area)) {
      
      next
      
    } else {
      
    area_loop <- sum(area) #km2 total area
    which(area == max(area)) #if area is separated by continents, use the maximum one
    
    #centroid
    centroid <- centroids(distribution_ocean_loop, inside=T)
    lon_loop <- as.data.frame(centroid, geom="XY")[which(area == max(area)),]$x #longitude
    lat_loop <- as.data.frame(centroid, geom="XY")[which(area == max(area)),]$y #latitude
    
    #distribution information
    stock_distribution_area_centroid_loop <- data.frame(GRSF_uuid = uuid,
                                                        area = area_loop,
                                                        centroid_lon = lon_loop,
                                                        centroid_lat = lat_loop)
    
    stock_distribution_area_centroid <- bind_rows(stock_distribution_area_centroid, stock_distribution_area_centroid_loop)
    
    }
    
  }
 
}

write_rds(stock_distribution_area_centroid,file="Data/stock_distribution_area_centroid.rds")


#world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

#plot world map
map <- ggplot(data = world)+
  # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  geom_sf(fill="black",color="black")+
  geom_point(data = stock_distribution_area_centroid, aes(x = centroid_lon, y = centroid_lat), color= "red")+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1))



