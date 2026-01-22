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





# test --------------------------------------------------------------------
#test North Sea cod
stock_information %>% 
  filter(GRSF_uuid=="d2362bce-b855-3367-9c46-c479eeaca2af")

str_js = '{"type":"Polygon","coordinates":[[[4.995632607347535,62.0358188123137],[-4.10618856404417,62.02118766148825],[-3.969022513404653,58.57051935809459],[-3.025231359446345,58.68366459693922],[-3.280204762742447,58.30455237989563],[-3.919736013733805,57.93082587924359],[-4.065851009688791,57.65203993394169],[-3.48494701615244,57.74656930193135],[-2.863103204678534,57.60883243257903],[-1.906312757674805,57.61269820019326],[-2.127378638083612,57.11073462494557],[-2.676671124181143,56.46426397658389],[-3.293913630277683,56.09510202089188],[-2.517059170446935,56.00612839043123],[-1.895088648708398,55.76499246781332],[-1.528794296123353,55.27599533668434],[-1.347407075610112,54.81672906928909],[-0.7593650143346342,54.5460221336952],[-0.2653977814052622,54.27621740891947],[-0.1445687252136674,53.84211364938801],[0.02171129110755121,53.45607457568998],[0.2645733090881941,53.04021230653989],[0.1952595768006127,52.83925679284135],[0.6349594522398538,52.97322029371145],[1.219616751240291,52.94469229895419],[1.595964570063959,52.6703867991073],[1.569210893721194,52.23881110827916],[1.212902870022612,51.79798795569815],[0.6843933965710036,51.5422195560283],[0.5532885053368064,51.45391560027611],[0.8730854841801383,51.38481668451105],[1.228945899199538,51.3753160191151],[1.41572476295345,51.38302451407577],[1.442002688314925,51.27422525705885],[1.271508922200635,51.08643874048872],[0.9868783354055521,50.98468828724366],[0.9472225610992591,50.90583855643961],[0.7936098842693662,50.92247704163143],[0.4548751039656085,50.7999003354746],[0.203631800565236,50.71539537658264],[-0.7361470620596455,50.74074277759286],[-1.214042177443807,50.8126273739202],[-1.889926139972488,50.66713985379991],[-2.054466322592638,50.54694363122503],[-1.908834102816306,49.6836260224062],[-1.585531845516109,49.66142237047767],[-1.345363518221113,49.68240261158098],[-1.254526588210319,49.51283736650952],[-1.141700658661401,49.3562078354303],[-0.8607050973400832,49.37494059730094],[-0.1027132823156107,49.2766521474065],[0.2584348027845684,49.44117458270349],[0.01598672749221208,49.54937035812215],[0.2974360000596232,49.74461624371045],[0.8132448593888753,49.91313167727459],[0.8364447124586194,49.92575736859253],[1.3287090739283,50.06481506242193],[1.595976548949503,50.2700026734846],[1.607742317247687,50.91673540261801],[1.966840352239085,50.97743726451053],[3.562907129460562,51.43106414865189],[4.527002585532669,52.30620428366265],[4.786478808578265,53.24096973877723],[5.580343920010855,53.46251840642709],[6.983910507781075,53.67697572811039],[8.100068544722564,53.81520794476966],[8.43205918006735,53.61228764610885],[8.814778491630008,53.92178715746206],[8.604656210527908,54.49011726083855],[8.319292380218219,54.8541404343552],[8.578036144052332,55.25252366708562],[8.021429702970435,55.57569908646646],[8.150059972868867,56.08684447758792],[8.247309655549532,56.74861923704645],[8.52876504921735,57.05642082337066],[9.186117368330041,57.17773556397455],[9.914200924930814,57.4597291276029],[10.57060033650071,57.7497197305957],[11.40189676458094,57.99364630654726],[11.21637558178205,58.69054379205739],[10.86876017302327,59.15941377383609],[10.31362962099044,59.07332244588125],[9.474899625530087,58.61893756874367],[8.531028734468238,58.30095299384966],[7.872021384802002,58.09978094369148],[7.099833074016894,58.07905396147742],[6.441477381197583,58.28834872634275],[5.652738275066218,58.72735682436647],[5.444755919006903,59.16470980265076],[4.915940753137775,60.57618982029149],[4.960925649588479,61.57622940926572],[4.995632607347535,62.0358188123137]]]}'

char_js = FROM_GeoJson(url_file_string = str_js)

crdref <- "+proj=longlat +datum=WGS84"

test_north_sea_cod <- vect(char_js$coordinates,type="polygons", crs=crdref)

plot(test_north_sea_cod)



# test scrape from web pages rvest package -------------------------------------
#scrape from web
a <- read_html("https://ckan-grsf.d4science.org/dataset/d01df356-a478-3bf5-9922-97de69baf055")

#change to tibble
b <- a %>%  html_elements("section") %>% html_table()

#find what we need, spatial term
c <- as.character(filter(b[[4]],Field=="spatial")[2])

#delete part we do not need
d <- str_match(c,"\\s*(.*?)\\s*manage")[2]

#change it to geojson format
char_js = FROM_GeoJson(url_file_string = d)

#change to shapefile
crdref <- "+proj=longlat +datum=WGS84"
test <- vect(char_js$coordinates,type="polygons", crs=crdref)

#plot
plot(test)







# test distribution area and centroid -------------------------------------
#distribution data
data <- read_rds("Data/stock_distribution.rds")

#ocean data
ocean <- ne_download(type="ocean",category = 'physical')
ocean <- crop(as.polygons(ext(c(-180,180,-85.60904,89.999999)), crs="+proj=longlat +datum=WGS84"),
              vect(ocean)) #change the ymax to 89.999999, if it is 90, some problems with the expanse function
plot(ocean,col="black")

#distribution data
a <- data %>% filter(GRSF_uuid == "d8d6f353-4053-38fa-81e4-d2cf43296a58")
a <- data %>% filter(GRSF_uuid == "6ef91e5a-4a51-3ff8-bfc9-065cba1a5f79")
a <- data %>% filter(GRSF_uuid == "d2362bce-b855-3367-9c46-c479eeaca2af")
a <- data %>% filter(GRSF_uuid == "e2fa0934-8ba8-391c-a53f-48653487a32d")

str_js = a[1,2]
char_js = FROM_GeoJson(url_file_string = str_js)
crdref <- "+proj=longlat +datum=WGS84"
test_a <- vect(char_js$coordinates,type="polygons", crs=crdref)
plot(test_a,col="black")

#crop only ocean area
b <- crop(x=ocean,y=test_a)
plot(b,col="black")

#area
area <- expanse(b,unit="km")
which(area == max(area)) #if area is separated by continents, use the maximum one
sum(area) #km2

#centroid
centroid <- centroids(b, inside=T)
as.data.frame(centroid, geom="XY")[which(area == max(area)),]$x #longitude
as.data.frame(centroid, geom="XY")[which(area == max(area)),]$y #latitude



# test stock distribution centroid -------------------------------------------------
data <- read_rds("Data/stock_distribution.rds")

stock_information <- read_rds("Data/stock_success_full_information_final.rds")

data <- left_join(data,stock_information)
unique(data$scientificname)
data <- read_rds("Data/stock_distribution_area_centroid.rds")





















