### URP 535: Urban Informatics
### Healthcare Access in Detroit
### Tessa Swanson
### 11/10/2019

library(data.table)
library(rgdal)
library(sf)
library(XML)
library(httr)
library(otpr)
library(geojsonio)
library(ggplot2)
library(scales)
library(maptools)

setwd("/Users/tessa/Desktop/coursework/urban_informatics/transit and health")
#### format npi data ####
npi_file <- "NPPES_Data_Dissemination_November_2019/npidata_pfile_20050523-20191110.csv"
# read full npid data file
npi_raw <- fread(npi_file, fill=TRUE, sep=",")

# filter to locations in MI
mi_facs <- npi_raw[`Provider Business Practice Location Address State Name`=="MI"]
rm(npi_raw)

# filter to only include practicing physicians (Type Code==1 represents individuals, Type Code==2 represents facilities)
mi_facs <- mi_facs[`Entity Type Code`==2]

# write dataset with facility locations and number of physicians by primary service type (Taxonomy Code_1)
mi_facs <- mi_facs[,.N,by=.(`Healthcare Provider Taxonomy Code_1`,
                            `Provider First Line Business Practice Location Address`,
                            `Provider Second Line Business Practice Location Address`,
                            `Provider Business Practice Location Address City Name`,
                            `Provider Business Practice Location Address State Name`,
                            `Provider Business Practice Location Address Postal Code`)]

colnames(mi_facs)<-c("code","address1","address2","city","state","zip","count")

mi_facs[,address1:=sub("\\#.*", "",address1)]

fwrite(mi_facs, "mi_npi_facilities.csv")
mi_facs<-fread("mi_npi_facilities.csv")

#### geocode npi mi facility addresses ####
# geocoder: http://geoservices.tamu.edu/Services/Geocode/WebService/)
# r code inspo: https://github.com/trinker/mapit/blob/master/R/geo_code.R

api_key <- "796b1058a3594530ad910aba56ec4ecd"

tamu_geocoder <- function(api_key, address, city, state, zip){
    root <- "http://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?streetAddress="
    end <- "&format=XML&census=false&notStore=false&version=4.01"

    url <- sprintf("%s%s&city=%s&state=%s&zip=%s&apikey=%s%s", root, address,
               city, state, zip, api_key, end)
    url <- gsub(" ","%",url)

    output <- xmlParse(url)
    output <- xmlToList(output)
    lat <- output$OutputGeocodes$OutputGeocode$Latitude
    lng <- output$OutputGeocodes$OutputGeocode$Longitude
    return(c(lat,lng))
}

coords <- data.table(lat = numeric(), lng = numeric())
start <- Sys.time()
coords <- mapply(tamu_geocoder, api_key, mi_facs$address1, mi_facs$city, mi_facs$state, mi_facs$zip)
mi_facs[,lat:=as.numeric(coords[1,])][,lng:=as.numeric(coords[2,])]
end <- Sys.time()
run_time <- end-start
#~.333 s/row?
fwrite(mi_facs,"mi_npi_facilities_geocoded.csv")


#### import detroit census tracts/data ####
detroit <- readOGR("https://opendata.arcgis.com/datasets/99a8619acf5f433396166b9af9ea8d9c_0.geojson") #block groups

tract_data <- as.data.table(detroit)
tract_data[, GEO.id2 := paste0(STATEFP10,COUNTYFP10,TRACTCE10)]
tract_data <- tract_data[,.N,by=(GEO.id2)]

detroit <- unionSpatialPolygons(detroit,detroit$TRACTCE10) # merge by census tract
det_centroids <- as.data.table(coordinates(detroit))
colnames(det_centroids)<-c("lng","lat")
det_centroids<-det_centroids[115, lat := 42.324778]
det_centroids<-det_centroids[115, lng := -83.053128]
det_centroids<-det_centroids[295, lat := 42.389047]
det_centroids<-det_centroids[295, lng := -83.043268]


pop <- fread("ACS_17_5YR_DP05/ACS_17_5YR_DP05_with_ann.csv")
pop <- pop[2:nrow(pop)]
pop <- pop[,c(2,4,12,96,76,148,260,284)]
colnames(pop)<-c("GEO.id2", "total", "female", "over_65", "under_18", "black", "white", "hispanic")

car_ownership <- fread("car_ownership/ACS_17_5YR_B08141_with_ann.csv")
car_ownership <- car_ownership[2:nrow(car_ownership),c(2,6)] #col 6- no vehicle available
colnames(car_ownership)<-c("GEO.id2","no_veh")

income <- fread("income/ACS_17_5YR_B19001_with_ann.csv")
income <- income[2:nrow(income),c(2,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36)]
colnames(income) <- c("GEO.id2","under_10","10_15","15_20","20_25","25_30","30_35","35_40","40_45",
                      "45_50","50_60","60_75","75_100","100_125","125_150","150_200","over_200")

tract_data <- merge(tract_data,pop, by="GEO.id2", all.x=TRUE)
tract_data <- merge(tract_data,car_ownership,by="GEO.id2", all.x=TRUE)
tract_data <- merge(tract_data,income, by="GEO.id2", all.x=TRUE)


#### open trip planner isochrones ####
otp_con<-otp_connect()
modes_list<-c('WALK','TRANSIT')
travel_times<- seq(15, 90, 15)

centroid_data <- as.data.table(det_centroids)
start <- Sys.time()
for(i in modes_list){
    for(j in travel_times){
        for(n in 1:nrow(centroid_data)){
            centroid_data[n,iso_column:=try(otp_get_isochrone(otp_connect(),location=c(Y,X),cutoffs=j*60,mode=i)[2])]
        }
        colnames(centroid_data)[colnames(centroid_data)=="iso_column"] <- paste0("iso_",i,"_",j)
    }
}
end <- Sys.time()
run_time <- end-start #14.1 hours

#intersect isochrones with mi facilities

#for(i in 1:length(modes_list)){
#    for(j in 1:length(travel_times)){
#        col_number <- j+j*(i-1)+2
#        iso_column <- centroid_data[,..col_number]
#        for(k in 1:nrow(iso_column)){
#            poly <-  geojson_sf(geojson_json(as.list(iso_column[k,]), geometry="polygon"))
#            count <- over(mi_facs_provider, poly, fn = sum(N))
#            # add count to table
#        }
#    }
#}

#### open trip planner drive times ####

mi_facs <- fread("mi_npi_facilities_geocoded.csv")
physician_codes <- c("207Q","208D","207R")
#"207V"-women's health (756), 103T-mental health (1789), 208VP-pain management (28), 2080- pediatrics (913), 1835- pharmacy (1097), 1223-dentist
non_physician_codes <- c() # list within a list? #PAs, RNs,clinical nurse specialists not included?, gyno & women's health NPs included
#"363LW0102X", "363LX0001X"-women's health, "1835P1300X"-pharmacy tech
mi_facs_provider <- mi_facs[code%like%physician_codes[1]|code%like%physician_codes[2]|code%like%physician_codes[3]| code %in% non_physician_codes]
mi_facs_provider <- mi_facs_provider[lat <= max(det_centroids[,2]) & lat >= min(det_centroids[,2]) & lng <= max(det_centroids[,1]) & lng >= min(det_centroids[,1])]

modes_list<-c('WALK','TRANSIT','CAR')
interval<-30
max_time <- 30
travel_times<- seq(interval, max_time, interval)
start <- Sys.time()
facs_columns<-NULL
otp_con <- otp_connect()
for(m in modes_list){
    for(tract in 1:nrow(det_centroids)){
        total_facs <- c(rep(0,length(travel_times)))
        for(fac in 1:nrow(mi_facs_provider)){
            drive_time <- as.numeric(otp_get_times(otp_con, fromPlace = det_centroids[tract, c(lat,lng)],toPlace = mi_facs_provider[fac, c(lat,lng)], mode=m, date = "11-21-2019", time = "12:30:00")[2])
            if(!is.na(drive_time)){
                for(j in 1:length(travel_times)){
                    if(drive_time<travel_times[j]){
                        total_facs[j] <- total_facs[j]+mi_facs_provider[fac,count]
                    }
                }
            }
        }
        facs_columns<-rbind(facs_columns, total_facs)
    }
    colnames(facs_columns) <- paste(m,"_", seq(min(travel_times),max(travel_times),interval), sep = "")
    det_centroids<-cbind(det_centroids,facs_columns)
}
end <- Sys.time()
run_time <- end-start
fwrite(det_centroids,"primary_care_tt.csv")

#### write final dataset ####

primary<-fread("primary_care_tt.csv")
mental<-fread("mental_health_tt.csv")
dental<-fread("dental_tt.csv")
pediatric<-fread("pediatric_tt.csv")
womens<-fread("womens_care_tt.csv")
pharma<-fread("pharmacy_tt.csv")

tract_data<-cbind(tract_data, primary[,3:5])
tract_data<-cbind(tract_data, mental[,3:5])
tract_data<-cbind(tract_data,dental[,3:5])
tract_data<-cbind(tract_data,pediatric[,3:5])
tract_data<-cbind(tract_data,womens[,3:5])
tract_data<-cbind(tract_data,pharma[,3:5])

colnames(tract_data) <- c(colnames(tract_data[,1:26]),
                          "prmry_w","prmry_t","prmry_c",
                          "mental_w","mental_t","mental_c",
                          "dental_w","dental_t","dental_c",
                          "ped_w","ped_t","ped_c",
                          "womens_w","womens_t","womens_c",
                          "pharma_w","pharma_t","pharma_c")

tract_data[,total:=as.numeric(total)]

tract_data[,prmry_w_r :=    prmry_w/total]
tract_data[,prmry_t_r := prmry_t/total]
tract_data[,prmry_c_r :=     prmry_c/total]

tract_data[,mental_w_r :=    mental_w/total]
tract_data[,mental_t_r := mental_t/total]
tract_data[,mental_c_r :=     mental_c/total]

tract_data[,dental_w_r :=    dental_w/total]
tract_data[,dental_t_r := dental_t/total]
tract_data[,dental_c_r :=     dental_c/total]

tract_data[,ped_w_r :=    ped_w/total]
tract_data[,ped_t_r := ped_t/total]
tract_data[,ped_c_r :=     ped_c/total]

tract_data[,womens_w_r :=    womens_w/total]
tract_data[,womens_t_r := womens_t/total]
tract_data[,womens_c_r :=     womens_c/total]

tract_data[,pharma_w_r :=    pharma_w/total]
tract_data[,pharma_t_r := pharma_t/total]
tract_data[,pharma_c_r :=     pharma_c/total]


# join detroit polygons with dataset
detroit<-st_as_sf(detroit)
output<-cbind(as.data.table(det_centroids),tract_data)
coordinates(output)<-~lng+lat
output<-st_as_sf(output)
st_crs(output) <- 4326
joined<-st_join(detroit,output)

# write to shapefile
st_write(joined, "/Users/tessa/Desktop/coursework/urban_informatics/transit and health/ratios_test.shp")


#### demographic analysis ####
tract_data<-as.data.table(joined)

cols<-c("under_10","10_15","15_20","20_25","25_30","30_35","35_40","40_45","45_50")
for (col in cols)
    set(tract_data, j=col, value=as.numeric(tract_data[[col]]))

tract_data[,low_income_prop:=(under_10+`10_15`+`15_20`+`20_25`+`25_30`+`30_35`+`35_40`+`40_45`+`45_50`)/total]
tract_data[,low_income:=0]
tract_data[low_income_prop>.3,low_income:=1]

tract_data[,older_pop_prop:=as.numeric(over_65)/total]
tract_data[,older_pop:=0]
tract_data[older_pop_prop>.20,older_pop:=1]

tract_data[,child_pop_prop:=as.numeric(under_18)/total]
tract_data[,child_pop:=0]
tract_data[child_pop_prop>.3,child_pop:=1]
tract_data[,child_ped_access:=ped_t/as.numeric(under_18)]

tract_data[,carless_prop := as.numeric(no_veh)/total]
tract_data[,carless:=0]
tract_data[carless_prop>.10,carless:=1]

# low income access to primary care (majority of households have income <50k)
ggplot()+
    geom_density(data=tract_data, aes(x=prmry_t_r, color=(low_income==1)),size=1) +
    xlab("Ratio of primary care providers within 30 minutes \nby transit to census tract population") + ylab("Density") +
    ggtitle("Probability Density Function of \nTransit Access to Primary Care Providers for \nLow Income vs. All Other Census Tracts") +
    xlim(0,1) +
    labs(color='Majority low income') +
    scale_color_manual(values=c("skyblue","dodgerblue"))
ggsave("plots/primary_income.png")

# older population access to pharmacists (tracts with >20% older)
ggplot()+
    geom_density(data=tract_data, aes(x=pharma_t_r, color=(older_pop==1)),size=1) +
    xlab("Ratio of pharmacists within 30 minutes \nby transit to census tract population") + ylab("Density") + xlim(0,.10) +
    ggtitle("Probability Density Function of \nTransit Access to Pharmacies for \nOlder vs. All Other Census Tracts") +
    labs(color='Population over 65 > 20%') +
    scale_color_manual(values=c("red","red4"))
ggsave("plots/pharma_older.png")

# children pop access to pediatrics(tracts with >30% children)
ggplot()+
    geom_density(data=tract_data, aes(x=child_ped_access, color=(child_pop==1)),size=1) +
    xlab("Ratio of pediatricians within 30 minutes \nby transit to census tract population under 18") + ylab("Density") + xlim(0,.5) +
    ggtitle("Probability Density Function of \nTransit Access to Pediatricians for High \nChild Populations vs. All Other Census Tracts") +
    labs(color='Population under 18 > 30%') +
    scale_color_manual(values=c("purple","purple4"))
ggsave("plots/ped_children.png")

# carless access to primary (carless hh/total pop >10%) #recalc?
ggplot()+
    geom_density(data=tract_data, aes(x=prmry_t_r, color=(carless==1)),size=1) +
    xlab("Ratio of pprimary care providers within 30 minutes \nby transit to census tract population") + ylab("Density") + xlim(0,1) +
    ggtitle("Probability Density Function of Transit Access \nto Primary Care for High Proportions of \nCarless Households vs. All Other Census Tracts") +
    labs(color='High proportion of \ncarless households') +
    scale_color_manual(values=c("skyblue","dodgerblue"))
ggsave("plots/primary_carless.png")
