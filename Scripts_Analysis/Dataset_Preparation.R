library(here)
library(tidyverse)
library(readr)
library(lubridate)
library(sf)
library(sp)
library(reshape2)
library(spdep)


# Step 1: Input data
crime_data <- read_csv("*/Input_Data/NYPD_Arrests_Data__Historic_.csv") # read data
admin_data <- read_sf("*/Input_Data/NY_PP/nypp.shp") #the chosen file should be as shape file (".shp"), this refers to precincts as study zones

# Step 2: Select relevant attributes
crime_data$DATE <- as.Date(crime_data$ARREST_DATE, format = "%m/%d/%Y") #change ARREST_DATE from string to date 
cr_sel <- crime_data %>% dplyr::select(OFNS_DESC, ARREST_PRECINCT ,X_COORD_CD, Y_COORD_CD, DATE) 
pp <- admin_data[,c(1,3,4)]# selection of useful attributes (at least precinct id, area and their geometry)
pp_list <- as.list(pp$Precinct)

# Step X: Selection of the study period
#cr_sel_start <- cr_sel %>% filter(cr_sel$DATE >= "2006-01-01")
#cr_sel_end <- cr_sel_start %>% filter(cr_sel_start$DATE <= "2006-12-31")

# Step 3: Clear N.A. and strange data
clr_temp_1 <- cr_sel  #use cr_sel_end if a subset of date is used
clr_temp_2 <- clr_temp_1 %>% drop_na() # delete empty rows  
cr_clr <- clr_temp_2[(!(clr_temp_2$OFNS_DESC=="F.C.A. P.I.N.O.S.")),] # delete strange data 

# Step 4: Spatial Join of crime data & Police Precincts
cr_sf <- cr_clr %>% st_as_sf(coords=c("X_COORD_CD", "Y_COORD_CD"), crs=2263, remove=FALSE) #create sf objects for spatial join
pp_sf <- pp  %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)

cr_pp_sj <- st_join(cr_sf, pp_sf, join = st_intersects) #spatial join

cr_pp_sj$Precinct <- ifelse(is.na(cr_pp_sj$Precinct), cr_pp_sj$ARREST_PRECINCT, cr_pp_sj$Precinct) #replace not joined point with ARREST_PRECINCT info

cr_pp_nc <- cr_pp_sj[,c(1,5,7)] #dropping coordinates and geometry for faster processing
cr_pp_ng <- st_drop_geometry(cr_pp_nc)

# Step 5: Categorize arrest data into crime types
div_temp <- cr_pp_ng
div_temp$all <- 1
size <- nrow(div_temp)

for(i in 1:size){
  if(div_temp[i,1]=="ABORTION"){
    div_temp$p[i]<-0 # property = p
    div_temp$v[i]<-0 # violent = v
  }else if(div_temp[i,1]=="ADMINISTRATIVE CODE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ADMINISTRATIVE CODES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="AGRICULTURE & MRKTS LAW-UNCLASSIFIED"){ 
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ALCOHOLIC BEVERAGE CONTROL LAW"){ 
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ANTICIPATORY OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ARSON"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ASSAULT 3 & RELATED OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="BURGLAR'S TOOLS"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="BURGLARY"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="CHILD ABANDONMENT/NON SUPPORT"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="CHILD ABANDONMENT/NON SUPPORT 1"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="CRIMINAL MISCHIEF & RELATED OF"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="CRIMINAL MISCHIEF & RELATED OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="CRIMINAL TRESPASS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="DANGEROUS DRUGS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="DANGEROUS WEAPONS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="DISORDERLY CONDUCT"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="DISRUPTION OF A RELIGIOUS SERV"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="DISRUPTION OF A RELIGIOUS SERVICE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ENDAN WELFARE INCOMP"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="ESCAPE 3"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="FELONY ASSAULT"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="FOR OTHER AUTHORITIES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="FORCIBLE TOUCHING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="FORGERY"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="FRAUDS"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="FRAUDULENT ACCOSTING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="GAMBLING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="GRAND LARCENY"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="GRAND LARCENY OF MOTOR VEHICLE"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="HARRASSMENT"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="HARRASSMENT 2"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="HOMICIDE-NEGLIGENT-VEHICLE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="HOMICIDE-NEGLIGENT,UNCLASSIFIE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="HOMICIDE-NEGLIGENT,UNCLASSIFIED"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="INTOXICATED & IMPAIRED DRIVING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="INTOXICATED/IMPAIRED DRIVING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="JOSTLING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="KIDNAPPING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="KIDNAPPING & RELATED OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="KIDNAPPING AND RELATED OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="LOITERING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="LOITERING FOR DRUG PURPOSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="LOITERING,BEGGING"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="LOITERING/GAMBLING (CARDS, DIC"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="LOITERING/GAMBLING (CARDS, DICE, ETC)"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="MISCELLANEOUS PENAL LAW"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="MOVING INFRACTIONS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="MURDER & NON-NEGL. MANSLAUGHTE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="MURDER & NON-NEGL. MANSLAUGHTER"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="NEW YORK CITY HEALTH CODE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="NYS LAWS-UNCLASSIFIED FELONY"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="NYS LAWS-UNCLASSIFIED VIOLATION"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFF. AGNST PUB ORD SENSBLTY &"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFF. AGNST PUB ORD SENSBLTY & RGHTS TO PRIV"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES AGAINST MARRIAGE UNCLASSIFIED"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES AGAINST PUBLIC ADMINI"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES AGAINST PUBLIC ADMINISTRATION"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES AGAINST PUBLIC SAFETY"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES AGAINST THE PERSON"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="OFFENSES INVOLVING FRAUD"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OFFENSES RELATED TO CHILDREN"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER OFFENSES RELATED TO THEF"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER OFFENSES RELATED TO THEFT"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER STATE LAWS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER STATE LAWS (NON PENAL LA"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER STATE LAWS (NON PENAL LAW)"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="OTHER TRAFFIC INFRACTION"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="PARKING OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="PETIT LARCENY"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="POSSESSION OF STOLEN PROPERTY"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="POSSESSION OF STOLEN PROPERTY 5"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="PROSTITUTION & RELATED OFFENSES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="RAPE"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="ROBBERY"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="SEX CRIMES"){
    div_temp$p[i]<-0
    div_temp$v[i]<-1
  }else if(div_temp[i,1]=="THEFT OF SERVICES"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="THEFT-FRAUD"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="UNAUTHORIZED USE OF A VEHICLE"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="UNAUTHORIZED USE OF A VEHICLE 3 (UUV)"){
    div_temp$p[i]<-1
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="UNDER THE INFLUENCE, DRUGS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="UNLAWFUL POSS. WEAP. ON SCHOOL"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="UNLAWFUL POSS. WEAP. ON SCHOOL GROUNDS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }else if(div_temp[i,1]=="VEHICLE AND TRAFFIC LAWS"){
    div_temp$p[i]<-0
    div_temp$v[i]<-0
  }
}

cr_div <- div_temp

# Step 6: Extract information about temporal resolution + 
  # Step 7: group & sum the data + 
    # Step 8: Transpose data for model implementation

# 1 week
cr_pp_w <- cr_div 
cr_pp_w$tp <- floor_date(cr_pp_w$DATE, unit = "week", 1)

cr_pp_sum_w <- cr_pp_w %>%
  group_by(tp, Precinct) %>% # group by the unique code of precincts & time period
  summarize(sumall = sum(all), sump = sum(p), sumv = sum(v)) #sum the crime

cr_pp_all_w <- cr_pp_sum_w[,c(1,2,3)] 
cr_pp_p_w <- cr_pp_sum_w[,c(1,2,4)]
cr_pp_v_w <- cr_pp_sum_w[,c(1,2,5)]

cr_pp_sts_w_all <- dcast(cr_pp_all_w, tp~Precinct, value.var = "sumall") #transpose for crime type all
cr_pp_sts_w_all[is.na(cr_pp_sts_w_all)] <- 0 #replace NA with 0
cr_pp_sts_w_p <- dcast(cr_pp_p_w, tp~Precinct, value.var = "sump") #transpose for crime type p
cr_pp_sts_w_p[is.na(cr_pp_sts_w_p)] <- 0
cr_pp_sts_w_v <- dcast(cr_pp_v_w, tp~Precinct, value.var = "sumv") #transpose for crime type v
cr_pp_sts_w_v[is.na(cr_pp_sts_w_v)] <- 0


# 1 month
cr_pp_1m <- cr_div 
cr_pp_1m$tp <- floor_date(cr_pp_1m$DATE, unit = "month")

cr_pp_sum_1m <- cr_pp_1m %>%
  group_by(tp, Precinct) %>% # group by the unique code of precincts & time period
  summarize(sumall = sum(all), sump = sum(p), sumv = sum(v)) #sum the crime

cr_pp_all_1m <- cr_pp_sum_1m[,c(1,2,3)] 
cr_pp_p_1m <- cr_pp_sum_1m[,c(1,2,4)]
cr_pp_v_1m <- cr_pp_sum_1m[,c(1,2,5)]

cr_pp_sts_1m_all <- dcast(cr_pp_all_1m, tp~Precinct, value.var = "sumall") #transpose for crime type all
cr_pp_sts_1m_p <- dcast(cr_pp_p_1m, tp~Precinct, value.var = "sump") #transpose for crime type p
cr_pp_sts_1m_v <- dcast(cr_pp_v_1m, tp~Precinct, value.var = "sumv") #transpose for crime type v

# 3 months
cr_pp_3m <- cr_div 
cr_pp_3m$tp <- floor_date(cr_pp_3m$DATE, unit = "quarter")

cr_pp_sum_3m <- cr_pp_3m %>%
  group_by(tp, Precinct) %>% # group by the unique code of precincts & time period
  summarize(sumall = sum(all), sump = sum(p), sumv = sum(v)) #sum the crime

cr_pp_all_3m <- cr_pp_sum_3m[,c(1,2,3)] 
cr_pp_p_3m <- cr_pp_sum_3m[,c(1,2,4)]
cr_pp_v_3m <- cr_pp_sum_3m[,c(1,2,5)]

cr_pp_sts_3m_all <- dcast(cr_pp_all_3m, tp~Precinct, value.var = "sumall") #transpose for crime type all
cr_pp_sts_3m_p <- dcast(cr_pp_p_3m, tp~Precinct, value.var = "sump") #transpose for crime type p
cr_pp_sts_3m_v <- dcast(cr_pp_v_3m, tp~Precinct, value.var = "sumv") #transpose for crime type v

# 6 months

cr_pp_6m <- cr_div 
cr_pp_6m$tp <- floor_date(cr_pp_6m$DATE, unit = "halfyear")

cr_pp_sum_6m <- cr_pp_6m %>%
  group_by(tp, Precinct) %>% # group by the unique code of precincts & time period
  summarize(sumall = sum(all), sump = sum(p), sumv = sum(v)) #sum the crime

cr_pp_all_6m <- cr_pp_sum_6m[,c(1,2,3)] 
cr_pp_p_6m <- cr_pp_sum_6m[,c(1,2,4)]
cr_pp_v_6m <- cr_pp_sum_6m[,c(1,2,5)]

cr_pp_sts_6m_all <- dcast(cr_pp_all_6m, tp~Precinct, value.var = "sumall") #transpose for crime type all
cr_pp_sts_6m_p <- dcast(cr_pp_p_6m, tp~Precinct, value.var = "sump") #transpose for crime type p
cr_pp_sts_6m_v <- dcast(cr_pp_v_6m, tp~Precinct, value.var = "sumv") #transpose for crime type v

# 1 year
cr_pp_y <- cr_div 
cr_pp_y$tp <- format(as.Date(cr_div$DATE, format="%Y-%m-%d"),"%Y")

cr_pp_sum_y <- cr_pp_y %>%
  group_by(tp, Precinct) %>% # group by the unique code of precincts & time period
  summarize(sumall = sum(all), sump = sum(p), sumv = sum(v)) #sum the crime

cr_pp_all_y <- cr_pp_sum_y[,c(1,2,3)] 
cr_pp_p_y <- cr_pp_sum_y[,c(1,2,4)]
cr_pp_v_y <- cr_pp_sum_y[,c(1,2,5)]

cr_pp_sts_y_all <- dcast(cr_pp_all_y, tp~Precinct, value.var = "sumall") #transpose for crime type all
cr_pp_sts_y_p <- dcast(cr_pp_p_y, tp~Precinct, value.var = "sump") #transpose for crime type p
cr_pp_sts_y_v <- dcast(cr_pp_v_y, tp~Precinct, value.var = "sumv") #transpose for crime type v

# Step 9: Replace geometry of polygons with their centroid point
pp_centr <- st_centroid(pp) #get centroid of police precincts
pp_geom<- pp_centr [,c(1,3)]
pp_XY <- pp_centr %>% st_coordinates() %>% as.data.frame() # get X and Y coordinates of the centroids of police precincts
pp_XY_mtx <- as.matrix(pp_XY) 

cr_pp <- merge(cr_pp_sum,pp_geom, by="Precinct")
st_write(cr_pp, "*/Outputs/R_Outputs/cr_pp.shp", append=FALSE)
