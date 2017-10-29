
### Loading all the libraries - 

library(plyr)
library(jpeg)
library(png)
library(dplyr)
library(gmapsdistance)
library(ggmap)
library(XML)
library(RCurl)
library(methods)
library(stats)
library(RgoogleMaps)

### Setting all the keys, constants and constraints in the code

google_api_key = Or8lzQheRPDggsWSX9r4~SSDSIzvAfpQZdxIRbYhjFA~AoFHbA_9LvwdBXtG9L86_7D1EIPTNpbqaigckqknnz_QOamAt7fFKVwyR4LF-0e6
hsr_btm_lat = 12.9172365
hsr_btm_long = 77.6230248
btm_hsr_lat = 12.9172574
btm_hsr_long = 77.6224830 
madiwala_ec_lat = 12.9175868
madiwala_ec_long = 77.6227727 
ec_madiwala_lat = 12.9170901
ec_madiwala_long = 77.6226815 

alpha_wt = 0.6 
alpha_ri = 0.35
alpha_rw = 0.15
constant = 0

tci_min = 500
tci_max = 1500
cycle_time_min = 90
cycle_time_max = 360

df = data.frame()
df_raw = data.frame()
gt_data = data.frame()
sb_coords = read.csv("D:/GridLock_Hackathon_FK/Open_CV/Images/Final/silkboard_coordinates.csv")

### Plotting Latitude and Longitude for every pixel in the image

# lat_min = 12.8979
# long_min = 77.6045
# lat_max = 12.9379
# long_max = 77.6445

lat_max = 12.94462
long_min = 77.59711

silk_board_lat = 12.917349
silk_board_long  = 77.622759

lat_per_pixel = (silk_board_lat- lat_max)/330
long_per_pixel = (silk_board_long - long_min)/301

# lat_per_pixel = (lat_max-lat_min)/640  
# long_per_pixel = (long_max-long_min)/640

geomap_latitude = data.frame()
geomap_longitude = data.frame()
geomap = data.frame()


### Plotting the GeoMap - 1 time activity

for(i in 1:640)
{
  for (j in 1:640)
  {
    geomap_latitude[i,j] = lat_max + lat_per_pixel*(i-0.5)
    geomap_longitude[i,j] = long_min + long_per_pixel*(j-0.5)
    geomap[i,j] = paste(lat_max + lat_per_pixel*(i-0.5),long_min + long_per_pixel*(j-0.5),sep = ",") 
  }
}

### Reading the Road data and corresponding manipulations

up_down_roads = readPNG("D:/GridLock_Hackathon_FK/Open_CV/Images/Final/silk_board_roads.jpeg")
up_down_roads = up_down_roads*255

madiwala_ec_up = data.frame()
madiwala_ec_down = data.frame()
btm_hsr_down = data.frame()
btm_hsr_up = data.frame()
up_down_roads_map = data.frame()

for (i in 1:640)
{
  for (j in 1:640)
  {
    up_down_roads_map[i,j] = paste(up_down_roads[i,j,1],up_down_roads[i,j,2],up_down_roads[i,j,3],sep = ",")
  }
}
madiwala_ec_up = up_down_roads_map
madiwala_ec_down = madiwala_ec_up
btm_hsr_down = madiwala_ec_up
btm_hsr_up = madiwala_ec_up

madiwala_ec_up[madiwala_ec_up != "100,0,0"] = ""
madiwala_ec_down[madiwala_ec_down != "0,0,0"] = ""
btm_hsr_up[btm_hsr_up != "0,100,0"] = ""
btm_hsr_down[btm_hsr_down != "0,0,100"] = ""

index_madi_ec = data.frame()
index_ec_madi = data.frame()
index_hsr_btm = data.frame()
index_btm_hsr = data.frame()

for ( i in 1:640)
{
  for ( j in 1:640 )
  {
    if(up_down_roads_map[i,j] == "100,0,0")
    {
      index_madi_ec = rbind.fill(index_madi_ec,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,0,0")
    {
      index_ec_madi = rbind.fill(index_ec_madi,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,100,0")
    {
      index_btm_hsr = rbind.fill(index_btm_hsr,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,0,100")
    {
      index_hsr_btm = rbind.fill(index_hsr_btm,data.frame(i,j))    
    }
  }
}

madi_ec_x = 327
madi_ec_y = 302
ec_madi_x = 332
ec_madi_y = 300
hsr_btm_x = 329
hsr_btm_y = 304
btm_hsr_x = 327
btm_hsr_y = 295

index_madi_ec$distance = sqrt((index_madi_ec[1:nrow(index_madi_ec),1]-madi_ec_x)^2 + (index_madi_ec[1:nrow(index_madi_ec),2]-madi_ec_y)^2)
index_dist_ec_madi = sqrt((index_madi_ec[1:nrow(index_madi_ec),1]-madi_ec_x)^2 + (index_madi_ec[1:nrow(index_madi_ec),2]-madi_ec_y)^2)

index_madi_ec$distance = sqrt((index_madi_ec$i-madi_ec_x)^2 + (index_madi_ec$j-madi_ec_y)^2)
index_ec_madi$distance = sqrt((index_ec_madi$i-ec_madi_x)^2 + (index_ec_madi$j-ec_madi_y)^2)
index_hsr_btm$distance = sqrt((index_hsr_btm$i-hsr_btm_x)^2 + (index_hsr_btm$j-hsr_btm_y)^2)
index_btm_hsr$distance = sqrt((index_btm_hsr$i-btm_hsr_x)^2 + (index_btm_hsr$j-btm_hsr_y)^2)

index_ec_madi = index_ec_madi[order(index_ec_madi$distance),]
index_madi_ec = index_madi_ec[order(index_madi_ec$distance),]
index_hsr_btm = index_hsr_btm[order(index_hsr_btm$distance),]
index_btm_hsr = index_btm_hsr[order(index_btm_hsr$distance),]

#### Putting everything in one loop - 

for (i in 1:100)
{
  time1 = Sys.time()
  ## Getting all the 4 current wait times for the 500 metre distance - 
  
  hsr_btm_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "HSR"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "HSR"],mode = "driving",key = "AIzaSyBGt0hu2DXDV0suKzuUJHx58ZR4Plmf180")$Time
  btm_hsr_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "BTM"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "BTM"],mode = "driving",key = "AIzaSyBGt0hu2DXDV0suKzuUJHx58ZR4Plmf180")$Time
  ec_madiwala_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "EC"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "EC"],mode = "driving",key = "AIzaSyBGt0hu2DXDV0suKzuUJHx58ZR4Plmf180")$Time
  madiwala_ec_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "Madiwala"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "Madiwala"],mode = "driving",key = "AIzaSyBGt0hu2DXDV0suKzuUJHx58ZR4Plmf180")$Time
  
  ### Reading the image and corresponding manipulations
  
  time = Sys.time()
  
  filename = paste("mapsbj",as.integer(Sys.time()),".jpeg",sep = "_")
  image = GetBingMap(center="silk%20%board%20flyover", zoom=14,mapArea = c(12.8979,77.6045,12.9379,77.6445),size = c(640,640), extraURL="&mapLayer=TrafficFlow",apiKey="Or8lzQheRPDggsWSX9r4~SSDSIzvAfpQZdxIRbYhjFA~AoFHbA_9LvwdBXtG9L86_7D1EIPTNpbqaigckqknnz_QOamAt7fFKVwyR4LF-0e6",verbose=1, destfile=filename)
  image = readPNG(filename)
  image = image*255
  
  image_rgb = data.frame()
  
  for (i in 1:640)
  {
    for (j in 1:640)
    {
      image_rgb[i,j] = paste(image[i,j,1],image[i,j,2],image[i,j,3],sep = ",")
    }
  }
  
  index_madi_ec = index_madi_ec[,c(1:2)]
  index_ec_madi = index_ec_madi[,c(1:2)]
  index_hsr_btm = index_hsr_btm[,c(1:2)]
  index_btm_hsr = index_btm_hsr[,c(1:2)]
  
  for ( i in 1:nrow(index_madi_ec))
  {
    index_madi_ec$rgb1[i] = image_rgb[index_madi_ec[i,1],index_madi_ec[i,2]]
  }
  
  for ( i in 1:nrow(index_ec_madi))
  {
    index_ec_madi$rgb1[i] = image_rgb[index_ec_madi[i,1],index_ec_madi[i,2]]
  }
  
  for ( i in 1:nrow(index_btm_hsr))
  {
    index_btm_hsr$rgb1[i] = image_rgb[index_btm_hsr[i,1],index_btm_hsr[i,2]]
  }
  
  for ( i in 1:nrow(index_hsr_btm))
  {
    index_hsr_btm$rgb1[i] = image_rgb[index_hsr_btm[i,1],index_hsr_btm[i,2]]
  }
  
  index_ec_madi = cbind(index_ec_madi[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_ec_madi$rgb1),',',fixed=TRUE))))
  colnames(index_ec_madi) = c("x","y","r","g","b") 
  
  index_madi_ec = cbind(index_madi_ec[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_madi_ec$rgb1),',',fixed=TRUE))))
  colnames(index_madi_ec) = c("x","y","r","g","b") 
  
  index_hsr_btm = cbind(index_hsr_btm[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_hsr_btm$rgb1),',',fixed=TRUE))))
  colnames(index_hsr_btm) = c("x","y","r","g","b") 
  
  index_btm_hsr = cbind(index_btm_hsr[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_btm_hsr$rgb1),',',fixed=TRUE))))
  colnames(index_btm_hsr) = c("x","y","r","g","b") 
  
  index_ec_madi$red_flag = ifelse(as.numeric(paste(index_ec_madi$r)) > 200 & as.numeric(paste(index_ec_madi$g)) < 120 ,1,0)
  index_madi_ec$red_flag = ifelse(as.numeric(paste(index_madi_ec$r)) > 200 & as.numeric(paste(index_madi_ec$g)) < 120 ,1,0)
  index_hsr_btm$red_flag = ifelse(as.numeric(paste(index_hsr_btm$r)) > 200 & as.numeric(paste(index_hsr_btm$g)) < 120 ,1,0)
  index_btm_hsr$red_flag = ifelse(as.numeric(paste(index_btm_hsr$r)) > 200 & as.numeric(paste(index_btm_hsr$g)) < 120 ,1,0)
  
  index_ec_madi$amber_flag = ifelse(as.numeric(paste(index_ec_madi$r)) > 200 & as.numeric(paste(index_ec_madi$g)) > 140 & as.numeric(paste(index_ec_madi$b))<100,1,0)
  index_madi_ec$amber_flag = ifelse(as.numeric(paste(index_madi_ec$r)) > 200 & as.numeric(paste(index_madi_ec$g)) > 140 & as.numeric(paste(index_madi_ec$b))<100,1,0)
  index_hsr_btm$amber_flag = ifelse(as.numeric(paste(index_hsr_btm$r)) > 200 & as.numeric(paste(index_hsr_btm$g)) > 140 & as.numeric(paste(index_hsr_btm$b))<100,1,0)
  index_btm_hsr$amber_flag = ifelse(as.numeric(paste(index_btm_hsr$r)) > 200 & as.numeric(paste(index_btm_hsr$g)) > 140 & as.numeric(paste(index_btm_hsr$b))<100 ,1,0)
  
  ### Calculation of all 4 distances 
  
  latlon_dist <- function(origin,destination){
    xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=walking&sensor=false')
    xmlfile <- xmlParse(getURL(xml.url))
    dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
    distance <- as.numeric(sub(" km","",dist))
    return(distance)
  }
  
  # or_madi_ec = paste(madiwala_ec_lat,madiwala_ec_long,sep = ",")
  # or_ec_madi = paste(ec_madiwala_lat,ec_madiwala_long,sep = ",")
  # or_hsr_btm = paste(hsr_btm_lat,hsr_btm_long,sep = ",")
  # or_btm_hsr = paste(btm_hsr_lat,btm_hsr_long,sep = ",")
  
  # or_madi_ec_lat0 = geomap_latitude[index_madi_ec[1,1],index_madi_ec[1,2]]
  # or_ec_madi_lat0 = geomap_latitude[index_ec_madi[1,1],index_ec_madi[1,2]]
  # or_hsr_btm_lat0 = geomap_latitude[index_hsr_btm[1,1],index_hsr_btm[1,2]]
  # or_btm_hsr_lat0 = geomap_latitude[index_btm_hsr[1,1],index_btm_hsr[1,2]]
  # 
  # or_madi_ec_long0 = geomap_longitude[index_madi_ec[1,1],index_madi_ec[1,2]]
  # or_ec_madi_long0 = geomap_longitude[index_ec_madi[1,1],index_ec_madi[1,2]]
  # or_hsr_btm_long0 = geomap_longitude[index_hsr_btm[1,1],index_hsr_btm[1,2]]
  # or_btm_hsr_long0 = geomap_longitude[index_btm_hsr[1,1],index_btm_hsr[1,2]]
  # 
  # 
  # dest_madi_ec_lat0 = geomap_latitude[index_madi_ec[index_madi_ec$red_flag == 0,][1,1],index_madi_ec[index_madi_ec$red_flag == 0,][1,2]]
  # dest_ec_madi_lat0 = geomap_latitude[index_ec_madi[index_ec_madi$red_flag == 0,][1,1],index_ec_madi[index_ec_madi$red_flag == 0,][1,2]]
  # dest_hsr_btm_lat0 = geomap_latitude[index_hsr_btm[index_hsr_btm$red_flag == 0,][1,1],index_hsr_btm[index_hsr_btm$red_flag == 0,][1,2]]
  # dest_btm_hsr_lat0 = geomap_latitude[index_btm_hsr[index_btm_hsr$red_flag == 0,][1,1],index_btm_hsr[index_btm_hsr$red_flag == 0,][1,2]]
  # 
  # dest_madi_ec_long0 = geomap_longitude[index_madi_ec[index_madi_ec$red_flag == 0,][1,1],index_madi_ec[index_madi_ec$red_flag == 0,][1,2]]
  # dest_ec_madi_long0 = geomap_longitude[index_ec_madi[index_ec_madi$red_flag == 0,][1,1],index_ec_madi[index_ec_madi$red_flag == 0,][1,2]]
  # dest_hsr_btm_long0 = geomap_longitude[index_hsr_btm[index_hsr_btm$red_flag == 0,][1,1],index_hsr_btm[index_hsr_btm$red_flag == 0,][1,2]]
  # dest_btm_hsr_long0 = geomap_longitude[index_btm_hsr[index_btm_hsr$red_flag == 0,][1,1],index_btm_hsr[index_btm_hsr$red_flag == 0,][1,2]]
  
  
  # madi_ec_distance = latlon_dist(origin=paste(or_madi_ec_lat0,or_madi_ec_long0,sep = ","),destination=paste(dest_madi_ec_lat0,dest_madi_ec_long0,sep = ","))
  # ec_madi_distance = latlon_dist(origin=paste(or_ec_madi_lat0,or_ec_madi_long0,sep = ","),destination=paste(dest_ec_madi_lat0,dest_ec_madi_long0,sep = ","))
  # hsr_btm_distance = latlon_dist(origin=paste(or_hsr_btm_lat0,or_hsr_btm_long0,sep = ","),destination=paste(dest_hsr_btm_lat0,dest_hsr_btm_long0,sep = ","))
  # btm_hsr_distance = latlon_dist(origin=paste(or_btm_hsr_lat0,or_btm_hsr_long0,sep = ","),destination=paste(dest_btm_hsr_lat0,dest_btm_hsr_long0,sep = ","))
  
  index_hsr_btm$row_num = seq.int(nrow(index_hsr_btm))
  index_btm_hsr$row_num = seq.int(nrow(index_btm_hsr))
  index_madi_ec$row_num = seq.int(nrow(index_madi_ec))
  index_ec_madi$row_num = seq.int(nrow(index_ec_madi))
  
  ## Getting Immediate Red Pixeks
  
  hsr_btm_ri = ifelse(is.na(index_hsr_btm$row_num[index_hsr_btm$red_flag  == 0][1]),nrow(index_hsr_btm),index_hsr_btm$row_num[index_hsr_btm$red_flag  == 0][1]-1)
  btm_hsr_ri = ifelse(is.na(index_btm_hsr$row_num[index_btm_hsr$red_flag  == 0][1]),nrow(index_btm_hsr),index_btm_hsr$row_num[index_btm_hsr$red_flag  == 0][1]-1)
  madi_ec_ri = ifelse(is.na(index_madi_ec$row_num[index_madi_ec$red_flag  == 0][1]),nrow(index_madi_ec),index_madi_ec$row_num[index_madi_ec$red_flag  == 0][1]-1)
  ec_madi_ri = ifelse(is.na(index_ec_madi$row_num[index_ec_madi$red_flag  == 0][1]),nrow(index_ec_madi),index_ec_madi$row_num[index_ec_madi$red_flag  == 0][1]-1)
  
  ## Getting Immediate Amber Pixels
  
  hsr_btm_ai = ifelse(is.na(index_hsr_btm$row_num[index_hsr_btm$amber_flag  == 0][1]),nrow(index_hsr_btm),index_hsr_btm$row_num[index_hsr_btm$amber_flag  == 0][1]-1)
  btm_hsr_ai = ifelse(is.na(index_btm_hsr$row_num[index_btm_hsr$amber_flag  == 0][1]),nrow(index_btm_hsr),index_btm_hsr$row_num[index_btm_hsr$amber_flag  == 0][1]-1)
  madi_ec_ai = ifelse(is.na(index_madi_ec$row_num[index_madi_ec$amber_flag  == 0][1]),nrow(index_madi_ec),index_madi_ec$row_num[index_madi_ec$amber_flag  == 0][1]-1)
  ec_madi_ai = ifelse(is.na(index_ec_madi$row_num[index_ec_madi$amber_flag  == 0][1]),nrow(index_ec_madi),index_ec_madi$row_num[index_ec_madi$amber_flag  == 0][1]-1)

  ## Getting the Total Number of Red Pixels
  
  hsr_btm_rw = sum(index_hsr_btm$red_flag)
  btm_hsr_rw = sum(index_btm_hsr$red_flag)
  madi_ec_rw = sum(index_madi_ec$red_flag)
  ec_madi_rw = sum(index_ec_madi$red_flag)
  
  hsr_btm_aw = sum(index_hsr_btm$amber_flag)
  btm_hsr_aw = sum(index_btm_hsr$amber_flag)
  madi_ec_aw = sum(index_madi_ec$amber_flag)
  ec_madi_aw = sum(index_ec_madi$amber_flag)
  
  df_1 = data.frame(hsr_btm_time,btm_hsr_time,madiwala_ec_time,ec_madiwala_time,hsr_btm_ri,hsr_btm_ai,btm_hsr_ri,btm_hsr_ai,madi_ec_ri,madi_ec_ai,ec_madi_ri,ec_madi_ai,hsr_btm_rw,hsr_btm_aw,btm_hsr_rw,btm_hsr_aw,madi_ec_rw,madi_ec_aw,ec_madi_rw,ec_madi_aw,time)
  df_raw  = rbind.fill(df_raw,df_1)
  
  hsr_btm_tci = constant + alpha_wt*(hsr_btm_time) + alpha_ri*(hsr_btm_ri+0.4*hsr_btm_ai) + alpha_rw*(hsr_btm_rw+0.4*hsr_btm_aw) 
  btm_hsr_tci = constant + alpha_wt*(btm_hsr_time) + alpha_ri*(btm_hsr_ri+0.4*btm_hsr_ai) + alpha_rw*(btm_hsr_rw+0.4*btm_hsr_aw) 
  madi_ec_tci = constant + alpha_wt*(madiwala_ec_time) + alpha_ri*(madi_ec_ri+0.4*madi_ec_ai) + alpha_rw*(madi_ec_rw+0.4*madi_ec_aw) 
  ec_madi_tci = constant + alpha_wt*(ec_madiwala_time) + alpha_ri*(ec_madi_ri+0.4*ec_madi_ai) + alpha_rw*(ec_madi_rw+0.4*ec_madi_aw) 

  tci_total = hsr_btm_tci +  btm_hsr_tci +  max(madi_ec_tci,ec_madi_tci)
  
  df = rbind.fill(df,data.frame(tci_total,hsr_btm_tci,btm_hsr_tci,madi_ec_tci,ec_madi_tci,time))
  write.csv(df_raw,"D:/GridLock_Hackathon_FK/Open_CV/Solution1/All_Variables_Run1.csv",row.names = F)
  write.csv(df,"D:/GridLock_Hackathon_FK/Open_CV/Solution1/TCI_Run1.csv")
  
## tct = ((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90

hsr_btm_gt = (hsr_btm_tci/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)
hsr_btm_gt = ifelse(hsr_btm_gt < 15, 15, hsr_btm_gt)

btm_hsr_gt = (btm_hsr_tci/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)
btm_hsr_gt = ifelse(btm_hsr_gt < 15, 15, btm_hsr_gt)

madi_ec_gt = (max(madi_ec_tci,ec_madi_tci)/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)
madi_ec_gt = ifelse(madi_ec_gt < 15, 15, madi_ec_gt)

#ec_madi_gt = (max(madi_ec_tci,ec_madi_tci)/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)
#ec_madi_gt = ifelse(ec_madi_gt < 15, 15, ec_madi_gt)

tct = hsr_btm_gt + btm_hsr_gt + madi_ec_gt

gt_data = rbind.fill(gt_data, data.frame(hsr_btm_gt,btm_hsr_gt,madi_ec_gt,ec_madi_gt,tct))
write.csv(gt_data,"D:/Affine/Work/Flipkart - Hackathon/gt_data.csv")

time2 = Sys.time()
deltime = as.numeric(time2-time1)
  Sys.sleep(tct-deltime)
  }


