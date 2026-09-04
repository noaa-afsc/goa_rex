###REX SOLE GROWTH - SPATIAL VARIATION

library(tidyverse)
library(magrittr)
library(modelr)
library(sf)
library(rnaturalearth)
library(rgdal)
library(nlstools)



# Load and prep data ------------------------------------------------------
#assessment year (AY) [need to create new folders in Data and Output]
AY <- 2026 #UPDATE THIS IN EACH NEW ASSESSMENT YEAR




#Data were queried from age3 (fish with ages). Includes all GOA Survey Rex Sole
Data <- read.csv(paste0("../Data/", AY, "/Rex_GOAsurvey_query101421.csv")) 

#Drop irrelevant columns
Data %<>% select(haul, date_collected, longitude, latitude, fish_id, specimen, 
                 length, weight, sex, collection_year, cruise_number, ageing_collection, 
                 final_age, final_method, readability, reader_name, vessel_code)


#Remove specimens where no final age was assigned and sex is unknown
Data %<>% drop_na(final_age) %>% filter(sex !=3)

#Code sex as a factor
Data$sex <- factor(Data$sex,
                   levels = c(1,2),
                   labels = c("Male", "Female"))

#Code method as factor and recode blank method as surface ages
Data$final_method <- factor(Data$final_method, 
                            levels = c("", "B", "M", "S", "U", "V"),
                            labels = c("Surface", "Break-and-burn", "Mixed", "Surface", "Unburned", "Toast"))

#Use data from 2001 survey to present
Data <- Data[Data$collection_year >= 2001,]



# Fit von Bertalanffy -----------------------------------------------------
males <- Data[Data$sex == "Male",]
females <- Data[Data$sex == "Female",]

#Males
start<-list(Linf=450, k=0.2, t0=-1)
vonB_M_sur<-males$length~Linf*(1-exp(-k*(males$final_age-t0)))
fitvonB_M_sur<-nls(vonB_M_sur,data=males,start=start)
summary(fitvonB_M_sur)
confint(fitvonB_M_sur)
males <- add_residuals(males, fitvonB_M_sur, var = "resid")
std_resid <- nlsResiduals(fitvonB_M_sur)[[2]][,2]
males <- cbind(males, std_resid)

#Females
start<-list(Linf=500, k=0.2, t0=-1)
vonB_F_sur<-females$length~Linf*(1-exp(-k*(females$final_age-t0)))
fitvonB_F_sur<-nls(vonB_F_sur,data=females,start=start)
summary(fitvonB_F_sur)
confint(fitvonB_F_sur)
females <- add_residuals(females, fitvonB_F_sur, var = "resid")
std_resid <- nlsResiduals(fitvonB_F_sur)[[2]][,2]
females <- cbind(females, std_resid)






# Draw map ----------------------------------------------------------------
#Combine males and females back into single datafile for mapping
Data_map <- rbind(males,females)

#adjust data points for 180 line, shouldn't be an issue with this dataset but necessary for BSAI
Data_map$longitude = ifelse(Data_map$longitude > 0, Data_map$longitude - 360, Data_map$longitude)

#get land
world <- ne_countries(scale = "medium", returnclass = "sp")

#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)
#russia
russia <- subset(world, admin == "Russia")
russia <- fortify(russia)
russia$long = ifelse(russia$long > 0, russia$long - 360, russia$long)
#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy <- readOGR("../Data/race_bathy_to_200_NAD1983_HARN")
race_bathy_df <- fortify(race_bathy)
race_bathy_df$long <- ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)




# .Show VBGF residuals as gradient ----------------------------------------

growth_map_resid_gradient <- 
  ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#8c8c8c", size = 0.2, alpha=0.5) +
  geom_point(data = Data_map,
             aes(x = longitude, y = latitude, color=resid), #you can change this to std_resid if you prefer standardized
             shape=15, size=1, alpha=0.5) +
  scale_color_gradient2(low="mediumblue", high="red") +
  scale_x_continuous(breaks = seq(-175,-135, by=5)) +
  scale_y_continuous(breaks = seq(45,65, by=1)) +
  labs(color = "VBGF Residuals") +
  coord_map(projection = "albers",lat0=40, lat1=55,
            xlim = c(min(Data_map$longitude), max(Data_map$longitude)),
            ylim = c(min(Data_map$latitude), max(Data_map$latitude)))+
  theme_bw() +
  theme(legend.position = "top",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"))

ggsave(filename = paste0("../Output/", AY, "/growth-map-gradient.png"),
       plot = growth_map_resid_gradient, dpi = 300, width = 6.5, height = 4, units = "in")



# .Show VBGF residuals as binary (positive or negative) ----------------------------

#To categorize residuals as negative or positive
Data_map %<>%
  mutate(resid_binary = ifelse(resid < 0, "Negative", 
                               ifelse(resid > 0, "Positive", "Neutral")))


growth_map_resid_binary <- 
  ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_polygon(data = russia, aes(long, lat, group=group), fill= "gray70", color = "gray40", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#8c8c8c", size = 0.2, alpha=0.5) +
  geom_point(data = Data_map,
             aes(x = longitude, y = latitude, color=resid_binary),
             shape=16, size=1, alpha=0.5) +
  scale_color_manual(values = c("dodgerblue", "red3")) +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  scale_x_continuous(breaks = seq(-175,-135, by=5)) +
  scale_y_continuous(breaks = seq(45,65, by=1)) +
  labs(color = "VBGF Residuals") +
  coord_map(projection = "albers",lat0=40, lat1=55,
            xlim = c(min(Data_map$longitude), max(Data_map$longitude)),
            ylim = c(min(Data_map$latitude), max(Data_map$latitude)))+
  theme_bw() +
  theme(legend.position = "top",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"))

ggsave(filename = paste0("../Output/", AY, "/growth-map-binary.png"),
       plot = growth_map_resid_binary, dpi = 300, width = 6.5, height = 4, units = "in")



