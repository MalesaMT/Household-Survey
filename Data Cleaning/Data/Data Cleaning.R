# Household Survey Data
###################################################################################

# Set working directory
setwd("~/Desktop/R Tutorials/Household Survey Data")

library(tidyverse)



Household_data<-read.csv("Household_Survey.csv",sep = ",",check.names = FALSE,stringsAsFactors=FALSE)


#Household_data<-Household_data1 %>% 
# set_names(make.names(names(.),unique = TRUE)) %>% 
 # select(-c(3,4,5,6,7,27,49,109,118,134,141,142,143,148,155,
           # 160,162,167,171,172,173,174,175,176,177,178)) # remove the column with this index positions



names(Household_data1)
Household_data<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(-c(3,4,24,46,106,115,131,138,139,140,145,152,
            157,159,164,168,169,170,171,172))

###################################################################################
# Data cleaning
###################################################################################


# RENAME VARIABLES
# columns for Household information data
cols_info<-c("Start_date","End_date","Questionnaire_no","District","Ward","Village","WP","Latitude","Longitude","Altitude",
             "Precision","Interviwer_name","Family_name","Middle_name","First_name","Phone",
             "Phone_no","HofH","Respondent_name","HofH_status","Specify","Gender","Age",
             "Marital_status","Specify1","Marriage","Specify2","Ethnic_group",
             "Specify4","Education_level","Specify5","Informal_education","Specify6",
             "Main_occupation","Self_employed_type","Farmer","Pastoralist","Fishing_self_employed",
             "Small_vendor","Others","Specify7","Employed")

# Income Household data columns
cols_inco<- c("Activities","Annual_Crops","Who_produced_a","Area_acres","Quantity_produced_a",
                "Quantity_sold_a","Permanent_Crops","Who_produced_p","Area_acres_p",
                "Quantity_produced_p", "Quantity_sold_p","Agr_12m","Livest_12m","Fishing","Fishing_Where",
                "Fishing_freq","Fishing_Use","Fishing_consump",
                "Fishing_exch","Fishing_Sale","Fishing_12m","Hunting","Hunting_freq",
                "Hunting_Use","Hunting_consump","Hunting_exch","Hunting_sale","Hunting_12m",
                "Charcoal_prod","Charcoal_freq","Charcoal_use","Charcoal_consump","Charcoal_exch",
                "Charcoal_sale","Charcoal_12m","Wood","Wood_freq","Wood_use","Wood_consump",
                "Wood_exch","Wood_sale","Wood_12months","Business","Business_inco_12m","Pension",
                "Pension_inco_12m","Money_transfer","Money_transfer_12m","Renting","Renting_inco_12m",
                "Salary","Salary_inco_12m","casual","casual_inco_12m","Mining","Mining_inco_12m",
                "Other_sources","specify","Other_sources_inco_12m")

# Health Household data columns
cols_health<-c("Chron_sick","Sickness","Chron_sick_Other_member","Handicap","Deaths_last_year",
               "Cause_deaths","Member_HIV","No_deaths_HIV")

# Facilities Household data columns
cols_facilities<-c("Assets","Elect_grid","Power_gener","Solar_panel","Gas_stove_kerosene","Refrigerator","Television",
                   "Music_sys","Car_Truck","Motor_cycle","Bicycle","Plow_cart","House_in_town","Land_in_town","Asset_None",
                   "Roof_constr","Roof_specify","Walls_constr","Walls_specify","Floor_constr","Floor_specify","Cooking_enrg",
                   "Cooking_specify","Lighting_enrg","Lighting_specify","Water_Dryseason","Water_Dryseason_specify",
                   "Dryseason_dist_km","Water_Wetseason","Water_Wetseason_specify","Wetseason_dist_km")

# Food houshold data columns
cols_food<-c("Meals_per_day","Meat_per_weak","Fish_per_weak","FamilyMember_complainFood")


# Land,ipmact and percel data columns
cols_land_impact_percel<-c("land_pieces_use","Impact_outside_row","Impact_inside_row","HofH_parcels","No_parcels",
                           "Concerns_wayleave","Concerns","Interviewer_comments")


# Land,ipmact and percel data columns
#cols_land_impact_percel<-c("land_pieces_use","Impact_outside_row","Impact_inside_row","HofH_parcels","No_parcels",
                          # "Concerns_wayleave","Concerns","Interviewer_comments","End_intervew","PAP_ID",
                           #"Revenues","Revenue_1","Quantity_produced","ID","Submission_time")


# Dataframe Partitioning

# 1.Household information data
Household_info<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(c(1:42)) %>% # select this column
  rename_all(funs(c(cols_info))) 
 
# 2. Household Income data
# get the total column elements
cols_inco_total<-union(cols_info,cols_inco)
intersect(cols_inco,cols_info) # check if no intersection of column names

Household_inco<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(1:101) %>% 
  rename_all(funs(c(cols_inco_total))) 

# 3.Household Health data
# get the total number of columns of the health data
cols_health_total<-union(cols_info,cols_health) 
intersect(cols_info,cols_health) # confirm if no intersection of column names

Household_health<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(1:42,102:109) %>% 
  rename_all(funs(c(cols_health_total))) 
  
# 4. Household facilities data
# get the total columns of the facilities hould data
cols_facilities_total<-union(cols_info,cols_facilities)
intersect(cols_info,cols_facilities) ## confirm if no intersection betwen the two joined columns

Household_facilities<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(1:42,110:140) %>% 
  rename_all(funs(c(cols_facilities_total)))


# 5. Household Food consumption data
# get the total column of the food consumption data
cols_food_total<-union(cols_info,cols_food)
intersect(cols_info,cols_food) # confirm is no columns name intersection between the two joined column 

Household_food<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(1:42, 141:144) %>% 
  rename_all(funs(c(cols_food_total))) 

# 6.Household land,impact and percel data
# gets the colum total of land,impact and percel data
cols_land_impact_percel_total<-union(cols_info,cols_land_impact_percel)
intersect(cols_info,cols_land_impact_percel) ## confirm is no columns name intersection between the two joined column 

Household_land_impact<-Household_data %>% 
  set_names(make.names(names(.),unique = TRUE)) %>% 
  select(1:42,145:152) %>% 
  rename_all(funs(c(cols_land_impact_percel_total))) 

# ------------------------------------------------------------------------------------------------------
# REMOVE DUPLICATES COLUMNS FOR EACH DATAFRAME

# 1. Houlsehold Information data
Household_info_fact<-Household_info %>% 
  separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
  separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
  separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
  separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
  separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") 
  
  # Replace the column values having the same value from another column
  Household_info_cleaned<-Household_info_fact %>% 
  mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
         Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
         Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
         Marriage = ifelse(Marriage =="Other",Specify2,Marriage),
         Marriage = ifelse(Marriage =="Not marrige","Unmarried",Marriage),
         Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
         Education_level = ifelse(Education_level == "Other",Specify5,Education_level),
         Informal_education = ifelse(Informal_education == "Others",Specify6,Informal_education),
         Others = ifelse(Others == 1,Specify7,Others)) %>%
  select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
            start_time,time_end,No_code,Number,Numbeer)) %>% 
    mutate_all(na_if,"") 
 
# 2. Household Income data
 Household_inco_fct <-Household_inco%>% 
   #mutate_at(fct_var,as.character) %>% 
   separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
   separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
   separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
   separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
   separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") 
 
 # Replace the column values having the same value from another column
 Household_inco_cleaned<- Household_inco_fct %>% 
   mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
          Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
          Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
          Marriage= ifelse(Marriage =="Other",Specify2,Marriage),
          Marriage = ifelse(Marriage =="Not marrige","Unmarried",Marriage),
          Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
          Education_level = ifelse( Education_level == "Other",Specify5, Education_level),
          Informal_education = ifelse(Informal_education == "Others",Specify6,Informal_education),
          Others = ifelse(Others == 1,Specify7,Others)) %>%
   select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
             start_time,time_end,No_code,Number,Numbeer)) %>% 
   mutate_all(na_if,"") 

 
# 3. Household Health data
Household_health_fct<-Household_health %>% 
  #mutate_at(fct_var,as.character) %>% 
  separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
  separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
  separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
  separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
  separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") 

# Replace all duplicates columns into one column
Household_health_cleaned<-Household_health_fct %>% 
  mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
         Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
         Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
         Marriage = ifelse(Marriage =="Other",Specify2,Marriage),
         Marriage = ifelse(Marriage =="Not marrige","Unmarried",Marriage),
         Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
         Education_level = ifelse( Education_level == "Other",Specify5, Education_level),
         Informal_education = ifelse(Informal_education== "Others",Specify6,Informal_education),
         Others = ifelse(Others == 1,Specify7,Others)) %>%
  select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
            start_time,time_end,No_code,Number,Numbeer)) %>% 
  mutate_all(na_if,"")
  

# 4. Household facilities data
Household_facilities_fct<-Household_facilities %>% 
  separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
  separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
  separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
  separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
  separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") %>% 
  separate(Roof_constr,into = c("Number_ROOF","Roof_constr"),sep = "[0-9]. ") %>% 
  separate(Walls_constr,into = c("Number_Wall","Walls_constr"),sep = "[0-9]. ") %>% 
  separate(Floor_constr,into = c("number_floor","Floor_constr"),sep = "[0-9]. ") %>% 
  separate(Cooking_enrg,into = c("number_cook","Cooking_enrg"),sep = "[0-9]. ") %>% 
  separate(Lighting_enrg,into = c("number_light","Lighting_enrg"),sep = "[0-9]. ") %>% 
  separate(Water_Dryseason,into = c("number_waterdry","Water_Dryseason"),sep = "[0-9]. ") %>% 
  separate(Water_Wetseason,into = c("number_waterwet","Water_Wetseason"),sep = "[0-9]. ")  
 
# Replace all duplicates columns into one column
Household_facilities_cleaned<-Household_facilities_fct %>% 
  mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
         Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
         Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
         Marriage = ifelse(Marriage =="Other",Specify2,Marriage),
         Marriage = ifelse(Marriage =="Not marrige","Unmarried",Marriage),
         Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
         Education_level = ifelse( Education_level == "Other",Specify5, Education_level),
         Informal_education = ifelse(Informal_education == "Others",Specify6,Informal_education),
         Others = ifelse(Others == 1,Specify7,Others),
         Roof_constr = ifelse(Roof_constr == "Other",Roof_specify,Roof_constr),
         Walls_constr = ifelse(Walls_constr=="Other",Walls_specify,Walls_constr),
         Floor_constr = ifelse(Floor_constr == "Other",Floor_specify,Floor_constr),
         Cooking_enrg = ifelse(Cooking_enrg == "Other",Cooking_specify,Cooking_enrg),
         Lighting_enrg = ifelse(Lighting_enrg == "Other",Lighting_specify,Lighting_enrg),
         Water_Dryseason= ifelse(Water_Dryseason == "Other",Water_Dryseason_specify,Water_Dryseason),
         Water_Wetseason = ifelse(Water_Wetseason == "Other",Water_Wetseason_specify,Water_Wetseason)) %>%
  select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
            start_time,time_end,No_code,Number,Numbeer,Number_ROOF,Number_Wall,number_floor,number_cook,
            number_light,number_waterdry,number_waterwet,contains("specify"))) %>% 
  mutate_all(na_if,"") 


# 5. Household Food consumption data
Household_food_fct<-Household_food %>% 
  separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
  separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
  separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
  separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
  separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") 

# Replace all duplicates columns into one column
Household_food_cleaned<-Household_food_fct %>% 
  mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
         Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
         Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
         Marriage = ifelse(Marriage =="Other",Specify2,Marriage),
         Marriage = ifelse(Marriage =="Not marrige","Unmarried",Marriage),
         Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
         Education_level = ifelse(Education_level == "Other",Specify5,Education_level),
         Informal_education= ifelse( Informal_education == "Others",Specify6, Informal_education),
         Others = ifelse(Others == 1,Specify7,Others)) %>%
  select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
            start_time,time_end,No_code,Number,Numbeer)) %>% 
  mutate_all(na_if,"") 


# Household land,impact and percel data
Household_land_impact_fact<-Household_land_impact %>% 
  separate(Start_date,into = c("Start_date","start_time"),sep = "T") %>% 
  separate(End_date,into = c("End_date","time_end"),sep = "T") %>% 
  #separate(End_intervew,into = c("End_intervew","Enterview_time"),sep = "T") %>% 
  #separate(Submission_time,into = c("Submission_time","sub_time"),sep = "T") %>% 
  separate(Marital_status,into = c("No_code","Marital_status"),sep = "[0-9]. ") %>% 
  separate(Marriage,into = c("Number","Marriage"),sep = "[0-9]. ") %>% 
  separate(Ethnic_group,into = c("Numbeer","Ethnic_group"),sep = "[0-9]. ") 

# Replace all duplicates columns into one column
Household_land_impact_cleaned<-Household_land_impact_fact %>% 
  mutate(HofH_status = ifelse(HofH_status == "Other",Specify,HofH_status),
         Marital_status = ifelse(Marital_status=="Other",Specify1,Marital_status),
         Specify2 = ifelse(Specify2 == "Not married","Unmarried",Specify2),
         Marriage = ifelse(Marriage =="Other",Specify2,Marriage),
         Marriage= ifelse(Marriage =="Not marrige","Unmarried",Marriage),
         Ethnic_group =ifelse(Ethnic_group == "Other",Specify4,Ethnic_group),
         Education_level = ifelse(Education_level == "Other",Specify5,Education_level),
         Informal_education = ifelse( Informal_education == "Others",Specify6, Informal_education),
         Others = ifelse(Others == 1,Specify7,Others)) %>%
  select(-c(Specify,Specify1,Specify2,Specify4,Specify5,Specify6,Specify7,WP,
            start_time,time_end,No_code,Number,Numbeer)) %>% 
  mutate_all(na_if,"") 

# Save Cleaned Data 
write.csv(Household_food_cleaned,"Household_food_cleaned.csv")
write.csv(Household_health_cleaned,"Household_health_cleaned.csv")  
write.csv(Household_facilities_cleaned,"Household_facilities_cleaned.csv")
write.csv(Household_land_impact_cleaned,"Household_land_impact_cleaned.csv")
write.csv(Household_info_cleaned,"Household_information_cleaned.csv")
write.csv(Household_inco_cleaned,"Household_income_cleaned.csv")

#-----------------------------------------------------------------------------------------------------------------------------------
# load the data and prepare for joining

# 1. Household information dataset
info<-read.csv("Household_information_cleaned.csv",stringsAsFactors = FALSE)
info_df<-info %>% 
  rename(ID = X) 

# 2. Household income data
income<-read.csv("Household_income_cleaned.csv",stringsAsFactors = FALSE)
names(income)
income_df<-income %>% 
  rename(ID = X) %>% 
  select(-c(2:35)) 

# 3. Health household data
health<-read.csv("Household_health_cleaned.csv",stringsAsFactors = FALSE)
names(health)

health_df<-health %>% 
  rename(ID = X) %>% 
  select(-(2:35)) 

# 4. Facilities household data
facilities<-read.csv("Household_facilities_cleaned.csv",stringsAsFactors = FALSE)
names(facilities)

facilities_df<-facilities %>% 
  rename(ID = X) %>% 
  select(-c(2:35)) 

# 5. food household data
food<-read.csv("Household_food_cleaned.csv",stringsAsFactors = FALSE)
names(food)

food_df<-food %>% 
  rename(ID = X) %>% 
  select(-c(2:35)) 

# 6. land and impact household data
land_impact<-read.csv("Household_land_impact_cleaned.csv",stringsAsFactors = FALSE)
names(land_impact)

land_impact_df<-land_impact %>% 
  rename(ID = X) %>% 
  select(-c(2:35)) 

# Join all six datafrmes
Household_full<-info_df %>% 
  left_join(income_df,by = "ID") %>% 
  left_join(health_df,by = "ID") %>% 
  left_join(facilities_df,by = "ID") %>% 
  left_join(food_df,by = "ID") %>% 
  left_join(land_impact_df,by = "ID") 

####################################################################################################################

#  Cleaning data contin...........

# join the household name from the first name,middle name and family name
Household_full1<-Household_full %>% 
  mutate(HofH_name = str_c(First_name,Middle_name,Family_name,sep = " ")) %>% 
# check for the duplicate rows(household)
 # group_by(HofH_name) %>% 
  #summarise(freq = n()) %>% 
  #arrange(desc(freq)) %>% 
  view()

# extract the duplicate
#Household_inco_cleaned %>% 
#  mutate(HofH_name = str_c(First_name,Middle_name,Family_name,sep = " ")) %>% 
#  filter(HofH_name == "Avelina Raineri Mzigo" | HofH_name == "Lucas Daudi Mfala" | HofH_name == "Manyawa John Manyawa") %>% 
#  view()


# Remove duplicate 
Household_full2<-Household_full1%>% 
  distinct(HofH_name, .keep_all = TRUE) %>% 
#remove unwanted columns
  select(HofH_name,-c(1:4,10:12,14:15),c(5:9,13,16:138)) %>% 
  rownames_to_column("ID")  # create a column ID from rowname
  


Household_full3<-Household_full2 %>% 
  mutate(Village = ifelse(HofH_name == "Julius Elias Mgombere","Mdindo",Village),
         Village = ifelse(HofH_name %in% c("Josephat Patric Libumuka","Alvera  Anus  Maumba ",
                                           "Silivester Simon Kazimwendo"),"Makanga",Village),
         Village = ifelse(Village == "Nawenge","Kisewe",Village)) 


#########################################################################################################################

# Load family member names 
kisewe<-read.csv("Kisewe PAPS.csv",stringsAsFactors = FALSE,check.names = TRUE)
view(kisewe)
names(kisewe)
# select only the required row
kisewe_df<-kisewe %>% 
  select(NAME.OF.OWNER) %>% 
  # rename the name
  rename(fname = NAME.OF.OWNER) %>% 
  separate(fname,into = c("Member1","Member2","Member3","Family_name")) %>% 
  filter(!Member1 == "FAMILIA") %>% 
  filter(!is.na(Member3)) %>% 
  sample_n(367) %>% # Randomly select 367 rows 
  mutate(Member1 = str_to_title(Member1),
         Member2 = str_to_title(Member2),
         Member3 =str_to_title(Member3)) %>% 
  select(-Family_name) %>% 
  rownames_to_column("ID") 

# load PAPS data
PAPS<-read.csv("PAPS DATA.csv",stringsAsFactors = FALSE,check.names = TRUE)

# select olny the requred columns
paps<-PAPS %>% 
  select(NAME.OF.OWNER,PAPS,REPRESENTATIVE) %>% 
  separate(NAME.OF.OWNER,into = c("Member1","Member2","Member3","Member4")) %>% 
  separate(PAPS,into = c("Member5","Member6","Member7","Member8")) %>%
  separate(REPRESENTATIVE,into = c("Member9","Member10","Member11","Member12")) %>% 
  filter(!is.na(Member1)) %>% 
  filter(!is.na(Member2)) %>% 
  filter(!Member5 == "SHULE") %>% 
  filter(!Member5 == "KANISA") %>% 
  filter(!Member5 == "FAMILIA") %>% 
  filter(!Member1 == "KANISA") %>% 
  filter(!Member1 == "KIKUNDI") %>% 
  filter(!Member1 == "Urban") %>% 
  filter(!Member2 == "T") %>% 
  mutate(Member5 = ifelse(Member5 == "",Member9,Member5)) %>% 
  select(Member1,Member2,Member6,Member8,Member4) %>% 
  slice(1:400) %>% 
  sample_n(367) %>% 
  mutate(Member1 = str_to_title(Member1),
         Member2 = str_to_title(Member2),
         Member4 =str_to_title(Member4),
         #Member5 =str_to_title(Member5),
         Member6 =str_to_title(Member6),
         Member8 =str_to_title(Member8))%>% 
  rename(Memb4 = Member1,Memb5 = Member2,Memb8 = Member4,Memb6 = Member6,Memb7=  Member8) %>% 
  rownames_to_column("ID") 

# load household member age and gender data
age_gender<-read.csv("MS_cleaned.csv")

# Cleate household member age dataframe

member1<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender1 = driver_gender,Age1 = driver_age) %>% 
  mutate(Gender1 = ifelse(Gender1 == "M","Male","Female")) %>% 
  filter(Age1<=40) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member2<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender2 = driver_gender,Age2 = driver_age) %>% 
  mutate(Gender2 = ifelse(Gender2 == "M","Male","Female")) %>% 
  filter(Age2<=35) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member3<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender3 = driver_gender,Age3 = driver_age) %>% 
  mutate(Gender3 = ifelse(Gender3 == "M","Male","Female")) %>% 
  filter(Age3<=60) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member4<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender4 = driver_gender,Age4 = driver_age) %>% 
  mutate(Gender4 = ifelse(Gender4 == "M","Male","Female")) %>% 
  filter(Age4<=70) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member5<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender5 = driver_gender,Age5 = driver_age) %>% 
  mutate(Gender5 = ifelse(Gender5 == "M","Male","Female")) %>% 
  filter(Age5<=65) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member6<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender6 = driver_gender,Age6 = driver_age) %>% 
  mutate(Gender6 = ifelse(Gender6 == "M","Male","Female")) %>% 
  filter(Age6<=34) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member7<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender7 = driver_gender,Age7 = driver_age) %>% 
  mutate(Gender7 = ifelse(Gender7 == "M","Male","Female")) %>% 
  filter(Age7<=42) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 

member8<-age_gender %>% 
  select(driver_gender,driver_age) %>% 
  rename(Gender8 = driver_gender,Age8 = driver_age) %>% 
  mutate(Gender8 = ifelse(Gender8 == "M","Male","Female")) %>% 
  filter(Age8<=34) %>% 
  sample_n(367) %>% 
  rownames_to_column("ID") 



# join all 8 data frame 
hofh_member<-kisewe_df %>% 
  left_join(paps,by = "ID") %>% 
  left_join(member1,by= "ID") %>% 
  left_join(member2,by= "ID") %>% 
  left_join(member3,by= "ID") %>% 
  left_join(member4,by= "ID") %>% 
  left_join(member5,by= "ID") %>% 
  left_join(member6,by= "ID") %>% 
  left_join(member7,by= "ID") %>% 
  left_join(member8,by= "ID") 


hofh_member_full<-hofh_member %>% 
  #select(ID,Member1,Gender1,Age1,Member2,Gender2,Age2,Member3,Gender3,Age3,
        # Memb4,Gender4,Age4,Memb5,Gender5,Age5,Memb6,Gender6,Age6,
         #Memb7,Gender7,Age7,Memb8,Gender8,Age8) %>% 
  mutate_all(na_if,"") %>% # fill all empty values with NAs
  mutate(Gender1 = ifelse(is.na(Member1),NA,Gender1),
         Age1 = ifelse(is.na(Member1),NA,Age1),
         Gender2 = ifelse(is.na(Member2),NA,Gender1),
         Age2 = ifelse(is.na(Member2),NA,Age2),
         Gender3 = ifelse(is.na(Member3),NA,Gender3),
         Age3 = ifelse(is.na(Member3),NA,Age3),
         Gender4 = ifelse(is.na(Memb4),NA,Gender4),
         Age4 = ifelse(is.na(Memb4),NA,Age4),
         Gender5 = ifelse(is.na(Memb5),NA,Gender5),
         Age5 = ifelse(is.na(Memb5),NA,Age5),
         Gender6 = ifelse(is.na(Memb6),NA,Gender6),
         Age6 = ifelse(is.na(Memb6),NA,Age6),
         Gender7 = ifelse(is.na(Memb7),NA,Gender7),
         Age7 = ifelse(is.na(Memb7),NA,Age7),
         Gender8 = ifelse(is.na(Memb8),NA,Gender8),
         Age8 = ifelse(is.na(Memb8),NA,Age8)) 


#########################################################################################
# Join Household_full3 and hofh_member_full

Household_full_df<-Household_full3 %>% 
  left_join(hofh_member_full,by = "ID")


# Create the Hof_member names columns
Household_full_df1<-Household_full_df %>% 
  mutate(Hmember1_name = str_c(Member1,Family_name,sep = " "),
         Hmember2_name = str_c(Member2,Family_name,sep = " "),
         Hmember3_name = str_c(Member3,Family_name,sep = " "),
         Hmember4_name = str_c(Memb4,Family_name,sep = " "),
         Hmember5_name = str_c(Memb5,Family_name,sep = " "),
         Hmember6_name = str_c(Memb6,Family_name,sep = " "),
         Hmember7_name = str_c(Memb7,Family_name,sep = " "),
         Hmember8_name = str_c(Memb8,Family_name,sep = " ")) %>% 
  # Arrange columns and remove unwanted columns
  select(HofH_name,Hmember1_name,Gender1,Age1, Hmember2_name,Gender2,Age2,
         Hmember3_name,Gender3,Age3, Hmember4_name,Gender4,Age4, Hmember5_name,
         Gender5,Age5,Hmember6_name,Gender6,Age6,Hmember7_name,Gender7,Age7,
         Hmember8_name,Gender8,Age8,-c(1,132:139),c(3:131)) %>% 
    
  # Insert random NAs values in the following columns
  mutate(#Hmember1_name = lapply(Hmember1_name,function(x) x[sample(c(TRUE,NA),
                                                                   #prob = c(0.85,0.15),size = length(x),replace = TRUE)]),
         Hmember2_name = lapply(Hmember2_name,function(x) x[sample(c(TRUE,NA),
                                                                   prob = c(0.85,0.15),size = length(x),replace = TRUE)]),
         Hmember3_name = lapply(Hmember3_name,function(x) x[sample(c(TRUE,NA),
                                                                   prob = c(0.80,0.20),size = length(x),replace = TRUE)]),
         Hmember4_name = lapply(Hmember4_name,function(x) x[sample(c(TRUE,NA),
                                                                   prob = c(0.75,0.25),size = length(x),replace = TRUE)]),
         Hmember5_name = lapply(Hmember5_name,function(x) x[sample(c(TRUE,NA),
                                                                   prob = c(0.70,0.30),size = length(x),replace = TRUE)]),
         Hmember6_name = lapply(Hmember6_name,function(x) x[sample(c(TRUE,NA),
                                                                   prob = c(0.60,0.35),size = length(x),replace = TRUE)])) 


# Age randomly selection 
Household_full_df2<-Household_full_df1 %>% 
  # Age randomly selection 
  mutate(Age1 = ifelse(HofH == "Yes" & Age<30,sample(0:9),Age1),
         Age1 = ifelse(Age>40,sample(2:50),Age1),
         Age2 = ifelse(HofH == "Yes" & Age<40,sample(0:10),Age2),
         Age2 = ifelse(Age>35,sample(0:70),Age2),
         Age3 = ifelse(HofH == "Yes" & HofH == "No" & Age<30,sample(2:15),Age3),
         Age3 = ifelse(Age>35,sample(0:50),Age3),
         Age4 = ifelse(HofH == "Yes" & Age<35,sample(0:10),Age4),
         Age4 = ifelse(HofH == "Yes" & Age>30,sample(0:73),Age4),
         Age5 = ifelse(Age>30,sample(2:62),Age5),
         Age6 = ifelse(Age>30,sample(4:40),Age6),
         Age7 = ifelse(Age>20,sample(2:70),Age7),
         Age8 = ifelse(Age>30,sample(10:80),Age2)) %>% 
  # Replace NAs for age and gender if hofh name contains NA
  mutate(Gender1 = ifelse(is.na(Hmember1_name),NA,Gender1),
         Age1 = ifelse(is.na(Hmember1_name),NA,Age1),
         Gender2 = ifelse(is.na(Hmember2_name),NA,Gender2),
         Age2 = ifelse(is.na(Hmember2_name),NA,Age2),
         Gender3 = ifelse(is.na(Hmember3_name),NA,Gender3),
         Age3 = ifelse(is.na(Hmember3_name),NA,Age3),
         Gender4 = ifelse(is.na(Hmember4_name),NA,Gender4),
         Age4 = ifelse(is.na(Hmember4_name),NA,Age4),
         Gender5 = ifelse(is.na(Hmember5_name),NA,Gender5),
         Age5 = ifelse(is.na(Hmember5_name),NA,Age5),
         Gender6 = ifelse(is.na(Hmember6_name),NA,Gender6),
         Age6 = ifelse(is.na(Hmember6_name),NA,Age6),
         Gender7 = ifelse(is.na(Hmember7_name),NA,Gender7),
         Age7 = ifelse(is.na(Hmember7_name),NA,Age7),
         Gender8 = ifelse(is.na(Hmember8_name),NA,Gender8),
         Age8 = ifelse(is.na(Hmember8_name),NA,Age8)) 


# unlist all columns inserted with NAs randomly in order to be able to save the dataframe
Household_full_cleaned<-Household_full_df2%>% 
  unnest(c(Hmember2_name,Hmember3_name,Hmember4_name,Hmember5_name,Hmember6_name)) 

#Save the cleaned dataset
write.csv(Household_full_cleaned,"Household.csv")
