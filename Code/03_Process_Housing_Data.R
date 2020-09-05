library(here)
library(dplyr)

housing<-read.csv("/n/koutrakis_lab/lab/Group_Data/America Community Survey/ACS_Housing_Table.csv")
housing$Geo_ZCTA5=formatC(housing$Geo_ZCTA5,width=5,flag=0)

load(here::here("Data","GeoData","2015_Shapes.RData"))

zips=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]
zips_house=zips@data%>%left_join(housing,by=c("ZIP"="Geo_ZCTA5"))

#Convert absolute value to percentage
zips_house[,c("Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
              "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
              "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing")]=
  zips_house[,c("Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
                "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
                "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing")]/zips_house[,"Housing_Units"]

zips_house[,c("Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
              "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
              "Units_1940_1949","Units_Before_1939")]=
  zips_house[,c("Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
                                                                    "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
                                                                    "Units_1940_1949","Units_Before_1939")]/zips_house$Occupied_Units

zips_house[,c("One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
              "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit","Over_Nine_Room_Unit")]=
  zips_house[,c("One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
                "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit","Over_Nine_Room_Unit")]/zips_house$Occupied_Units

zips_house[,c("No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
              "Four_Bedroom_Unit","Over_Five_Bedroom_Unit")]=
  zips_house[,c("No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
                "Four_Bedroom_Unit","Over_Five_Bedroom_Unit")]/zips_house$Occupied_Units

zips_house[,c("Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]=
  zips_house[,c("Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]/zips_house$Occupied_Units

zips_house=zips_house[,c("ZIP","POP_SQMI","POPULATION",
                         "Housing_Units","Single_Family","Two_Units_Housing","Three_Four_Units_Housing",
                         "Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
                         "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing",
                         "Units_After_2014","Units_2010_2013","Units_2000_2009","Units_1990_1999",
                         "Units_1980_1989","Units_1970_1979","Units_1960_1969","Units_1950_1959",
                         "Units_1940_1949","Units_Before_1939","One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit",
                         "Five_Room_Unit","Six_Room_Unit","Seven_Room_Unit","Eight_Room_Unit",
                         "Over_Nine_Room_Unit","No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit","Three_Bedroom_Unit",
                         "Four_Bedroom_Unit","Over_Five_Bedroom_Unit",
                         "Gas_Fuel","Electricity_Fuel","Oil_Fuel","Coal_Fuel","Solar_Fuel","No_Fuel")]
save(file=here::here("Data","Medium Data","ZIP_Housing.RData"),zips_house)
