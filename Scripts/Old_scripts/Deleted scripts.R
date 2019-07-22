The_PSP <- read_csv("Data/19PSP.csv")
The_PSP<-The_PSP %>% mutate(land_tenure = ifelse(Location  == "Yoma_Unclassfied_Forest", "Unclassified", "Reserve"))
The_PSP$land_tenure