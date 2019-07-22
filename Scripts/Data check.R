Data_check <- unique(`All_data_Final(All_points)`)
duplicated_rows <- duplicated(`All_data_Final(All_points)`)

dup  <- `All_data_Final(All_points)`[duplicated_rows,]

names(Data_check)

colnames(Data_check)[1] <- "Forest_type"

names(Data_check)

unique(Forests_All_info$Plot_id) %in% unique(Forests_WCdata$Plot_id) to see the row.