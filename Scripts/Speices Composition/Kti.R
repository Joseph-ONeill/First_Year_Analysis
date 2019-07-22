Forests <- read.csv("Data/All_data.csv")
f_types = unique(Forests$Forest_Type)
for (f in f_types){
  this_type = Forests[Forests$Forest_Type == f, ]
  this_mean = mean(this_type$C_Tree, na.rm = TRUE)
  print (c(f, this_mean))
}