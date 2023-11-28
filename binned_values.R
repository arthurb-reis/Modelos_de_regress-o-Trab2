aux_column <- data$income
data$income <- as.numeric(data$income)
for (i in 1:length(data$income)) {
    if(aux_column[i] == ">50K"){ 
        data$income[i] <- 1 
    }
    else{
        data$income[i] <- 0    
    }
    if(data[i, "native.country"] == "Holand-Netherlands"){ 
        data[i, "native.country"] <- "?" 
    }
}

library(Information)

info_values <- create_infotables(data = data, y = "income")
info_values$Summary
info_values$Tables

data$age.binned <- cut(data$age, breaks = c(17,21,25,29,32,36,40,44,50,57,90), include.lowest = TRUE)
