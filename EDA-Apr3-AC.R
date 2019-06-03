
setwd('C:/Users/Alissa Hayes/Desktop/Springboard')


# read_data <- function() {
#  ins.ds <- read.csv('Project_File2.csv')
#   return(ins.ds)
# }

## Plot percent of 0s and 1s by level of categorical variable

#Marital Status
plot_categorical_marital_status <- function(Cancels) {
  T1 <- table(Cancels$marital_status, Cancels$Cancelled) # contingency table
  print(T1)
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("marital_status", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "marital_status")
  #print(T2)                            

  g1 <- ggplot(T2, aes(x=marital_status, y=value, fill=variable)) +
         geom_bar(stat='identity', position='dodge')
  
  print(g1)
  return(0)
}

#Education
plot_categorical_education <- function(Cancels) {
  T1 <- table(Cancels$education, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("education", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "education")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=education, y=value, fill=variable)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
  return(0)
}

#Agent

plot_categorical_agent <- function(Cancels) {
  T1 <- table(Cancels$EMPLY_EMAIL_ADDR_TXT.x, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("Agent Name", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "EMPLY_EMAIL_ADDR_TXT.x")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=EMPLY_EMAIL_ADDR_TXT.x, y=value, fill=variable)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)

}

#Currently Insured
plot_categorical_insured <- function(Cancels) {
  T1 <- table(Cancels$currently_insured.x, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1),2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("currently_insured.x", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "currently_insured.x")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=currently_insured.x, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Gender
plot_categorical_gender <- function(Cancels) {
  T1 <- table(Cancels$gender, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("gender", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "gender")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=gender, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Risk Profile
plot_categorical_risk_profile <- function(Cancels) {
  T1 <- table(Cancels$risk_profile, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("risk_profile", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "risk_profile")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=risk_profile, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Previous Policy Type
plot_categorical_policy_type <- function(Cancels) {
  T1 <- table(Cancels$previous_policy_type, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("previous_policy_type", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "previous_policy_type")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=previous_policy_type, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#BI limit
plot_categorical_bi_limit <- function(Cancels) {
  T1 <- table(Cancels$bi_limit, Cancels$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("bi_limit", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "bi_limit")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=bi_limit, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Vehicle Ownership
plot_categorical_vehicle_ownership <- function(ins.ds) {
  T1 <- table(ins.ds$vehicle_ownership, ins.ds$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("vehicle_ownership", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "vehicle_ownership")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=vehicle_ownership, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#State
plot_categorical_state <- function(ins.ds) {
  T1 <- table(ins.ds$State, ins.ds$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("State", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "State")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=State, y=value, fill=variable)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Same Day vs. Follow-up sales
plot_categorical_SD_vs_followup <- function(ins.ds) {
  T1 <- table(ins.ds$SameDay_vs_Followup, ins.ds$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("SameDay_vs_Followup", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "SameDay_vs_Followup")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=SameDay_vs_Followup, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#Months Coverage
plot_categorical_months <- function(ins.ds) {
  T1 <- table(ins.ds$Months_Coverage, ins.ds$Cancelled) # contingency table
  T1.prop <- round(prop.table(T1, margin=1), 2)  #proportions margin 1 is row
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c("Months_Coverage", "percent_0", "percent_1")
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = "Months_Coverage")
  #print(T2)                            
  
  g1 <- ggplot(T2, aes(x=Months_Coverage, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}


#Numeric Plots

#LM Premium
plot_numeric_premium <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, LQ_FB_PREM_AMT)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="LM Premium by cancellation status",
         x="Cancelled",
         y="LQ_FB_PREM_AMT")  
  
  print(g1)
  
  print(mean(Cancels$LQ_FB_PREM_AMT[Cancels$Cancelled == 1]))
  print(mean(Cancels$LQ_FB_PREM_AMT[Cancels$Cancelled == 0]))
  
}

#Cost Per vehicle
plot_numeric_cost_per_vehicle <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, cost_per_vehicle)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Cost per vehicle by cancellation status",
         x="Cancelled",
         y="Cost per vehicle")  
  
  print(g1)
  
  print(mean(Cancels$cost_per_vehicle[Cancels$Cancelled == 1]))
  print(mean(Cancels$cost_per_vehicle[Cancels$Cancelled == 0]))
  
}


#Customer Age
plot_numeric_age <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, customer_age)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Customer age by cancellation status",
         x="Cancelled",
         y="customer_age")  
  
  print(g1)
  
  print(mean(Cancels$customer_age[Cancels$Cancelled == 1]))
  print(mean(Cancels$customer_age[Cancels$Cancelled == 0]))
  
}


#Number of vehicles
plot_numeric_number_vehicles <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, no_of_vehicles)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Number of vehicles by cancellation status",
         x="Cancelled",
         y="no_of_vehicles")  
  
  print(g1)
  
  print(mean(Cancels$no_of_vehicles[Cancels$Cancelled == 1]))
  print(mean(Cancels$no_of_vehicles[Cancels$Cancelled == 0]))
  
}

#Number of claims
plot_numeric_number_claims <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, number_of_claims)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Number of claims by cancellation status",
         x="Cancelled",
         y="number_of_claims")  
  
  print(g1)
  
  print(mean(Cancels$number_of_claims[Cancels$Cancelled == 1]))
  print(mean(Cancels$number_of_claims[Cancels$Cancelled == 0]))
  
}

#Number of accidents
plot_numeric_number_accidents <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, number_of_accidents)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Number of accidents by cancellation status",
         x="Cancelled",
         y="number_of_accidents")  
  
  print(g1)
  
  print(mean(Cancels$number_of_accidents[Cancels$Cancelled == 1]))
  print(mean(Cancels$number_of_accidents[Cancels$Cancelled == 0]))
  
}

#months coverage
plot_numeric_months <- function(Cancels) {
  Cancels$Cancelled_cat <- ifelse(Cancels$Cancelled == 1, "Yes", "No")
  g1 <- ggplot(Cancels, aes(Cancelled_cat, Months_Coverage)) +
    geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Box plot", 
         subtitle="Months coverage  by cancellation status",
         x="Cancelled",
         y="Months")  
  
  print(g1)
  
  print(mean(Cancels$Months_Coverage[Cancels$Cancelled == 1]))
  print(mean(Cancels$Months_Coverage[Cancels$Cancelled == 0]))
  
}


#CATEGORICAL PLOT AS A REUSABLE FUNCTION: 
plot_categorical <- function(IV) {
  T1 <- table(IV, Cancels3$Cancelled) # contingency table
  cat("T1\n")
  print(T1)
  
  T1.prop <- prop.table(T1, margin=1)  #proportions margin 1 is row
  cat("T1.prop\n")
  print(T1.prop)
  
  ##create a tibble with three cols
  T1.prop.tibble <- as.tibble(cbind(rownames(T1.prop), as.numeric(T1.prop[,1]), as.numeric(T1.prop[,2])))
  colnames(T1.prop.tibble) <- c(colnames(IV), "percent_0", "percent_1")
  cat("T1.prop.tibble\n")
  print(T1.prop.tibble)
  
  ##melt to have a dataset to produce side-by-side bars
  T2 <- melt(T1.prop.tibble, id.vars = colnames(IV))
  cat("T2\n")
  print(T2)                            
  
  g1 <- ggplot(T2, aes(x=IV, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge')
  
  print(g1)
}

#NUMERICAL PLOT AS A REUSABLE FUNCTION: 
#plot_numeric_premium <- function(IV, DV) {
 # ins.ds$Cancelled_cat <- ifelse(ins.ds$Cancelled == 1, "Yes", "No")
  #g1 <- ggplot(ins.ds, aes(Cancelled_cat, IV)) +
   # geom_boxplot(varwidth=T, fill="plum") + 
    #labs(title="Box plot", 
     #    subtitle="LM Premium by cancellation status",
      #   x="Cancelled",
       #  y="LQ_FB_PREM_AMT")  
  
  #print(g1)
  
  #print(mean(ins.ds$LQ_FB_PREM_AMT[ins.ds$Cancelled == 1]))
  #print(mean(ins.ds$LQ_FB_PREM_AMT[ins.ds$Cancelled == 0]))
  
#}


#CHI_SQUARE AS AN INDIVIDUAL FUNCTION
chi_marital_status <- function(Cancels) {
  table <- table(Cancels$marital_status, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_education <- function(Cancels) {
  table <- table(Cancels$education, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_gender <- function(Cancels) {
  table <- table(Cancels$gender, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_currently_insured <- function(Cancels) {
  table <- table(Cancels$currently_insured.x, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_risk_profile <- function(Cancels) {
  table <- table(Cancels$risk_profile, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_previous_policy_type <- function(Cancels) {
  table <- table(Cancels$previous_policy_type, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_bi_limit <- function(Cancels) {
  table <- table(Cancels$bi_limit, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_vehicle_ownership <- function(Cancels) {
  table <- table(Cancels$vehicle_ownership, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_state <- function(Cancels) {
  table <- table(Cancels$State, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_SameDay_vs_Followup <- function(Cancels) {
  table <- table(Cancels$SameDay_vs_Followup, Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_lead_seller <- function(Cancels) {
  table <- table(factor(Cancels$lead_seller), Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_agent <- function(Cancels) {
  table <- table(factor(Cancels$EMPLY_EMAIL_ADDR_TXT.x), Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}

chi_months <- function(Cancels) {
  table <- table(factor(Cancels$Months_Coverage), Cancels$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test)
}




#T-TEST AS AN INDIVIDUAL FUNCTION
t_test_age <- function(Cancels) {
  t_test <- t.test(Cancels$customer_age ~ Cancels$Cancelled)
  return(t_test)
}

t_test_LM_premium <- function(Cancels) {
  t_test <- t.test(Cancels$LQ_FB_PREM_AMT ~ Cancels$Cancelled)
  return(t_test)
}

t_test_cost_per_vehicle <- function(Cancels) {
  t_test <- t.test(Cancels$cost_per_vehicle ~ Cancels$Cancelled)
  return(t_test)
}

t_test_no_of_vehicles <- function(Cancels) {
  t_test <- t.test(Cancels$no_of_vehicles ~ Cancels$Cancelled)
  return(t_test)
}

t_test_no_of_claims <- function(Cancels) {
  t_test <- t.test(Cancels$number_of_claims ~ Cancels$Cancelled)
  return(t_test)
}

t_test_no_of_accidents <- function(Cancels) {
  t_test <- t.test(Cancels$number_of_accidents ~ Cancels$Cancelled)
  return(t_test)
}



t_test_months <- function(Cancels) {
  t_test <- t.test(Cancels$Months_Coverage ~ Cancels$Cancelled)
  return(t_test)
}

#T-TEST AS A REUSABLE FUNCTION
#t_test_age <- function(IV, DV) {
 # t_test <- t.test(IV ~ DV)
  #return(t_test)
#}



#CHI-SQAURE AS A REUSABLE FUNCTION
chi_test <- function(ds, column) {
  table <- table(ds[,column], ds$Cancelled)
  chisq_test <- chisq.test(table)
  return(chisq_test) 
}


insurance_main <- function() {
  setwd("C:/Users/Alissa Hayes/Desktop/Springboard")

  require(ggplot2)
  require(tidyverse)
  require(reshape2)

  library(ryouready)
  library(Hmisc)
  library(reshape2)
  
  source("Wrangling.R")
  
#Put in functions from wrangling file  
    Cancels_Read <- read_data()
    Cancels_add_columns <- add_calculated_columns(Cancels_Read) 
    Cancels_Missing <- remove_missing(Cancels_add_columns)
    Cancels_Clean <- final_cleaning(Cancels_Missing)
    
#Start completing EDA functions

    #Result <- plot_categorical_education(Cancels_Clean)
   #Result <- plot_categorical_months (Cancels_Clean)
   #Result <- plot_numeric_months (Cancels_Clean)
    t_test <- t_test_no_of_accidents(Cancels_Clean)
   # Chi <- chi_months (Cancels_Clean)
  #chi_test(Cancels_Clean, "education")
   #return(chi_test)
}


