#Deliverable 1
library(dplyr)

mechacar_mpg_data <- read.csv('MechaCar_mpg.csv')
head(mechacar_mpg_data)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_mpg_data)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_mpg_data))

#Deliverable 2
suspension_coil_table <- read.csv('Suspension_Coil.csv')
total_summary <- suspension_coil_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')
lot_summary <- suspension_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

#Deliverable 3
t.test(suspension_coil_table$PSI,mu=1500)

t.test(subset(suspension_coil_table,Manufacturing_Lot=='Lot1')$PSI,mu=1500)
t.test(subset(suspension_coil_table,Manufacturing_Lot=='Lot2')$PSI,mu=1500)
t.test(subset(suspension_coil_table,Manufacturing_Lot=='Lot3')$PSI,mu=1500)
