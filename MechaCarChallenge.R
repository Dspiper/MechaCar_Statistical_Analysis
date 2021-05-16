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

lot1_table <- suspension_coil_table %>% subset(Manufacturing_Lot=='Lot1')
t.test(lot1_table$PSI,mu=1500)

lot2_table <- suspension_coil_table %>% subset(Manufacturing_Lot=='Lot2')
t.test(lot2_table$PSI,mu=1500)

lot3_table <- suspension_coil_table %>% subset(Manufacturing_Lot=='Lot3')
t.test(lot3_table$PSI,mu=1500)