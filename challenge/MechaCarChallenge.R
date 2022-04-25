library(dplyr)
MechaCar_MPG_table <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #import and read in files as a DF
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_MPG_table)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_MPG_table))
Suspension_Coil_table <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #import and read in files as a DF
total_summary <- Suspension_Coil_table %>% group_by() %>% summarize(Mean=mean(PSI),Median=median(PSI),Varience=var(PSI),SD=sd(PSI))
lot_summary <- Suspension_Coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Varience=var(PSI),SD=sd(PSI))
Sample_coil_table <- Suspension_Coil_table %>% sample_n(50)
t.test(log10(Sample_coil_table$PSI),mu=mean(log10(Suspension_Coil_table$PSI)))
> t.test(log10(Sample_coil_table$PSI,subset(Suspension_Coil_table,Manufacturing_Lot == 'lot1')),mu=mean(log10(Suspension_Coil_table$PSI)))