# Explore cleaned ALSPAC dataset

setwd("/Users/aes/OneDrive - University of Edinburgh/PhD/Studies/ALSPAC_inflam_episodes/Data_prep/")

# Read in librarys and clear working envrironment
source("Scripts/startup.R")

# ----------------------------------------
# Read in QC'd wide data:
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

# ----------------------------------------
dataSubQCLong <- gather(data, age_cat, age, c(age_t01:age_t11, PLE_11, PLE_13, PLE_14, PLE_24, PLE_26), factor_key=TRUE)
dataSubQCLong <- dataSubQCLong[order(dataSubQCLong$Subject, dataSubQCLong$age_cat),]
data <- dataSubQCLong
# ----------------------------------------
#Set the factors
data$age_cat <- factor(data$age_cat)
# ----------------------------------------
# Table of mean ages at each time point
# PLE at age 17/18 at clinic and CCX are practically the same time point so just use CCX

order <- c("age_t01","PLE_11","age_t02",  "PLE_13", "age_t03", "PLE_14",
           "age_t04","age_t05", "age_t06", "age_t07",
           "age_t08", "age_t09", "PLE_24" ,"age_t10",  "PLE_26", "age_t11")


ageRanges <- lapply(order, function(ageCat){
  data %>%
    filter(age_cat == ageCat) %>%
    summarise(minAge = round(min(age, na.rm = T) , 2),
              maxAge = round(max(age, na.rm = T), 2),
              meanAge = round(mean(age, na.rm = T), 2))
}) %>% do.call(rbind, .)

ageRanges$Appointment_Time_Point <- c(1:nrow(ageRanges))
ageRanges <- select(ageRanges, c(4,1:3))
ageRanges
write.csv(ageRanges, "Output/Age_at_appointment.csv", row.names = FALSE, quote = FALSE)
# ----------------------------------------
# Violin plots of ages at each appointment time point for depression and PLE time points
# change order of x-axis labels
data$age_cat <- factor(data$age_cat, levels=order)

# colours for PLE or depression time points
data <- data %>%
  mutate(Mean = ifelse(age_cat %in% c("PLE_11", "PLE_13", "PLE_14","PLE_18", "PLE_24", "PLE_26"), "PE only" ,
                  ifelse(age_cat %in% c("age_t02", "age_t04", "age_t05", "age_t07"), "PE and Depression" , "Depression Only") )) 

p1 <- ggplot(data, aes(x=age_cat, y=age)) + 
  geom_violin() +
  stat_summary(fun = "mean", 
               geom = "point", 
               size =2, 
               aes(color = Mean)) +
  scale_colour_manual(values = c("PE and Depression" = "mediumorchid4", "Depression Only" = "royalblue3", "PE only" = "indianred2")) +
  xlab("Appointment Time Point") +
  ylab("Age (years)") +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle=45, hjust=1))+ 
  scale_x_discrete(labels=1:16) +
  guides(fill=guide_legend(title=" Mean "))
p1


png("Output/ViolinPlot_age.png", height = 15, width = 18, units = "cm", res = 200)
print(p1)
dev.off()

# ----------------------------------------
# Stacked barplot of number of people with/without PLEs at each appointment time point
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")
dataLong <- gather(data, PLE, PLE_value, paste0("ple_t", str_pad(1:9, pad = 0,width = 2 , "left")),
                   factor_key=TRUE, na.rm = T)



png("Output/Stacked_plot_PLE.png", height = 15, width = 18, units = "cm", res = 200)
 p2 <- ggplot(dataLong, aes(x=PLE, fill = PLE_value)) + 
    geom_bar(position="stack", stat="count") +
    scale_x_discrete(labels = c(2,3,4,6,7,8,10,13,15))+
    xlab("Appointment Time Point")+
    ylab("Number of Participants")+
    scale_fill_manual(values = c("TRUE" = "indianred2", "FALSE" = "lightcyan3"))+
    guides(fill=guide_legend(title="Participant\nExperienced\nPE"))
 print(p2)
dev.off()

# ----------------------------------------
# Stacked barplot of number of people with/without depression episodes (ie. scores > 11) at each appointment time point
data <- data %>%
  mutate("1" = ifelse(mfq_t01 > 11, TRUE, ifelse(mfq_t01 <= 11, FALSE, NA))) %>%
  mutate("3" = ifelse(mfq_t02 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("5" = ifelse(mfq_t03 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("7" = ifelse(mfq_t04 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("8" = ifelse(mfq_t05 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("9" = ifelse(mfq_t06 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("10" = ifelse(mfq_t07 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("11" = ifelse(mfq_t08 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("12" = ifelse(mfq_t09 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("14" = ifelse(mfq_t10 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA))) %>%
  mutate("16" = ifelse(mfq_t11 > 11, TRUE, ifelse(mfq_t02 <= 11, FALSE, NA)))

dataLong <- gather(data, Dep, Dep_value, c("1", "3", "5", "7", "8", "9", "10", "11", "12", "14", "16"), factor_key=TRUE, na.rm = T)
head(dataLong)

png("Output/Stacked_plot_dep.png", height = 15, width = 18, units = "cm", res = 200)
p3 <- ggplot(dataLong, aes(x=Dep, fill = Dep_value)) + 
    geom_bar(position="stack", stat="count") +
    xlab("Appointment Time Point")+
    ylab("Number of Participants")+
    scale_fill_manual(values = c("TRUE" = "royalblue3", "FALSE" = "lightcyan3"))+
    guides(fill=guide_legend(title="Participant\nExperienced\nDepressive Episode"))
print(p3)
dev.off()

# ----------------------------------------
# Histograms for total number of PLEs or depressive episodes
data <- read.csv("/Volumes/ALSPAC/users/amelia/Data/dataSubQCWide.csv")

png("Output/Hist_PLE.png", height = 15, width = 18, units = "cm", res = 200)
p4 <- ggplot(data, aes(PLE_total)) + 
  geom_bar(stat= "count", fill = "indianred2") +
  scale_x_discrete(limits=c(0:7),
                   label=c("0","1","2","3","4","5","6","7")) +
  xlab("Total Number of PEs")+
  ylab("Number of Participants")
print(p4)
dev.off()

png("Output/Hist_dep.png", height = 15, width = 18, units = "cm", res = 200)
p5 <- ggplot(data, aes(dep_episodes)) + 
  geom_bar(stat= "count", fill = "royalblue3") +
  scale_x_discrete(limits=c(0:10),
                   label=c("0","1","2","3","4","5","6","7", "8", "9", "10")) +
  xlab("Total Number of Depressive Episodes")+
  ylab("Number of Participants")
print(p5)
dev.off()

# ----------------------------------------
# Combine figures into pannel
png("Output/Hist_and_stacked_barplot.png", height = 25, width = 25, units = "cm", res = 200)
ggarrange(p2,p3,p4,p5, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
dev.off()

ggarrange(p2,p3,p4,p5, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
# ----------------------------------------


