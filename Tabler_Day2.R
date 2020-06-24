library(knitr)
library(psych)
library(kableExtra)
library(ggplot2)
library(ggsignif)
library(grid)
library(ComplexHeatmap)
library(gridExtra)
library(limma)
library(dtw)
library(scater)
library(DescTools)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(reshape2)
library(gt)
library("ggpubr")


setwd("C://Users//laure//Documents//CarbFunc/")


baseline <- read.csv("df_dpid_baseline.csv")

baseline$FoodDiettNoName <- gsub("Ã¸","ø",baseline$FoodDiettNoName)
baseline$FoodDiettNoName <- gsub("Ã¥","å",baseline$FoodDiettNoName)
baseline$FoodDishName <- gsub("Ã¸","ø",baseline$FoodDishName)
baseline$FoodDishName <- gsub("Ã¥","å",baseline$FoodDishName)
baseline$NutrientsSource <- gsub("Ã¸","ø",baseline$NutrientsSource)
baseline$NutrientsSource <- gsub("Ã¥","å",baseline$NutrientsSource)

baseline$MVT_CategoryLevel1 <- gsub("Ã¸","ø",baseline$MVT_CategoryLevel1)
baseline$MVT_CategoryLevel1 <- gsub("Ã¥","å",baseline$MVT_CategoryLevel1)
baseline$MVT_CategoryLevel1 <- gsub("kjKj","Kj",baseline$MVT_CategoryLevel1)
baseline$MVT_CategoryLevel1 <- gsub("Melk og melkeprodukter","Melk / melkeprodukter",baseline$MVT_CategoryLevel1)

baseline$MVT_CategoryLevel2 <- gsub("Ã¸","ø",baseline$MVT_CategoryLevel2)
baseline$MVT_CategoryLevel2 <- gsub("Ã¥","å",baseline$MVT_CategoryLevel2)
baseline$MVT_CategoryLevel2 <- gsub("korn","Korn",baseline$MVT_CategoryLevel2)

baseline$MVT_CategoryLevel3 <- gsub("Ã¸","ø",baseline$MVT_CategoryLevel3)
baseline$MVT_CategoryLevel3 <- gsub("Ã¥","å",baseline$MVT_CategoryLevel3)

baseline$MVT_CategoryLevel4 <- gsub("Ã¸","ø",baseline$MVT_CategoryLevel4)
baseline$MVT_CategoryLevel4 <- gsub("Ã¥","å",baseline$MVT_CategoryLevel4)

baseline$MVT_CategoryLevel1 <- gsub("Ã¦r","æ",baseline$MVT_CategoryLevel1)
baseline$MVT_CategoryLevel2 <- gsub("Ã¦r","æ",baseline$MVT_CategoryLevel2)
baseline$MVT_CategoryLevel3 <- gsub("Ã¦r","æ",baseline$MVT_CategoryLevel3)
baseline$MVT_CategoryLevel4 <- gsub("Ã¦r","æ",baseline$MVT_CategoryLevel4)

baseline <- baseline[!is.na(baseline$Energy_kcal_per100g),]
baseline <- baseline[!baseline$Energy_kcal_per100g > 3000,]

baseline$Cholesterol <- baseline$Cholesterol/1000

baseline_red <- baseline[,c(1:12,14,17,19:54)]

baseline_red$Time <- as.numeric(gsub(":","",baseline_red$Time))/100

master <- read.csv("Master sheet.csv")

master$SubjectDNID <- gsub("df","",gsub("cf","",master$DiettNoID))
master$SubjectDNID <- gsub("cf","",gsub("cf","",master$SubjectDNID))


#Replace "NAs" in each dietary column with 0s
baseline_red[,c(12:42)][is.na(baseline_red[,c(12:42)])] <- 0

#Remove all rows which contain "Vann"
baseline_red <- baseline_red[!baseline_red$CategoryLevel2 == "Vann",]

baseline_numeric <- baseline_red[,c(1:5,7,16:40)]

baseline_numeric$SubjectDNID <- as.character(baseline_numeric$SubjectDNID)

baseline_red[baseline_red$FoodDiettNoName == "Te, svart",]$MVT_CategoryLevel2 <- "Vann, kaffe, te"
baseline_red[baseline_red$FoodDiettNoName == "Søtningstabletter, Natreen",]$MVT_CategoryLevel2 <- "Sukker, honning og søte pålegg"
baseline_red[baseline_red$FoodDiettNoName == "Druer, Frukt",]$MVT_CategoryLevel2 <- "Frukt og bæ"
baseline_red[baseline_red$FoodDiettNoName == "Havrebrød Bakers Håndverk 750 g, Bakers",]$MVT_CategoryLevel2 <- "Brødvarer, industribakt"
baseline_red[baseline_red$FoodDiettNoName == "Kvikk Lunsj (47 g), Freia",]$MVT_CategoryLevel2 <- "Sjokolade og andre søtsaker"
baseline_red[baseline_red$FoodDiettNoName == "Kyllingfilet naturell pålegg, Prior",]$MVT_CategoryLevel2 <- "Diverse kjøttprodukter"
baseline_red[baseline_red$FoodDiettNoName == "Hjertesalat",]$MVT_CategoryLevel2 <- "Grønnsaker"
baseline_red[baseline_red$FoodDiettNoName == "Kornmoknekkebrød Havre & Solsikke, SÃ¦tre",]$MVT_CategoryLevel2 <- "Knekkebrød, smørbrødkjeks o.l."
baseline_red[baseline_red$FoodDiettNoName == "Risgrøt 1000 g, Fjordland",]$MVT_CategoryLevel2 <- "Grøt"
baseline_red[baseline_red$FoodDiettNoName == "Røkelaks i skiver, Lofoten",]$MVT_CategoryLevel2 <- "Fisk og fiskeprodukter"
baseline_red[baseline_red$FoodDiettNoName == "Skinkeost Tube, Kavli",]$MVT_CategoryLevel2 <- "Ost"
baseline_red[baseline_red$FoodDiettNoName == "Eplejuice 1.5 l, 100% juice, Eldorado",]$MVT_CategoryLevel2 <- "Juice, saft, brus o.l."
baseline_red[baseline_red$FoodDiettNoName == "Te, svart, tilberedt",]$MVT_CategoryLevel2 <- "Vann, kaffe, te"
baseline_red[baseline_red$FoodDiettNoName == "Energibrød, Mesterbakeren",]$MVT_CategoryLevel2 <- "Brødvarer, industribakt"
baseline_red[baseline_red$FoodDiettNoName == "Fun Light Appelsin, 1l, Stabburet",]$MVT_CategoryLevel2 <- "Juice, saft, brus o.l."
baseline_red[baseline_red$FoodDiettNoName == "Pepsi Max",]$MVT_CategoryLevel2 <- "Juice, saft, brus o.l."
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Alkohol",]$MVT_CategoryLevel2 <- "Alkoholholdige drikkevarer"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "BÃ¦r / bÃ¦rprodukter",]$MVT_CategoryLevel2 <- "Frukt og bæ"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Belgfrukter / belgfruktprodukter",]$MVT_CategoryLevel2 <- "Belgfrukter"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel2 == "Te",]$MVT_CategoryLevel2 <- "Vann, kaffe, te"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel2 == "Kaffe",]$MVT_CategoryLevel2 <- "Vann, kaffe, te"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel2 == "SÃ¸te drikker, med sukkererstatninger",]$MVT_CategoryLevel2 <- "Juice, saft, brus o.l."
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Egg / eggeprodukter",]$MVT_CategoryLevel2 <- "Egg, tilberedt"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Fisk / fiskeprodukter",]$MVT_CategoryLevel2 <- "Fisk og fiskeprodukter"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Frukt / fruktprodukter",]$MVT_CategoryLevel2 <- "Frukt og bæ"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "GrÃ¸nnsaker / grÃ¸nnsakprodukter",]$MVT_CategoryLevel2 <- "Grønnsaker"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Kakao / kakaoprodukter",]$MVT_CategoryLevel2 <- "Sjokolade og andre søtsaker"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "KjÃ¸tt / kjÃ¸ttprodukter",]$MVT_CategoryLevel2 <- "Diverse kjøttprodukter"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "kjKjÃ¸tt / kjÃ¸ttprodukter",]$MVT_CategoryLevel2 <- "Diverse kjøttprodukter"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Korn / kornprodukter",]$MVT_CategoryLevel2 <- "Brødvarer, industribakt"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Melk / melkeprodukter",]$MVT_CategoryLevel2 <- "Melk og melkebasert drikke"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "NÃ¸tter / frÃ¸ / kjerner",]$MVT_CategoryLevel2 <- "Nøtter, mandler og frø"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Poteter / potetprodukter",]$MVT_CategoryLevel2 <- "Poteter"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Sukker / sÃ¸te produkter",]$MVT_CategoryLevel2 <- "Sukker, honning og søte pålegg"
baseline_red[baseline_red$MVT_CategoryLevel2 == "uspesifisert" & 
               baseline_red$CategoryLevel1 == "Urter / krydder / smakstilsetninger",]$MVT_CategoryLevel2 <- "Urter og krydder"
baseline_red[baseline_red$MVT_CategoryLevel2 == "Svin",]$MVT_CategoryLevel2 <- "Lam, storfe, svin, tilberedt"
baseline_red[baseline_red$MVT_CategoryLevel2 == "Storfe, kalv",]$MVT_CategoryLevel2 <- "Lam, storfe, svin, tilberedt"
baseline_red[baseline_red$MVT_CategoryLevel2 == "Lam, får",]$MVT_CategoryLevel2 <- "Lam, storfe, svin, tilberedt"

baseline_red[baseline_red$MVT_CategoryLevel2 == "Egg, rå",]$MVT_CategoryLevel2 <- "Egg"
baseline_red[baseline_red$MVT_CategoryLevel2 == "Egg, tilberedt",]$MVT_CategoryLevel2 <- "Egg"

baseline_numeric2 <- baseline_red[,c(1:5,7,16:40,47)]

ct_data <- read.csv("CT_Data.csv")
ct_data <- ct_data[ct_data$Time == 0,]
ct_data <- ct_data[,c(1,4,5,6)]

master <- merge(master,ct_data,by="SubjectID",all.x=T)
master$SubjectDNID <- as.numeric(master$SubjectDNID)

#Now, itis clear we need to separate out our data before PCA, let's do that now
baseline_numeric <- baseline_red[,c(1:5,7,16:40)]

baseline_numeric$SubjectDNID <- as.character(baseline_numeric$SubjectDNID)
subject_by_Day2 <- aggregate(. ~ SubjectDNID + Day2, FUN = sum, data = baseline_numeric, na.action = NULL)
subject_by_Day2 <- subject_by_Day2[,c(1:6,8:31)]
daily.average <- aggregate(.  ~ SubjectDNID, FUN = mean, data = subject_by_Day2)

daily.average.add <- merge(daily.average,master,by="SubjectDNID")

out <- daily.average.add[,c(1,8,10,31:43,46,47,48)]

write.csv(x = out, file = "Energy_kcal_Info.csv")



out[out$BMR > 1.55*out$Energy_kcal,]

t <- sum(baseline[baseline$SubjectDNID == "1473",])

daily.average.male <- daily.average.add[daily.average.add$Sex2 == "M",]
daily.average.female <- daily.average.add[daily.average.add$Sex2 == "F",]

matrix <- matrix(nrow=14,ncol=2)
colnames(matrix) <- c("Male","Female")
matrix[2,1] <- paste(round(mean(daily.average.male$Age),0),round(sd(daily.average.male$Age),1),sep=" +/- ")
matrix[2,2] <- paste(round(mean(daily.average.female$Age),0),round(sd(daily.average.female$Age),1),sep=" +/- ")

matrix[3,1] <- paste(round(mean(daily.average.male$BMI),0),round(sd(daily.average.male$BMI),1),sep=" +/- ")
matrix[3,2] <- paste(round(mean(daily.average.female$BMI),0),round(sd(daily.average.female$BMI),1),sep=" +/- ")

matrix[1,1] <- length(daily.average.male$SubjectDNID)
matrix[1,2] <- length(daily.average.female$SubjectDNID)

matrix[5,1] <- paste(round(mean(daily.average.male$Protein),0),round(sd(daily.average.male$Protein),1),sep=" +/- ")
matrix[5,2] <- paste(round(mean(daily.average.female$Protein),0),round(sd(daily.average.female$Protein),1),sep=" +/- ")

matrix[7,1] <- paste(round(mean(daily.average.male$Fat),0),round(sd(daily.average.male$Fat),1),sep=" +/- ")
matrix[7,2] <- paste(round(mean(daily.average.female$Fat),0),round(sd(daily.average.female$Fat),1),sep=" +/- ")

matrix[6,1] <- paste(round(mean(daily.average.male$Carbohydrate),0),round(sd(daily.average.male$Carbohydrate),1),sep=" +/- ")
matrix[6,2] <- paste(round(mean(daily.average.female$Carbohydrate),0),round(sd(daily.average.female$Carbohydrate),1),sep=" +/- ")

matrix[8,1] <- paste(round(mean(daily.average.male$MUFA),0),round(sd(daily.average.male$MUFA),1),sep=" +/- ")
matrix[8,2] <- paste(round(mean(daily.average.female$MUFA),0),round(sd(daily.average.female$MUFA),1),sep=" +/- ")

matrix[9,1] <- paste(round(mean(daily.average.male$PUFA),0),round(sd(daily.average.male$PUFA),1),sep=" +/- ")
matrix[9,2] <- paste(round(mean(daily.average.female$PUFA),0),round(sd(daily.average.female$PUFA),1),sep=" +/- ")

matrix[10,1] <- paste(round(mean(daily.average.male$SFA),2),round(sd(daily.average.male$SFA),1),sep=" +/- ")
matrix[10,2] <- paste(round(mean(daily.average.female$SFA),2),round(sd(daily.average.female$SFA),1),sep=" +/- ")

matrix[4,1] <- paste(round(mean(daily.average.male$Energy_kcal),0),round(sd(daily.average.male$Energy_kcal),0),sep=" +/- ")
matrix[4,2] <- paste(round(mean(daily.average.female$Energy_kcal),0),round(sd(daily.average.female$Energy_kcal),0),sep=" +/- ")

matrix[11,1] <- paste(round(mean(daily.average.male$Alcohol),0),round(sd(daily.average.male$Alcohol),1),sep=" +/- ")
matrix[11,2] <- paste(round(mean(daily.average.female$Alcohol),0),round(sd(daily.average.female$Alcohol),1),sep=" +/- ")

matrix[12,1] <- paste(round(mean(daily.average.male$SugarAdded),0),round(sd(daily.average.male$SugarAdded),1),sep=" +/- ")
matrix[12,2] <- paste(round(mean(daily.average.female$SugarAdded),0),round(sd(daily.average.female$SugarAdded),1),sep=" +/- ")

matrix[13,1] <- paste(round(mean(daily.average.male$Cholesterol),2),round(sd(daily.average.male$Cholesterol),1),sep=" +/- ")
matrix[13,2] <- paste(round(mean(daily.average.female$Cholesterol),2),round(sd(daily.average.female$Cholesterol),1),sep=" +/- ")

matrix[14,1] <- paste(round(mean(daily.average.male$Fiber),2),round(sd(daily.average.male$Fiber),1),sep=" +/- ")
matrix[14,2] <- paste(round(mean(daily.average.female$Fiber),2),round(sd(daily.average.female$Fiber),1),sep=" +/- ")


rownames(matrix) <- c("# Participants","Age","BMI","Energy (kcal)","Protein (g)","Carbohydrate (g)","Fat (g)","MUFA (g)","PUFA (g)","SFA (g)","Alcohol (g)","Added Sugar (g)","Cholesterol (g)","Fiber (g)")

matrix <- matrix[c(1,2,3,4,5,6,12,14,7,8,9,10,13,11),]

rownames(matrix)[6] <- "Carbohydrate (Excl. Fiber) (g)"

as.data.frame(matrix) %>% gt(rownames_to_stub = T) %>%  
  tab_header(title = "Summary Statistics (M and F)")

matrix <- as.data.frame(matrix)

matrix$energy_M <- gsub(" .*","",matrix$Male)
matrix[c(5,6,8,9,14),]$energy_M <- as.numeric(matrix[c(5,6,8,9,14),]$energy_M)

matrix[5,3] <- 4*as.numeric(matrix[5,3])
matrix[6,3] <- 4*as.numeric(matrix[6,3])
matrix[8,3] <- 2*as.numeric(matrix[8,3])
matrix[9,3] <- 9*as.numeric(matrix[9,3])
matrix[14,3] <- 7*as.numeric(matrix[14,3])

matrix$energy_M[c(1,2,3,4,7,10,11,12,13)] <- ""

matrix$energy_F <- gsub(" .*","",matrix$Female)
matrix[c(5,6,8,9,14),]$energy_F <- as.numeric(matrix[c(5,6,8,9,14),]$energy_F)

matrix[5,4] <- 4*as.numeric(matrix[5,4])
matrix[6,4] <- 4*as.numeric(matrix[6,4])
matrix[8,4] <- 2*as.numeric(matrix[8,4])
matrix[9,4] <- 9*as.numeric(matrix[9,4])
matrix[14,4] <- 7*as.numeric(matrix[14,4])

matrix$energy_F[c(1,2,3,4,7,10,11,12,13)] <- ""

matrix <- matrix[,c(1,3,2,4)]

colnames(matrix)[2] <- "Energy (kcal, M)"
colnames(matrix)[4] <- "Energy (kcal, F)"


matrix$`Energy (kcal, M)`[5] <- paste(matrix$`Energy (kcal, M)`[5], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, M)`[5])/5081.64,2), "%)", sep = "")
matrix$`Energy (kcal, M)`[6] <- paste(matrix$`Energy (kcal, M)`[6], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, M)`[6])/5081.64,2), "%)", sep = "")
matrix$`Energy (kcal, M)`[8] <- paste(matrix$`Energy (kcal, M)`[8], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, M)`[8])/5081.64,2), "%)", sep = "")
matrix$`Energy (kcal, M)`[9] <- paste(matrix$`Energy (kcal, M)`[9], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, M)`[9])/5081.64,2), "%)", sep = "")
matrix$`Energy (kcal, M)`[14] <- paste(matrix$`Energy (kcal, M)`[14], " (",
                                       round(100*as.numeric(matrix$`Energy (kcal, M)`[14])/5081.64,2), "%)", sep = "")


matrix$`Energy (kcal, F)`[5] <- paste(matrix$`Energy (kcal, F)`[5], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, F)`[5])/4245.46,2), "%)", sep = "")
matrix$`Energy (kcal, F)`[6] <- paste(matrix$`Energy (kcal, F)`[6], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, F)`[6])/4245.46,2), "%)", sep = "")
matrix$`Energy (kcal, F)`[8] <- paste(matrix$`Energy (kcal, F)`[8], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, F)`[8])/4245.46,2), "%)", sep = "")
matrix$`Energy (kcal, F)`[9] <- paste(matrix$`Energy (kcal, F)`[9], " (",
                                      round(100*as.numeric(matrix$`Energy (kcal, F)`[9])/4245.46,2), "%)", sep = "")
matrix$`Energy (kcal, F)`[14] <- paste(matrix$`Energy (kcal, F)`[14], " (",
                                       round(100*as.numeric(matrix$`Energy (kcal, F)`[14])/4245.46,2), "%)", sep = "")

as.data.frame(matrix[c(4,5,6,8,7,9:14),]) %>% gt(rownames_to_stub = T) %>%  
  tab_header(title = "Summary Statistics (M and F)") %>% cols_align(align = "center")


matrix2 <- matrix(nrow=9,ncol=2)
colnames(matrix2) <- c("Male","Female")

matrix2[1,1] <- length(daily.average.male$SubjectDNID)
matrix2[1,2] <- length(daily.average.female$SubjectDNID)

matrix2[2,1] <- paste(round(mean(daily.average.male$Age),0),round(sd(daily.average.male$Age),1),sep=" +/- ")
matrix2[2,2] <- paste(round(mean(daily.average.female$Age),0),round(sd(daily.average.female$Age),1),sep=" +/- ")

matrix2[3,1] <- paste(round(mean(daily.average.male$Height),2),round(sd(daily.average.male$Height),2),sep=" +/- ")
matrix2[3,2] <- paste(round(mean(daily.average.female$Height),2),round(sd(daily.average.female$Height),2),sep=" +/- ")

matrix2[4,1] <- paste(round(mean(daily.average.male$Weight),0),round(sd(daily.average.male$Weight),1),sep=" +/- ")
matrix2[4,2] <- paste(round(mean(daily.average.female$Weight),0),round(sd(daily.average.female$Weight),1),sep=" +/- ")

matrix2[5,1] <- paste(round(mean(daily.average.male$BMI),1),round(sd(daily.average.male$BMI),1),sep=" +/- ")
matrix2[5,2] <- paste(round(mean(daily.average.female$BMI),1),round(sd(daily.average.female$BMI),1),sep=" +/- ")

matrix2[6,1] <- paste(round(mean(daily.average.male$AntVATcm.3, na.rm = T),0),round(sd(daily.average.male$AntVATcm.3, na.rm = T),0),sep=" +/- ")
matrix2[6,2] <- paste(round(mean(daily.average.female$AntVATcm.3, na.rm = T),0),round(sd(daily.average.female$AntVATcm.3, na.rm = T),0),sep=" +/- ")

matrix2[7,1] <- paste(round(mean(daily.average.male$AntSATcm.3, na.rm = T),0),round(sd(daily.average.male$AntSATcm.3, na.rm = T),0),sep=" +/- ")
matrix2[7,2] <- paste(round(mean(daily.average.female$AntSATcm.3, na.rm = T),0),round(sd(daily.average.female$AntSATcm.3, na.rm = T),0),sep=" +/- ")

matrix2[8,1] <- paste(round(mean(daily.average.male$AntVATSATtotalcm.3, na.rm = T),0),round(sd(daily.average.male$AntVATSATtotalcm.3, na.rm = T),0),sep=" +/- ")
matrix2[8,2] <- paste(round(mean(daily.average.female$AntVATSATtotalcm.3, na.rm = T),0),round(sd(daily.average.female$AntVATSATtotalcm.3, na.rm = T),0),sep=" +/- ")

matrix2[9,1] <- paste(round(mean(daily.average.male$BMR),0),round(sd(daily.average.male$BMR),1),sep=" +/- ")
matrix2[9,2] <- paste(round(mean(daily.average.female$BMR),0),round(sd(daily.average.female$BMR),1),sep=" +/- ")

rownames(matrix2) <- c("# of participants", "Age", "Height (m)", "Weight (kg)", "BMI", "VAT (cm^3)", "SAT (cm^3)", "Total Fat (cm^3)", "BMR")

as.data.frame(matrix2) %>% gt(rownames_to_stub = T) %>%  
  tab_header(title = "Biometric Summary Statistics (M and F)") %>% cols_align(align = "center")




#Pie charts
114*4
247*4
37*4
116*9
sum(c(456,(988-148),148,1044))
14*9
42.74*9
male_energy <- c(456,(988-148),148,(1044-126-385),126,385)
male_energy <- 100*male_energy/sum(male_energy)
male_energy
21.42+5.06+15.47
33.76+5.95
sum(c(356,(864-156),156,837))
12*9
34.04*9
female_energy <- c(356,(864-156),156,(837-108-306),108,306)
female_energy <- 100*female_energy/sum(female_energy)
female_energy
34.42+7.58
20.56+5.25+14.88

names(male_energy) <- c("Protein","Carbohydrates","Added Sugars","Total Fat","PUFA","SFA")
names(female_energy) <- c("Protein","Carbohydrates","Added Sugars","Total Fat","PUFA","SFA")

par(mfrow=c(1,2))

n <- 2
p1 <- pie(male_energy, main = "Male Intake", radius = n*0.2552, col = c("brown1","goldenrod1","floralwhite","navy"))
p2 <- pie(female_energy, main = "Female Intake", radius = n*0.2117, col = c("brown1","goldenrod1","floralwhite","navy"))



baseline_numeric2.male <- baseline_numeric2[baseline_numeric2$Sex == "2",]
baseline_numeric2.female <- baseline_numeric2[baseline_numeric2$Sex == "1",]

#Let's make that table
foodComp.male <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew <- dcast(foodComp.male, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
save_male <- foodComp.malew
foodComp.malew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew)
rownames(foodComp.malew) <- foodComp.malew$SubjectDNID
foodComp.malew <- foodComp.malew[,-c(1,2)]

#Let's do the same for women
foodComp.female <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew <- dcast(foodComp.female, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
save_fem <- foodComp.femalew
foodComp.femalew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew)
rownames(foodComp.femalew) <- foodComp.femalew$SubjectDNID
foodComp.femalew <- foodComp.femalew[,-c(1,2)]

#SANITY CHECK
sum(foodComp.femalew$`Alkoholholdige drikkevarer`)
sum(baseline_numeric2.female[baseline_numeric2.female$MVT_CategoryLevel2 == "Alkoholholdige drikkevarer",]$Gram)/6
#Makes sense
#Remove extras
foodComp.malew <- foodComp.malew[,-c(40,24,36,42)]
foodComp.femalew <- foodComp.femalew[,-c(40,24,36,42)]

food.pca.female <- princomp(foodComp.femalew, cor = T)
food.pca.male <- princomp(foodComp.malew, cor = T)

f1 <- fviz_eig(food.pca.male, ncp = 30, main = "Male")
f2 <- fviz_eig(food.pca.female, ncp = 30, main = "Female")

grid.arrange(f1,f2,ncol=2)

p1 <- fviz_pca_ind(food.pca.male, title = "Male")
p2 <- fviz_pca_ind(food.pca.female, title = "Female")

grid.arrange(p1,p2,ncol=2)

extra.male <- rbind(food.pca.male$sdev[1:10]^2,100*food.pca.male$sdev[1:10]^2/sum(food.pca.male$sdev))
male.tab <- rbind(round(food.pca.male$loadings[,c(1:10)],3),round(extra.male,3))
rownames(male.tab)[c(42,43)] <- c("Eigenvalue","Proportion of Variance")
male.tab[c(1:41),][abs(male.tab[c(1:41),]) < 0.2] <- ""
extra.female <- rbind(food.pca.female$sdev[1:10]^2,100*food.pca.female$sdev[1:10]^2/sum(food.pca.female$sdev))
female.tab <- rbind(round(food.pca.female$loadings[,c(1:10)],3),round(extra.female,3))
rownames(female.tab)[c(42,43)] <- c("Eigenvalue","Proportion of Variance")
female.tab[c(1:41),][abs(female.tab[c(1:41),]) < 0.2] <- ""

as.data.frame(male.tab) %>% gt(rownames_to_stub = T)
as.data.frame(female.tab) %>% gt(rownames_to_stub = T)


scaled_timewide.male <- scale(foodComp.malew, center = T, scale = T)
scaled_timewide.female <- scale(foodComp.femalew, center = T, scale = T)

set.seed(123)

clus1 <- fviz_nbclust(foodComp.malew, FUNcluster = kmeans, method = "wss") + ggtitle("Optimal Number of Clusters - Male")

clus2 <- fviz_nbclust(foodComp.femalew, FUNcluster = kmeans, method = "wss") + ggtitle("Optimal Number of Clusters - Female")

clus1
clus2

kmeans.male <- kmeans(foodComp.malew, centers = 4, iter.max = 500)
kmeans.female <- kmeans(foodComp.femalew, centers = 3, iter.max = 500)

cluster_identities.male <- aggregate(foodComp.malew, by=list(cluster=kmeans.male$cluster), mean)
cluster_identities.female <- aggregate(foodComp.femalew, by=list(cluster=kmeans.female$cluster), mean)

kmeans.male$size
kmeans.female$size

Heatmap(t(scale(cluster_identities.male)[,c(2:42)]), 
        column_labels = paste(cluster_identities.male$cluster," - " ,kmeans.male$size, " Subjects"
                              , sep = ""), column_names_rot = 45, column_title = "Male")

Heatmap(t(scale(cluster_identities.female)[,c(2:42)]), 
        column_labels = paste(cluster_identities.female$cluster," - " ,kmeans.female$size, " Subjects"
                              , sep = ""), column_names_rot = 45, column_title = "Female")

save.image("Cluster.RData")


as.data.frame(kmeans.female$centers) %>% gt(rownames_to_stub = T) %>% cols_align(align = "center")
as.data.frame(kmeans.male$centers) %>% gt(rownames_to_stub = T) %>% cols_align(align = "center")

cluster.male <- as.data.frame(kmeans.male$cluster)
cluster.male$SubjectDNID <- as.numeric(rownames(cluster.male))
baseline_cluster.male <- merge(cluster.male, baseline_numeric2.male,by="SubjectDNID")

cluster.female <- as.data.frame(kmeans.female$cluster)
cluster.female$SubjectDNID <- as.numeric(rownames(cluster.female))
baseline_cluster.female <- merge(cluster.female, baseline_numeric2.female,by="SubjectDNID")

colnames(baseline_cluster.male)[2] <- "Cluster"
colnames(baseline_cluster.female)[2] <- "Cluster"

cluster_nut.female <- aggregate(.~Cluster+SubjectDNID+Day2,data = baseline_cluster.female[,c(1:32)],FUN=sum)
cluster_nut.female <- aggregate(.~Cluster+SubjectDNID,data = cluster_nut.female,FUN=mean)

cluster.test <- aggregate(.~Cluster+SubjectDNID,data = baseline_cluster.female[,c(1:32)],FUN=mean)

for(i in c(12,14,16,18,20,22,24,26,28,30,32)) {
  nam <- paste("g", i, sep = "")
  assign(nam, ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,12],group=Cluster)) + 
           geom_boxplot() + 
           theme_bw() + 
           ylab(colnames(cluster_nut.female[i])))
}

g12 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,12],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[12]))
g14 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,14],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[14]))
g16 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,16],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[16]))
g18 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,18],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[18]))
g20 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,20],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[20]))
g22 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,22],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[22]))
g24 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,24],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[24]))
g26 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,26],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[26]))
g28 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,28],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[28]))
g30 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,30],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[30]))
g32 <- ggplot(cluster_nut.female,aes(x=Cluster,y=cluster_nut.female[,32],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.female[32]))

grid.arrange(g12,g14,g16,g18,g20,g22,g24,g26,g28,g30,g32,ncol=3)

cluster_nut.male <- aggregate(.~Cluster+SubjectDNID,data = baseline_cluster.male[,c(1:32)],FUN=mean)

g12 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,12],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[12]))
g14 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,14],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[14]))
g16 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,16],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[16]))
g18 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,18],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[18]))
g20 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,20],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[20]))
g22 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,22],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[22]))
g24 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,24],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[24]))
g26 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,26],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[26]))
g28 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,28],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[28]))
g30 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,30],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[30]))
g32 <- ggplot(cluster_nut.male,aes(x=Cluster,y=cluster_nut.male[,32],group=Cluster)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(colnames(cluster_nut.male[32]))

grid.arrange(g12,g14,g16,g18,g20,g22,g24,g26,g28,g30,g32,ncol=3)







foodComp.female <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew <- dcast(foodComp.female, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
foodComp.femalew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew)
rownames(foodComp.femalew) <- foodComp.femalew$SubjectDNID
foodComp.femalew <- foodComp.femalew[,-c(1,2)]
female_data <- as.data.frame(apply(foodComp.femalew,2,mean))
colnames(female_data) <- "Mean (g)"
female_data$SD <- apply(foodComp.femalew,2,sd)
foodComp.female2 <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew2 <- dcast(foodComp.female2, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Energy_kcal", fill = 0)
foodComp.femalew2 <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew2)
rownames(foodComp.femalew2) <- foodComp.femalew2$SubjectDNID
foodComp.femalew2 <- foodComp.femalew2[,-c(1,2)]
foodComp.femalew2$Total <- rowSums(foodComp.femalew2)
foodComp.femalew2 <- 100*foodComp.femalew2/foodComp.femalew2$Total
female_data$`Energy_Intake (f)` <- colMeans(foodComp.femalew2)[-46]


foodComp.male <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew <- dcast(foodComp.male, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
foodComp.malew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew)
rownames(foodComp.malew) <- foodComp.malew$SubjectDNID
foodComp.malew <- foodComp.malew[,-c(1,2)]
male_data <- as.data.frame(apply(foodComp.malew,2,mean))
colnames(male_data) <- "Mean (g)"
male_data$SD <- apply(foodComp.malew,2,sd)
foodComp.male2 <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew2 <- dcast(foodComp.male2, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Energy_kcal", fill = 0)
foodComp.malew2 <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew2)
rownames(foodComp.malew2) <- foodComp.malew2$SubjectDNID
foodComp.malew2 <- foodComp.malew2[,-c(1,2)]
foodComp.malew2$Total <- rowSums(foodComp.malew2)
foodComp.malew2 <- 100*foodComp.malew2/foodComp.malew2$Total
male_data$`Energy_Intake (f)` <- colMeans(foodComp.malew2)[-46]

male_data$`Mean (g)` <- round(male_data$`Mean (g)`,0)
male_data$SD <- round(male_data$SD,0)
male_data$`Energy_Intake (f)` <- round(male_data$`Energy_Intake (f)`,2)

female_data$`Mean (g)` <- round(female_data$`Mean (g)`,0)
female_data$SD <- round(female_data$SD,0)
female_data$`Energy_Intake (f)` <- round(female_data$`Energy_Intake (f)`,2)

colnames(male_data) <- c("Mean (M)","SD (M)","% Calorie Intake (M)")
colnames(female_data) <- c("Mean (F)","SD (F)","% Calorie Intake (F)")

male_data$`Mean +/- SD (M)` <- paste(male_data$`Mean (M)`,male_data$`SD (M)`,sep=" +/- ")
female_data$`Mean +/- SD (M)` <- paste(female_data$`Mean (F)`,female_data$`SD (F)`,sep=" +/- ")

rownames(male_data) <- paste(rownames(male_data), "(g)", sep = " ")
rownames(female_data) <- paste(rownames(female_data), "(g)", sep = " ")

male_data$MVT_Cat2 <- rownames(male_data)
female_data$MVT_Cat2 <- rownames(female_data)

table_data <- cbind(male_data,female_data)
table_data <- table_data[,c(5,4,3,9,8)]

cat_mat <- as.data.frame(cbind(baseline_red$MVT_CategoryLevel1,baseline_red$MVT_CategoryLevel2))
colnames(cat_mat) <- c("MVT_Cat1","MVT_Cat2")
cat_mat <- cat_mat[!duplicated(cat_mat$MVT_Cat2),]
cat_mat$MVT_Cat2 <- paste(cat_mat$MVT_Cat2, "(g)", sep = " ")

table_data <- merge(table_data,cat_mat,by="MVT_Cat2")

table_data <- table_data[,c(6,1,2,3,4,5)]

table_data$MVT_Cat2 <- gsub("\\(g\\)","",table_data$MVT_Cat2)
table_data[8,]$MVT_Cat1 <- "Fjørfe og kjøtt"
table_data[13,]$MVT_Cat1 <- "Poteter, grønnsaker, frukt og bæ"
table_data[18,]$MVT_Cat1 <- "Korn- og bakevarer, frø og nøtter"
table_data[25,]$MVT_Cat1 <- "Melk / melkeprodukter"

table_data <- table_data[order(table_data$MVT_Cat1),]
aggregate(`% Calorie Intake (F)`~MVT_Cat1,table_data,sum)

table_data[duplicated(table_data$MVT_Cat1),]$MVT_Cat1 <- ""
rownames(table_data) <- c(1:length(table_data$MVT_Cat1))
table_data[2,]$MVT_Cat1 <- "M - 9.77% | F - 8.01%"
table_data[10,]$MVT_Cat1 <- "M - 4.91% | F - 4.84%"
table_data <- rbind(rbind(table_data[c(1:12),],""),table_data[c(13:45),])
rownames(table_data) <- c(1:length(table_data$MVT_Cat1))
table_data[13,]$MVT_Cat1 <- "M - 2.55% | F - 2.97%"
table_data[15,]$MVT_Cat1 <- "M - 3.52% | F - 2.66%"
table_data[18,]$MVT_Cat1 <- "M - 13.60% | F - 11.34%"
table_data[22,]$MVT_Cat1 <- "M - 24.53% | F - 26.24%"
table_data[32,]$MVT_Cat1 <- "M - 5.16% | F - 5.36%"
table_data[36,]$MVT_Cat1 <- "M - 10.19% | F - 9.81%%"
table_data[40,]$MVT_Cat1 <- "M - 5.10% | F - 6.33%"
table_data[45,]$MVT_Cat1 <- "M - 5.35% | F - 8.26%"
table_data <- rbind(table_data,"")
table_data[47,]$MVT_Cat1 <- "M - 15.34% | F - 14.17%"


table_data[table_data == "uspesifisert"] <- "Unspecified"
table_data[table_data == "uspesifisert (g)"] <- "Unspecified (g)"

matlist <- as.data.frame(matrix(nrow=45))

for(i in c(1:45)) {
  
  rownames(matlist)[i] <- paste(colnames(foodComp.femalew2)[i])
  matlist[i,] <- t.test(foodComp.femalew2[,i],foodComp.malew2[,i])$p.value
  
}

matlist$p.adjust <- p.adjust(matlist$V1)
table_data$MVT_Cat2
table_data$`% Calorie Intake (F)`[4] <- paste(table_data$`% Calorie Intake (F)`[4], "*", sep = " ")
table_data$`% Calorie Intake (F)`[17] <- paste(table_data$`% Calorie Intake (F)`[17], "*", sep = " ")
table_data$`% Calorie Intake (F)`[11] <- paste(table_data$`% Calorie Intake (F)`[11], "*", sep = " ")

colnames(table_data)[3] <- paste(colnames(table_data)[3], " (grams)", sep = "")
colnames(table_data)[5] <- paste(colnames(table_data)[5], " (grams)", sep = "")

table_data[c(1:8),c(2,3,4,5,6)] <- table_data[c(1:8),c(2,3,4,5,6)][order(table_data[c(1:8),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(9:11),c(2,3,4,5,6)] <- table_data[c(9:11),c(2,3,4,5,6)][order(table_data[c(9:11),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(14:16),c(2,3,4,5,6)] <- table_data[c(14:16),c(2,3,4,5,6)][order(table_data[c(14:16),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(17:20),c(2,3,4,5,6)] <- table_data[c(17:20),c(2,3,4,5,6)][order(table_data[c(17:20),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(21:30),c(2,3,4,5,6)] <- table_data[c(21:30),c(2,3,4,5,6)][order(as.numeric(table_data[c(21:30),]$`% Calorie Intake (M)`), decreasing = T),]
table_data[c(31:34),c(2,3,4,5,6)] <- table_data[c(31:34),c(2,3,4,5,6)][order(table_data[c(31:34),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(35:38),c(2,3,4,5,6)] <- table_data[c(35:38),c(2,3,4,5,6)][order(table_data[c(35:38),]$`% Calorie Intake (M)`, decreasing = T),]
table_data[c(39:43),c(2,3,4,5,6)] <- table_data[c(39:43),c(2,3,4,5,6)][order(table_data[c(39:43),]$`% Calorie Intake (M)`, decreasing = T),]

table_data %>% gt() %>% cols_align(align = "center")

matlist <- as.data.frame(matrix(nrow=45))

#Add dashed line for SFA and PUFA - DONE
#Add SFA and PUFA - DONE
#Move reccomend energy intake  - DONE
#Add kcal and kjoules underneath piecharts - DONE
#Change reccomended pie chart to reccomended reference table - DONE

#Change everything to box-whisker for cluster graphs = Done
#Reorganise clusters to be ordered by energy intake - Done
#Bars with SD - Done
#Rearrange always to be in a certain order - Done

#Separate Milk and dairy into sub-categories and re-graph clusters - DONE
#Separate alcohol into wine and beer - DONE
#Separate carbohydrates - DONE

#Correlate drinks vs food and see if it's the same in male and females  

#Dotplot of meal pattern analysis - X=Meals, Y=FoodGroups, Dots=Frequency of Eating

#Monday and Wednesday