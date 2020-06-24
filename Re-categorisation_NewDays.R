library(knitr)
library(psych)
library(kableExtra)
library(ggplot2)
library(ggsignif)
library(grid)
library(ComplexHeatmap)
library(gridExtra)
library(EMCluster)
library(limma)
library(dtw)
library(DescTools)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(reshape2)
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

new_cat$MVT_CategoryLevel1 <- gsub("Ã¦r","æ",new_cat$MVT_CategoryLevel1)

new_cat$MVT_CategoryLevel3 <- gsub("Ã¦r","æ",new_cat$MVT_CategoryLevel3)
new_cat$MVT_CategoryLevel4 <- gsub("Ã¦r","æ",new_cat$MVT_CategoryLevel4)


baseline <- baseline[!is.na(baseline$Energy_kcal_per100g),]
baseline <- baseline[!baseline$Energy_kcal_per100g > 3000,]
baseline$Cholesterol <- baseline$Cholesterol/1000

baseline_red <- baseline[,c(1:12,14,17,19:54)]

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

baseline_red[grep("Pizza ", baseline_red$FoodDiettNoName),]$MVT_CategoryLevel2 <- "Pizza, pai, taco o.l."

new_cat <- read.csv("Re-categorisation - Sheet1.csv", header = T)
colnames(new_cat) <- c("MVT_CategoryLevel2","PCA_Categories")
new_cat$MVT_CategoryLevel2 <- gsub("Ã¦","æ",new_cat$MVT_CategoryLevel2)
new_cat$MVT_CategoryLevel2 <- gsub("Ã¸","ø",new_cat$MVT_CategoryLevel2)
new_cat$MVT_CategoryLevel2 <- gsub("Ã¥","å",new_cat$MVT_CategoryLevel2)
baseline_red <- merge(baseline_red,new_cat,by="MVT_CategoryLevel2", all.x = T)

baseline_red_pca <- baseline_red[baseline_red$PCA_Categories != "DELETE",]
baseline_red_pca <- baseline_red[baseline_red$MVT_CategoryLevel2 != "uspesifisert",]

baseline_numeric2 <- baseline_red_pca[,c(51,8,2,17,4)]
baseline_numeric2 <- baseline_numeric2[!baseline_numeric2$PCA_Categories == "DELETE",]
#Now, it is clear we need to separate out our data before PCA, let's do that now
baseline_numeric2.male <- baseline_numeric2[baseline_numeric2$Sex == 2,]
baseline_numeric2.female <- baseline_numeric2[baseline_numeric2$Sex == 1,]

sum(aggregate(Day2~SubjectDNID,baseline_numeric2.female,function(x) length(unique(x)))$Day2)
sum(aggregate(Day2~SubjectDNID,baseline_numeric2.male,function(x) length(unique(x)))$Day2)

aggregate(Day2~SubjectDNID,baseline_numeric2.female,function(x) length(unique(x)))$Day2
aggregate(Day2~SubjectDNID,baseline_numeric2.male,function(x) length(unique(x)))$Day2
length(unique(baseline_numeric2.male$SubjectDNID)$Day2 == 6)



length(unique(paste(baseline$SubjectDNID[baseline$Sex == 1],baseline$Day2[baseline$Sex == 1])))

foodComp.male <- aggregate(. ~ PCA_Categories+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew <- dcast(foodComp.male, formula = Day2+SubjectDNID ~ PCA_Categories, value.var = "Gram", fill = 0)
foodComp.malew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew)
rownames(foodComp.malew) <- foodComp.malew$SubjectDNID
foodComp.malew <- foodComp.malew[,-c(1,2)]

dayCount_male <- aggregate(Day2~SubjectDNID,foodComp.male,FUN = function(x) length(unique(x)))
foodComp.malew <- dayCount_male$Day2 * foodComp.malew / 6

#Let's do the same for women
foodComp.female <- aggregate(. ~ PCA_Categories+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew <- dcast(foodComp.female, formula = Day2+SubjectDNID ~ PCA_Categories, value.var = "Gram", fill = 0)
save_female <- foodComp.femalew
foodComp.femalew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew)
rownames(foodComp.femalew) <- foodComp.femalew$SubjectDNID
foodComp.femalew <- foodComp.femalew[,-c(1,2)]

dayCount_female <- aggregate(Day2~SubjectDNID,foodComp.female,FUN = function(x) length(unique(x)))
foodComp.femalew <- dayCount_female$Day2 * foodComp.femalew / 6

#Sanity Checking
sum(foodComp.malew$Vegetables)
sum(baseline_numeric2.male$Gram[baseline_numeric2.male$PCA_Categories == "Vegetables"], na.rm = T)/6

sum(foodComp.femalew$Vegetables)
sum(baseline_numeric2.female$Gram[baseline_numeric2.female$PCA_Categories == "Vegetables"], na.rm = T)/6

#Okay!
#PCA
food.pca.female <- princomp(foodComp.femalew, cor = T)
food.pca.male <- princomp(foodComp.malew, cor = T)

f1 <- fviz_eig(food.pca.male, ncp = 30, main = "Male")
f2 <- fviz_eig(food.pca.female, ncp = 30, main = "Female")

grid.arrange(f1,f2,ncol=2)

p1 <- fviz_pca_ind(food.pca.male, title = "Male", label = "none")
p2 <- fviz_pca_ind(food.pca.female, title = "Female", label = "none")

grid.arrange(p1,p2,ncol=2)

v1 <- fviz_pca_var(food.pca.male, title = "Male", repel = T)
v2 <- fviz_pca_var(food.pca.female, title = "Female", repel = T)

grid.arrange(v1,v2,ncol=2)


extra.male <- rbind(food.pca.male$sdev[1:8]^2,100*food.pca.male$sdev[1:8]^2/sum(food.pca.male$sdev))
male.tab <- rbind(round(food.pca.male$loadings[,c(1:8)],3),round(extra.male,3))
rownames(male.tab)[c(19,20)] <- c("Eigenvalue","Proportion of Variance")
male.tab[c(1:18),][abs(male.tab[c(1:18),]) < 0.15] <- ""
extra.female <- rbind(food.pca.female$sdev[1:9]^2,100*food.pca.female$sdev[1:9]^2/sum(food.pca.female$sdev))
female.tab <- rbind(round(food.pca.female$loadings[,c(1:9)],3),round(extra.female,3))
rownames(female.tab)[c(19,20)] <- c("Eigenvalue","Proportion of Variance")
female.tab[c(1:18),][abs(female.tab[c(1:18),]) < 0.15] <- ""

as.data.frame(male.tab) %>% gt(rownames_to_stub = T)
as.data.frame(female.tab) %>% gt(rownames_to_stub = T)

#Clustering 
scaled_timewide.male <- scale(foodComp.malew, center = T, scale = T)
scaled_timewide.female <- scale(foodComp.femalew, center = T, scale = T)

set.seed(123)

clus1 <- fviz_nbclust(foodComp.malew, FUNcluster = kmeans, method = "wss") + ggtitle("Optimal Number of Clusters - Male")
clus2 <- fviz_nbclust(foodComp.femalew, FUNcluster = kmeans, method = "wss") + ggtitle("Optimal Number of Clusters - Female")

clus1
clus2

kmeans.male <- kmeans(foodComp.malew, centers = 4, iter.max = 500)
kmeans.female <- kmeans(foodComp.femalew, centers = 4, iter.max = 500)

kmeans.male$size
kmeans.female$size

#save.image("ClusterData.RData")
load("ClusterData.RData")

foodComp_male_cluster <- merge(foodComp.malew,as.data.frame(kmeans.male$cluster),by="row.names")
foodComp_female_cluster <- merge(foodComp.femalew,as.data.frame(kmeans.female$cluster),by="row.names")

rownames(foodComp_male_cluster) <- foodComp_male_cluster$Row.names
rownames(foodComp_female_cluster) <- foodComp_female_cluster$Row.names

foodComp_male_cluster <- as.data.frame(foodComp_male_cluster[,-1])
foodComp_female_cluster <- as.data.frame(foodComp_female_cluster[,-1])

colnames(foodComp_male_cluster)[19] <- "Cluster"
colnames(foodComp_female_cluster)[19] <- "Cluster"

cluster_ident_male <- aggregate(. ~ Cluster, data = foodComp_male_cluster, FUN = mean)
cluster_ident_female <- aggregate(. ~ Cluster, data = foodComp_female_cluster, FUN = mean)

cluster_ident_maleSD <- aggregate(. ~ Cluster, data = foodComp_male_cluster, FUN = sd)
cluster_ident_femaleSD <- aggregate(. ~ Cluster, data = foodComp_female_cluster, FUN = sd)

Heatmap(t(scale(cluster_ident_male[,c(2:19)])), cluster_columns = T, cluster_rows = T, column_labels = paste("Cluster ",cluster_ident_male$Cluster," - " ,kmeans.male$size, " Subjects"
                                                                                                             , sep = ""))
Heatmap(t(scale(cluster_ident_female[,c(2:19)])), cluster_columns = T, cluster_rows = T, column_labels = paste("Cluster ",cluster_ident_female$Cluster," - " ,kmeans.female$size, " Subjects"
                                                                                                               , sep = ""))
cluster_male_melt <- melt(cluster_ident_male, id.vars = "Cluster")
cluster_male_melt$Cluster <- factor(cluster_male_melt$Cluster)

cluster_male_meltSD <- melt(cluster_ident_maleSD, id.vars = "Cluster")
cluster_male_meltSD$Cluster <- factor(cluster_male_meltSD$Cluster)


cluster_female_melt <- melt(cluster_ident_female, id.vars = "Cluster")
cluster_female_melt$Cluster <- factor(cluster_female_melt$Cluster)

cluster_female_meltSD <- melt(cluster_ident_femaleSD, id.vars = "Cluster")
cluster_female_meltSD$Cluster <- factor(cluster_female_meltSD$Cluster)

cluster_male_melt$Cluster <- factor(cluster_male_melt$Cluster, levels = c("3","2","4","1"))
cluster_male_melt$variable <- factor(cluster_male_melt$variable, levels = levels(cluster_male_melt$variable)[c(2,9,17,5,10,3,13,7,18,6,15,8,11,12,14,16,1,4)])

cluster_female_melt$Cluster <- factor(cluster_female_melt$Cluster, levels = c("1","4","2","3"))
cluster_female_melt$variable <- factor(cluster_female_melt$variable, levels = levels(cluster_male_melt$variable))

cluster_male_meltSD$Cluster <- factor(cluster_male_meltSD$Cluster, levels = c("3","2","4","1"))
cluster_male_meltSD$variable <- factor(cluster_male_meltSD$variable, levels = levels(cluster_male_melt$variable))

cluster_female_meltSD$Cluster <- factor(cluster_female_meltSD$Cluster, levels = c("1","4","2","3"))
cluster_female_meltSD$variable <- factor(cluster_female_meltSD$variable, levels = levels(cluster_male_melt$variable))

cluster_female_melt$SD <- cluster_female_meltSD$value
cluster_male_melt$SD <- cluster_male_meltSD$value

cluster_female_melt$ymax <- cluster_female_melt$value + cluster_female_melt$SD
cluster_male_melt$ymax <- cluster_male_melt$value + cluster_male_melt$SD

ggplot(cluster_female_melt, aes(x=Cluster,y=value, fill = Cluster)) + 
  geom_bar(stat = "identity") + geom_errorbar(aes(ymin = value, ymax=ymax), width = .4) +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Female Food Groups - Clustered")

ggplot(cluster_male_melt, aes(x=Cluster,y=value, fill = Cluster)) + 
  geom_bar(stat = "identity") + geom_errorbar(aes(ymin = value, ymax=ymax), width = .4) +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Male Food Groups - Clustered")



foodComp_female_cluster$SubjectDNID <- rownames(foodComp_female_cluster)
foodComp_female_cluster_melt <- melt(foodComp_female_cluster, id.vars = c("SubjectDNID","Cluster"))

foodComp_female_cluster_melt$variable <- factor(foodComp_female_cluster_melt$variable, levels = levels(cluster_male_melt$variable))
foodComp_female_cluster_melt$Cluster <- factor(foodComp_female_cluster_melt$Cluster, levels = c("1","4","2","3"))

ggplot(foodComp_female_cluster_melt, aes(x=,y=value, group = Cluster, colour = Cluster)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Female Food Groups - Clustered")

foodComp_male_cluster$SubjectDNID <- rownames(foodComp_male_cluster)
foodComp_male_cluster_melt <- melt(foodComp_male_cluster, id.vars = c("SubjectDNID","Cluster"))

foodComp_male_cluster_melt$variable <- factor(foodComp_male_cluster_melt$variable, levels = levels(cluster_male_melt$variable))
foodComp_male_cluster_melt$Cluster <- factor(foodComp_male_cluster_melt$Cluster, levels = c("3","2","4","1"))

ggplot(foodComp_male_cluster_melt, aes(x=,y=value, group = Cluster, colour = Cluster)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Male Food Groups - Clustered")


#foodComp_male_cluster <- merge(scaled_timewide.male,as.data.frame(kmeans.male$cluster),by="row.names")
#foodComp_female_cluster <- merge(scaled_timewide.female,as.data.frame(kmeans.female$cluster),by="row.names")

cluster_ident_female %>% gt()
cluster_ident_male %>% gt()

rownames(foodComp_male_cluster) <- foodComp_male_cluster$Row.names
rownames(foodComp_female_cluster) <- foodComp_female_cluster$Row.names

foodComp_male_cluster <- as.data.frame(foodComp_male_cluster[,-1])
foodComp_female_cluster <- as.data.frame(foodComp_female_cluster[,-1])

colnames(foodComp_male_cluster)[19] <- "Cluster"
colnames(foodComp_female_cluster)[19] <- "Cluster"


foodComp_male_cluster <- foodComp_male_cluster[order(foodComp_male_cluster$Cluster),]
Heatmap(foodComp_male_cluster[,c(1:18)], cluster_columns = F, cluster_rows = F, split = foodComp_male_cluster$Cluster)
Heatmap(scale(cluster_ident_male[,c(2:19)]), cluster_columns = T, cluster_rows = T, row_labels = paste("Cluster ",cluster_ident_male$Cluster," - " ,kmeans.male$size, " Subjects", sep = ""))




baseline_numeric2 <- baseline_red_pca[,c(51,8,2,17,4,21)]
baseline_numeric2 <- baseline_numeric2[!baseline_numeric2$PCA_Categories == "DELETE",]
baseline_numeric2.male <- baseline_numeric2[baseline_numeric2$Sex == 2,]
baseline_numeric2.female <- baseline_numeric2[baseline_numeric2$Sex == 1,]


female_data <- as.data.frame(apply(foodComp.femalew,2,mean))
colnames(female_data) <- "Mean (g)"
female_data$SD <- apply(foodComp.femalew,2,sd)
foodComp.female2 <- aggregate(. ~ PCA_Categories+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew2 <- dcast(foodComp.female2, formula = Day2+SubjectDNID ~ PCA_Categories, value.var = "Energy_kcal", fill = 0)
foodComp.femalew2 <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew2)
rownames(foodComp.femalew2) <- foodComp.femalew2$SubjectDNID
foodComp.femalew2 <- foodComp.femalew2[,-c(1,2)]
foodComp.femalew2$Total <- rowSums(foodComp.femalew2)
foodComp.femalew2 <- 100*foodComp.femalew2/foodComp.femalew2$Total
female_data$`Energy_Intake (f)` <- colMeans(foodComp.femalew2)[-19]

male_data <- as.data.frame(apply(foodComp.malew,2,mean))
colnames(male_data) <- "Mean (g)"
male_data$SD <- apply(foodComp.malew,2,sd)
foodComp.male2 <- aggregate(. ~ PCA_Categories+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew2 <- dcast(foodComp.male2, formula = Day2+SubjectDNID ~ PCA_Categories, value.var = "Energy_kcal", fill = 0)
foodComp.malew2 <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew2)
rownames(foodComp.malew2) <- foodComp.malew2$SubjectDNID
foodComp.malew2 <- foodComp.malew2[,-c(1,2)]
foodComp.malew2$Total <- rowSums(foodComp.malew2)
foodComp.malew2 <- 100*foodComp.malew2/foodComp.malew2$Total
male_data$`Energy_Intake (f)` <- colMeans(foodComp.malew2)[-19]

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

table_data$MVT_Cat2 <- gsub("\\(g\\)","",table_data$MVT_Cat2)


table_data <- table_data[order(table_data$MVT_Cat2),]

colnames(table_data)[2] <- paste(colnames(table_data)[3], " (grams)", sep = "")
colnames(table_data)[5] <- paste(colnames(table_data)[5], " (grams)", sep = "")

colnames(table_data)[1] <- "Food Category"


table_data <- table_data[order(table_data$`% Calorie Intake (M) (grams)`, decreasing = T),]
colnames(table_data)[4] <- "Mean +/- SD (F)"
table_data %>% gt() %>% cols_align(align = "center")

table(baseline_red_pca[baseline_red_pca$PCA_Categories == "Bread and Cereal Products",]$FoodDiettNoName)

#Add reccomended vs actual pie-charts - dashed line for Rest vs PUFA vs SFA - DONE
#Take out the wrong food 400kcal/gram  - DONE
#Bar graphs for cluster means - Done
#Swap fiber and added sugar - Done
#Separate summary into 2 things - Done
#Biometrics and VAT/SAT - Nutrition - Done
#Summary graph for each cluster - 


ct_data <- read.csv("CT_Data.csv")

ct_data <- ct_data[ct_data$Time == 0,]
ct_data <- ct_data[,c(1,4,5,6)]
master <- read.csv("Master sheet.csv")

master$SubjectDNID <- gsub("df","",gsub("cf","",master$DiettNoID))
master <- merge(master,ct_data,by="SubjectID")

#Now, itis clear we need to separate out our data before PCA, let's do that now
baseline_red2 <- baseline_red
baseline_red2[,c(12:42)][is.na(baseline_red2[,c(12:42)])] <- 0
baseline_numeric <- baseline_red2[,c(2:5,8,16:41)]
baseline_numeric$SubjectDNID <- as.character(baseline_numeric$SubjectDNID)

subject_by_Day2 <- aggregate(. ~ SubjectDNID + Day2, FUN = sum, data = baseline_numeric)
subject_by_Day2 <- subject_by_Day2[,c(1:6,8:31)]
daily.average <- aggregate(.  ~ SubjectDNID, FUN = mean, data = subject_by_Day2)



daily.average.add <- merge(daily.average,master,by="SubjectDNID")
daily.average.male <- daily.average.add[daily.average.add$Sex2 == "M",]
daily.average.female <- daily.average.add[daily.average.add$Sex2 == "F",]

m_clust <- as.data.frame(kmeans.male$cluster)
m_clust$SubjectDNID <- rownames(m_clust)
colnames(m_clust)[1] <- "Cluster"
f_clust <- as.data.frame(kmeans.female$cluster)
f_clust$SubjectDNID <- rownames(f_clust)
colnames(f_clust)[1] <- "Cluster"


daily.average.male <- merge(daily.average.male,m_clust,by="SubjectDNID")
daily.average.female <- merge(daily.average.female,f_clust,by="SubjectDNID")

daily.average.summary.male <- daily.average.male[,c(1,10,12,14,16,18,20,22,24,26,28,30,36,40,46,47,48,49)]
daily.average.summary.female <- daily.average.female[,c(1,10,12,14,16,18,20,22,24,26,28,30,36,40,46,47,48,49)]

daily.average.summary.male <- melt(daily.average.summary.male, id.vars = c("SubjectDNID","Cluster"))
daily.average.summary.female <- melt(daily.average.summary.female, id.vars = c("SubjectDNID","Cluster"))

daily.average.summary.female$Cluster <- as.factor(daily.average.summary.female$Cluster)
daily.average.summary.male$Cluster <- as.factor(daily.average.summary.male$Cluster)

daily.average.summary.male$variable <- factor(daily.average.summary.male$variable, 
                                              levels = levels(daily.average.summary.male$variable)[c(1,2,5,7,3,4,9,10,8,11,6,12,13,14,15,16)])

ggplot(daily.average.summary.male,aes(x=Cluster,y=value,group=Cluster,colour=Cluster)) + geom_boxplot() + facet_wrap(~variable, scales="free_y")

daily.average.summary.female$variable <- factor(daily.average.summary.female$variable, 
                                              levels = levels(daily.average.summary.male$variable))


ggplot(daily.average.summary.female,aes(x=Cluster,y=value,group=Cluster,colour=Cluster)) + geom_boxplot() + facet_wrap(~variable, scales="free_y")

unique_vars <- unique(daily.average.summary.female$variable)

tukey_mat_f <- matrix(ncol=6,nrow=length(unique_vars))
rownames(tukey_mat_f) <- unique_vars

daily.average.summary.female$Cluster <- factor(daily.average.summary.female$Cluster)
daily.average.summary.female$value

for(i in c(1:length(unique_vars))) {
  
  tukey_mat_f[i,] <- TukeyHSD(aov(value~Cluster,daily.average.summary.female[daily.average.summary.female$variable==unique_vars[i],]))$Cluster[,4]
  
}

colnames(tukey_mat_f) <- c("C2 v C1", "C3 v C1", "C4 v C1", "C3 v C2", "C4 v C2", "C4 v C3")

as.data.frame(round(tukey_mat_f,3)) %>% gt(rownames_to_stub = T)

tukey_mat_m <- matrix(ncol=6,nrow=length(unique_vars))
rownames(tukey_mat_m) <- unique_vars

daily.average.summary.male$Cluster <- as.factor(daily.average.summary.male$Cluster)

for(i in c(1:length(unique_vars))) {
  
  tukey_mat_m[i,] <- TukeyHSD(aov(value~Cluster,daily.average.summary.male[daily.average.summary.male$variable==unique_vars[i],]))$Cluster[,4]
  
}

colnames(tukey_mat_m) <- c("C2 v C1", "C3 v C1", "C4 v C1", "C3 v C2", "C4 v C2", "C4 v C3")

as.data.frame(round(tukey_mat_m,3)) %>% gt(rownames_to_stub = T)


clust_nut_means_f <- aggregate(value~variable+Cluster,daily.average.summary.female,mean)
colnames(clust_nut_means_f)[3] <- "Mean"

clust_nut_means_f <- clust_nut_means_f[order(clust_nut_means_f$variable),]
clust_nut_means_f$Mean <- round(clust_nut_means_f$Mean,2)
clust_nut_means_f %>% gt()


clust_nut_means_m <- aggregate(value~variable+Cluster,daily.average.summary.male,mean)
colnames(clust_nut_means_m)[3] <- "Mean"

clust_nut_means_m <- clust_nut_means_m[order(clust_nut_means_m$variable),]
clust_nut_means_m$Mean <- round(clust_nut_means_m$Mean,2)
clust_nut_means_m %>% gt()

dcast(clust_nut_means_f, formula = variable~Cluster)

#Female
TukeyHSD(aov(value~Cluster,daily.average.summary.female[daily.average.summary.female$variable==unique_vars[1],]))$Cluster[,4]

TukeyHSD(aov(value~Cluster,daily.average.summary.female[daily.average.summary.female$variable=="Carbohydrate",]))$Cluster


daily.average.female[daily.average.female$Cluster == 4,]

test <- baseline[baseline$SubjectDNID == "6154",]




# Dairy and Carbs separated out...

baseline_numeric2 <- baseline_red_pca[,c(1,8,2,17,4)]
baseline_numeric2.male <- baseline_numeric2[baseline_numeric2$Sex == 2,]
baseline_numeric2.female <- baseline_numeric2[baseline_numeric2$Sex == 1,]


foodComp.male <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew <- dcast(foodComp.male, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
foodComp.malew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew)
rownames(foodComp.malew) <- foodComp.malew$SubjectDNID
foodComp.malew <- foodComp.malew[,-c(1,2)]

dayCount_male <- aggregate(Day2~SubjectDNID,foodComp.male,FUN = function(x) length(unique(x)))
foodComp.malew <- dayCount_male$Day2 * foodComp.malew / 6

#Let's do the same for women
foodComp.female <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew <- dcast(foodComp.female, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
save_female <- foodComp.femalew
foodComp.femalew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew)
rownames(foodComp.femalew) <- foodComp.femalew$SubjectDNID
foodComp.femalew <- foodComp.femalew[,-c(1,2)]

dayCount_female <- aggregate(Day2~SubjectDNID,foodComp.female,FUN = function(x) length(unique(x)))
foodComp.femalew <- dayCount_female$Day2 * foodComp.femalew / 6

foodComp_male_cluster <- merge(foodComp.malew,as.data.frame(kmeans.male$cluster),by="row.names")
foodComp_female_cluster <- merge(foodComp.femalew,as.data.frame(kmeans.female$cluster),by="row.names")

rownames(foodComp_male_cluster) <- foodComp_male_cluster$Row.names
rownames(foodComp_female_cluster) <- foodComp_female_cluster$Row.names

foodComp_male_cluster <- as.data.frame(foodComp_male_cluster[,-1])
foodComp_female_cluster <- as.data.frame(foodComp_female_cluster[,-1])

colnames(foodComp_male_cluster)[45] <- "Cluster"
colnames(foodComp_female_cluster)[45] <- "Cluster"

foodComp_female_cluster <- foodComp_female_cluster[,c(22,25,44,12,4,5,18,19,45)]
foodComp_male_cluster <- foodComp_male_cluster[,c(22,25,44,12,4,5,18,19,45)]

colnames(foodComp_female_cluster) <- c("Margarine and Butter","Milk and Milk-based Drinks","Yoghurt","Cream and etc.","Homemade Bread","Industry-made Bread","Knekkebrod","Cereal","Cluster")
colnames(foodComp_male_cluster) <- c("Margarine and Butter","Milk and Milk-based Drinks","Yoghurt","Cream and etc.","Homemade Bread","Industry-made Bread","Knekkebrod","Cereal","Cluster")

cluster_ident_male <- aggregate(. ~ Cluster, data = foodComp_male_cluster, FUN = mean)
cluster_ident_female <- aggregate(. ~ Cluster, data = foodComp_female_cluster, FUN = mean)

cluster_ident_maleSD <- aggregate(. ~ Cluster, data = foodComp_male_cluster, FUN = sd)
cluster_ident_femaleSD <- aggregate(. ~ Cluster, data = foodComp_female_cluster, FUN = sd)

cluster_male_melt <- melt(cluster_ident_male, id.vars = "Cluster")
cluster_male_melt$Cluster <- factor(cluster_male_melt$Cluster)

cluster_male_meltSD <- melt(cluster_ident_maleSD, id.vars = "Cluster")
cluster_male_meltSD$Cluster <- factor(cluster_male_meltSD$Cluster)


cluster_female_melt <- melt(cluster_ident_female, id.vars = "Cluster")
cluster_female_melt$Cluster <- factor(cluster_female_melt$Cluster)

cluster_female_meltSD <- melt(cluster_ident_femaleSD, id.vars = "Cluster")
cluster_female_meltSD$Cluster <- factor(cluster_female_meltSD$Cluster)

cluster_male_melt$Cluster <- factor(cluster_male_melt$Cluster, levels = c("3","2","4","1"))
levels(cluster_male_melt$variable)
cluster_male_melt$variable <- factor(cluster_male_melt$variable, levels = levels(cluster_male_melt$variable)[c(2,1,3,4,5,6,7,8)])

cluster_female_melt$Cluster <- factor(cluster_female_melt$Cluster, levels = c("1","4","2","3"))
cluster_female_melt$variable <- factor(cluster_female_melt$variable, levels = levels(cluster_male_melt$variable))

cluster_male_meltSD$Cluster <- factor(cluster_male_meltSD$Cluster, levels = c("3","2","4","1"))
cluster_male_meltSD$variable <- factor(cluster_male_meltSD$variable, levels = levels(cluster_male_melt$variable))

cluster_female_meltSD$Cluster <- factor(cluster_female_meltSD$Cluster, levels = c("1","4","2","3"))
cluster_female_meltSD$variable <- factor(cluster_female_meltSD$variable, levels = levels(cluster_male_melt$variable))

cluster_female_melt$SD <- cluster_female_meltSD$value
cluster_male_melt$SD <- cluster_male_meltSD$value

cluster_female_melt$ymax <- cluster_female_melt$value + cluster_female_melt$SD
cluster_male_melt$ymax <- cluster_male_melt$value + cluster_male_melt$SD

ggplot(cluster_female_melt, aes(x=Cluster,y=value, fill = Cluster)) + 
  geom_bar(stat = "identity") + geom_errorbar(aes(ymin = value, ymax=ymax), width = .4) +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Female Food Groups - Split")

ggplot(cluster_male_melt, aes(x=Cluster,y=value, fill = Cluster)) + 
  geom_bar(stat = "identity") + geom_errorbar(aes(ymin = value, ymax=ymax), width = .4) +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Male Food Groups - Clustered")



foodComp_female_cluster$SubjectDNID <- rownames(foodComp_female_cluster)
foodComp_female_cluster_melt <- melt(foodComp_female_cluster, id.vars = c("SubjectDNID","Cluster"))

foodComp_female_cluster_melt$variable <- factor(foodComp_female_cluster_melt$variable, levels = levels(cluster_male_melt$variable))
foodComp_female_cluster_melt$Cluster <- factor(foodComp_female_cluster_melt$Cluster, levels = c("1","4","2","3"))

ggplot(foodComp_female_cluster_melt, aes(x=,y=value, group = Cluster, colour = Cluster)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Female Food Groups - Clustered")

foodComp_male_cluster$SubjectDNID <- rownames(foodComp_male_cluster)
foodComp_male_cluster_melt <- melt(foodComp_male_cluster, id.vars = c("SubjectDNID","Cluster"))

foodComp_male_cluster_melt$variable <- factor(foodComp_male_cluster_melt$variable, levels = levels(cluster_male_melt$variable))
foodComp_male_cluster_melt$Cluster <- factor(foodComp_male_cluster_melt$Cluster, levels = c("3","2","4","1"))

ggplot(foodComp_male_cluster_melt, aes(x=,y=value, group = Cluster, colour = Cluster)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 6) + ggtitle("Male Food Groups - Clustered")

#Find wine and beer and go again LOL

baseline_numeric2 <- baseline_red_pca[,c(13,1,8,2,17,4)]

baseline_numeric2[grepl("Vin|Hvitvin|Rødvin|Brennevin",baseline_numeric2$FoodDiettNoName) & baseline_numeric2$MVT_CategoryLevel2 == "Alkoholholdige drikkevarer",]$MVT_CategoryLevel2 <- "Wine"

baseline_numeric2[grepl("Pils|Carlsberg|Pilsner|Ã~l|Fatøl|San Miguel|Hveteøl|Cider|Grevens|Clausthaler|Lettøl|",baseline_numeric2$FoodDiettNoName) & baseline_numeric2$MVT_CategoryLevel2 == "Alkoholholdige drikkevarer",]$MVT_CategoryLevel2 <- "Beer"

baseline_numeric2 <- baseline_numeric2[baseline_numeric2$MVT_CategoryLevel2 %in% c("Wine","Beer"),]
baseline_numeric2 <- baseline_numeric2[,-1]

baseline_numeric2.male <- baseline_numeric2[baseline_numeric2$Sex == 2,]
baseline_numeric2.female <- baseline_numeric2[baseline_numeric2$Sex == 1,]

foodComp.male <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.male)
foodComp.malew <- dcast(foodComp.male, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
foodComp.malew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.malew)
rownames(foodComp.malew) <- foodComp.malew$SubjectDNID
foodComp.malew <- foodComp.malew[,-c(1,2)]

dayCount_male <- aggregate(Day2~SubjectDNID,foodComp.male,FUN = function(x) length(unique(x)))
foodComp.malew <- dayCount_male$Day2 * foodComp.malew / 6

#Let's do the same for women
foodComp.female <- aggregate(. ~ MVT_CategoryLevel2+SubjectDNID+Day2, FUN = sum, data = baseline_numeric2.female)
foodComp.femalew <- dcast(foodComp.female, formula = Day2+SubjectDNID ~ MVT_CategoryLevel2, value.var = "Gram", fill = 0)
save_female <- foodComp.femalew
foodComp.femalew <- aggregate(. ~ SubjectDNID, FUN = mean, data = foodComp.femalew)
rownames(foodComp.femalew) <- foodComp.femalew$SubjectDNID
foodComp.femalew <- foodComp.femalew[,-c(1,2)]

dayCount_female <- aggregate(Day2~SubjectDNID,foodComp.female,FUN = function(x) length(unique(x)))
foodComp.femalew <- dayCount_female$Day2 * foodComp.femalew / 6

foodComp_male_cluster <- merge(foodComp.malew,as.data.frame(kmeans.male$cluster),by="row.names")
foodComp_female_cluster <- merge(foodComp.femalew,as.data.frame(kmeans.female$cluster),by="row.names")

rownames(foodComp_male_cluster) <- foodComp_male_cluster$Row.names
rownames(foodComp_female_cluster) <- foodComp_female_cluster$Row.names

foodComp_male_cluster <- as.data.frame(foodComp_male_cluster[,-1])
foodComp_female_cluster <- as.data.frame(foodComp_female_cluster[,-1])

colnames(foodComp_male_cluster)[3] <- "Cluster"
colnames(foodComp_female_cluster)[3] <- "Cluster"

foodComp_male_cluster$Sex <- "M"
foodComp_female_cluster$Sex <- "F"

foodComp_alcohol <- rbind(foodComp_male_cluster,foodComp_female_cluster)

foodComp_alcohol <- melt(foodComp_alcohol, id.vars = c("Sex","Cluster"))

foodComp_alcohol$Cluster_Sex <- paste(foodComp_alcohol$Sex,foodComp_alcohol$Cluster, sep = "_")

foodComp_alcohol$Cluster_Sex <- factor(foodComp_alcohol$Cluster_Sex)
levels(foodComp_alcohol$Cluster_Sex)

foodComp_alcohol$Cluster_Sex <- factor(foodComp_alcohol$Cluster_Sex, levels = levels(foodComp_alcohol$Cluster_Sex)[c(1,4,2,3,7,6,8,5)])

foodComp_alcohol_agg <- aggregate(value ~ variable+Cluster_Sex+Sex, foodComp_alcohol, mean)

ggplot(foodComp_alcohol, aes(x=Cluster_Sex,y=value,colour=Cluster_Sex)) + geom_boxplot() + facet_wrap(~variable, scales = "free_y")

ggplot(foodComp_alcohol_agg, aes(x=Cluster_Sex, y=value,fill=Cluster_Sex)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~variable, scales = "free_y") + geom_boxplot(aes(x=Cluster_Sex,y=value,group=Sex,alpha = 0.0005))




table(baseline[baseline$SubjectDNID %in% daily.average.male$SubjectDNID[daily.average.male$Cluster == 4] & 
           baseline$CategoryLevel1 == "Alkohol",]$FoodDishName)

table(baseline[baseline$SubjectDNID %in% daily.average.male$SubjectDNID[daily.average.male$Cluster == 2] & 
                 baseline$CategoryLevel1 == "Alkohol",]$FoodDishName)

