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
library(DescTools)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(reshape2)
library("ggpubr")

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

baseline[baseline$FoodDishName == "Kaffe, traktet, kokt",]$Energy_kcal <- baseline[baseline$FoodDishName == "Kaffe, traktet, kokt",]$Energy_kcal * 0.4

baseline$Energy_kJ <- 4.184*baseline$Energy_kcal

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

baseline_red$Time <- as.numeric(gsub(":","",baseline_red$Time))/100

master <- read.csv("Master sheet.csv")

#And create a matching column by removing "df" and "cf"
master$SubjectDNID <- gsub("df","",gsub("cf","",master$DiettNoID))

#Replace "NAs" in each dietary column with 0s

baseline_red[,c(12:42)][is.na(baseline_red[,c(12:42)])] <- 0

#Remove all rows which contain "Vann"
baseline_red <- baseline_red[!baseline_red$CategoryLevel2 == "Vann",]

baseline_red$SubjectDNID

time_tester <- aggregate(Time ~ SubjectDNID, baseline_red, sum)
baseline_red <- baseline_red[baseline_red$SubjectDNID %in% time_tester$SubjectDNID,]

uniques <- unique(baseline_red[c("SubjectDNID", "Meal","Time","Day2")])

#Exclude combinations where Time is NA
uniques <- uniques[!is.na(uniques$Time),]

#Find those indices of Baseline_red where is.na(Time) is true
na.Time <- which(is.na(baseline_red$Time))

#Select the combinations of SubjectID, Day2 and Meal along where is.na(Time) is true
na.otherId <- baseline_red[na.Time,c(1,6,9)]

#Match those unique rows which do have associated times with the indices of those rows without time and replace the original dataframe information based on where is.na(time) is true
baseline_red$Time[na.Time] <- uniques$Time[match(apply(na.otherId,1,paste,collapse=""),
                                                 apply(uniques[,c(1,4,2)],1,paste,collapse=""))]


time_tester2 <- aggregate(Time ~ SubjectDNID, baseline_red, sum, na.action = na.pass)
time_tester3 <- aggregate(Time ~ SubjectDNID + Day2, baseline_red, sum, na.action = na.pass)
time_remover <- time_tester3[!is.na(time_tester3$Time),]

table(paste(baseline_red$SubjectDNID,baseline_red$Day2) %in% paste(time_remover$SubjectDNID,time_remover$Day2))

baseline_red <- baseline_red[paste(baseline_red$SubjectDNID,baseline_red$Day2) %in% paste(time_remover$SubjectDNID,time_remover$Day2),]

time_numeric <- baseline_red[,c(1,7,9,10,16:40)]

new_cat <- read.csv("Re-categorisation - Sheet1.csv", header = T)
colnames(new_cat) <- c("MVT_CategoryLevel2","PCA_Categories")
new_cat$MVT_CategoryLevel2 <- gsub("Ã¦","æ",new_cat$MVT_CategoryLevel2)
new_cat$MVT_CategoryLevel2 <- gsub("Ã¸","ø",new_cat$MVT_CategoryLevel2)
new_cat$MVT_CategoryLevel2 <- gsub("Ã¥","å",new_cat$MVT_CategoryLevel2)
baseline_red2 <- merge(baseline_red,new_cat,by="MVT_CategoryLevel2", all.x = T)

baseline_red_pca <- baseline_red2[!baseline_red2$MVT_CategoryLevel2 == "uspesifisert",]
baseline_red_pca <- baseline_red_pca[!baseline_red_pca$PCA_Categories == "DELETE",]


#Sum up all foods which were eaten at the same time
time_agg <- aggregate(. ~ SubjectDNID+Day2+Meal+Time, time_numeric, sum)

#Okay! So we have Day2s separated still, let's graph all three Day2s for a patient, energy vs time:
graph_test <- time_agg[time_agg$SubjectDNID == "7522",]
ggplot(graph_test, aes(x=Time,y=Energy_kcal)) + geom_point() + geom_smooth() + facet_wrap(~Day2)

graph_test <- time_agg[time_agg$SubjectDNID == "1103",]
ggplot(graph_test, aes(x=Time,y=Energy_kcal)) + geom_point() + geom_smooth() + facet_wrap(~Day2)

#And let's average out those Day2s:
time_agg_Day2less <- aggregate(. ~ SubjectDNID+Meal+Time, time_numeric, mean)

time_agg_reduced <- time_agg_Day2less[,c(1,3,9)]

#Ah, 712 columns is way too many!! Let's round all of these times down to two hour chunks...

time_agg_reduced$Time <- RoundTo(time_agg_reduced$Time, 4)

#And sum up over the chunks
time_agg_reduced <- aggregate(.~Time+SubjectDNID, time_agg_reduced, sum)

time.wide <- dcast(time_agg_reduced, formula = SubjectDNID ~ Time, value.var = "Energy_kcal", fun.aggregate = sum)

#Oh, 7 columns? Hmmm. 24 and 0 are currently separate times, let's add them both to the 24 column.

time.wide$`24` <- time.wide$`24` + time.wide$`0`

rownames(time.wide) <- time.wide[,1]
time.wide <- time.wide[,-c(1,2)]

ggplot(time_agg_reduced[time_agg_reduced$SubjectDNID %in% 
                          unique(time_agg_reduced$SubjectDNID)[1:10],], aes(x=Time,y=Energy_kcal)) + facet_wrap(~SubjectDNID, ncol = 1, scales = "free_y") + geom_point() + geom_line() + geom_smooth() + theme_bw()

dist <- dist(time.wide)

#Let's cluster it
hc <- hclust(dist)

#And take a sneak peek
plot(hc, labels=rownames(time.wide), main="")

ggplot(time_agg_reduced[time_agg_reduced$SubjectDNID == "7496",], aes(x=Time,y=Energy_kcal)) + facet_wrap(~SubjectDNID, ncol = 1, scales = "free_y") + geom_point() + geom_line() + geom_smooth() + theme_bw() + xlim(c(0,24))

time.wide$total <- rowSums(time.wide)
time.wide2 <- apply(time.wide,2,FUN = function(x) x/time.wide$total)

set.seed(123)

hm <- Heatmap(as.matrix(time.wide2[,c(1:6)]), cluster_rows = T, cluster_columns = F, row_km = 5)
hm <- draw(hm)
order <- row_order(hm)

c1 <- melt(time.wide2[order$`1`,c(1:6)])
c2 <- melt(time.wide2[order$`2`,c(1:6)])
c3 <- melt(time.wide2[order$`3`,c(1:6)])
c4 <- melt(time.wide2[order$`4`,c(1:6)])
c5 <- melt(time.wide2[order$`5`,c(1:6)])

gc1 <- ggplot(c1, aes(x=Var2,y=value,group=Var1)) + theme_bw() + geom_line(lwd=0.05) + geom_line(inherit.aes = F, data = aggregate(data = c1, .~Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = "red") + xlab("Hours") + scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + ylab("Relative consumed kcal")

gc2 <- ggplot(c2, aes(x=Var2,y=value,group=Var1)) + theme_bw() + geom_line(lwd=0.05) + geom_line(inherit.aes = F, data = aggregate(data = c2, .~Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = "blue") + xlab("Hours") + scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + ylab("Relative consumed kcal")

gc3 <- ggplot(c3, aes(x=Var2,y=value,group=Var1)) + theme_bw() + geom_line(lwd=0.05) + geom_line(inherit.aes = F, data = aggregate(data = c3, .~Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = "yellow") + xlab("Hours") + scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + ylab("Relative consumed kcal")

gc4 <- ggplot(c4, aes(x=Var2,y=value,group=Var1)) + theme_bw() + geom_line(lwd=0.05) + geom_line(inherit.aes = F, data = aggregate(data = c4, .~Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = "green") + xlab("Hours") + scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + ylab("Relative consumed kcal")

gc5 <- ggplot(c5, aes(x=Var2,y=value,group=Var1)) + theme_bw() + geom_line(lwd=0.05) + geom_line(inherit.aes = F, data = aggregate(data = c5, .~Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = "cyan") + xlab("Hours") + scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + ylab("Relative consumed kcal")

grid.arrange(gc1,gc2,gc3,gc4,gc5,ncol=2)



#With cats

time_numeric2 <- baseline_red_pca[,c(51,2,8,10,11,17:41)]
time_numeric2 <- time_numeric2[,-4]
time_numeric2$Time <- RoundTo(time_numeric2$Time, 4)

time.wide3 <- dcast(time_numeric2, formula = PCA_Categories+SubjectDNID ~ Time, value.var = "Gram", fun.aggregate = sum)

time.wide3$`24` <- time.wide3$`0`+time.wide3$`24`

time.wide3 <- time.wide3[,-3]

time.wide3$PCA_Categories <- factor(time.wide3$PCA_Categories)
timewide_order <- aggregate(.~PCA_Categories,time.wide3[,c(1,3:8)],sum)
time.wide3$PCA_Categories <- factor(time.wide3$PCA_Categories, 
                                    levels = timewide_order[order(rowSums(timewide_order[,c(2:7)]), decreasing = T),]$PCA_Categories)

c1.cat <- time.wide3[time.wide3$SubjectDNID %in% c1$Var1,]
c2.cat <- time.wide3[time.wide3$SubjectDNID %in% c2$Var1,]
c3.cat <- time.wide3[time.wide3$SubjectDNID %in% c3$Var1,]
c4.cat <- time.wide3[time.wide3$SubjectDNID %in% c4$Var1,]
c5.cat <- time.wide3[time.wide3$SubjectDNID %in% c5$Var1,]

c1.cat <- c1.cat[,-2]
c1.agg <- aggregate(.~PCA_Categories,c1.cat,sum)
c1.melt <- melt(c1.agg)
c1.melt$value <- c1.melt$value / length(order$`1`)

cplot1 <- ggplot(c1.melt, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,16)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + theme(legend.position = "none")

c2.cat <- c2.cat[,-2]
c2.agg <- aggregate(.~PCA_Categories,c2.cat,sum)
c2.melt <- melt(c2.agg)
c2.melt$value <- c2.melt$value / length(order$`2`)

cplot2 <- ggplot(c2.melt, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,16)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + theme(legend.position = "none",axis.title.y=element_blank(),
                                                                          axis.text.y=element_blank(),
                                                                          axis.ticks.y=element_blank())
c3.cat <- c3.cat[,-2]
c3.agg <- aggregate(.~PCA_Categories,c3.cat,sum)
c3.melt <- melt(c3.agg)
c3.melt$value <- c3.melt$value / length(order$`3`)

cplot3 <- ggplot(c3.melt, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,16)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + theme(legend.position = "none",axis.title.y=element_blank(),
                                                                          axis.text.y=element_blank(),
                                                                          axis.ticks.y=element_blank()) 

c4.cat <- c4.cat[,-2]
c4.agg <- aggregate(.~PCA_Categories,c4.cat,sum)
c4.melt <- melt(c4.agg)
c4.melt$value <- c4.melt$value / length(order$`4`)

cplot4 <- ggplot(c4.melt, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,16)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + theme(legend.position = "none",axis.title.y=element_blank(),
                                                                          axis.text.y=element_blank(),
                                                                          axis.ticks.y=element_blank()) 

c5.cat <- c5.cat[,-2]
c5.agg <- aggregate(.~PCA_Categories,c5.cat,sum)
c5.melt <- melt(c5.agg)
c5.melt$value <- c5.melt$value / length(order$`5`)

cplot5 <- ggplot(c5.melt, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,16)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + theme(axis.title.y=element_blank(),
                                                                          axis.text.y=element_blank(),
                                                                          axis.ticks.y=element_blank())

c1.melt$cluster <- 1
c2.melt$cluster <- 2
c3.melt$cluster <- 3
c4.melt$cluster <- 4
c5.melt$cluster <- 5

all_c <- rbind(rbind(rbind(rbind(c1.melt,c2.melt),c3.melt),c4.melt),c5.melt)

eatgraphs <- ggplot(all_c, aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Gram", range = c(1,7)) + 
  scale_colour_continuous(name = "Gram", type = "viridis") + 
  facet_wrap(~cluster, nrow = 1)

c1$cluster <- 1
c2$cluster <- 2
c3$cluster <- 3
c4$cluster <- 4
c5$cluster <- 5

all_early_c <- rbind(rbind(rbind(rbind(c1,c2),c3),c4),c5)

timegraphs <- ggplot(all_early_c, aes(x=Var2,y=value,group=Var1)) + 
    theme_bw() + 
    geom_line(lwd=0.05) + 
    geom_line(inherit.aes = F, data = aggregate(data = all_early_c, .~cluster+Var2, FUN = mean), aes(x=Var2,y=value), lwd = 2, color = c("cyan")) + 
    xlab("Hours") + 
    scale_x_continuous(breaks = c(4,8,12,16,20,24),labels=c("4" = "04:00","8" = "08:00", "12" = "12:00", "16" = "16:00", "20" = "20:00","24" = "00:00")) + 
    ylab("Relative consumed kcal") +
    facet_wrap(~cluster,nrow=1)

grid.arrange(timegraphs,eatgraphs,ncol=1)

sum(all_c[all_c$cluster == 1,]$value)/length(c5$Var1)


eatgraphs_red <- ggplot(all_c[!all_c$PCA_Categories %in% c("Pulses","Oil","Coffee and Tea","Nuts"),],
                        aes(y=PCA_Categories,x=variable,size=value, colour=value)) + 
  geom_point() +
  scale_size_continuous(name = "Energy_kcal", range = c(1,7)) + 
  scale_colour_continuous(name = "Energy_kcal", type = "viridis") + 
  facet_wrap(~cluster, nrow = 1)


grid.arrange(timegraphs,eatgraphs_red,ncol=1)

time_clus_order <- all_early_c[,c(1,4)]
colnames(time_clus_order) <- c("SubjectDNID","Time_cluster")
time_clus_order <- time_clus_order[!duplicated(time_clus_order$SubjectDNID),]

baseline_red2 <- baseline_red
baseline_red2[,c(12:42)][is.na(baseline_red2[,c(12:42)])] <- 0
baseline_numeric <- baseline_red2[,c(1,7,16:40)]
baseline_numeric$SubjectDNID <- as.character(baseline_numeric$SubjectDNID)

subject_by_Day2 <- aggregate(. ~ SubjectDNID + Day2, FUN = sum, data = baseline_numeric)
daily.average <- aggregate(.  ~ SubjectDNID, FUN = mean, data = subject_by_Day2)

daily.average.add <- merge(daily.average,master,by="SubjectDNID")
daily.average.add <- merge(daily.average.add,time_clus_order,by="SubjectDNID")

daily.average.summary <- daily.average.add[,c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,33,35:39,43)]

daily.average.summary <- melt(daily.average.summary, id.vars = c("SubjectDNID","Time_cluster"))

unique_vars <- unique(daily.average.summary$variable)

tukey_mat <- matrix(ncol=10,nrow=length(unique_vars))
rownames(tukey_mat) <- unique_vars

daily.average.summary$Time_cluster <- factor(daily.average.summary$Time_cluster)

for(i in c(1:length(unique_vars))) {
  
  tukey_mat[i,] <- TukeyHSD(aov(value~Time_cluster,daily.average.summary[daily.average.summary$variable == unique_vars[i],]))$Time_cluster[,4]
  
}

names_here <- names(TukeyHSD(aov(value~Time_cluster,daily.average.summary[daily.average.summary$variable == unique_vars[i],]))$Time_cluster[,4])
colnames(tukey_mat) <- names_here


as.data.frame(tukey_mat) %>% gt(rownames_to_stub = T)

time_agg_summ <- aggregate(value~Time_cluster+variable,daily.average.summary,mean)

dcast(time_agg_summ, formula = Time_cluster~variable) %>% gt()

#Eaten together frequency
time_numeric2.4 <- time_numeric2[time_numeric2$Time == 4,c(1,2,3)]
time_numeric2.8 <- time_numeric2[time_numeric2$Time == 8,c(1:3)]
time_numeric2.12 <- time_numeric2[time_numeric2$Time == 12,c(1:3)]
time_numeric2.16 <- time_numeric2[time_numeric2$Time == 16,c(1:3)]
time_numeric2.20 <- time_numeric2[time_numeric2$Time == 20,c(1:3)]
time_numeric2.24 <- time_numeric2[time_numeric2$Time == 24,c(1:3)]

time_numeric3 <- time_numeric2[,c(1,2,3,4)]

time_numeric3$countcol <- paste(time_numeric3$SubjectDNID,time_numeric3$Day2,time_numeric3$Time)

time_numeric4 <- time_numeric3[,c(1,5)]

count_matrix <- crossprod(table(time_numeric4[2:1]))

as.data.frame(count_matrix) %>% gt(rownames_to_stub = T)

count_matrix[upper.tri(count_matrix)] <- 0

letter_matrix <- count_matrix
letter_matrix[upper.tri(letter_matrix)] <- ""

Heatmap(count_matrix, 
        col = colorRamp2(c(0,1,10,100,500,1000),c("white","blue","dodgerblue","white","red","darkred")), 
        cluster_rows = F, 
        cluster_columns = F,
        row_names_side = "left", 
        cell_fun = function(j, i, x, y, w, h, col) { 
          grid.text(letter_matrix[i, j], x, y)})




#Replace kcal and kJ measures in the original database
baseline$Protein
baseline$Carbohydrate
baseline$Fat
table(baseline$Energy_kcal/baseline$Energy_kJ)

#Use filters of 5000kCal, 5000kCal
#Sort by men and women separately 
#Change pies to 1 decimal place
#Remove calories from pie
#Move legend to right-hand side

baseline[grep("Smør", baseline$FoodDiettNoName),]

#Find some way to test reccomendations
#Categorise participants by this

#Name the clusters


#Flip the time-eat graphs

#Expand sugary goods
#Expand meat
#Expand cereals

#Look for questionnaire data for these time clusters
baseline$total_fat <- baseline$MUFA + baseline$PUFA + baseline$SFA
daily.average$total_fat <- daily.average$MUFA + daily.average$PUFA + daily.average$SFA 


baseline$total_energy <- (4*baseline$Protein)+(9*baseline$Fat)+(4*baseline$Carbohydrate)+(2*baseline$Fiber)+(7*baseline$Alcohol)#+(4*baseline$SugarAdded)

daily.average$total_energy <- (4*daily.average$Protein)+(9*daily.average$total_fat)+(4*daily.average$Carbohydrate)+(7*daily.average$Alcohol)+(2*daily.average$Fiber)+(4*daily.average$SugarAdded)

mean(daily.average$total_energy)
mean(daily.average$Energy_kcal)

100*sum(baseline$total_energy-baseline$Energy_kcal, na.rm = T)/sum(baseline$Energy_kcal, na.rm = T)

(4*29.9) + (4*2.6) + (2*7.4) + (4*12.4)

212/896

29.9+2.6+12.4
4*44.9
2*7.4

179.6+14.8+18
colnames(baseline)

coff <- baseline[baseline$FoodDishName == "Kaffe, traktet, kokt",c(15,20,24,57,26,28,30,32,34,36)]

test <- baseline[order(baseline$total_energy-baseline$Energy_kcal, decreasing = T),c(15,20,24,57,26,28,30,32,34,36)]
test2 <- baseline[order(baseline$total_energy-baseline$Energy_kcal),c(15,20,24,57,26,28,30,32,34,36)]

(baseline$Fiber+baseline$SugarAdded) == baseline$Carbohydrate

(44.60*4)+(7.2*4)+(9*35.50)+(2*8.50)+(7*8.70)

