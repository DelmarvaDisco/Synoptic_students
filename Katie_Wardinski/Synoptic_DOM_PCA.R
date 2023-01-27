#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Synoptic data exploration in Data Analytics Class
#Coder: Katie Wardinski
#Created: 2022-12-14
#Purpose: Preliminary exploration in PCA/clustering analyses (code used in class project)
#Question: How does DOM concentration and composition vary as surficial groundwater
#moves through a wetland-dominated catchment?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#set working directory
setwd("C:/Workspace/Synoptic_students/Katie_Wardinski")

#load relevant packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lubridate)
library(raster)
library(robustHD)
library(pracma)
library(FactoMineR)
library(factoextra)
library(powerplus)
library(fpc)
library(boot)


#set theme classic
theme_set(theme_classic())

#read in data
water <- read_csv("dly_mean_output_JM_2019_2022.csv")
synoptic <-read_csv("SynopticCurrent.csv") #synoptic data through 2022-03
site <- read_csv("wetland_info.csv") #site info including wetland order

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0  Hierarchical Clustering -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##2.1 All synoptic data ------------------------------
#select data I want to use for cluster
all <- synoptic %>% dplyr::select(Date_M,Site,Sample_Name,
                                  Type,SW_GW,Cl_mg_L,SO4_mg_L,NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                  NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                  P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                  e2e3,s275_295,s350_400,S_R) 

#drop variables that are not continuous (site name, GW vs SW, etc)
syn <- synoptic %>% dplyr::select(Cl_mg_L,SO4_mg_L,NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                  NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                  P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                  e2e3,s275_295,s350_400,S_R) %>% drop_na()

syn_label <- synoptic %>% dplyr::select(Date_M,Site,Sample_Name, Type,SW_GW,Cl_mg_L,SO4_mg_L,NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                        NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                        P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                        As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,e2e3,s275_295,s350_400,S_R) %>% drop_na()

syn_names <- paste(syn_label$Date_M, "-", syn_label$Sample_Name)

#standardize data
Z_syn <- standardize(syn,centerFun = mean,scaleFun = sd)

#compute pairwise euclidean distance
#DMAT <- dist(Z_syn, method = "euclidean", diag = TRUE, upper = TRUE)
#View(as.matrix(DMAT))

#try correlation distance
transpose <- t(Z_syn)
DMAT <- 1-cor(as.matrix(transpose),method="pearson")
DMAT <- as.dist(DMAT)
#View(as.matrix(DMAT))

#quick look at potential clustering
fviz_dist(DMAT,show_labels = FALSE)

#use R's hclust package on the various methods
#method with highest cophenetic corelation = "the best"
COMPLETE <- hclust(d = DMAT,method ='complete')
SINGLE <- hclust(d = DMAT,method ='single')
AVERAGE <- hclust(d = DMAT,method ='average')
WARD <- hclust(d = DMAT,method ='ward.D2')

#estimate the cophenetic correlation
COMPLETE_Distcoph <- cophenetic(COMPLETE)
C_complete = cor(DMAT,COMPLETE_Distcoph); C_complete #0.725

SINGLE_Distcoph <- cophenetic(SINGLE)
C_single = cor(DMAT,SINGLE_Distcoph); C_single #0.43

AVERAGE_Distcoph <- cophenetic(AVERAGE)
C_average <- cor(DMAT,AVERAGE_Distcoph); C_average #0.819 *****BEST*****

WARD_Distcoph <- cophenetic(WARD)
C_ward <- cor(DMAT,WARD_Distcoph); C_ward #0.765

#use fviz_dend see the best full hierarchical tree
fviz_dend(AVERAGE,cex = 0.5,ylab = "Correlation Distance")

#Choose where to cut the tree
#1 Scree plot
fviz_nbclust(Z_syn,hcut,diss = DMAT, method ="wss",k.max = 30)

#2 Gap statistics
fviz_nbclust(Z_syn,hcut,method ="gap",k.max = 30,diss = DMAT,nboot = 100)

#I'd say 6-10 looks reasonable 
#add labels back to data
Z_syn$Names <- syn_names
AVERAGE$labels <- syn_names
fviz_dend(AVERAGE,k=10,cex=0.5,ylab = "Correlation Dist",color_labels_by_k = TRUE,rect = TRUE)

#cluster validation
grp <- cutree(AVERAGE, k = 10)
STATS <- cluster.stats(DMAT,grp)
STATS$dunn #0.089 #Using correlation instead of euclidean made dunn stat worse 

#using hcut to redo our heirarchical 
#agglomerative clustering because 
#fviz_silhouette won't take hclust output
CAGAIN <- hcut(Z_syn,k = 10,hcfunc = "hclust",hc_method = "average",hc_metric = "pearson")
fviz_silhouette(CAGAIN)

#further look at final tree cut
table(grp)
#cluster of results
fviz_cluster(list(data = syn, cluster = grp))

## 2.2 SW Only ---------------------------------
#join site and synoptic data
join <- left_join(synoptic,site,by="Site")
sw_only <- join %>% filter(SW_GW == "SW")

#sw data for PCA
sw_all <- sw_only %>% dplyr::select(Date_M,Site,Sample_Name,
                                    Type,SW_GW,Cl_mg_L,SO4_mg_L,
                                    NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                    NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                    TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                    P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                    As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                    e2e3,s275_295,S_R,wet_order) %>% drop_na()

sw_labels <- as.character(sw_all$wet_order)

#drop variables that are not continuous (site name, GW vs SW, etc)
sw_syn <- sw_all %>% dplyr::select(Cl_mg_L,SO4_mg_L,
                                   NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                   NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                   TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                   P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                   As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                   e2e3,s275_295,S_R) %>% drop_na()



#standardize data
Z_sw_syn <- standardize(sw_syn,centerFun = mean,scaleFun = sd)

#try correlation distance
transpose <- t(Z_sw_syn)
DMAT <- 1-cor(as.matrix(transpose),method="pearson")
DMAT <- as.dist(DMAT)
#View(as.matrix(DMAT))

#quick look at potential clustering
fviz_dist(DMAT,show_labels = FALSE)

#use R's hclust package on the various methods
COMPLETE <- hclust(d = DMAT,method ='complete')
SINGLE <- hclust(d = DMAT,method ='single')
AVERAGE <- hclust(d = DMAT,method ='average')
WARD <- hclust(d = DMAT,method ='ward.D2')

#estimate the cophenetic correlation
COMPLETE_Distcoph <- cophenetic(COMPLETE)
C_complete = cor(DMAT,COMPLETE_Distcoph)
C_complete #0.53

SINGLE_Distcoph <- cophenetic(SINGLE)
C_single = cor(DMAT,SINGLE_Distcoph)
C_single #0.17

AVERAGE_Distcoph <- cophenetic(AVERAGE)
C_average <- cor(DMAT,AVERAGE_Distcoph)
C_average #0.61 *****BEST*****

WARD_Distcoph <- cophenetic(WARD)
C_ward <- cor(DMAT,WARD_Distcoph)
C_ward #0.55

#use fviz_dend see the best full hierarchical tree
fviz_dend(AVERAGE,cex = 0.5,
          ylab = "Correlation Distance")

#choose where to cut the tree
#Scree plot - between 6 and 12
fviz_nbclust(Z_sw_syn,hcut,diss = DMAT, 
             method ="wss",
             k.max = 30)#hcut means you use

#Gap statistics
fviz_nbclust(Z_sw_syn,hcut,method ="gap",
             k.max = 30,diss = DMAT,
             nboot = 100)

#Try 12
AVERAGE$labels <- sw_labels
fviz_dend(AVERAGE,k=6,cex=0.5,
          ylab = "Correlation Dist",
          color_labels_by_k = TRUE,rect = TRUE)

#plot(AVERAGE,labels=Z_syn$Names)

#cluster validation
grp <- cutree(AVERAGE, k = 12)
STATS <- cluster.stats(DMAT,grp)
STATS$dunn #0.15

#using hcut to redo our heirarchical 
#agglomerative clustering because 
#fviz_silhouette won't take hclust output
CAGAIN <- hcut(Z_sw_syn,k = 12,hcfunc = "hclust",
               hc_method = "average",
               hc_metric = "pearson")

fviz_silhouette(CAGAIN)

#further look at final tree cut
table(grp)
#cluster of results
fviz_cluster(list(data = noriver_syn, cluster = grp))



## 2.3 No rivers ------------------------------
#all data filter out rivers
noriver <- synoptic %>% dplyr::select(Date_M,Site,Sample_Name,
                                      Type,SW_GW,Cl_mg_L,SO4_mg_L,
                                      NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                      NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                      TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                      P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                      As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                      e2e3,s275_295,S_R) %>% drop_na() %>% 
  filter(Type != "River")


#drop variables that are not continuous (site name, GW vs SW, etc)
noriver_syn <- noriver %>% dplyr::select(Cl_mg_L,SO4_mg_L,
                                         NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                         NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                         TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                         P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                         As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                         e2e3,s275_295,S_R) %>% drop_na()

#standardize data
Z_noriver_syn <- standardize(noriver_syn,centerFun = mean,scaleFun = sd)

#try correlation distance
transpose <- t(Z_noriver_syn)
DMAT <- 1-cor(as.matrix(transpose),method="pearson")
DMAT <- as.dist(DMAT)
#View(as.matrix(DMAT))

#quick look at potential clustering
fviz_dist(DMAT,show_labels = FALSE)

#use R's hclust package on the various methods
COMPLETE <- hclust(d = DMAT,method ='complete')
SINGLE <- hclust(d = DMAT,method ='single')
AVERAGE <- hclust(d = DMAT,method ='average')
WARD <- hclust(d = DMAT,method ='ward.D2')

#estimate the cophenetic correlation
COMPLETE_Distcoph <- cophenetic(COMPLETE)
C_complete = cor(DMAT,COMPLETE_Distcoph)
C_complete #0.709

SINGLE_Distcoph <- cophenetic(SINGLE)
C_single = cor(DMAT,SINGLE_Distcoph)
C_single #0.269

AVERAGE_Distcoph <- cophenetic(AVERAGE)
C_average <- cor(DMAT,AVERAGE_Distcoph)
C_average #0.799 *****BEST*****

WARD_Distcoph <- cophenetic(WARD)
C_ward <- cor(DMAT,WARD_Distcoph)
C_ward #0.738

#use fviz_dend see the best full hierarchical tree
fviz_dend(AVERAGE,cex = 0.5,
          ylab = "Correlation Distance")

#choose where to cut the tree
#Scree plot - between 6 and 12
fviz_nbclust(Z_noriver_syn,hcut,diss = DMAT, 
             method ="wss",
             k.max = 30)#hcut means you use

#Gap statistics
fviz_nbclust(Z_noriver_syn,hcut,method ="gap",
             k.max = 30,diss = DMAT,
             nboot = 100)

#I'd say 12 looks reasonable 
#add labels back to data 
Z_syn$Names <- syn_names
fviz_dend(AVERAGE,k=12,cex=0.5,
          ylab = "Correlation Dist",
          color_labels_by_k = TRUE,rect = TRUE)

#plot(AVERAGE,labels=Z_syn$Names)

#cluster validation
grp <- cutree(AVERAGE, k = 12)
STATS <- cluster.stats(DMAT,grp)
STATS$dunn #0.11 #Using correlation instead of euclidean made dunn stat worse 

#using hcut to redo our heirarchical 
#agglomerative clustering because 
#fviz_silhouette won't take hclust output
CAGAIN <- hcut(Z_noriver_syn,k = 12,hcfunc = "hclust",
               hc_method = "average",
               hc_metric = "pearson")

fviz_silhouette(CAGAIN)

#further look at final tree cut
table(grp)
#cluster of results
fviz_cluster(list(data = noriver_syn, cluster = grp))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 PCA -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data is continuous - can use PCA

##3.05 Check shape of distributions --------------------------------
#looking for generally normal distribution - if bad, log data
#hist(syn$Cl_mg_L) #bad
hist(log(syn$Cl_mg_L)) #better
#hist(syn$SO4_mg_L) #bad
hist(log(syn$SO4_mg_L)) #better
hist(syn$NPOC_mgC_L) #not great but leave as is
#hist(log(syn$NPOC_mgC_L)) #worse
hist(syn$d2H_VSMOW) #ok
hist(syn$d18O_VSMOW) #ok
#hist(syn$NH3_mgN_L) #bad
hist(log(syn$NH3_mgN_L)) #better
#hist(syn$NO3_mgN_L) #bad
hist(log(syn$NO3_mgN_L)) #better
#hist(syn$oPO4_mgP_L) #bad
hist(log(syn$oPO4_mgP_L)) #better
#hist(syn$TDN_mgN_L)
hist(log(syn$TDN_mgN_L)) #better
#hist(syn$TDP_mgP_L)
hist(log(syn$TDP_mgP_L)) #better
#hist(syn$Na_ppb)
hist(log(syn$Na_ppb)) #better
#hist(syn$Mg_ppb)
hist(log(syn$Mg_ppb)) #better
hist(syn$Al_ppb) #ok - keep
#hist(log(syn$Al_ppb))
hist(syn$Si_ppb) #ok
hist(syn$P_ppb)
hist(log10(syn$P_ppb)) #get rid of P - negative values
hist(syn$S_ppb)
hist(log(syn$S_ppb)) #get rid of S - negative values
hist(syn$Cl_ppb) #get rid of Cl - negative values
#hist(syn$K_ppb)
hist(log(syn$K_ppb)) #better
hist(syn$Ca_ppb)
hist(syn$Fe_ppb) #bad - negative values
hist(syn$Mn_ppb) #bad negative values
hist(syn$As_ppb) #bad negative values
hist(syn$Ag_ppb)
hist(syn$Pb_ppb)
hist(syn$FI) #ok
hist(syn$BIX) #ok
#hist(syn$zHIX) 
hist(log(syn$zHIX)) #better
hist(syn$oHIX)
hist(syn$e2e3)
hist(syn$abs254_m) #not great
#hist(log(syn$abs254_m)) #not better
hist(syn$s275_295) #ok
hist(syn$s350_400) #ok
hist(syn$S_R) #ok

#variables to log: Cl, SO4, NH3, NO3, oPO4, TDN, TDP, Na, Mg, K, zHIX 
#figure out what to do with negative metal values

##3.1 PCA for all data (SW, GW, Channel, River) ----------------------------------
#all data for PCA
all <- synoptic %>% dplyr::select(Date_M,Site,Sample_Name,
                                  Type,SW_GW,Cl_mg_L,SO4_mg_L,
                                  NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                  NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                  TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                  P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                  As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                  e2e3,s275_295,s350_400,S_R) %>% drop_na()


#drop variables that are not continuous (site name, GW vs SW, etc)
syn <- synoptic %>% dplyr::select(Cl_mg_L,SO4_mg_L,
                                  NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                  NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                  TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                  P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                  As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                  e2e3,s275_295,s350_400,S_R) %>% drop_na()

#standardize data
Z_syn <- standardize(syn,centerFun = mean,scaleFun = sd)

#run PCA on all data, full ncp = 13
all.syn.pca <- PCA(Z_syn, graph = FALSE,ncp = 13)

eval <- all.syn.pca$eig[,1]
mode <- all.syn.pca$svd$V 
score <- as.matrix(Z_syn)%*%as.matrix(mode)

#plot PCA
fviz_pca_biplot(all.syn.pca,label="var",
                title = "All Synoptic Data - PCA",
                addEllipses=TRUE,col.ind=all$Type,
                legend.title = "Sample Type",repel = TRUE)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

fviz_pca_var(all.syn.pca ,col.var="contrib",
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),repel = TRUE)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))


#PART2: 10% Rule and Scree Plot

#estimate percent variance explained for 
#each mode
PC_PerVE = eval/sum(eval)

#plot %VE for each mode, constructing a scree plot
plot(PC_PerVE,pch = 19,cex = 1.5,xlab = 'Mode Number',ylab = 'Percent Variance Explained')

#more formal stopping rule
#step1 :set the number of resampling events and the number of variables
N = 1000 
k = ncol(Z_syn)

#step2: set the size of our desired 
#eignevalue matrix (k by N)
Eval_boot = matrix(NA,k,N)

#step3: estimate eigenvalues from randomized 
#data N times
DATArand = matrix(NA,nrow(syn),ncol(syn))
for (i in 1:N){
  for (j in 1:k){
    DATArand[,j] = cbind(sample(Z_syn[,j],nrow(Z_syn),replace =FALSE))
  }
  A = PCA(DATArand, graph = FALSE,ncp = 13)
  Eval_boot[,i] = A$eig[,1]
}


#step4: calculate desired thresholds for Evals 
#from randomized data using percentile resampling 
Thresh_50per = matrix(NA,k)
Thresh_90per = matrix(NA,k)
Thresh_95per = matrix(NA,k)

for (i in 1:k){
  #50th percentile of each Eval
  Thresh_50per[i] = as.numeric(quantile(Eval_boot[i,],
                        0.50,type=1))
  #upper 90th percentile of each Eval
  Thresh_90per[i] = as.numeric(quantile(Eval_boot[i,],
                        0.90,type=1))
  #upper 95th percentile of each Eval
  Thresh_95per[i] = as.numeric(quantile(Eval_boot[i,],
                        0.95,type=1))
}

#step 5: plot data eigenvalues and thresholds to 
#determine which modes should be retained

#data eigegnvalues                      
plot(eval,pch=19,cex = 1,col = "black")
#random threshold
lines(Thresh_50per,col = "blue")
#marginally sig diff than random threshold
lines(Thresh_90per,col = "black") 
#sig diff than random threshold
lines(Thresh_95per,col = "red")

#looks like 7 pass - re-do PCA
all.syn.pca.7 <- PCA(Z_syn, graph = FALSE,ncp = 7)

eval.7 <- all.syn.pca.7$eig[,1]
mode.7 <- all.syn.pca.7$svd$V 
score.7 <- as.matrix(Z_syn)%*%as.matrix(mode)

#plot PCA
fviz_pca_biplot(all.syn.pca.7,label="var",
                title = "All Data - PCA",addEllipses=TRUE,
                col.ind=all$SW_GW,legend.title = "Sample Type", repel = TRUE)

fviz_pca_var(all.syn.pca.7 ,col.var="contrib",
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"), repel = TRUE)

## 3.2 PCA drop rivers -------------------------

#all data for PCA
noriver <- synoptic %>% dplyr::select(Date_M,Site,Sample_Name,
                                      Type,SW_GW,Cl_mg_L,SO4_mg_L,
                                      NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                      NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                      TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                      P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                      As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                      e2e3,s275_295,S_R) %>% drop_na() %>% 
  filter(Type != "River")


#drop variables that are not continuous (site name, GW vs SW, etc)
noriver_syn <- noriver %>% dplyr::select(Cl_mg_L,SO4_mg_L,
                                         NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                         NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                         TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                         P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                         As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                         e2e3,s275_295,S_R) %>% drop_na()

#standardize data
Z_noriver_syn <- standardize(noriver_syn,centerFun = mean,scaleFun = sd)

#run PCA on all data, full ncp = 13
noriver.syn.pca <- PCA(Z_noriver_syn, graph = FALSE,ncp = 13)

eval <- noriver.syn.pca$eig[,1]
mode <- noriver.syn.pca$svd$V 
score <- as.matrix(Z_noriver_syn)%*%as.matrix(mode)

#plot PCA
fviz_pca_biplot(noriver.syn.pca,
                label="var",
                title = "No Rivers - PCA",
                addEllipses=TRUE,
                col.ind=noriver$Type,
                legend.title = "Sample Type",
                repel = TRUE)

fviz_pca_var(noriver.syn.pca,
             col.var="contrib",
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))



## 3.3 PCA looking at just SW and wetland order -------------------------
#join site and synoptic data
join <- left_join(synoptic,site,by="Site")
sw_only <- join %>% filter(SW_GW == "SW")

#sw data for PCA
sw_all <- sw_only %>% dplyr::select(Date_M,Site,Sample_Name,
                                    Type,SW_GW,Cl_mg_L,SO4_mg_L,
                                    NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                    NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                    TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                    P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                    As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                    e2e3,s275_295,S_R,wet_order) %>% drop_na()


#drop variables that are not continuous (site name, GW vs SW, etc)
sw_syn <- sw_all %>% dplyr::select(Cl_mg_L,SO4_mg_L,
                                   NPOC_mgC_L,d2H_VSMOW,d18O_VSMOW,
                                   NH3_mgN_L,NO3_mgN_L,oPO4_mgP_L,TDN_mgN_L,
                                   TDP_mgP_L,Na_ppb,Mg_ppb,Al_ppb,Si_ppb,
                                   P_ppb,S_ppb,Cl_ppb,K_ppb,Ca_ppb,Fe_ppb,Mn_ppb,
                                   As_ppb,Ag_ppb,Pb_ppb,FI,BIX,zHIX,oHIX,abs254_m,
                                   e2e3,s275_295,S_R) %>% drop_na()

#standardize data
Z_SW <- standardize(sw_syn,centerFun = mean,scaleFun = sd)

#run PCA on all data, full ncp = 13
sw.syn.pca <- PCA(Z_SW, graph = FALSE,ncp = 13)

#plot results
#plot PCA
fviz_pca_biplot(sw.syn.pca ,
                label="var",
                title = "SW Only Data - PCA",
                addEllipses=TRUE,
                col.ind=as.factor(sw_all$wet_order),
                legend.title = "Wetland Order",
                repel=TRUE)

fviz_pca_var(sw.syn.pca ,
             col.var="contrib",
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)

#stopping rule
sw_eval = sw.syn.pca$eig[,1]
sw_evec = sw.syn.pca$svd$V 
sw_PCScores = as.matrix(Z_SW)%*%sw_evec

#10% Rule and Scree Plot
#estimate percent variance explained for each mode
sw_PC_PerVE = sw_eval/sum(sw_eval)

#plot %VE for each mode, constructing a scree plot
plot(sw_PC_PerVE,pch = 19,cex = 1.5,
     xlab = 'Mode Number',
     ylab = 'Percent Variance Explained')

#more formal stopping rule
#step1 :set the number of resampling events and the number of variables
N = 1000 
k = ncol(Z_SW)

#step2: set the size of our desired 
#eignevalue matrix (k by N)
Eval_boot = matrix(NA,k,N)

#step3: estimate eigenvalues from randomized 
#data N times
DATArand = matrix(NA,nrow(sw_syn),ncol(sw_syn))
for (i in 1:N){
  for (j in 1:k){
    DATArand[,j] = cbind(sample(Z_SW[,j],
                                nrow(Z_SW),
                                replace =FALSE))
  }
  A = PCA(DATArand, graph = FALSE,ncp = 13)
  Eval_boot[,i] = A$eig[,1]
}


#step4: calculate desired thresholds for Evals 
#from randomized data using percentile resampling 
Thresh_50per = matrix(NA,k)
Thresh_90per = matrix(NA,k)
Thresh_95per = matrix(NA,k)

for (i in 1:k){
  #50th percentile of each Eval
  Thresh_50per[i] = 
    as.numeric(quantile(Eval_boot[i,],
                        0.50,type=1))
  #upper 90th percentile of each Eval
  Thresh_90per[i] = 
    as.numeric(quantile(Eval_boot[i,],
                        0.90,type=1))
  #upper 95th percentile of each Eval
  Thresh_95per[i] = 
    as.numeric(quantile(Eval_boot[i,],
                        0.95,type=1))
}

#step 5: plot data eigenvalues and thresholds to 
#determine which modes should be retained
#data eigegnvalues                      
plot(sw_eval,pch=19,cex = 1,col = "black")
#random threshold
lines(Thresh_50per,col = "blue")
#marginally sig diff than random threshold
lines(Thresh_90per,col = "black") 
#sig diff than random threshold
lines(Thresh_95per,col = "red")

#looks like 6 pass - re-do PCA
sw.syn.pca.6 <- PCA(Z_SW, graph = FALSE,ncp = 6)

eval.6 <- sw.syn.pca.6$eig[,1]
mode.6 <- sw.syn.pca.6$svd$V 
score.6 <- as.matrix(Z_SW)%*%as.matrix(mode.6)

#plot PCA
fviz_pca_biplot(sw.syn.pca.6,
                label="var",
                title = "SW Only Data - PCA",
                addEllipses=TRUE,
                col.ind=as.factor(sw_all$wet_order),
                legend.title = "Wetland Order",
                repel=TRUE)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

fviz_pca_var(sw.syn.pca.6,
             col.var="contrib",
             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

## 3.4 Constrained PCA for SW --------------------------
#do things like wetland order and area of wetland influence water chemistry?
#cut down on water chem variables to the ones I think are most important
#constraint data = wetland order, area, watershed area etc

#clean up constraint data
site_clean <- site %>% drop_na() %>% 
  dplyr::select(watershed_area_m2,
                wetland_storage_volume_m3,
                perimeter_m,area_m2,p_a_ratio,
                hand_m,mean_elevation_m,wet_order) 

#zscore the constraint data as prep for vector rotation
Z_site <- standardize(site_clean,
                      centerFun = mean,
                      scaleFun = sd)

#the number of observations we have
n <- nrow(Z_site)
#correlation matrix for X vals
COR_C <- (1/(n-1))*(t(Z_site)%*%as.matrix(Z_site))

#evaluate the inverse of the correlation matix (1/COR_C) {matrix inverse}
INV <- Matpow(COR_C,-1) 

#select the right diagonal of your inverse 
#These are VIF values
VIF <- diag(INV)
VIF

#drop Storage Volume first
Z_site2 <- Z_site %>% dplyr::select(-c(wetland_storage_volume_m3))

COR_C <- (1/(n-1))*(t(as.matrix(Z_site2))%*%as.matrix(Z_site2))
INV <- Matpow(COR_C,-1)  
VIF <- diag(INV)
VIF 

#drop area
Z_site2 <- Z_site %>% dplyr::select(-c(wetland_storage_volume_m3,area_m2))

COR_C <- (1/(n-1))*(t(as.matrix(Z_site2))%*%as.matrix(Z_site2))
INV <- Matpow(COR_C,-1)  
VIF <- diag(INV)
VIF 

#drop watershed area
Z_site2 <- Z_site %>% dplyr::select(-c(wetland_storage_volume_m3,area_m2,watershed_area_m2))

COR_C <- (1/(n-1))*(t(as.matrix(Z_site2))%*%as.matrix(Z_site2))
INV <- Matpow(COR_C,-1)  
VIF <- diag(INV)
VIF #good to go

#do MLR fits for each dependent var using all independent variables from cdata and an intercept
#need to narrow water chemistry data down to one observation per site so row numbers match
chem <- sw_all %>% filter(Type == "Wetland SW") %>% 
  group_by(Site) %>% drop_na() %>% 
  summarize(Mean_Cl = mean(Cl_mg_L,na.rm=TRUE),
            Mean_SO4 = mean(SO4_mg_L,na.rm=TRUE),
            Mean_NPOC = mean(NPOC_mgC_L,na.rm=TRUE),
            Mean_d18O = mean(d18O_VSMOW,na.rm=TRUE),
            Mean_TDN = mean(TDN_mgN_L,na.rm=TRUE),
            Mean_TDP = mean(TDP_mgP_L,na.rm=TRUE),
            Mean_Na = mean(Na_ppb,na.rm=TRUE),
            Mean_Mg = mean(Mg_ppb,na.rm=TRUE),
            Mean_Al = mean(Al_ppb,na.rm=TRUE),
            Mean_Si = mean(Si_ppb,na.rm=TRUE),
            Mean_Ca = mean(Ca_ppb,na.rm=TRUE),
            Mean_Mn = mean(Mn_ppb,na.rm=TRUE),
            Mean_FI = mean(FI,na.rm=TRUE),
            Mean_BIX = mean(BIX,na.rm=TRUE),
            Mean_oHIX = mean(oHIX,na.rm=TRUE),
            Mean_abs254 = mean(abs254_m,na.rm=TRUE),
            Mean_e2e3 = mean(e2e3,na.rm=TRUE),
            Mean_s275_295 = mean(s275_295,na.rm=TRUE)) %>% 
  dplyr::select(-c(Site))

chem_names <- colnames(chem)

#standardize chem data
Zchem <- standardize(chem,centerFun = mean,scaleFun = sd)

z <- ncol(Z_site2)+1 #number of betas
B <- matrix(NA,z,ncol(Zchem)) #column of model outputs

#data and constraints = MASTER
MASTER <- as.data.frame(cbind(Zchem,Z_site2))

for (i in 1:ncol(Zchem)){
  FIT = lm(MASTER[,i] ~ MASTER$perimeter_m+MASTER$p_a_ratio+
             MASTER$hand_m+MASTER$mean_elevation_m+
             MASTER$wet_order) #fit each dependent variable to our independent variables
  B[,i] = FIT$coefficients
}

#use beta values to predict ZMDATA 
#(y = mx+b)
PREDICTED <- matrix(NA,n,ncol(Zchem))

for (i in 1:ncol(Zchem)){
  PREDICTED[,i] = B[1,i]+ B[2,i]*Z_site2[,1] + 
    B[3,i]*Z_site2[,2] + B[4,i]*Z_site2[,3]+ 
    B[5,i]*Z_site2[,4]+B[6,i]*Z_site2[,5]
}

#perform PCA on the zscored, predicted values
PREDICTED <- as.data.frame(PREDICTED)
colnames(PREDICTED) <- chem_names

#standardize predictions
CDATA_PRED <- standardize(PREDICTED)

PCfit <- PCA(CDATA_PRED,graph = FALSE,ncp = 7)
L <- PCfit$eig[,1] #EIVENVALS
M <- PCfit$svd$V #MODES
S <- as.matrix(CDATA_PRED)%*%as.matrix(M) #SCORES

#correlate fitted PC scores for mode 1 and 2 and the environmental data that 
#generated them and weighting those correlations by the square root of the %VE for each mode
Cmat <- matrix(NA,ncol(Z_site2),2)

for (i in 1:(z-1)){
  c1 = sqrt(L[1]/sum(L)) #VE Correction for PC1
  c2 = sqrt(L[2]/sum(L)) #VE Correction for PC2
  Cmat[i,1:2] = c(cor(Z_site2[,i],S[,1])/c1, #correlation between constraints and scores divided by weighting factor
                  cor(Z_site2[,i],S[,2])/c2)
}

#plot your results (pc modes and scores and environmental vectors Cmat) using a biplot
Snames <- c('Perimeter','P_A_Ratio','HAND','Mean Elevation','Wetland Order')
Cmat <- as.data.frame(Cmat)
rownames(Cmat) <- Snames

p2 <- fviz_pca_biplot(PCfit,repel = TRUE,title="Constrained PCA")
fviz_add(p2,as.data.frame(Cmat*4),
         geom = "arrow",
         repel = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Linear Regression -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#edit date format
synoptic$Date <- ymd(synoptic$Date)

water <- water %>% rename(
  Sample_Name = Site_ID)

#match water level on date of sampling to synoptic data
full <- left_join(synoptic,water,by=c("Date","Sample_Name"))

#parse to variables for prediction
data <- full %>% filter(Type %in% c("Wetland SW", "Wetland GW")) %>% 
  dplyr::select(Date,Site,Sample_Name,SW_GW,NPOC_mgC_L,FI,oHIX,BIX,dly_mean_wtrlvl)

#visualize data
#NPOC vs Water Level
data %>% 
  ggplot(aes(dly_mean_wtrlvl,NPOC_mgC_L,col=SW_GW))+
  geom_point()

#FI vs Water Level
data %>%  
  ggplot(aes(dly_mean_wtrlvl,FI,col=SW_GW))+
  geom_point()

#BIX vs Water Level
data %>%  
  ggplot(aes(dly_mean_wtrlvl,BIX,col=SW_GW))+
  geom_point()

#Think focusing on GW will be best for determining predictive relationships
GW <- data %>% filter(SW_GW == "GW") %>% 
  dplyr::select(NPOC_mgC_L,dly_mean_wtrlvl,FI) %>% drop_na
SW <- data %>% filter(SW_GW == "SW") %>% 
  dplyr::select(NPOC_mgC_L,dly_mean_wtrlvl,FI) %>% drop_na


## 4.1 DOC vs Water Level --------------------------------------------

### 4.1.1 Regression ------------------------------------
#define x and y variables
DOC <- GW$NPOC_mgC_L #y
water_level <- GW$dly_mean_wtrlvl #x

#define SE function
#BETA is a vector containing our intercept (first value)
#and islope (second value)
bfline = function (BETA) {(BETA[1] + BETA[2]*water_level)}
SE = function (BETA) {sum((DOC - bfline(BETA))^2)}

STRUCT = fminsearch(SE,c(1,10)) 
#this will give you B1 and B0

B = STRUCT$xmin #optimal B values (slope,intercept)
B

Y = B[1] + B[2]*water_level
#or Y2 = bfline(B)
plot(water_level,DOC,
     col = "black",pch = 19,
     xlab = 'Mean Water Level (m)',
     ylab = 'GW DOC (mg C/L)')
#new bit
lines(water_level,Y,col = "red")

#do this as a quick code
#Quick Codes
MODEL1 <- lm(GW$NPOC_mgC_L~GW$dly_mean_wtrlvl)
summary(MODEL1)
Bval_model1 <- MODEL1$coefficients
R2_model1 <- summary(MODEL1)$r.squared

#estimate analytical 95% CI bounds for the regression at different values of xi 

#STEP1: estimate the standard deviation of the residuals
n = length(GW$dly_mean_wtrlvl)
s = sqrt( 1/(n-2) * SE(Bval_model1))

#STEP2: find T025 and T975 (T stat at probability 0.025 
#and 0.975) for n-2 dof by evaluating the inverse cdf
#of the student's T distribution
T025 = qt(0.025,n-2)
T975 = qt(0.975,n-2)

#STEP3: evaluate CI bounds bottom two lines are correction for less 
#confidence in slope as we move away from the mean of x
CI975 = bfline(Bval_model1) + T975*s*sqrt( (1/n) 
                                           + ((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2 / 
                                                (sum((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2))))
CI025 = bfline(Bval_model1) + T025*s*sqrt( (1/n) 
                                           + ((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2 / 
                                                (sum((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2))))

CI_MAT = cbind(GW$dly_mean_wtrlvl,CI025,CI975)

#sorts matrix by smallest to largest FINES
CI_MATsorted = sortrows(CI_MAT,1) 

DOC = Bval_model1[1] + Bval_model1[2]*GW$dly_mean_wtrlvl
#or Y2 = bfline(B)
plot(GW$dly_mean_wtrlvl,GW$NPOC_mgC_L,
     col = "black",pch = 19,
     xlab = 'Groundwater Level (m)',
     ylab = 'DOC (mg C / L)',
     ylim = c(-5,60))
#new bit
lines(GW$dly_mean_wtrlvl,DOC,col = "red")
lines(CI_MATsorted[,1],CI_MATsorted[,2],col = "red",
      lty = "dashed")
lines(CI_MATsorted[,1],CI_MATsorted[,3],col = "red",
      lty = "dashed")

### 4.1.2 Bootstrapping correlation ----------------------------------
GW <- data %>% filter(SW_GW == "GW") %>% 
  dplyr::select(NPOC_mgC_L,dly_mean_wtrlvl) %>% drop_na

X <- as.matrix(GW$dly_mean_wtrlvl)
Y <- as.matrix(GW$NPOC_mgC_L)

#empty matricies
CIup = matrix(NA,nrow = ncol(X)) 
CIlow = matrix(NA,nrow = ncol(X)) 
COR = matrix(NA,nrow = ncol(X)) 

#set seed 
set.seed(100) 
#set bootstrap number 
N = 10000 
#assume your dependent variable is Y 
#and your matrix of predictors is X (if you only have 1 predictor this still works) 
for (i in 1:ncol(X)) { 
  MAT = cbind(X[,i],Y) 
  BOOTCOR = boot(data = GW, statistic = function(data, i) {cor(data[i, 1], data[i, 2], method = "pearson")}, R = N) 
  CIs = boot.ci(BOOTCOR, type = c("bca")) 
  CIlow[i,] = CIs$bca[4] 
  CIup[i,] = CIs$bca[5] 
  COR[i,]=cor(X[,i],Y) } 

#correlation, lowerbound, upper bound CORMAT 
#If you want the full series of bootstrapped correlations
#you can get that from the BOOTCOR output. 
CORMAT = cbind(COR,CIlow,CIup)

## 4.2 FI vs GW level -----------------------

### 4.2.1 Regression --------------------
MODEL2 <- lm(GW$FI~GW$dly_mean_wtrlvl)
summary(MODEL2)
Bval_model2 <- MODEL2$coefficients
R2_model2 <- summary(MODEL2)$r.squared

FI = Bval_model2[1] + Bval_model2[2]*GW$dly_mean_wtrlvl
#or Y2 = bfline(B)
plot(GW$dly_mean_wtrlvl,GW$FI,
     col = "black",pch = 19,
     xlab = 'Mean Water Level (m)',
     ylab = 'GW FI')
#new bit
lines(GW$dly_mean_wtrlvl,FI,col = "red")

#estimate analytical 95% CI bounds for the regression at different values of xi 

#STEP1: estimate the standard deviation of the residuals
n = length(GW$dly_mean_wtrlvl)
s = sqrt( 1/(n-2) * SE(Bval_model2) )

#STEP2: find T025 and T975 (T stat at probability 0.025 
#and 0.975) for n-2 dof by evaluating the inverse cdf
#of the student's T distribution
T025 = qt(0.025,n-2)
T975 = qt(0.975,n-2)

#STEP3: evaluate CI bounds bottom two lines are correction for less 
#confidence in slope as we move away from the mean of x
CI975 = bfline(Bval_model2) + T975*s*sqrt( (1/n) 
                                           + ((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2 / 
                                                (sum((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2))))
CI025 = bfline(Bval_model2) + T025*s*sqrt( (1/n) 
                                           + ((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2 / 
                                                (sum((GW$dly_mean_wtrlvl-mean(GW$dly_mean_wtrlvl))^2))))

CI_MAT = cbind(GW$dly_mean_wtrlvl,CI025,CI975)

#sorts matrix by smallest to largest FINES
CI_MATsorted = sortrows(CI_MAT,1) 

FI = Bval_model2[1] + Bval_model2[2]*GW$dly_mean_wtrlvl
#or Y2 = bfline(B)
plot(GW$dly_mean_wtrlvl,GW$FI,
     col = "black",pch = 19,
     xlab = 'Groundwater Level (m)',
     ylab = 'GW FI',
     ylim = c(-1,4))
#new bit
lines(GW$dly_mean_wtrlvl,FI,col = "red")
lines(CI_MATsorted[,1],CI_MATsorted[,2],col = "red",
      lty = "dashed")
lines(CI_MATsorted[,1],CI_MATsorted[,3],col = "red",
      lty = "dashed")

### 4.2.2 Bootstrapping correlation ----------------------------------
GW <- data %>% filter(SW_GW == "GW") %>% 
  dplyr::select(FI,dly_mean_wtrlvl) %>% drop_na

X <- as.matrix(GW$dly_mean_wtrlvl)
Y <- as.matrix(GW$FI)

#empty matricies
CIup = matrix(NA,nrow = ncol(X)) 
CIlow = matrix(NA,nrow = ncol(X)) 
COR = matrix(NA,nrow = ncol(X)) 

#set seed 
set.seed(100) 
#set bootstrap number 
N = 10000 
#assume your dependent variable is Y 
#and your matrix of predictors is X (if you only have 1 predictor this still works) 
for (i in 1:ncol(X)) { 
  MAT = cbind(X[,i],Y) 
  BOOTCOR = boot(data = GW, statistic = function(data, i) {cor(data[i, 1], data[i, 2], method = "pearson")}, R = N) 
  CIs = boot.ci(BOOTCOR, type = c("bca")) 
  CIlow[i,] = CIs$bca[4] 
  CIup[i,] = CIs$bca[5] 
  COR[i,]=cor(X[,i],Y) } 

#correlation, lowerbound, upper bound CORMAT 
#If you want the full series of bootstrapped correlations
#you can get that from the BOOTCOR output. 
CORMAT = cbind(COR,CIlow,CIup)

