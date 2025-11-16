pkgs <- c(
  "RKaggle","factoextra","ggplot2","ggfortify","cluster","ggpubr","klaR","dbscan"
)
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install, quiet = TRUE)


library(RKaggle)
library(factoextra)
library(ggplot2)
library(ggfortify)
library(cluster)
library(ggpubr)
library(klaR)
library(dbscan)
 

data<-RKaggle::get_dataset("urvishahir/electric-vehicle-specifications-dataset-2025")



numeric_data<-data[sapply(data,is.numeric)]#Leaving only numeric data for calculations of descriptive statistics

cov(numeric_data, use = "complete.obs") #Calculate variance covariace matrix dropping NA's

cor(numeric_data,use="complete.obs") ##Calculate correlation matrix dropping NA's

##First we have to run a diagnostic test to determine if we can do the clustering in the first place. 

##I use hopkins statistic to for this.

get_clust_tendency(numeric_data,ceiling(nrow(numeric_data)/10)) #Hopkins statistic is 0.94 this means that there exist meaningful clusters.

#Now lets use principal components.

numeric_data<-na.omit(cbind(numeric_data,data["brand"])) #add brand back so we can color by brand


numeric_data[1:(length(colnames(numeric_data))-2)] #We want to select only numeric data

cars.pca<-prcomp(numeric_data[1:(length(colnames(numeric_data))-2)],center=T,scale.=T) #compute principal components


cars.pca.plot<-autoplot(cars.pca,data=na.omit(numeric_data),colour="brand") #Plot principal components along with labels



Principal_component_data<-cars.pca$x[,1:2] #Select first 2 principal components for plotting


get_clust_tendency(Principal_component_data,ceiling(nrow(Principal_component_data)/10))  #Just checking again to make sure (Hopkins statistic is 0.86 meaningful clusters exist)

##Now lets use the elbow method to get how many clusters we need

fviz_nbclust(Principal_component_data, kmeans, method = "wss")

fviz_nbclust(Principal_component_data,kmeans,method="silhouette")

##Both of these methods indicate 3-4 splits lets use 3 splits.

km.res<-kmeans(Principal_component_data,3) ##We perform k means clustering 
plot_1<-fviz_cluster(km.res,geom="point",data=Principal_component_data)
plot_1



  
agglo.clust <- agnes(Principal_component_data,method="complete",metric = "euclidean") ##Agglomerative hierarchical clustering 

sub1<-cutree(agglo.clust,k=3) #Cut tree at 3 clusters
 

Plot_1_hierarchical <- fviz_cluster(list(data = Principal_component_data, cluster = sub1))
Plot_1_hierarchical ##Plotting clustering 


##Checking the adequacy of silhoutte.


mean(silhouette(sub1,dist = dist(Principal_component_data))[,3]) ##Mean Silhoute coefficient for kmeans

mean(silhouette(km.res$cluster,dist=dist(Principal_component_data))[,3]) ## Mean silhoutte coefficient for hierarchical


sil=silhouette(sub1,dist = dist(Principal_component_data))[,3]
mean(sapply(sil, function(x) x<0 ))*100


##Checking Optics algorithm. 



op1<-optics(Principal_component_data,eps=0.7,minPts=5)
res_op <- extractDBSCAN(op1, eps_cl =0.8)

plot(res_op)
hullplot(Principal_component_data, res_op)
