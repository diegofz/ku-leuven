#Multivariate Assignment 1
##########################

#import
load("C:/Users/Diego/Google Drive/KUL/1_sem/Multivar_stats/Assignment1/wvs.Rdata")


#Exercise 1. 1st Part
#####################

#set up
n<-dim(wvs)[1]
names(wvs)
countries<-unique(wvs$country)

  wvs_sd<-scale(wvs[1:10], center = TRUE, scale= TRUE) #standardize for exercise
  wvs_bycountry<-aggregate(wvs_sd,list(wvs$country),mean)
  wvs_1<-wvs_bycountry[-1]
  rownames(wvs_1)<-unique(wvs$country) 
  wvs_sd2<-scale(wvs_1, center = TRUE, scale= TRUE) #standardize for pca
  
  wvs_sample<-wvs[1:10] #for Horn's sampling

 
#PCA
  wvs_pca<-prcomp(wvs_sd2)
  eigenvalues<-wvs_pca$sdev^2
  prop_var<-wvs_pca$sdev^2/10 #p=10 
  
  A<-wvs_pca$rotation%*%diag(wvs_pca$sdev) #matrix of component loadings
  diag(A%*%t(A)) #confirm all 1s

#plot of component loadings (corr.)
  par(mfrow=c(2,1))
  plot(A[,1:2], xlim = c(-1,1.2), ylim = c(-1,1), xlab="PC1", ylab="PC2", main="Corr.")
  pointLabel(A[,1], A[,2], names(wvs[1:10]))

#plot of Z
  Z<-wvs_sd2%*%wvs_pca$rotation%*%diag(1/wvs_pca$sdev)
  plot(Z[,1:2], xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), xlab="PC1", ylab="PC2",main="Z values")
  pointLabel(Z[,1], Z[,2], countries)

#biplot
  par(mfrow=c(1,1))
  biplot(wvs_pca,pc.biplot=TRUE,xlim=c(-3,3), ylim=c(-3,3))

#Kaiser and Screeplot
  par(mfrow=c(1,1))
  screeplot(wvs_pca, type = "lines", ylim=c(0,6))
  abline(h=1, col="red")

#Horn for n_ci samples
n_ci=25
CI<-matrix(rep(0,10*n_ci), ncol = 10) #multiple sampling to plot a CI

  for(p in 1:n_ci){
    
  #Horn for sample i
  bootmat<-matrix(rep(0,n*10), ncol=10)
      for(i in 1:10){
        samp_h<-sample(seq(1,n),size=n, replace=TRUE)
        bootmat[,i]<-wvs_sample[samp_h,i] #sampling has to be from original data?
      }
  wvs_sample_sd<-scale(bootmat, center = TRUE, scale= TRUE) #standardize for exercise
  wvs_sample_sd2<-aggregate(wvs_sample_sd,list(wvs$country),mean)
  wvs_sample_sd2<-wvs_sample_sd2[-1]
  rownames(wvs_sample_sd2)<-unique(wvs$country) 
  wvs_boot<-scale(wvs_sample_sd2, center = TRUE, scale= TRUE) #standardize for pca
  
  wvs_bootpca<-prcomp(wvs_boot)
  eigen_boot<-wvs_bootpca$sdev^2
  CI[p,]<-eigen_boot
  
  }
  
par(mfrow=c(2,1))  
plot(c(1:10),wvs_pca$sdev^2, col="blue", type="b",xlab="component",
     ylab="eigenvalue",main="Elbow vs. Kaiser vs. Horn for 1 sample")
lines(c(1:10),wvs_bootpca$sdev^2, col="red", type = "b")
abline(h=1, col="green", lty="dashed")
legend(7,5,c("wvs original","wvs bootstrapped"),lty = c(1,1), col=c("blue", "red"))

boxplot(CI,xlab="component",ylab="eigenvalue", main="Horn for 25 samples")
abline(h=1, col="green", lty="dashed")


#Exercise 1. 2nd Part
#####################