#This code is intended to produce maps of the current and future An stephensi
#suitability for Pf and Pv malaria 
#Sadie J. Ryan
#July, 2022

setwd("YOURWORKINGDIRECTORY") #Change this to be the right folder (this one) 

#Load up a bunch of the packages we will need 
# library calls the packages - if they're not installed locally on the computer, you can install them
#You may get messages about them being retired soon, but for now they should still work

library(raster)
library(rgdal)
library(maptools)
library(ncdf4)
library(sf)
library(rgeos)


#FOR NOW, USE THE TEMP FOLDER
rasterOptions(tmpdir='YOUR TEMP DIRECTORY') #if you want to point this at your dumpster folder, do that, otherwise this will work on the R drive

##########################################################################################
##CURRENT temperature data (WorldClim)
#Need to use version 1.4 because future scenerios are based on it, for future comparisons

#Mean temperature data
y <- getData('worldclim', var='tmean', res=5)
yy<-y*0.1

plot(yy) #here's a nice map of the data for you - it will show you 12 panels
#each represents a monthly average temperature for the whole globe

plot(yy$tmean1) #This will plot just January mean temperature

##############################################################
#CLIMATE LAYER CRUNCHER MACHINE HERE
##############################################################

xx<-yy

#########################################
#Anopheles stephensi and Plasmodium falciparum
#Transmission suitability boundaries from Villena et al paper
#AS_PF
#R0>0 at 97.5
#16.0-36.5
a<-xx
a[a<16.0]<-NA
a[a>36.5]<-NA


#Turning it into 0,1s 
aa<-a
aa[aa>0]<-1


#Adding up months in the year for persistence
sum_aa<-sum(aa, na.rm=TRUE)

#If you want to see it on a map here in R, 
plot(sum_aa) #this will plot the numbers of months that transmission is suitable

#Save the sums
writeRaster(sum_aa, filename="ASPFSUM.tif", format="GTiff", overwrite=TRUE)

###more mapping opporunity here####
#Here, you can re-import the data just written to the folder (take the # away and run the line)
ASPFCUR<-raster("ASPFSUM.tif")

#Then you can plot it to see it's the same thing
plot(ASPFCUR)

#------------------------------------------
#Anopheles stephensi and Plasmodium vivax
#AS_PV
#R0>0 at 97.5
#16.6 - 31.7
b<-xx
b[b<16.6]<-NA
b[b>31.7]<-NA


#Turning it into 0,1s 
bb<-b
bb[bb>0]<-1


#Adding up months in the year for persistence
sum_bb<-sum(bb, na.rm=TRUE)

#If you want to see it on a map here in R, 
plot(sum_bb)

#Save the sums
writeRaster(sum_bb, filename="ASPVSUM.tif", format="GTiff", overwrite=TRUE)
ASPVCUR<-raster("ASPVSUM.tif")

#################################################
#FUTURE CLIMATE CRUNCHER
################################################
#Checklist
#Futures monthly means data 
#You will need to create a directory of the future scenarios, with your naming
#convention of choice


##Temp data futures

meanlist<-c("/rcp45/2030/HE",
            "/rcp45/2050/HE",
            "/rcp45/2080/HE",
            "/rcp85/2030/HE",
            "/rcp85/2050/HE",
            "/rcp85/2080/HE",
            "/rcp45/2030/HD",
            "/rcp45/2050/HD",
            "/rcp45/2080/HD",
            "/rcp85/2030/HD",
            "/rcp85/2050/HD",
            "/rcp85/2080/HD",
            "/rcp45/2030/CC",
            "/rcp45/2050/CC",
            "/rcp45/2080/CC",
            "/rcp85/2030/CC",
            "/rcp85/2050/CC",
            "/rcp85/2080/CC",
            "/rcp45/2050/BC",
            "/rcp45/2030/BC",
            "/rcp45/2080/BC",
            "/rcp85/2030/BC",
            "/rcp85/2050/BC",
            "/rcp85/2080/BC")

#################################################

for (i in 1:length(meanlist)) {
  
  #Generic temperature data stack creations
  #Here, choose the year, RCP, and model, put it in the filepath
  
  meanT  <- list.files(path=meanlist[i], pattern=".asc",full.names=TRUE)
  
  ##Create raster stack of mean temp layers
  meanT_Stack<- stack()
  for(j in 1:NROW(meanT)){
    meanT1 = raster(meanT[j])
    meanT_Stack = stack(meanT_Stack, meanT1)
  }
  
  xx<-meanT_Stack*0.1

  #AS_PF
  #R0>0 at 97.5
  #16.0-36.5
  c<-xx
  c[c<16.0]<-NA
  c[c>36.5]<-NA
  
  
  #Turning it into 0,1s 
  cc<-c
  cc[cc>0]<-1
  
  #AS_PV
  #R0>0 at 97.5
  #16.6 - 31.7
  d<-xx
  d[d<16.6]<-NA
  d[d>31.7]<-NA


  dd<-d
  dd[dd>0]<-1

 
  #Save the bricks
  #Filename convention is climate model - year (50/80)- Bayesian probability level - 'ae' or 'al'
  writeRaster(cc, filename=paste(gsub('/','_',gsub('/','',meanlist[i])),"StPfLCI.tif",sep='_'), options="INTERLEAVE=BAND", overwrite=TRUE)
  writeRaster(dd, filename=paste(gsub('/','_',gsub('/','',meanlist[i])),"StPvLCI.tif",sep='_'), options="INTERLEAVE=BAND", overwrite=TRUE)
   
  
  #Adding up months in the year for persistence
  sum_cc<-sum(cc, na.rm=TRUE)
  sum_dd<-sum(dd, na.rm=TRUE)

  #Save the sums (You'll need to sort out your own file naming conventions)
  
  writeRaster(sum_cc, filename=paste(gsub('/','_',gsub('/','',meanlist[i])),"StPfLCI_sum.tif",sep='_'), options="INTERLEAVE=BAND", overwrite=TRUE)
  writeRaster(sum_dd, filename=paste(gsub('/','_',gsub('/','',meanlist[i])),"StPvLCI_sum.tif",sep='_'), options="INTERLEAVE=BAND", overwrite=TRUE)
  
  print(i)
  
}


####Sadie messing around with some plotting 09/15/22
library(rasterVis)

aspfs<-raster("ASPFSUM.tif")
aspf_4530_HE<-raster("rcp45_2030_HE_StPfLCI_sum.tif")
aspf_4550_HE<-raster("rcp45_2050_HE_StPfLCI_sum.tif")

aspf_stack<-stack(aspfs, aspf_4530_HE, aspf_4550_HE)
names(aspf_stack)<-c("Baseline","RCP4.5_2030","RCP4.5_2050")

myTheme<-plasmaTheme

levelplot(aspfs, par.settings= myTheme, margin=F)



levelplot(aspf_stack, par.settings=BuRdTheme, margin=F)

tiff('aspf_BuRd.tif',  units='in', width=8, height=8, res=300, compression = 'lzw')
levelplot(aspf_stack, par.settings=BuRdTheme, margin=F)
dev.off()

jpeg("aspf_BuRd.jpeg", width = 8, height = 8, units = 'in', res = 300)
levelplot(aspf_stack, par.settings=BuRdTheme, margin=F)
dev.off()
