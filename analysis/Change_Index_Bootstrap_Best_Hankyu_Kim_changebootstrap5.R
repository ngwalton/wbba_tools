#This is an archived version of code primarily authored by Hankyu Kim to run Wisconsin Breeding Bird Atlas II means and bootstrap
#confidence intervals for the change index comparing distributions from atlas 1 to atlas 2 for each species.

#The Change index is based on:
#Herrando, S., P. Milanesi, L. Brotons, M. Franch, H.-G. Bauer, D. Villero, A. Morán-Ordóñez, M. Kéry, P. Voříšek, and V. Keller
#(2020). Methods. In European Breeding Bird Atlas 2. European Bird Census Council & Lynx Editions, Barcelona, pp. 23–39.

#This version archived by Nicholas M. Anich 11/10/26.

#This process creates bootstrapped confidence intervals for the WBBA Change Map
#Run this first, then it exports a csv file which you can feed to the change map code.

### change index confidence interval bootstrapping with logistic regression
### for bootstrapping, we need 'samples' and each blocks are our samples. So I rearranged the ebird data with only observations
### into a detection history table (0/1) at each blocks for WBBA period 1 and 2 first, then calculated 'statistics' over them (mean naive occupancy)
### using logistic regression. We do not necessarily need to do logistic regression, and just calculate mean 'naive occupancy' for period 1 and 2,
### and bootstrap that as well, but I believe the mathematics are the same in a way estimate the means.

## load packages and set wd
#################################################################################
library(sf)    # for loading shapefiles (block data)
library(dplyr) # for handling data
library(here)     # for loading files
library(purrr)
library(tidyr)

#setwd(here::here("C:\\Users\\hkim969\\Desktop\\WIBBA\\wbba_change_index_boot")) # set wd

## load bird observation and block data 
#################################################################################
# read in the atlas data; each atlas project will be a list within a list

sp <- list(i = read.delim("WBBA1_ATLASWI_plussensitivepluszerocount_onlycodeds_nospuhslash_EBDMar2023_GOOD_082823.txt", quote = ""),
           ii = read.delim("WBBA2_ATLASWI_plussensitivepluszerocount_EBDMar2023_GOOD_092526.txt", quote = ""))

# fix column names
sp <- sp %>% 
  # capitalize
  map(~dplyr::rename_with(.x, toupper)) %>% 
  # underscores become periods
  map(function(y) {
    colnames(y) <- gsub("\\_", "\\.", colnames(y))
    return(y)
  })

## only keep the pertinent columns
sp <- sp %>% 
  map(dplyr::select, 
      CATEGORY, 
      COMMON.NAME,         
      BREEDING.CODE,        
      BREEDING.CATEGORY,    
      BLOCK_ID = ATLAS.BLOCK, # this needed a rename
      APPROVED
  )


# Read in your 858 fair comparison blocks
fair <- st_read(dsn = "fair", layer = "Fair_comparison_blocks_final") %>%
  dplyr::select(BLOCK_ID, BLOCK_NAME, BLOCK_STAT)

# Drop non-fair block data from sp
sp <- sp %>% 
  map(~filter(.x, BLOCK_ID %in% fair$BLOCK_ID))

gc() 

glimpse(sp) # see an overview of the datbase
  
### filter out C1 and empty
sp$ii<-sp$ii%>%filter(!BREEDING.CATEGORY%in%c("C1","",NA))
sp$i<-sp$i%>%filter(!BREEDING.CATEGORY%in%c("C1","",NA))

glimpse(sp) # see an overview of the dataset


## Rearrange observation data into detection history and make it into a long format for regression
#################################################################################
split2species<-function(x){split(x,f=x$COMMON.NAME)}  # set function name, to split the data into list with dataframes per each species

sp_split<-lapply(sp,split2species) # list apply split

# make detection history table - we need a table with each row are unique sites 
makeDH<-function(x){lapply(x,function(y){a<-y%>%as.data.frame()%>%
  mutate(occu=ifelse(is.na(COMMON.NAME),0,1))%>% #replace species name with 1 and 0 (0s somehow left as NAs so we treat 2 rows below)
  group_by(BLOCK_ID)%>% # group by blocks to summarize and remove duplicate rows 
  summarize(occu=max(occu,na.rm=T))%>%mutate(occu=replace_na(occu,0)) # summarize it to 0,1 and replace NA with 0
z<-left_join(fair,a,by="BLOCK_ID",copy=T) # join with block ID to just work with the fair blocks
z["occu"][is.na(z["occu"])]<-0 # replace NA with 0
return(z)})
}

sp_DH<-lapply(sp_split,makeDH)  # list apply above function to all species


### Now we will pivot data into long format and 
##########################################################################################################
species<-unique(names(sp_split$ii),names(sp_split$i)) # make a character vector of species names that we will refer on to.

## we will prep each species data into a long format for running regression
dhl<-list()

# NOTE THAT THE NUMBER 858 IS THE NUMBER OF FAIR COMPARISON BLOCKS - CHANGE THIS MANUALLY IF NEEDED

for(i in 1:length(species)){
  dhl[[species[i]]]<-data.frame(occu1=rep(0,858),
                                occu2=rep(0,858),
                                species=species[i])## make a blank dataset with 0s
  ## if else statement for putting in atlas 1 data
  if(!is.null(sp_DH[["i"]][[species[i]]][["occu"]])){
    dhl[[species[i]]]["occu1"]=sp_DH[["i"]][[species[i]]][["occu"]]}
  else{dhl[[species[i]]]["occu1"]<-0}
  
  if(!is.null(sp_DH[["ii"]][[species[i]]][["occu"]])){
    dhl[[species[i]]]["occu2"]=sp_DH[["ii"]][[species[i]]][["occu"]]}
  else{dhl[[species[i]]]["occu2"]<-0}
  
  dhl[[species[i]]]<-dhl[[species[i]]]%>%pivot_longer(cols=starts_with("occu"),names_to = "period")}



### run a bootstrapped logistic regression to calculate mean naive occupancy of each species at wbba1 and wbba2 periods
##############################################################################################################
### We will calcuate a bootstrap resample of random (resample size) permutation bootstapping


#### double for-loop for bootstrapping for each species in the list 
boot_estimates<-function(nboot=100000,species=species,datalist=dhl,seed=123){ ## nboot is set to 10^4 but can increase
  start<-Sys.time() # record start time
  bootest<-list() ## make a blank list for filling for loop
  ### species level for loop
  for(i in 1:length(species)){
    (M1<-glm(value~period,family="binomial",data=dhl[[species[i]]])) # logistic regression
    
    bres <- matrix(NA,nrow=nboot,                    # make a blank matrix to fill bootstraped estimates
                   ncol=length(coef(M1))+1,
                   dimnames=list(rep=seq(nboot),
                                 coef=c(names(coef(M1)),"converged")))## coefficient name and converged or not as a column
    
    set.seed(seed)
    ### bootstap function for loop - this will return object bres with bootstraped data in it.
    for (j in seq(nboot)) {   
      bdat <- dhl[[species[i]]][sample(nrow(dhl[[species[i]]]),replace=TRUE),] ## resample
      bfit <- update(M1, data=bdat)  ## refit with new data
      bres[j,] <- c(coef(bfit),bfit[["converged"]]) # extract and save coefficients & convergence
    }
    
    bootest[[species[i]]]<-data.frame(bres) ## save bootstaped estimates for each species
  }
  print(paste(Sys.time()-start, ": time spent running")) #print time spent 
  return(bootest)# return boot estimates for each species
}



### since you have a function now, you need to run it to get bootstrapped estimates and assign output to an object
bootout<-boot_estimates(nboot=1000,species=species,datalist=dhl,seed=123) ## run the function

(warnings)
## for species with very high or low prevalence in both periods, 
## drawn bootstrap samples can be all 0 or 1, leading to model convergence failure.
## few failures are fine as the distribution can be drawn from the rest of the bootstrap samples



#### calculate derived estimate (change index) and its bootstrapped confidence interval
########################################################################################################
derive_change_index<-function(x, alpha=0.05){ ## change alpha level here
  x<-x[x[["converged"]]==T,] ## remove models that did not converge
  x["occu2"]<-(x[1]+x[2]) ## get wbba2 mean naive occupancy
  change_index<-(plogis(x[["occu2"]])-plogis(x[[1]]))/(plogis(x[["occu2"]])+plogis(x[[1]]))*100 ##calculate change index
  mean_change<-mean(change_index) ## mean estimate of change index from the bootstrapped samples
  ### this is CI based on bootstrap distribution (profile) -this is more appropriate
  confint<-quantile(change_index,c(alpha/2,1-(alpha/2)))  ## confidence interval of change index from the bootstrapped samples
  
  # Error according to t distribution
  #error <- qt((alpha)/2, df = length(change_index) - 2) * sd(change_index) / sqrt(length(change_index))
  # Error according to normal distribution
  #error <- qnorm((alpha)/2) * sd(change_index) / sqrt(length(change_index))
  # Confidence interval as a vector - this is CI assuming normal distribution
  #result <- c("lower" = mean_change - error, "upper" = mean_change + error)
  df<-data.frame(mean=mean_change, lower_q=confint[1],upper_q=confint[2])
  #, lower_n=result[1],upper_n=result[2]) ## put them in data frame
  return(df)
}

### extract and put them on a data frame
###############################################################################################
(summary<-data.frame(do.call(rbind,lapply(bootout,derive_change_index))))


### export this file


### add species column name
mydf <- cbind(rownames(summary), summary)
rownames(mydf) <- NULL
colnames(mydf) <- c("COMMON.NAME","Mean","LowerCI","UpperCI") # Where mean is the bootstrapped mean change index, lower CI is 2.5% and upper CI is 97.5%

write.csv(mydf, file= "110826_bootstrapped change_version3.csv", row.names=FALSE)

