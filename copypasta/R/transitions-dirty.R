## Megan Cattau
## Earth Lab, Project Forest
## Contact info: megan.cattau@gmail.com or megan.cattau@colorado.edu, 706.338.9436
## Project: Disturbance Interactions in the Southern Rockies
## Project overview: Forest transitions (i.e., Changes in ecosystem type / forest composition and structure) as a function of disturbance history across the Southern Rockies

# This code addresses:
# Q1: When is forest 'recovered'?
# 	a. At how many years sonce fire does post-fire VCF Resemble pre-fire VCF?
# 	b. Does this vary as a function of pre-fire MPB infestation?
#	c. Does this vary as a function of pre-fire VCF?
# Q2: Is a transition more likely to occur if fire is preceded by beetle infestation?
# Q3: How many years after beetle infestation does a fire have a similar recovery trajectory as an area that did not experience infestation?


# Data associated with this code:
fish_pts_SR2.txt
fish_pts_SR3.txt
fish_pts_SR4.txt
fish_pts_SR5.txt


# Target journal(s): Forest ecology and management
# Global change biology
# Reach - TREE, but prob for later papers. Review paper instead?


#### NOTE TO MAX:
# Areas of the code that I know could be more efficient ara flagged with "### Could be more efficient"


setwd("/Users/megancattau/Dropbox/0_EarthLab/Disturbance")
setwd("/Users/meca3122/Dropbox/0_EarthLab/Disturbance")
getwd()



# Import fire data 
# These are 250m rasters of the Geomac / MTBS data sampled at fishnet label points (corresponding to 250m fishnet). A separate raster was sampled for each year, and the fire-present values in each raster are the year that that fire occurred. Values for pixels where there was no fire are 0 or -9999
fire1<-read.table("fish_pts_SR2.txt", header=TRUE, sep=",")
names(fire1)
names(fire1)<-c("FID", "Id", "1984", "1986", "1987", "1988", "1989", "1990", "1993", "1994", "1996", "1997", "1998", "1999", "2000")

fire2<-read.table("fish_pts_SR3.txt", header=TRUE, sep=",")
names(fire2)
names(fire2)<-c("FID", "Id", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

# merge two fire data sets and get extra ID and FID cols out of there
fire<-cbind(fire1, fire2)
names(fire)
fire<-fire[,c(1,3:15, 18:32)]

# subset fire data to keep just pixels that have experienced fire in any (but not every) year
# keep rows where any row > 0
fire_yes<-fire[apply(fire[, -1], MARGIN = 1, function(x) any(x > 0)), ]
names(fire_yes)

# Get the max year for each row (i.e. year of last burn)
fire_yes$last_burn<-apply(fire_yes[,-1], 1, max)
head(fire_yes, n=50)


# Import mountain pine beetle (MPB) data
# These are 250m rasters of mountain pine beetle (MPB) infestation presence from the Aerial Detection Survey data sampled at fishnet label points (corresponding to 250m fishnet). A separate raster was sampled for each year, and the MPB-present values in each raster are the area of the infestation. Values for pixels where there was no infestation are 0 or -9999
MPB<-read.table("fish_pts_SR4.txt", header=TRUE, sep=",")
names(MPB)
names(MPB)<-c("FID", "Id", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
MPB<-MPB[,-2]

# change MPB to year rather than area
### Could be more efficient
MPB$"1994mpb"<-ifelse(MPB$"1994">0, 1994, 0)
MPB$"1995mpb"<-ifelse(MPB$"1995">0, 1995, 0)
MPB$"1996mpb"<-ifelse(MPB$"1996">0, 1996, 0)
MPB$"1997mpb"<-ifelse(MPB$"1997">0, 1997, 0)
MPB$"1998mpb"<-ifelse(MPB$"1998">0, 1998, 0)
MPB$"1999mpb"<-ifelse(MPB$"1999">0, 1999, 0)
MPB$"2000mpb"<-ifelse(MPB$"2000">0, 2000, 0)
MPB$"2001mpb"<-ifelse(MPB$"2001">0, 2001, 0)
MPB$"2002mpb"<-ifelse(MPB$"2002">0, 2002, 0)
MPB$"2003mpb"<-ifelse(MPB$"2003">0, 2003, 0)
MPB$"2004mpb"<-ifelse(MPB$"2004">0, 2004, 0)
MPB$"2005mpb"<-ifelse(MPB$"2005">0, 2005, 0)
MPB$"2006mpb"<-ifelse(MPB$"2006">0, 2006, 0)
MPB$"2007mpb"<-ifelse(MPB$"2007">0, 2007, 0)
MPB$"2008mpb"<-ifelse(MPB$"2008">0, 2008, 0)
MPB$"2009mpb"<-ifelse(MPB$"2009">0, 2009, 0)
MPB$"2010mpb"<-ifelse(MPB$"2010">0, 2010, 0)
MPB$"2011mpb"<-ifelse(MPB$"2011">0, 2011, 0)
MPB$"2012mpb"<-ifelse(MPB$"2012">0, 2012, 0)
MPB$"2013mpb"<-ifelse(MPB$"2013">0, 2013, 0)
MPB$"2014mpb"<-ifelse(MPB$"2014">0, 2014, 0)
MPB$"2015mpb"<-ifelse(MPB$"2015">0, 2015, 0)

# Get the max year for each row (i.e. year of last MPB infestation)
names(MPB)
MPB<-MPB[,c(-23:-2)]
MPB$last_infest<-apply(MPB[,-1], 1, max)
head(MPB, n=50)

# subset fire 1994-2015 (same range as beetle data)
names(fire_yes)
fire_yes<-fire_yes[,c(1,9:30)]
# No fires in 1995, so add that in there
fire_yes$"1995"<-rep(0,nrow(fire_yes))

# Merge fire and MPB together
merged_MPB_fire<-merge(fire_yes, MPB, by="FID")
names(merged_MPB_fire)
# vars yyyy = fire
# vars yyyympb = MPB

# Get years before fire that MPB infestation happened, just for rows that have had MPB and fire
# This is year of most recent fire minus year of most recent infestation (if same year = 0, if not both = -9999)
merged_MPB_fire$yrs_infest_bf_fire<-ifelse((merged_MPB_fire$last_infest>0 & merged_MPB_fire$last_burn > 0), merged_MPB_fire$last_burn-merged_MPB_fire$last_infest, -9999)
names(merged_MPB_fire)
head(merged_MPB_fire, n=50)

####################
write.csv(merged_MPB_fire, "merged_MPB_fire.csv")
####################



# Import VCF
# These are 250m rasters of MODIS vegetation continuous fields (VCF) data, or percent woody vegetation per pixel, sampled at fishnet label points (corresponding to 250m fishnet). A separate raster was sampled for each year. Value 200 is water and 253 is NA
VCF<-read.table("fish_pts_SR5.txt", header=TRUE, sep=",")
names(VCF)
names(VCF)<-c("FID", "Id", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")
VCF<-VCF[,-2]

### Change VCF > 100 to NA (200 is water and 253 is NA)

### Could be more efficient

VCF$"2000"<-ifelse(VCF$"2000">100, NA, VCF$"2000")
VCF$"2001"<-ifelse(VCF$"2001">100, NA, VCF$"2001")
VCF$"2002"<-ifelse(VCF$"2002">100, NA, VCF$"2002")
VCF$"2003"<-ifelse(VCF$"2003">100, NA, VCF$"2003")
VCF$"2004"<-ifelse(VCF$"2004">100, NA, VCF$"2004")
VCF$"2005"<-ifelse(VCF$"2005">100, NA, VCF$"2005")
VCF$"2006"<-ifelse(VCF$"2006">100, NA, VCF$"2006")
VCF$"2007"<-ifelse(VCF$"2007">100, NA, VCF$"2007")
VCF$"2008"<-ifelse(VCF$"2008">100, NA, VCF$"2008")
VCF$"2009"<-ifelse(VCF$"2009">100, NA, VCF$"2009")
VCF$"2010"<-ifelse(VCF$"2010">100, NA, VCF$"2010")
VCF$"2011"<-ifelse(VCF$"2011">100, NA, VCF$"2011")
VCF$"2012"<-ifelse(VCF$"2012">100, NA, VCF$"2012")
VCF$"2013"<-ifelse(VCF$"2013">100, NA, VCF$"2013")
VCF$"2014"<-ifelse(VCF$"2014">100, NA, VCF$"2014")
VCF$"2015"<-ifelse(VCF$"2015">100, NA, VCF$"2015")


# merge data
names(merged_MPB_fire)
merged_MPB_fire_VCF<-merge(merged_MPB_fire, VCF, "FID")
names(merged_MPB_fire_VCF)
head(merged_MPB_fire_VCF)
# vars yyyy.x = fire
# vars yyyympb = MPB
# vars yyyy.y = VCF




######### VCF 0-20 years after fire ######## 
# The below looks at VCF 0-20 years after a fire
# Outstanding: bias as get further away from fire year because less opportunity to capture pre-fire infestation. For example, VCF_since_fire0 begins with fires in 1999 and MPB dataset starts at 1994, so 6 years to capture MPB. By VCF_since_fire5, could start with fires in 1994 to capture VCF 5 years later (bc VCF dataset starts at 2000), leaving only MPB that happened in that year. Maybe there wasn't much MPB before this, so that assumption is ok?
# Outstanding: should stop at VCF_since_fire5 because the sample size starts to go down? Can account for this somehow?
# "_0 is the VCF from JD065 of the following year since the fire happened

### Could be more efficient

merged_MPB_fire_VCF$VCF_before_fire<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2011, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2012, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2013, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2014, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2015, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire0<-
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2011, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2012, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2013, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2014, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire1<-
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2011, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2012, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2013, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire2<-
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2011, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2012, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire3<-
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2011, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire4<-
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2010, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire5<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2000.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2009, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire6<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2001.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2008, merged_MPB_fire_VCF$"2015.y",
NA
)))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire7<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2002.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2007, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))))

merged_MPB_fire_VCF$VCF_since_fire8<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2003.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2006, merged_MPB_fire_VCF$"2015.y",
NA
)))))))))))))

merged_MPB_fire_VCF$VCF_since_fire9<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2004.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2005, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))))

merged_MPB_fire_VCF$VCF_since_fire10<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2005.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2004, merged_MPB_fire_VCF$"2015.y",
NA
)))))))))))

merged_MPB_fire_VCF$VCF_since_fire11<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2006.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2003, merged_MPB_fire_VCF$"2015.y",
NA
))))))))))

merged_MPB_fire_VCF$VCF_since_fire12<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2007.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2002, merged_MPB_fire_VCF$"2015.y",
NA
)))))))))

merged_MPB_fire_VCF$VCF_since_fire13<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2008.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2001, merged_MPB_fire_VCF$"2015.y",
NA
))))))))

merged_MPB_fire_VCF$VCF_since_fire14<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2009.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==2000, merged_MPB_fire_VCF$"2015.y",
NA
)))))))

merged_MPB_fire_VCF$VCF_since_fire15<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2010.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==1999, merged_MPB_fire_VCF$"2015.y",
NA
))))))

merged_MPB_fire_VCF$VCF_since_fire16<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2011.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==1998, merged_MPB_fire_VCF$"2015.y",
NA
)))))

merged_MPB_fire_VCF$VCF_since_fire17<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2012.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==1997, merged_MPB_fire_VCF$"2015.y",
NA
))))

merged_MPB_fire_VCF$VCF_since_fire18<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2013.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==1996, merged_MPB_fire_VCF$"2015.y",
NA
)))

merged_MPB_fire_VCF$VCF_since_fire19<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2014.y",
ifelse(merged_MPB_fire_VCF$last_burn==1995, merged_MPB_fire_VCF$"2015.y",
NA
))

merged_MPB_fire_VCF$VCF_since_fire20<-
ifelse(merged_MPB_fire_VCF$last_burn==1994, merged_MPB_fire_VCF$"2015.y",
NA
)

head(merged_MPB_fire_VCF, n=25)




######### Recovery 0-20 years after fire ######## 
# The below looks at recovery 0-20 years after a fire relative to the pre-fire state
# recovery is defined as difference between pre-fire VCF and post-fire VCF (0-20 years after fire) 
# Do this rather than just VCF bc bias (i.e., no MPB areas could have lower pre- and post-fire VCF because they're grassland, whereas MPB infested areas are going to be forest)
# Outstanding: bias bc can only look at fires since 2000 since VCF dataset starts at 2000 (therefore no pre-fire dataset before this)

# "_0 is the VCF from JD065 of the following year since the fire happened

### Could be more efficient

merged_MPB_fire_VCF$pre_minus_1yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2002.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2003.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2004.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2005.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2006.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2009, (merged_MPB_fire_VCF$"2009.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2010, (merged_MPB_fire_VCF$"2010.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2011, (merged_MPB_fire_VCF$"2011.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2012, (merged_MPB_fire_VCF$"2012.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2013, (merged_MPB_fire_VCF$"2013.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))))))))))))

merged_MPB_fire_VCF$pre_minus_2yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2003.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2004.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2005.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2006.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2009, (merged_MPB_fire_VCF$"2009.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2010, (merged_MPB_fire_VCF$"2010.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2011, (merged_MPB_fire_VCF$"2011.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2012, (merged_MPB_fire_VCF$"2012.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))))))))))))

merged_MPB_fire_VCF$pre_minus_3yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2004.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2005.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2006.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2009, (merged_MPB_fire_VCF$"2009.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2010, (merged_MPB_fire_VCF$"2010.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2011, (merged_MPB_fire_VCF$"2011.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))))))))))

merged_MPB_fire_VCF$pre_minus_4yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2005.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2006.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2009, (merged_MPB_fire_VCF$"2009.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2010, (merged_MPB_fire_VCF$"2010.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))))))))))

merged_MPB_fire_VCF$pre_minus_5yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2006.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2009, (merged_MPB_fire_VCF$"2009.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))))))))

merged_MPB_fire_VCF$pre_minus_6yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2007.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2008, (merged_MPB_fire_VCF$"2008.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))))))))

merged_MPB_fire_VCF$pre_minus_7yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2008.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2007, (merged_MPB_fire_VCF$"2007.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))))))

merged_MPB_fire_VCF$pre_minus_8yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2009.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2006, (merged_MPB_fire_VCF$"2006.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))))))

merged_MPB_fire_VCF$pre_minus_9yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2010.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2005, (merged_MPB_fire_VCF$"2005.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))))

merged_MPB_fire_VCF$pre_minus_10yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2011.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2004, (merged_MPB_fire_VCF$"2004.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))))

merged_MPB_fire_VCF$pre_minus_11yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2012.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2003, (merged_MPB_fire_VCF$"2003.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))))

merged_MPB_fire_VCF$pre_minus_12yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2013.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2002, (merged_MPB_fire_VCF$"2002.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)))

merged_MPB_fire_VCF$pre_minus_13yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2014.y"),
ifelse(merged_MPB_fire_VCF$last_burn==2001, (merged_MPB_fire_VCF$"2001.y" - merged_MPB_fire_VCF$"2015.y"),
NA
))

merged_MPB_fire_VCF$pre_minus_14yrs_post_fire_VCF<-
ifelse(merged_MPB_fire_VCF$last_burn==2000, (merged_MPB_fire_VCF$"2000.y" - merged_MPB_fire_VCF$"2015.y"),
NA
)


####################
write.csv(merged_MPB_fire_VCF, "merged_MPB_fire_VCF.csv")
####################
merged_MPB_fire_VCF<-read.csv("merged_MPB_fire_VCF.csv", header=TRUE)
merged_MPB_fire_VCF<-merged_MPB_fire_VCF[,-1]


# Just for pixels that were 'forest' prefire (39.3 VCF) to make sure no bias in pre-fire VCF between MPB infestationa and not
# VCF threshold that represents forest: 49.4 +/- 10.1; including sparse forest: 47.5 +/- 10.2. This was derived from selecting 100 forest GCPs (plus 25? 'sparse forest' GCPs) in Google Earth (imagery from 2012) and computing stats on VCF 2012 sampled at those points
# If we do this, we're only looking at fires 2000-2015

forest<-merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=39.3, ]

# Make sure no bias: that pre-fire MPB and pre-fire no MPB pixels have same distribution of pre-fire VCF. # To detect pre-fire bias in VCF, what's the mean VCF of pixels the year before a fire for pixels that have also had MPB vs those that have not?
t.test(merged_MPB_fire_VCF[merged_MPB_fire_VCF$yrs_infest_bf_fire>=0,]$VCF_before_fire, merged_MPB_fire_VCF[merged_MPB_fire_VCF$yrs_infest_bf_fire<0,]$VCF_before_fire, na.action="na.pass")
# They are sig diff, but only by 1.4% VCF
boxplot(merged_MPB_fire_VCF[merged_MPB_fire_VCF$yrs_infest_bf_fire>=0,]$VCF_before_fire, merged_MPB_fire_VCF[merged_MPB_fire_VCF$yrs_infest_bf_fire<0,]$VCF_before_fire, names=c("Infestation", "No infestation"), ylab="VCF pre-fire")


t.test(forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire, forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire, na.action="na.pass")
# They are not sig diff if just look at forest pixels
boxplot(forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire, forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire, names=c("Infestation", "No infestation"), ylab="VCF pre-fire")


###############
###############

# Q1: When is forest 'recovered'?
# a. At how many years since fire does post-fire VCF Resemble pre-fire VCF?
# b. Does this vary as a function of pre-fire MPB infestation?
# c. Does this vary as a function of pre-fire VCF?

### Plot post-fire regrowth as a function of years since fire
names(forest)
VCF_post_fire<-forest[,c(65:86)]
names(VCF_post_fire)

VCF_post_fire_mean<-apply(VCF_post_fire[1:16], 2, mean, na.rm=TRUE)
VCF_post_fire_sd<-apply(VCF_post_fire[1:16], 2, sd, na.rm=TRUE)
VCF_post_fire_low<-VCF_post_fire_mean-VCF_post_fire_sd
VCF_post_fire_high<-VCF_post_fire_mean+VCF_post_fire_sd
time<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
VCF_post_fire_time<-data.frame(VCF_post_fire_mean, VCF_post_fire_sd,time)
plot(VCF_post_fire_time$time, VCF_post_fire_time$VCF_post_fire_mean, xlab="Time since fire (years)", ylab="VCF", ylim=range(0:75))
lines(VCF_post_fire_time$time, VCF_post_fire_mean)
lines(VCF_post_fire_time$time, VCF_post_fire_low, lty=2)
lines(VCF_post_fire_time$time, VCF_post_fire_high, xlab="Time since fire (years)", ylab="VCF", lty=2)

# What accounts for the shape of this trend?
# Carol - 
# Could be post-fire die off, then 5-10 years seedling establishment, after 7-8 years no new establishment
# Paper - Yellowstone - 30 years to 'full recovery' (density)? Could use this to make case for incorporating Landsat
# ***diff pre and post-fire VCF

### Plot post-fire regrowth (relative to pre-fire VCF) as a function of years since fire
# Reminder: pre-fire VCF minus post-fire VCF (positive numbers still recovering, negative numbers exceeded pre-fire VCF)
# subset pre_minus_1yrs_post_fire_VCF to pre_minus_14yrs_post_fire_VCF
names(forest)
VCF_pre_minus_post_fire<-forest[,c(87:100)]
names(VCF_pre_minus_post_fire)

VCF_pre_minus_post_fire_mean<-apply(VCF_pre_minus_post_fire, 2, mean, na.rm=TRUE)
VCF_pre_minus_post_fire_sd<-apply(VCF_pre_minus_post_fire, 2, sd, na.rm=TRUE)
VCF_pre_minus_post_fire_low<-VCF_pre_minus_post_fire_mean-VCF_pre_minus_post_fire_sd
VCF_pre_minus_post_fire_high<-VCF_pre_minus_post_fire_mean+VCF_pre_minus_post_fire_sd

time<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
VCF_pre_minus_post_fire_time<-data.frame(VCF_pre_minus_post_fire_mean, time)
plot(VCF_pre_minus_post_fire_time$time, VCF_pre_minus_post_fire_time$VCF_pre_minus_post_fire_mean, xlab="Time since fire (years)", ylab = "Difference between pre- and post- fire VCF", ylim=range(-20:40))
lines(VCF_pre_minus_post_fire_time$time, VCF_pre_minus_post_fire_time$VCF_pre_minus_post_fire_mean)
lines(VCF_pre_minus_post_fire_time$time, VCF_pre_minus_post_fire_low, lty=2)
lines(VCF_pre_minus_post_fire_time$time, VCF_pre_minus_post_fire_high, lty=2)

# a. Q: At how many years since fire does post-fire VCF Resemble pre-fire VCF?
# A: More than 14

# Below, using na.pass (rather than na.exclude) is same as: 
# t.test(forest[complete.cases(forest[,65:66]),]$VCF_before_fire, forest[complete.cases(forest[,65:66]),]$VCF_since_fire0, na.action="na.exclude")
# https://stat.ethz.ch/pipermail/r-help/2010-August/249146.html
### Could be more efficient
t.test(forest$VCF_before_fire, forest$VCF_since_fire0, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire1, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire2, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire3, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire4, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire5, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire6, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire7, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire8, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire9, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire10, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire11, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire12, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire13, paired=TRUE, na.action="na.pass")
t.test(forest$VCF_before_fire, forest$VCF_since_fire14, paired=TRUE, na.action="na.pass")
# Can only look 14 years post - all sig diff, so forest still not recovered 14 years out


# Q: b. Does this vary as a function of pre-fire MPB infestation?
# A: Prob not
# Beetle infestation
# Below, using na.pass, is same as:
# t.test((forest[((complete.cases(forest[,c(65,66,48)])) & forest$yrs_infest_bf_fire>=0),]$VCF_before_fire), (forest[((complete.cases(forest[,c(65,66,48)])) & forest$yrs_infest_bf_fire>=0),]$VCF_since_fire0))
### Could be more efficient
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
# all above sig diff, so forest still not recovered after 11 years
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
# Not different, so maybe recovered by 12 years after, but prob not enough obs. How many obs complete cases VCF bf and 12 years?
nrow(forest[((complete.cases(forest[,c(65,78)])) & forest$yrs_infest_bf_fire>=0),]) #10
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
# not enough obs bc no MPB before fire in 2000 or 2001
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire14)), paired=TRUE, na.action="na.pass")
# not enough obs bc no MPB before fire in 2000

# No Beetle infestation
### Could be more efficient
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((forest[forest$yrs_infest_bf_fire<0,]$VCF_before_fire), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# all sig diff, so forest still not recovered after 14 years 

### Enough obs with MPB before fire to answer this?





# Q: c. Does this vary as a function of pre-fire VCF?
# A: not really. No pre-fire VCF category 'recovers,' but mean of pre-and post-fire VCF greater diff with increasing pre-fire VCF

### Could be more efficient
# Like, WAY more efficient
# pre-fire VCF 0-19
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>0 & merged_MPB_fire_VCF$VCF_before_fire<20,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# still not recovered after 14 years bc all sig diff, but mean diff VCF in groups is 0-2% VCF. Meaningfully different?


# pre-fire VCF 20-39
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=20 & merged_MPB_fire_VCF$VCF_before_fire<40,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# still not recovered after 14 years bc all sig diff, but mean diff VCF in groups is <6% VCF.

# pre-fire VCF 40-59
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=40 & merged_MPB_fire_VCF$VCF_before_fire<60,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# still not recovered after 14 years bc all sig diff, and mean diff VCF in groups is ~5-15% VCF.

# pre-fire VCF 60-79
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=60 & merged_MPB_fire_VCF$VCF_before_fire<80,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# still not recovered after 14 years bc all sig diff, and mean diff VCF in groups is ~10-20% VCF.

# pre-fire VCF 80-100
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire0), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire1), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire2), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire3), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire4), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire5), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire6), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire7), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire8), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire9), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire10), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire11), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire12), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire13), paired=TRUE, na.action="na.pass")
t.test((merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_before_fire), (merged_MPB_fire_VCF[merged_MPB_fire_VCF$VCF_before_fire>=80 & merged_MPB_fire_VCF$VCF_before_fire<=100,]$VCF_since_fire14), paired=TRUE, na.action="na.pass")
# Not enough obs for this one

###############
###############

# Q2: Is a transition more likely to occur if fire is preceded by beetle infestation?
# Is post-fire forest recovery different between groups for each year after fire: 1) MPB infestation year of fire or any previous year 2) NOT MPB infestation year of fire or any previous year


### Post-fire regrowth as a function of years since fire

names(forest)
# Reminder: yrs_infest_bf_fire is last_burn-last_infest, so positive numbers are when MPB happened before fire

### Could be more efficient

t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire0), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire0))
# Different mean**
# mean post-fire VCF of fire affected areas that have also had pre-fire MPB = 36.3, those that have not = 37.9

t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire1), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire1))
# Different mean**, pre-fire MPB lower VCF

t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire2), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire2))
# Different mean**, pre-fire MPB lower VCF

t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire3), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire3))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire4), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire4))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire5), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire5))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire6), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire6))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire7), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire7))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire8), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire8))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire9), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire9))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire10), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire10))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire11), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire11))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire12), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire12))
# NOT Different mean, 3-12 years after

t.test((forest[forest$yrs_infest_bf_fire>=0,]$VCF_since_fire13), (forest[forest$yrs_infest_bf_fire<0,]$VCF_since_fire13))
# not enough obs for mean or distribution 13-20 years after

# **p <= 0.05

### First years post-fire, less vegetation in areas that had been MBP-affected. At 3 years post-fire, vegetation looks the same independent of whether there was MPB infestation pre-fire or not


# Plot the above
names(forest)
VCF_post_fire_MPB<-forest[forest$yrs_infest_bf_fire>=0,c(66:87)]
VCF_post_fire_noMPB<-forest[forest$yrs_infest_bf_fire<0,c(66:87)]

VCF_post_fire_MPB_mean<-apply(VCF_post_fire_MPB, 2, mean, na.rm=TRUE)
VCF_post_fire_MPB_sd<-apply(VCF_post_fire_MPB, 2, sd, na.rm=TRUE)
VCF_post_fire_MPB_low<-VCF_post_fire_MPB_mean[2:5]-VCF_post_fire_MPB_sd[2:5]
VCF_post_fire_MPB_high<-VCF_post_fire_MPB_mean[2:5]+VCF_post_fire_MPB_sd[2:5]

VCF_post_fire_noMPB_mean<-apply(VCF_post_fire_noMPB, 2, mean, na.rm=TRUE)
VCF_post_fire_noMPB_sd<-apply(VCF_post_fire_noMPB, 2, sd, na.rm=TRUE)
VCF_post_fire_noMPB_low<-VCF_post_fire_noMPB_mean[2:5]-VCF_post_fire_noMPB_sd[2:5]
VCF_post_fire_noMPB_high<-VCF_post_fire_noMPB_mean[2:5]+VCF_post_fire_noMPB_sd[2:5]

time<-c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

VCF_post_fire_MPB_time<-data.frame(VCF_post_fire_MPB_mean,time)
VCF_post_fire_noMPB_time<-data.frame(VCF_post_fire_noMPB_mean, time)

plot(VCF_post_fire_MPB_time$time, VCF_post_fire_MPB_time$VCF_post_fire_MPB_mean, xlab="Time since fire (years)", ylab="VCF", type="p", col="red", xlim=range(0:3), ylim=range(0:75))
lines(VCF_post_fire_MPB_time$time, VCF_post_fire_MPB_time$VCF_post_fire_MPB_mean, col="red")
lines(VCF_post_fire_MPB_time$time[2:5], VCF_post_fire_MPB_low, col="red", lty=2)
lines(VCF_post_fire_MPB_time$time[2:5], VCF_post_fire_MPB_high, col="red", lty=2)

points(VCF_post_fire_noMPB_time$time, VCF_post_fire_noMPB_time$VCF_post_fire_noMPB_mean, col="blue")
lines(VCF_post_fire_noMPB_time$time, VCF_post_fire_noMPB_time$VCF_post_fire_noMPB_mean, col="blue")
lines(VCF_post_fire_noMPB_time$time[2:5], VCF_post_fire_noMPB_low, col="blue", lty=2)
lines(VCF_post_fire_noMPB_time$time[2:5], VCF_post_fire_noMPB_high, col="blue", lty=2)

# would this look more impressive on another scale or something?
# not log



### Post-fire regrowth (relative to pre-fire VCF) as a function of years since fire

### Could be more efficient

t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_1yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_1yrs_post_fire_VCF))
# Different**
# mean difference between pre-fire VCF and post-fire VCF of fire affected areas that have also had pre-fire MPB = 8.7, those that have not = 6.6
# Reminder: pre-fire VCF minus post-fire VCF (positive numbers still recovering, negative numbers exceeded pre-fire VCF)

t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_2yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_2yrs_post_fire_VCF))
# Different**, mean difference greater in areas that have also had pre-fire MPB

t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_3yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_3yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_4yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_4yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_5yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_5yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_6yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_6yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_7yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_7yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_8yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_8yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_9yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_9yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_10yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_10yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_11yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_11yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_12yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_12yrs_post_fire_VCF))
# NOT Different 3-12 years after

t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_13yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_13yrs_post_fire_VCF))
t.test((forest[forest$yrs_infest_bf_fire>=0,]$pre_minus_14yrs_post_fire_VCF), (forest[forest$yrs_infest_bf_fire<0,]$pre_minus_14yrs_post_fire_VCF))
# not enough obs 13-14 years after

# **p <= 0.05

### First years post-fire, recovery to a pre-disturbance state greater in areas that had NOT been MPB-affected. At 3 years post-fire, recovery looks the same independent of whether there was MPB infestation post-fire or not


# Plot the above
names(forest)
VCF_pre_post_fire_MPB<-forest[forest$yrs_infest_bf_fire>=0,c(87:100)]
VCF_pre_post_fire_noMPB<-forest[forest$yrs_infest_bf_fire<0,c(87:100)]

VCF_pre_post_fire_MPB_mean<-apply(VCF_pre_post_fire_MPB, 2, mean, na.rm=TRUE)
VCF_pre_post_fire_MPB_sd<-apply(VCF_pre_post_fire_MPB, 2, sd, na.rm=TRUE)
VCF_pre_post_fire_MPB_low<-VCF_pre_post_fire_MPB_mean[1:3]-VCF_pre_post_fire_MPB_sd[1:3]
VCF_pre_post_fire_MPB_high<-VCF_pre_post_fire_MPB_mean[1:3]+VCF_pre_post_fire_MPB_sd[1:3]

VCF_pre_post_fire_noMPB_mean<-apply(VCF_pre_post_fire_noMPB, 2, mean, na.rm=TRUE)
VCF_pre_post_fire_noMPB_sd<-apply(VCF_pre_post_fire_noMPB, 2, sd, na.rm=TRUE)
VCF_pre_post_fire_noMPB_low<-VCF_pre_post_fire_noMPB_mean[1:3]-VCF_pre_post_fire_noMPB_sd[1:3]
VCF_pre_post_fire_noMPB_high<-VCF_pre_post_fire_noMPB_mean[1:3]+VCF_pre_post_fire_noMPB_sd[1:3]


time<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)

VCF_pre_post_fire_MPB_time<-data.frame(VCF_pre_post_fire_MPB_mean,time)
VCF_pre_post_fire_noMPB_time<-data.frame(VCF_pre_post_fire_noMPB_mean,time)

plot(VCF_pre_post_fire_MPB_time$time, VCF_pre_post_fire_MPB_time$VCF_pre_post_fire_MPB_mean, xlab="Time since fire (years)", ylab="VCF", type="points", col="red", xlim=range(1:3), ylim=range(-25:50))
lines(VCF_pre_post_fire_MPB_time$time, VCF_pre_post_fire_MPB_time$VCF_pre_post_fire_MPB_mean, col="red")
lines(VCF_pre_post_fire_MPB_time$time[1:3], VCF_pre_post_fire_MPB_low, col="red", lty=2)
lines(VCF_pre_post_fire_MPB_time$time[1:3], VCF_pre_post_fire_MPB_high, col="red", lty=2)

points(VCF_pre_post_fire_noMPB_time$time, VCF_pre_post_fire_noMPB_time$VCF_pre_post_fire_noMPB_mean, col="blue")
lines(VCF_pre_post_fire_noMPB_time$time, VCF_pre_post_fire_noMPB_time$VCF_pre_post_fire_noMPB_mean, col="blue")
lines(VCF_pre_post_fire_noMPB_time$time[1:3], VCF_pre_post_fire_noMPB_low, col="blue", lty=2)
lines(VCF_pre_post_fire_noMPB_time$time[1:3], VCF_pre_post_fire_noMPB_high, col="blue", lty=2)

# would this look more impressive on another scale or something?




###############
# NEXT / further thought:

# Confounding factors - Climate, elevation / slope / aspect
# If fire was in drought year / high fire incidence year
# precip figure - fire severity
# Elevation, slope aspect - recovery 

# integrate other infestation in here

# instead of tests for each year, one test with pixel as random effect?

# FRP / severity for each fire / dNBR or rdNBR value


# Because pre-fire VCF higher in MPB-infested pixels as not MPB-infested pixels (but mimimally), then what does this mean?

# Q3: How many years after beetle infestation does a fire have a similar recovery trajectory as an area that did not experience infestation? (years between beetle and fire)
# Evaluate this trend as a function of time between MPB infestation and fire. VCF 0-20 years post-fire for 0-x years between MPB infestation and fire 

# buffer around fire and MPB (i.e., expand the area slightly to account for error)

###############


# New for Carol:
# Q. "recovery?
# A: "not recovered after 14 years, 
# A: Does this vary as a function of pre-fire MPB? Still not recovered after 14 years, independent of previous MPB (but not lot of MPB obs)
# A: Does this vary as a function of pre-fire VCF? Not really. No pre-fire VCF category 'recovers,' but mean of pre-and post-fire VCF greater diff with increasing pre-fire VCF

# If restricting analysis to pre-fire forest pixels, no diff in pre-fire VCF for MPB-infested and not MPB-infested pixels

##################################################
#################### SCRATCH####################
##################################################


###############




