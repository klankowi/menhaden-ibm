rm(list=ls())
library(raster)

# Load data
load(paste0('G:/My Drive/Documents_Backup/Modeling/Modeling_RCode/',
            'Raster_Data/ncdf_output.RData'))
time$timestamp <- time$timestamp + lubridate::hours(3)

# Load environmental raster lists
for(i in 1:length(env)){
  char <- names(env[i])
  print(char)
  char_rast <- readRDS(paste0("G:/My Drive/Documents_Backup/Modeling/",
                              "Modeling_RCode/Raster_Data/",
                              char, "_rast.RDS"))
  char_rast <- assign(paste0(char, "_rast"), char_rast)
}

# Remove intermediates
rm(char_rast)

correl <- matrix(nrow=5, ncol=5)
correl <- rep(list(correl), length(chl_rast))
correl <- array(unlist(correl), dim=c(5,5,length(chl_rast)))
rownames(correl) <- c('chl', 'dox', 'sal', 'tem', 'tdep')
colnames(correl) <- rownames(correl)

# Create stacks
for(i in 1:length(chl_rast)){
  st <- stack(chl_rast[[i]],
              dox_rast[[i]],
              sal_rast[[i]], 
              tem_rast[[i]],
              tdep_rast[[i]])
  correl[,,i] <- cor(sampleRandom(st, size=ncell(chl_rast[[i]] *0.5),
                             method="spearman"))
}

rsq <- matrix(nrow=5, ncol=5)
rsq <- rep(list(rsq), length(chl_rast))
rsq <- array(unlist(rsq), dim=c(5,5,length(chl_rast)))
rownames(rsq) <- c('chl', 'dox', 'sal', 'tem', 'tdep')
colnames(rsq) <- rownames(rsq)

for(i in 1:length(st)){
  v <- data.frame(na.omit(values(st)))
  m <- lm(chl ~ dox, data=v)
  m <- summary(m)
}

setwd("G:/My Drive/Documents_Backup/Modeling/Modeling_RCode/")
daylen <- read.csv('daylength.csv')
sunrises <- substr(daylen$Sunrise, start=1, stop=7)
sunrises <- paste0("0",sunrises)
sunsetsh <- substr(daylen$Sunset, start=1, stop=1)
sunsetsh <- as.numeric(sunsetsh) + 12
sunsets <- paste0(sunsetsh, substr(daylen$Sunset, start=2, stop=7))
justday <- substr(time$timestamp, start=1, stop=10)
justday <- unique(justday)

sunrisetimestamps <- as.POSIXct(paste0(justday, " ", sunrises),
                                format="%Y-%m-%d %H:%M:%S")
sunsettimestamps <- as.POSIXct(paste0(justday, " ", sunsets),
                                format="%Y-%m-%d %H:%M:%S")

rm(daylen, sunrises, sunsetsh, sunsets, justday)

collist <- c('darkgray', 'white')
setwd("G:/My Drive/Documents_Backup/Modeling/Modeling_RCode/Raster_Data/correlation/")
library(gifski)
library(corrplot)
for(k in 1:length(chl_rast)){
  timest <- '2006-05-31 23:00:00'
  timest <- as.POSIXct(timest,
                       format="%Y-%m-%d %H:%M:%S", 
                       tz="America/New_York")
  timest <- timest + lubridate::hours(k)
  if(timest < sunrisetimestamps[ceiling(k/24)] | timest > sunsettimestamps[ceiling(k/24)]){
    pickcol <- collist[1]
  }
  if(timest > sunrisetimestamps[ceiling(k/24)] & timest < sunsettimestamps[ceiling(k/24)]){
    pickcol <- collist[2]
  }
  
  timest <- gsub(":", "-", timest)
  if(nchar(timest)==10){
    timest <- paste0(timest, " 00-00-00")
  }
  
  png(paste0(timest, '.png'))
  testRes <-  cor.mtest(correl[,,k])
  corrplot(correl[,,k], p.mat = testRes$p, method = 'square', diag = FALSE, type = 'lower',
           sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5, col=COL2("BrBG", n=256),
           insig = 'label_sig', pch.col = 'white')
  mtext(paste0(timest), side=3, line=2.5)
  dev.off()
}
files  <- list.files()
files <- paste0(getwd(), '/', files)
setwd("G:/My Drive/Documents_Backup/Modeling/Modeling_RCode/GIFs")
gifski(files, "correlation.gif", loop = FALSE, delay = 0.01)

