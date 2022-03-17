# Patch Caculating the extreme temperature and precipitation indices
# Written by Dr. Panfeng Zhang, 
# Email: zhangpanfeng@cug.edu.cn

# The order of the input data from left to right is 
# "year", "month", "day", "prcp", "tmax", "tmin"
# the units of temperature is 1 degree Celsius
# the units of precipitation is 1mm

rm(list = ls())
library(lubridate)    # leap_year function
library(climdex.pcic) # 27 extreme climate index
library(tcltk)
diri <- "C:/documents/input" #  daily dataset input directory
missingValue <- "-999.9"
referencePeriod <- c(1961, 1990)
IfNorth <- TRUE  # Northern Hemisphere is TRUE, southern Hemisphere is False
PathNames <- list.files(diri, pattern = ".txt", full.names = TRUE)
# Creating the save directory of indices
diro <- paste0(diri, "_indices")  # indices output directory
# temperature
fdPath      <- file.path(diro, "01_FD")
suPath      <- file.path(diro, "02_SU")
idPath      <- file.path(diro, "03_ID")
trPath      <- file.path(diro, "04_TR")
gslPath     <- file.path(diro, "05_GSL")
txxPath     <- file.path(diro, "06_TXX")
tnxPath     <- file.path(diro, "07_TNX")
txnPath     <- file.path(diro, "08_TXN")
tnnPath     <- file.path(diro, "09_TNN")
tn10pPath   <- file.path(diro, "10_TN10P")
tx10pPath   <- file.path(diro, "11_TX10P")
tn90pPath   <- file.path(diro, "12_TN90P")
tx90pPath   <- file.path(diro, "13_TX90P")
wsdiPath    <- file.path(diro, "14_WSDI")
csdiPath    <- file.path(diro, "15_CSDI")
dtrPath     <- file.path(diro, "16_DTR")

# precipitation
rx1dayPath  <- file.path(diro, "17_rx1day")
rx5dayPath  <- file.path(diro, "18_rx5day")
sdiiPath    <- file.path(diro, "19_sdii")
r10mmPath   <- file.path(diro, "20_r10mm")
r20mmPath   <- file.path(diro, "21_r20mm")
r50mmPath   <- file.path(diro, "22_r50mm")
cddPath     <- file.path(diro, "23_cdd")
cwdPath     <- file.path(diro, "24_cwd")
r95ptotPath <- file.path(diro, "25_r95ptot")
r99ptotPath <- file.path(diro, "26_r99ptot")
prcptotPath <- file.path(diro, "27_prcptot")

if(!dir.exists(fdPath))    dir.create(fdPath,    recursive = TRUE)
if(!dir.exists(suPath))    dir.create(suPath,    recursive = TRUE)
if(!dir.exists(idPath))    dir.create(idPath,    recursive = TRUE)
if(!dir.exists(trPath))    dir.create(trPath,    recursive = TRUE)
if(!dir.exists(gslPath))   dir.create(gslPath,   recursive = TRUE)
if(!dir.exists(txxPath))   dir.create(txxPath,   recursive = TRUE)
if(!dir.exists(tnxPath))   dir.create(tnxPath,   recursive = TRUE)
if(!dir.exists(txnPath))   dir.create(txnPath,   recursive = TRUE)
if(!dir.exists(tnnPath))   dir.create(tnnPath,   recursive = TRUE)
if(!dir.exists(tn10pPath)) dir.create(tn10pPath, recursive = TRUE)
if(!dir.exists(tx10pPath)) dir.create(tx10pPath, recursive = TRUE)
if(!dir.exists(tn90pPath)) dir.create(tn90pPath, recursive = TRUE)
if(!dir.exists(tx90pPath)) dir.create(tx90pPath, recursive = TRUE)
if(!dir.exists(wsdiPath))  dir.create(wsdiPath,  recursive = TRUE)
if(!dir.exists(csdiPath))  dir.create(csdiPath,  recursive = TRUE)
if(!dir.exists(dtrPath))   dir.create(dtrPath,   recursive = TRUE)

if(!dir.exists(rx1dayPath))  dir.create(rx1dayPath,  recursive = TRUE)
if(!dir.exists(rx5dayPath))  dir.create(rx5dayPath,  recursive = TRUE)
if(!dir.exists(sdiiPath))    dir.create(sdiiPath,    recursive = TRUE)
if(!dir.exists(r10mmPath))   dir.create(r10mmPath,   recursive = TRUE)
if(!dir.exists(r20mmPath))   dir.create(r20mmPath,   recursive = TRUE)
if(!dir.exists(r50mmPath))   dir.create(r50mmPath,   recursive = TRUE)
if(!dir.exists(cddPath))     dir.create(cddPath,     recursive = TRUE)
if(!dir.exists(cwdPath))     dir.create(cwdPath,     recursive = TRUE)
if(!dir.exists(r95ptotPath)) dir.create(r95ptotPath, recursive = TRUE)
if(!dir.exists(r99ptotPath)) dir.create(r99ptotPath, recursive = TRUE)
if(!dir.exists(prcptotPath)) dir.create(prcptotPath, recursive = TRUE)

VarType <- c(rep("integer", 3), rep("numeric", 3))
StationNumbers <- length(PathNames)
pb <- tkProgressBar(title = "ProgressBar", label = "It has completed  %", min = 0, max = 100) 
for(i in 1:StationNumbers) {
  dd <- read.table(PathNames[i], header = FALSE, sep = "", na.strings = missingValue, colClasses = VarType)
  names(dd) <- c("year", "month", "day", "prcp", "tmax", "tmin")
  #dd$prcp <- dd$prcp * 0.01
  dd[, c("tmax", "tmin")] <- round(dd[, c("tmax", "tmin")] + runif(nrow(dd), min = -0.5, max = 0.5), digits = 1)
  dd[, "date"]  <- paste(dd$year, dd$month, dd$day, sep = "-")
  dates <- as.PCICt(dd$date, cal="gregorian")
  FileName <- basename(PathNames[i])
  ID <- tools::file_path_sans_ext(FileName)
  # IfNorth <- ifelse(ds[ds$ID == ID, "lat"] >= 0, TRUE, FALSE)
  
  ci <- climdexInput.raw(tmax = dd$tmax, tmin = dd$tmin, prec = dd$prcp,
                         tmax.dates = dates, tmin.dates = dates, prec.dates = dates,
                         base.range = referencePeriod, n = 5, northern.hemisphere = IfNorth,
                         max.missing.days = c(annual = 10, monthly = 7),
                         temp.qtiles = c(0.1, 0.9),
                         min.base.data.fraction.present = 0.1)
  #--------------------------------------------------------------------------------
  # 1. FD, Number of frost days
  fd <- climdex.fd(ci)
  fd <- as.data.frame(fd)
  fd[, "year"] <- as.integer(rownames(fd))
  fd <- fd[, c("year", "fd")]
  FDname <- file.path(fdPath, FileName)
  write.csv(fd, file = FDname, na = "NA", row.names = FALSE)
  rm(fd, FDname)
  # End FD
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 2. SU, Number of summer days
  su <- climdex.su(ci)
  su <- as.data.frame(su)
  su[, "year"] <- as.integer(rownames(su))
  su <- su[, c("year", "su")]
  SUname <- file.path(suPath, FileName)
  write.csv(su, file = SUname, na = "NA", row.names = FALSE)
  rm(su, SUname)
  # End SU
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 3. ID, Number of icing days
  id <- climdex.id(ci)
  id <- as.data.frame(id)
  id[, "year"] <- as.integer(rownames(id))
  id <- id[, c("year", "id")]
  IDname <- file.path(idPath, FileName)
  write.csv(id, file = IDname, na = "NA", row.names = FALSE)
  rm(id, IDname)
  # End ID
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 4. TR, Number of tropical nights
  tr <- climdex.tr(ci)
  tr <- as.data.frame(tr)
  tr[, "year"] <- as.integer(rownames(tr))
  tr <- tr[, c("year", "tr")]
  TRname <- file.path(trPath, FileName)
  write.csv(tr, file = TRname, na = "NA", row.names = FALSE)
  rm(tr, TRname)
  # End TR
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 5. GLS, Growing season length
  gsl <- climdex.gsl(ci)
  gsl <- as.data.frame(gsl)
  gsl[, "year"] <- as.integer(rownames(gsl))
  gsl <- gsl[, c("year", "gsl")]
  GSLname <- file.path(gslPath, FileName)
  write.csv(gsl, file = GSLname, na = "NA", row.names = FALSE)
  rm(gsl, GSLname)
  # End GSL
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 6. TXX, Monthly maximum value of daily maximum temperature
  txx <- climdex.txx(ci, freq = "annual")
  txx <- as.data.frame(txx)
  txx[, "year"] <- as.integer(rownames(txx))
  # txx[, "year"]  <- as.integer(substr(rownames(txx), start = 1, stop = 4))
  # txx[, "month"] <- as.integer(substr(rownames(txx), start = 6, stop = 7))
  txx <- txx[, c("year", "txx")]
  TXXname <- file.path(txxPath, FileName)
  write.csv(txx, file = TXXname, na = "NA", row.names = FALSE)
  rm(txx, TXXname)
  # End TXx
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 7. TNX, Monthly maximum value of daily minimum temperature
  tnx <- climdex.tnx(ci, freq = "annual")
  tnx <- as.data.frame(tnx)
  tnx[, "year"]  <- as.integer(rownames(tnx))
  # tnx[, "year"]  <- as.integer(substr(rownames(tnx), start = 1, stop = 4))
  # tnx[, "month"] <- as.integer(substr(rownames(tnx), start = 6, stop = 7))
  tnx <- tnx[, c("year", "tnx")]
  TNXname <- file.path(tnxPath, FileName)
  write.csv(tnx, file = TNXname, na = "NA", row.names = FALSE)
  rm(tnx, TNXname)
  # End TNx
  #--------------------------------------------------------------------------------
  
  # 8. TXN, Monthly minimum value of daily maximum temperature
  txn <- climdex.txn(ci, freq = "annual")
  txn <- as.data.frame(txn)
  txn[, "year"] <- as.integer(rownames(txn))
  # txn[, "year"]  <- as.integer(substr(rownames(txn), start = 1, stop = 4))
  # txn[, "month"] <- as.integer(substr(rownames(txn), start = 6, stop = 7))
  txn <- txn[, c("year", "txn")]
  TXNname <- file.path(txnPath, FileName)
  write.csv(txn, file = TXNname, na = "NA", row.names = FALSE)
  rm(txn, TXNname)
  # End TXn
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 9. TNN, Monthly minimum value of daily minimum temperature
  tnn <- climdex.tnn(ci, freq = "annual")
  tnn <- as.data.frame(tnn)
  tnn[, "year"] <- as.integer(rownames(tnn))
  # tnn[, "year"]  <- as.integer(substr(rownames(tnn), start = 1, stop = 4))
  # tnn[, "month"] <- as.integer(substr(rownames(tnn), start = 6, stop = 7))
  tnn <- tnn[, c("year", "tnn")]
  TNNname <- file.path(tnnPath, FileName)
  write.csv(tnn, file = TNNname, na = "NA", row.names = FALSE)
  rm(tnn, TNNname)
  # End TNn
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 10. TN10p, Percentage of days when TN < 10th percentile
  tn10p <- climdex.tn10p(ci, freq = "annual")
  tn10p <- as.data.frame(tn10p)
  tn10p[, "year"] <- as.integer(rownames(tn10p))
  # tn10p[, "year"]  <- as.integer(substr(rownames(tn10p), start = 1, stop = 4))
  # tn10p[, "month"] <- as.integer(substr(rownames(tn10p), start = 6, stop = 7))
  tn10p <- tn10p[, c("year", "tn10p")]
  TN10pname <- file.path(tn10pPath, FileName)
  write.csv(tn10p, file = TN10pname, na = "NA", row.names = FALSE)
  rm(tn10p, TN10pname)
  # End TN10p
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 11. TX10p, Percentage of days when TX < 10th percentile
  tx10p <- climdex.tx10p(ci, freq = "annual")
  tx10p <- as.data.frame(tx10p)
  tx10p[, "year"] <- as.integer(rownames(tx10p))
  # tx10p[, "year"]  <- as.integer(substr(rownames(tx10p), start = 1, stop = 4))
  # tx10p[, "month"] <- as.integer(substr(rownames(tx10p), start = 6, stop = 7))
  tx10p <- tx10p[, c("year", "tx10p")]
  TX10pname <- file.path(tx10pPath, FileName)
  write.csv(tx10p, file = TX10pname, na = "NA", row.names = FALSE)
  rm(tx10p, TX10pname)
  # End TX10p
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # 12. TN90p, Percentage of days when TN > 90th percentile
  tn90p <- climdex.tn90p(ci, freq = "annual")
  tn90p <- as.data.frame(tn90p)
  tn90p[, "year"]  <- as.integer(rownames(tn90p))
  # tn90p[, "year"]  <- as.integer(substr(rownames(tn90p), start = 1, stop = 4))
  # tn90p[, "month"] <- as.integer(substr(rownames(tn90p), start = 6, stop = 7))
  tn90p <- tn90p[, c("year", "tn90p")]
  TN90pname <- file.path(tn90pPath, FileName)
  write.csv(tn90p, file = TN90pname, na = "NA", row.names = FALSE)
  rm(tn90p, TN90pname)
  # End TN90p
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # 13. TX90p, Percentage of days when TX > 90th percentile
  tx90p <- climdex.tx90p(ci, freq = "annual")
  tx90p <- as.data.frame(tx90p)
  tx90p[, "year"]  <- as.integer(rownames(tx90p))
  # tx90p[, "year"]  <- as.integer(substr(rownames(tx90p), start = 1, stop = 4))
  # tx90p[, "month"] <- as.integer(substr(rownames(tx90p), start = 6, stop = 7))
  tx90p <- tx90p[, c("year", "tx90p")]
  TX90pname <- file.path(tx90pPath, FileName)
  write.csv(tx90p, file = TX90pname, na = "NA", row.names = FALSE)
  rm(tx90p, TX90pname)
  # End TX90p
  #-----------------------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # 14. WSDI, Warm spell duration index
  wsdi <- climdex.wsdi(ci)
  wsdi <- as.data.frame(wsdi)
  wsdi[, "year"]  <- as.integer(rownames(wsdi))
  wsdi <- wsdi[, c("year", "wsdi")]
  WSDIname <- file.path(wsdiPath, FileName)
  write.csv(wsdi, file = WSDIname, na = "NA", row.names = FALSE)
  rm(wsdi, WSDIname)
  # End WSDI
  #----------------------------------------------------------------------------- 
  
  #-----------------------------------------------------------------------------
  # 15. CSDI, Cold spell duration index
  csdi <- climdex.csdi(ci)
  csdi <- as.data.frame(csdi)
  csdi[, "year"]  <- as.integer(rownames(csdi))
  csdi <- csdi[, c("year", "csdi")]
  csdiname <- file.path(csdiPath, FileName)
  write.csv(csdi, file = csdiname, na = "NA", row.names = FALSE)
  rm(csdi, csdiname)
  # End csdi
  #-----------------------------------------------------------------------------  
  
  #-----------------------------------------------------------------------------
  # 16. DTR, Daily temperature range
  dtr <- climdex.dtr(ci, freq = "annual")
  dtr <- as.data.frame(dtr)
  dtr[, "year"]  <- as.integer(rownames(dtr))
  dtr <- dtr[, c("year", "dtr")]
  dtrname <- file.path(dtrPath, FileName)
  write.csv(dtr, file = dtrname, na = "NA", row.names = FALSE)
  rm(dtr, dtrname)
  # End dtr
  #-----------------------------------------------------------------------------  
  
  
  #-----------------------------------------------------------------------------
  # 17. Rx1day, Monthly maximum 1-day precipitation
  rx1day <- climdex.rx1day(ci, freq = "annual")
  rx1day <- as.data.frame(rx1day)
  # rx1day[, "year"]  <- as.integer(substr(rownames(rx1day), start = 1, stop = 4))
  # rx1day[, "month"] <- as.integer(substr(rownames(rx1day), start = 6, stop = 7))
  # rx1day <- rx1day[, c("year", "month", "rx1day")]
  rx1day[, "year"]  <- as.integer(rownames(rx1day))
  rx1day <- rx1day[, c("year", "rx1day")]
  rx1dayName <- file.path(rx1dayPath, FileName)
  write.csv(rx1day, file = rx1dayName, na = "NA", row.names = FALSE)
  rm(rx1day, rx1dayName)
  # End Rx1day
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # 18. Rx5day, Monthly maximum consecutive 5-day precipitation
  rx5day <- climdex.rx5day(ci, freq = "annual")
  rx5day <- as.data.frame(rx5day)
  # rx5day[, "year"]  <- as.integer(substr(rownames(rx5day), start = 1, stop = 4))
  # rx5day[, "month"] <- as.integer(substr(rownames(rx5day), start = 6, stop = 7))
  # rx5day <- rx5day[, c("year", "month", "rx5day")]
  rx5day[, "year"] <- as.integer(rownames(rx5day))
  rx5day <- rx5day[, c("year", "rx5day")]
  rx5dayName <- file.path(rx5dayPath, FileName)
  write.csv(rx5day, file = rx5dayName, na = "NA", row.names = FALSE)
  rm(rx5day, rx5dayName)
  # End Rx5day
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # 19. SDII, Simple pricipitation intensity index. This is defined as the sum of 
  #     precipitation in wet days (days with preciptitation over 1mm) during the 
  #     year divided by the number of wet days in the year.
  sdii <- climdex.sdii(ci)
  sdii <- as.data.frame(sdii)
  sdii[, "year"] <- as.integer(rownames(sdii))
  sdii <- sdii[, c("year", "sdii")]
  sdiiName <- file.path(sdiiPath, FileName)
  write.csv(sdii, file = sdiiName, na = "NA", row.names = FALSE)
  rm(sdii, sdiiName)
  # End SDII
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 20. R10mm, Annual counts of days when PRCP >= 10mm
  r10mm <- climdex.r10mm(ci)
  r10mm <- as.data.frame(r10mm)
  r10mm[, "year"] <- as.integer(rownames(r10mm))
  r10mm <- r10mm[, c("year", "r10mm")]
  r10mmName <- file.path(r10mmPath, FileName)
  write.csv(r10mm, file = r10mmName, na = "NA", row.names = FALSE)
  rm(r10mm, r10mmName)
  # End R10mm
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 21. R20mm, Annual counts of days when PRCP >= 20mm
  r20mm <- climdex.r20mm(ci)
  r20mm <- as.data.frame(r20mm)
  r20mm[, "year"] <- as.integer(rownames(r20mm))
  r20mm <- r20mm[, c("year", "r20mm")]
  r20mmName <- file.path(r20mmPath, FileName)
  write.csv(r20mm, file = r20mmName, na = "NA", row.names = FALSE)
  rm(r20mm, r20mmName)
  # End R20mm
  #-----------------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------------
  # 22. Rnnmm, Annual counts of days when PRCP >= nnmm, nn is a user defined threshold
  rnnmm <- climdex.rnnmm(ci, threshold = 50)
  rnnmm <- as.data.frame(rnnmm)
  rnnmm[, "year"] <- as.integer(rownames(rnnmm))
  names(rnnmm)[1] <- "r50mm"
  r50mm <- rnnmm
  r50mm <- r50mm[, c("year", "r50mm")]
  r50mmName <- file.path(r50mmPath, FileName)
  write.csv(r50mm, file = r50mmName, na = "NA", row.names = FALSE)
  rm(r50mm, r50mmName)
  # End R50mm
  #-----------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 23. CDD, Maximum length of dry spell, maximum number of consecutive days with RR < 1mm
  cdd <- climdex.cdd(ci)
  cdd <- as.data.frame(cdd)
  cdd[, "year"] <- as.integer(rownames(cdd))
  cdd <- cdd[, c("year", "cdd")]
  cddName <- file.path(cddPath, FileName)
  write.csv(cdd, file = cddName, na = "NA", row.names = FALSE)
  rm(cdd, cddName)
  # End CDD
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 24. CWD, Maximum length of wet spell, maximum number of consecutive days with RR >= 1mm
  cwd <- climdex.cwd(ci)
  cwd <- as.data.frame(cwd)
  cwd[, "year"] <- as.integer(rownames(cwd))
  cwd <- cwd[, c("year", "cwd")]
  cwdName <- file.path(cwdPath, FileName)
  write.csv(cwd, file = cwdName, na = "NA", row.names = FALSE)
  rm(cwd, cwdName)
  # End CWD
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 25. R95pTOT, Annual total PRCP when RR > 95p.
  r95ptot <- climdex.r95ptot(ci)
  r95ptot <- as.data.frame(r95ptot)
  r95ptot[, "year"] <- as.integer(rownames(r95ptot))
  r95ptot <- r95ptot[, c("year", "r95ptot")]
  r95ptotName <- file.path(r95ptotPath, FileName)
  write.csv(r95ptot, file = r95ptotName, na = "NA", row.names = FALSE)
  rm(r95ptot, r95ptotName)
  # End R95pTOT
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 26. R99pTOT, Annual total PRCP when RR > 99p.
  r99ptot <- climdex.r99ptot(ci)
  r99ptot <- as.data.frame(r99ptot)
  r99ptot[, "year"] <- as.integer(rownames(r99ptot))
  r99ptot <- r99ptot[, c("year", "r99ptot")]
  r99ptotName <- file.path(r99ptotPath, FileName)
  write.csv(r99ptot, file = r99ptotName, na = "NA", row.names = FALSE)
  rm(r99ptot, r99ptotName)
  # End R99pTOT
  #--------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  # 27. PRCPTOT, Annual total precipitation in wet days.
  prcptot <- climdex.prcptot(ci)
  prcptot <- as.data.frame(prcptot)
  prcptot[, "year"] <- as.integer(rownames(prcptot))
  prcptot <- prcptot[, c("year", "prcptot")]
  prcptotName <- file.path(prcptotPath, FileName)
  write.csv(prcptot, file = prcptotName, na = "NA", row.names = FALSE)
  rm(prcptot, prcptotName)
  # End PRCPTOT
  #-----------------------------------------------------------------------------
  info <- sprintf("It has completed %d%%", round(i*100/StationNumbers)) 
  setTkProgressBar(pb, value = i*100/StationNumbers, label = info)
}
close(pb)

