# Project title: "The longitudinal interplay between personal values and subjective well-being"
# what is code about: R code for Resubmission of Stage 1 Registered Report
# Authors (e-mail):
# R version: 3.6.1
# R packages used (versions): gdata (2.18.0), haven (2.1.1), MplusAutomation (0.7-3)
# Date of last changes: 2019.11.18
# What changed compared to last version: I added the measurement invariance analysis for robustness check (age, gender education)


### Table of contents #### (see document outline)
# Chapter 1: PREPARATION (LOADING PACKAGES and DATA, LABELS and NAMES, CLEANING DATA)
# Chapter 2: Testing Measurement Invariance accross measurement occasions
# Chapter 2.1: Openness to change values
# Chapter 2.2: Conservation values
# Chapter 2.3: Self-transcendence
# Chapter 2.4: Self-enhancement
# Chapter 2.5: SWB: cognitive component
# Chapter 2.6: SWB: affective component
# Chapter 3: Main Analysis: Random Intercept Cross-Lagged Panel Models (RI-CLPMs)
# Chapter 4: Robustness Check for acqiescience, scale usage, and relative importance of value
# Chapter 5: Robustness checks (Testing Measurement Invariance over groups + test effects in groups)
# Chapter 5.1 we check whether measurement models are invariant across age, gender, and education groups
# Chapter 5.2 we do robustness check regarding age, gender, and education groups


### Chap. 1: PREPARATION (LOADING PACKAGES and DATA, LABELS and NAMES, CLEANING DATA) ####

### loadings packages
library(gdata) # to rename variables
library(haven) # lo load Stata files GESIS panel 
library(MplusAutomation) # to use Mplus via R


### functions we need later on

# functions we need to format numbers
nozero <- function(x, k=2) sub('^(-)?0[.]', '\\1.', format(round(x, k), nsmall=k))

# I wrote a function to extract fit indices, warnings and errors from MPlus Output Files
extr.Mplus.fit <- function(file.out="", fit.indices=TRUE )
{
  if(!file.exists(file.out)) {print(data.frame(matrix(NA,13,2)))}
  else{
    if(!is.character(file.out)) stop("ARGUMENT HAS TO BE A CHARACTER-STRING (LINK ! )")
    if(exists("df.fit")){df.fit$test <- c(rep(NA,13)) & 
      rename.vars(df.fit,from=c("test"),to=c(file.out))}
    else{df.fit <- data.frame(matrix(NA,13,1))}
    N <- NA
    CFI <- NA
    TLI <- NA
    RMSEA <- NA
    SRMR <- NA
    AIC <- NA
    BIC <- NA
    chi_sq <- NA
    df.fit[1,file.out] <- file.out
    TEXT <- scan(file.out, what = "character", sep = "\n", strip.white = FALSE, blank.lines.skip = FALSE, quiet = TRUE)
    N <- grep("Number of observations ",TEXT)
    if(length(N)>0){
      df.fit[2,file.out] <- as.numeric(substr(TEXT[N],62,67))
    }
    else{N <- grep("Total sample size ",TEXT) 
    if(length(N)>0){
      df.fit[2,file.out] <- as.numeric(substr(TEXT[N],62,67))
    }
    }
    if(fit.indices){
      CFI <- grep("CFI ",TEXT)
      if(length(CFI)>0){
        df.fit[3,file.out] <- as.numeric(substr(TEXT[CFI],45,54))
      }
      TLI <- grep("TLI ",TEXT)
      if(length(TLI)>0){
        df.fit[4,file.out] <- as.numeric(substr(TEXT[TLI],45,54))
      }
      RMSEA <- grep("RMSEA ",TEXT)
      if(length(RMSEA)>0){
        df.fit[5,file.out] <- as.numeric(substr(TEXT[RMSEA+2],45,52))[1]
      }
      SRMR <- grep("SRMR ",TEXT)
      if(length(SRMR)>0){
        df.fit[6,file.out] <- as.numeric(substr(TEXT[SRMR+2],45,54))
      }
      AIC <- grep("Akaike",TEXT)
      if(length(AIC)>0){
        df.fit[7,file.out] <- as.numeric(substr(TEXT[AIC],40,55))
      }
      BIC <- grep("Bayesian",TEXT)
      if(length(BIC)>0){
        df.fit[8,file.out] <- as.numeric(substr(TEXT[BIC],40,55))
      }
      chi_sq <- grep("Value                           ",TEXT)[1]
      if(length(chi_sq)>0){
        df.fit[9,file.out] <- as.numeric(substr(TEXT[chi_sq],35,50))
      }
    }
    param <- grep("Number of Free Parameters",TEXT)[1]
    if(length(param)>0){
      df.fit[10,file.out] <- as.numeric(substr(TEXT[param],45,54))
    }
    df.fit[11,file.out] <- length(grep("* WARNING",TEXT))
    df.fit[12,file.out] <- length(grep("* ERROR",TEXT))
    df.fit[13,file.out] <- length(grep("NON-POSITIVE",TEXT))
    rownames(df.fit) <- c("Outfile","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi-Square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
    print(df.fit)
  }
}

### Labels

# Personal Values
ST_t2 = c("bdze011a", "bdze015a", "bdze019a", "bdze023a", "bdze026a") # self-transcendence at Time 2
SE_t2 = c("bdze012a", "bdze016a", "bdze021a", "bdze025a") # self-enhancement at Time 2
OP_t2 = c("bdze013a", "bdze018a", "bdze020a", "bdze024a", "bdze027a") # openness to change at Time 2
CO_t2 = c("bdze014a", "bdze017a", "bdze022a") # conservation at Time 2
ST_t4 = c("cdze011a", "cdze015a", "cdze019a", "cdze023a", "cdze026a")
SE_t4 = c("cdze012a", "cdze016a", "cdze021a", "cdze025a")
OP_t4 = c("cdze013a", "cdze018a", "cdze020a", "cdze024a", "cdze027a")
CO_t4 = c("cdze014a", "cdze017a", "cdze022a")
ST_t6 = c("ddze011a", "ddze015a", "ddze019a", "ddze023a", "ddze026a")
SE_t6 = c("ddze012a", "ddze016a", "ddze021a", "ddze025a")
OP_t6 = c("ddze013a", "ddze018a", "ddze020a", "ddze024a", "ddze027a")
CO_t6 = c("ddze014a", "ddze017a", "ddze022a")
ST_t8 = c("edze011a", "edze015a", "edze019a", "edze023a", "edze026a")
SE_t8 = c("edze012a", "edze016a", "edze021a", "edze025a")
OP_t8 = c("edze013a", "edze018a", "edze020a", "edze024a", "edze027a")
CO_t8 = c("edze014a", "edze017a", "edze022a")
ST_t10 = c("fdze011a", "fdze015a", "fdze019a", "fdze023a", "fdze026a")
SE_t10 = c("fdze012a", "fdze016a", "fdze021a", "fdze025a")
OP_t10 = c("fdze013a", "fdze018a", "fdze020a", "fdze024a", "fdze027a")
CO_t10 = c("fdze014a", "fdze017a", "fdze022a")


# Cognitive component of SWB 
# One global life satisfaction item asked panelists how satisfied they were all in all with their current life (from 0 extremely unsatisfied to 10 extremely satisfied) 
# and seven domain-specific life satisfaction items asked panelists how satisfied they were with specific domains of their life (family, work, leisure, friends, neighbors, financial situation, and health; from 1 very unsatisfied to 7 very satisfied).
SWB_cog_t1 <- c("bazb005a", "bazb013a", "bazb014a", "bazb015a", "bazb016a", "bazb017a", "bazb018a") # at Time 1, the health item was not administered
SWB_cog_t3 <- c("cazb005a", "cazb014a", "cazb015a", "cazb016a", "cazb017a", "cazb018a", "cazb019a")
SWB_cog_t5 <- c("dazb005a", "dazb014a", "dazb015a", "dazb016a", "dazb017a", "dazb018a", "dazb019a")
SWB_cog_t7 <- c("eazb005a", "eazb014a", "eazb015a", "eazb016a", "eazb017a", "eazb018a", "eazb019a")
SWB_cog_t9 <- c("fazb005a", "fazb014a", "fazb015a", "fazb016a", "fazb017a", "fazb018a", "fazb019a")

# Affective component of SWB 
# The affective component of SWB was assessed by eight 6-point rating scale items that measure eight different affective states experienced during the last seven days (depressed, exhausted, restless sleep, happy, lonely, enjoyed life, sadness)
SWB_aff_t1 <- c("bazb019a", "bazb020a", "bazb021a", "bazb022a", "bazb023a", "bazb024a", "bazb025a", "bazb026a") 
SWB_aff_t3 <- c("cazb021a", "cazb022a", "cazb023a", "cazb024a", "cazb025a", "cazb026a", "cazb027a", "cazb028a")
SWB_aff_t5 <- c("dazb021a", "dazb022a", "dazb023a", "dazb024a", "dazb025a", "dazb026a", "dazb027a", "dazb028a")
SWB_aff_t7 <- c("eazb021a", "eazb022a", "eazb023a", "eazb024a", "eazb025a", "eazb026a", "eazb027a", "eazb028a")
SWB_aff_t9 <- c("fazb021a", "fazb022a", "fazb023a", "fazb024a", "fazb025a", "fazb026a", "fazb027a", "fazb028a")


# Gender
gender <- c("a11d054a", "bfzh069a", "cfzh071a", "dfzh037a", "d11d054a", "d12d088a", "efzh031a") # we will use all variables in case there are missings in some of them

# Age/Year of Birth

birth_year <- c("bfzh070b", "a11d056a", "d11d056a") # we will use all variables in case there are missings in some of them

# Education
# Education will be assessed with two dummies: (a) highest degree of education is lower secondary school or less (yes/no) and (b) highest degree of education is general qualification for university entrance (the German Abitur) or higher (yes/no).
high_edu <- c("bfzh076a", "cfzh078a", "dfzh044a", "efzh038a") # we will use all variables in case there are missings in some of them; 
# If the highest education changed over the course of the assessments, we will use the higher education in the analysis.


### Loading data

# setting working directory
root<- "D:/Values_SWB/Analysis" # indicating the root directory
setwd(root)

# loading several files of the data
data1 <- read_dta("ZA5665_a1_a11-a12_v23-0-0.dta")
data2 <- read_dta("ZA5665_a1_aa-ac_v23-0-0.dta")
data3 <- read_dta("ZA5665_a1_ba-bf_v23-0-0.dta")
data4 <- read_dta("ZA5665_a1_ca-cf_v23-0-0.dta")
data5 <- read_dta("ZA5665_a1_da-df_v23-0-0.dta")
data6 <- read_dta("ZA5665_a1_ea-ed_v23-0-0.dta")
data7 <- read_dta("ZA5665_a1_fa-ed_v23-0-0.dta")

# merging the files
data <- merge(data1,data2, by="z000001a",all.x=TRUE)
data <- merge(data,data3, by="z000001a",all.x=TRUE)
data <- merge(data,data4, by="z000001a",all.x=TRUE)
data <- merge(data,data5, by="z000001a",all.x=TRUE)
data <- merge(data,data6, by="z000001a",all.x=TRUE)
data <- merge(data,data7, by="z000001a",all.x=TRUE)


#### extracting only the variables we need
data <- data[, c("a11d054a", "bfzh069a", "cfzh071a", "dfzh037a", "d11d054a", "d12d088a", "efzh031a", # gender
                 "bfzh070b", "a11d056a", "d11d056a",  # year of birth
                 "bfzh076a", "cfzh078a", "dfzh044a", "efzh038a", # highest education
                 ST_t2, SE_t2, OP_t2, CO_t2, 
                 ST_t4, SE_t4, OP_t4, CO_t4, 
                 ST_t6, SE_t6, OP_t6, CO_t6,
                 ST_t8, SE_t8, OP_t8, CO_t8,
                 ST_t10, SE_t10, OP_t10, CO_t10,
                 SWB_cog_t1, SWB_cog_t3, SWB_cog_t5, SWB_cog_t7, SWB_cog_t9,
                 SWB_aff_t1, SWB_aff_t3, SWB_aff_t5, SWB_aff_t7, SWB_aff_t9)]



# recoding impossible answers as missing values
data[data=="-111"]=NA # Ambiguous answer
data[data=="-99"]=NA # Item nonresponse
data[data=="-88"]=NA # Missing by filter
data[data=="-77"]=NA # Not reached
data[data=="-66"]=NA # Missing by design
data[data=="-55"]=NA # Missing by technical error
data[data=="-44"]=NA # Missing by m.o.p.
data[data=="-33"]=NA # Unit nonresponse
data[data=="-22"]=NA # Not in panel
data[data=="-11"]=NA # Not invited
data[data==""]=NA


### preparing gender, age, and education variable

# gender
data$gender <- data[, "a11d054a"]  # 1 = Male; 2 = Female
# if gender is a missing value in the variable "a11d054a", we will substitute the value with the gender indicated at other measurement occasions 
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "bfzh069a"]
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "cfzh071a"]
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "dfzh037a"]
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "d11d054a"]
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "d12d088a"]
data[is.na(data$gender), "gender"] <- data[is.na(data$gender), "efzh031a"]

# age
data$birth_year <- data[, "bfzh070b"]
# if year of birth is a missing value in the variable "bfzh070b", we will substitute the value with the year of birth indicated at another measurement occasions 
data[is.na(data$birth_year), "birth_year"] <- data[is.na(data$birth_year), "a11d056a"]
data[is.na(data$birth_year), "birth_year"] <- data[is.na(data$birth_year), "d11d056a"]

data$age <- 2014-data$birth_year

# for multiple group analysis, we need three age categories (a) 18-35 (b) 36-54 and (c) 55 and older

data$age_c <- cut(data$age, 
                   breaks=c(-Inf, 35, 54, Inf), 
                   labels=FALSE)
table(data$age_c)

# highest education
data$edu <- data[, "efzh038a"] # we start with the last measurement time to get the highest education at the end of the study
# if education is missing in the variable "efzh038a", we will substitute it with the education indicated at other measurement occasions 
data[is.na(data$edu), "edu"] <- data[is.na(data$edu), "dfzh044a"]
data[is.na(data$edu), "edu"] <- data[is.na(data$edu), "cfzh078a"]
data[is.na(data$edu), "edu"] <- data[is.na(data$edu), "bfzh076a"]

# creating dummy for education
data$edu_d <- ifelse(data$edu==9, 1, 0) # If the participant has the Abitur (general qualification for university entrance), then the value is 1. If not, the value is 0. 



### 2: Testing Measurement Invariance accross measurement occasions  ####


# setting new root and working directory for measurement invariance analysis
dir.create(file.path(root, "inv"))
setwd(file.path(root,"inv"))



### 2.1 Openness to change values ####

# Personal Values
OP_t2 = c("bdze013a", "bdze018a", "bdze020a", "bdze024a", "bdze027a") # openness to change at Time 2
OP_t4 = c("cdze013a", "cdze018a", "cdze020a", "cdze024a", "cdze027a")
OP_t6 = c("ddze013a", "ddze018a", "ddze020a", "ddze024a", "ddze027a")
OP_t8 = c("edze013a", "edze018a", "edze020a", "edze024a", "edze027a")
OP_t10 = c("fdze013a", "fdze018a", "fdze020a", "fdze024a", "fdze027a")



#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze013a bdze018a bdze020a bdze024a bdze027a
cdze013a cdze018a cdze020a cdze024a cdze027a
ddze013a ddze018a ddze020a ddze024a ddze027a
edze013a edze018a edze020a edze024a edze027a
fdze013a fdze018a fdze020a fdze024a fdze027a;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
          !Setting Loadings NOT Equal Across Time
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l6-l10);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l11-l15);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l16-l20);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l21-l25);
          
          !correlated uniqueness
          bdze013a WITH cdze013a ddze013a edze013a fdze013a;
          cdze013a WITH ddze013a edze013a fdze013a;
          ddze013a WITH edze013a fdze013a;
          edze013a WITH fdze013a;
          
          bdze018a WITH cdze018a ddze018a edze018a fdze018a;
          cdze018a WITH ddze018a edze018a fdze018a;
          ddze018a WITH edze018a fdze018a;
          edze018a WITH fdze018a;
          
          bdze020a WITH cdze020a ddze020a edze020a fdze020a;
          cdze020a WITH ddze020a edze020a fdze020a;
          ddze020a WITH edze020a fdze020a;
          edze020a WITH fdze020a;
          
          bdze024a WITH cdze024a ddze024a edze024a fdze024a;
          cdze024a WITH ddze024a edze024a fdze024a;
          ddze024a WITH edze024a fdze024a;
          edze024a WITH fdze024a;
          
          bdze027a WITH cdze027a ddze027a edze027a fdze027a;
          cdze027a WITH ddze027a edze027a fdze027a;
          ddze027a WITH edze027a fdze027a;
          edze027a WITH fdze027a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i11-i15);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i16-i20);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i21-i25);
          
          ")

MODEL <- paste0(meas,"
                [open2 open4 open6 open8 open10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
          !Setting Loadings Equal Across Time
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !correlated uniqueness
          bdze013a WITH cdze013a ddze013a edze013a fdze013a;
          cdze013a WITH ddze013a edze013a fdze013a;
          ddze013a WITH edze013a fdze013a;
          edze013a WITH fdze013a;
          
          bdze018a WITH cdze018a ddze018a edze018a fdze018a;
          cdze018a WITH ddze018a edze018a fdze018a;
          ddze018a WITH edze018a fdze018a;
          edze018a WITH fdze018a;
          
          bdze020a WITH cdze020a ddze020a edze020a fdze020a;
          cdze020a WITH ddze020a edze020a fdze020a;
          ddze020a WITH edze020a fdze020a;
          edze020a WITH fdze020a;
          
          bdze024a WITH cdze024a ddze024a edze024a fdze024a;
          cdze024a WITH ddze024a edze024a fdze024a;
          ddze024a WITH edze024a fdze024a;
          edze024a WITH fdze024a;
          
          bdze027a WITH cdze027a ddze027a edze027a fdze027a;
          cdze027a WITH ddze027a edze027a fdze027a;
          ddze027a WITH edze027a fdze027a;
          edze027a WITH fdze027a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i11-i15);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i16-i20);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i21-i25);
          
          ")

MODEL <- paste0(meas,"
                [open2 open4 open6 open8 open10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
          !Setting Loadings Equal Across Time
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !correlated uniqueness
          bdze013a WITH cdze013a ddze013a edze013a fdze013a;
          cdze013a WITH ddze013a edze013a fdze013a;
          ddze013a WITH edze013a fdze013a;
          edze013a WITH fdze013a;
          
          bdze018a WITH cdze018a ddze018a edze018a fdze018a;
          cdze018a WITH ddze018a edze018a fdze018a;
          ddze018a WITH edze018a fdze018a;
          edze018a WITH fdze018a;
          
          bdze020a WITH cdze020a ddze020a edze020a fdze020a;
          cdze020a WITH ddze020a edze020a fdze020a;
          ddze020a WITH edze020a fdze020a;
          edze020a WITH fdze020a;
          
          bdze024a WITH cdze024a ddze024a edze024a fdze024a;
          cdze024a WITH ddze024a edze024a fdze024a;
          ddze024a WITH edze024a fdze024a;
          edze024a WITH fdze024a;
          
          bdze027a WITH cdze027a ddze027a edze027a fdze027a;
          cdze027a WITH ddze027a edze027a fdze027a;
          ddze027a WITH edze027a fdze027a;
          edze027a WITH fdze027a;
          
          !Setting Item Intercepts Equal Across Time
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          ")

MODEL <- paste0(meas,"
                [open2 open4 open6 open8 open10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("open_conf_inv.out", "open_weak_inv.out", "open_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats



### 2.2 Conservation values ####

# Personal Values
CO_t2 = c("bdze014a", "bdze017a", "bdze022a") # conservation at Time 2
CO_t4 = c("cdze014a", "cdze017a", "cdze022a")
CO_t6 = c("ddze014a", "ddze017a", "ddze022a")
CO_t8 = c("edze014a", "edze017a", "edze022a")
CO_t10 = c("fdze014a", "fdze017a", "fdze022a")



#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze014a bdze017a bdze022a
cdze014a cdze017a cdze022a
ddze014a ddze017a ddze022a
edze014a edze017a edze022a
fdze014a fdze017a fdze022a;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
          !Setting Loadings NOT Equal Across Time
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l4-l6);
          cons6 BY ddze014a ddze017a ddze022a (l7-l9);
          cons8 BY edze014a edze017a edze022a (l10-l12);
          cons10 BY fdze014a fdze017a fdze022a (l13-l15);
          
          !correlated uniqueness
          bdze014a WITH cdze014a ddze014a edze014a fdze014a;
          cdze014a WITH ddze014a edze014a fdze014a;
          ddze014a WITH edze014a fdze014a;
          edze014a WITH fdze014a;
          
          bdze017a WITH cdze017a ddze017a edze017a fdze017a;
          cdze017a WITH ddze017a edze017a fdze017a;
          ddze017a WITH edze017a fdze017a;
          edze017a WITH fdze017a;

          bdze022a WITH cdze022a ddze022a edze022a fdze022a;
          cdze022a WITH ddze022a edze022a fdze022a;
          ddze022a WITH edze022a fdze022a;
          edze022a WITH fdze022a;

          !Setting Item Intercepts NOT Equal Across Time
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i4-i6);
          [ddze014a@0 ddze017a ddze022a] (i7-i9);
          [edze014a@0 edze017a edze022a] (i10-i12);
          [fdze014a@0 fdze017a fdze022a] (i13-i15);
          
          ")

MODEL <- paste0(meas,"
                [cons2 cons4 cons6 cons8 cons10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
          !Setting Loadings Equal Across Time
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
          
          !correlated uniqueness
          bdze014a WITH cdze014a ddze014a edze014a fdze014a;
          cdze014a WITH ddze014a edze014a fdze014a;
          ddze014a WITH edze014a fdze014a;
          edze014a WITH fdze014a;
          
          bdze017a WITH cdze017a ddze017a edze017a fdze017a;
          cdze017a WITH ddze017a edze017a fdze017a;
          ddze017a WITH edze017a fdze017a;
          edze017a WITH fdze017a;
          
          bdze022a WITH cdze022a ddze022a edze022a fdze022a;
          cdze022a WITH ddze022a edze022a fdze022a;
          ddze022a WITH edze022a fdze022a;
          edze022a WITH fdze022a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i4-i6);
          [ddze014a@0 ddze017a ddze022a] (i7-i9);
          [edze014a@0 edze017a edze022a] (i10-i12);
          [fdze014a@0 fdze017a fdze022a] (i13-i15);
          
          ")

MODEL <- paste0(meas,"
                [cons2 cons4 cons6 cons8 cons10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
          !Setting Loadings Equal Across Time
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
          
          !correlated uniqueness
          bdze014a WITH cdze014a ddze014a edze014a fdze014a;
          cdze014a WITH ddze014a edze014a fdze014a;
          ddze014a WITH edze014a fdze014a;
          edze014a WITH fdze014a;
          
          bdze017a WITH cdze017a ddze017a edze017a fdze017a;
          cdze017a WITH ddze017a edze017a fdze017a;
          ddze017a WITH edze017a fdze017a;
          edze017a WITH fdze017a;
          
          bdze022a WITH cdze022a ddze022a edze022a fdze022a;
          cdze022a WITH ddze022a edze022a fdze022a;
          ddze022a WITH edze022a fdze022a;
          edze022a WITH fdze022a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i4-i6);
          [ddze014a@0 ddze017a ddze022a] (i7-i9);
          [edze014a@0 edze017a edze022a] (i10-i12);
          [fdze014a@0 fdze017a fdze022a] (i13-i15);
          
          ")

MODEL <- paste0(meas,"
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cons_conf_inv.out", "cons_weak_inv.out", "cons_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats 


### 2.3 Self-transcendence values ####

# Personal Values
ST_t2 = c("bdze011a", "bdze015a", "bdze019a", "bdze023a", "bdze026a") # self-transcendence at Time 2
ST_t4 = c("cdze011a", "cdze015a", "cdze019a", "cdze023a", "cdze026a")
ST_t6 = c("ddze011a", "ddze015a", "ddze019a", "ddze023a", "ddze026a")
ST_t8 = c("edze011a", "edze015a", "edze019a", "edze023a", "edze026a")
ST_t10 = c("fdze011a", "fdze015a", "fdze019a", "fdze023a", "fdze026a")



#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze011a bdze015a bdze019a bdze023a bdze026a
cdze011a cdze015a cdze019a cdze023a cdze026a
ddze011a ddze015a ddze019a ddze023a ddze026a
edze011a edze015a edze019a edze023a edze026a
fdze011a fdze015a fdze019a fdze023a fdze026a;
AUXILIARY = (M) gender age edu_d;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
            !Setting Loadings NOT Equal Across Time
            trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a  (l1-l5);
            trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a  (l6-l10);
            trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a  (l11-l15);
            trans8 BY edze011a edze015a edze019a edze023a edze026a  (l16-l20);
            trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a  (l21-l25);

            !correlated uniqueness
            bdze011a WITH cdze011a ddze011a edze011a fdze011a;
            cdze011a WITH ddze011a edze011a fdze011a;
            ddze011a WITH edze011a fdze011a;
            edze011a WITH fdze011a;

            bdze015a WITH cdze015a ddze015a edze015a fdze015a;
            cdze015a WITH ddze015a edze015a fdze015a;
            ddze015a WITH edze015a fdze015a;
            edze015a WITH fdze015a;

            bdze019a WITH cdze019a ddze019a edze019a fdze019a;
            cdze019a WITH ddze019a edze019a fdze019a;
            ddze019a WITH edze019a fdze019a;
            edze019a WITH fdze019a;

            bdze023a WITH cdze023a ddze023a edze023a fdze023a;
            cdze023a WITH ddze023a edze023a fdze023a;
            ddze023a WITH edze023a fdze023a;
            edze023a WITH fdze023a;

            bdze026a WITH cdze026a ddze026a edze026a fdze026a;
            cdze026a WITH ddze026a edze026a fdze026a;
            ddze026a WITH edze026a fdze026a;
            edze026a WITH fdze026a;

            !Setting Item Intercepts NOT Equal Across Time
            [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
            [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
            [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i11-i15);
            [edze011a@0 edze015a edze019a edze023a edze026a] (i16-i20);
            [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i21-i25);
                 
                 ")

MODEL <- paste0(meas,"
                [trans2 trans4 trans6 trans8 trans10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
            !Setting Loadings Equal Across Time
            trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a  (l1-l5);
            trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a  (l1-l5);
            trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a  (l1-l5);
            trans8 BY edze011a edze015a edze019a edze023a edze026a  (l1-l5);
            trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a  (l1-l5);

            !correlated uniqueness
            bdze011a WITH cdze011a ddze011a edze011a fdze011a;
            cdze011a WITH ddze011a edze011a fdze011a;
            ddze011a WITH edze011a fdze011a;
            edze011a WITH fdze011a;
          
            bdze015a WITH cdze015a ddze015a edze015a fdze015a;
            cdze015a WITH ddze015a edze015a fdze015a;
            ddze015a WITH edze015a fdze015a;
            edze015a WITH fdze015a;
          
            bdze019a WITH cdze019a ddze019a edze019a fdze019a;
            cdze019a WITH ddze019a edze019a fdze019a;
            ddze019a WITH edze019a fdze019a;
            edze019a WITH fdze019a;
          
            bdze023a WITH cdze023a ddze023a edze023a fdze023a;
            cdze023a WITH ddze023a edze023a fdze023a;
            ddze023a WITH edze023a fdze023a;
            edze023a WITH fdze023a;
          
            bdze026a WITH cdze026a ddze026a edze026a fdze026a;
            cdze026a WITH ddze026a edze026a fdze026a;
            ddze026a WITH edze026a fdze026a;
            edze026a WITH fdze026a;
            
            !Setting Item Intercepts NOT Equal Across Time
            [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
            [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
            [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i11-i15);
            [edze011a@0 edze015a edze019a edze023a edze026a] (i16-i20);
            [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i21-i25);
                 
                 ")

MODEL <- paste0(meas,"
                [trans2 trans4 trans6 trans8 trans10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
            !Setting Loadings Equal Across Time
            trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a  (l1-l5);
            trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a  (l1-l5);
            trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a  (l1-l5);
            trans8 BY edze011a edze015a edze019a edze023a edze026a  (l1-l5);
            trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a  (l1-l5);

            !correlated uniqueness
            bdze011a WITH cdze011a ddze011a edze011a fdze011a;
            cdze011a WITH ddze011a edze011a fdze011a;
            ddze011a WITH edze011a fdze011a;
            edze011a WITH fdze011a;
          
            bdze015a WITH cdze015a ddze015a edze015a fdze015a;
            cdze015a WITH ddze015a edze015a fdze015a;
            ddze015a WITH edze015a fdze015a;
            edze015a WITH fdze015a;
          
            bdze019a WITH cdze019a ddze019a edze019a fdze019a;
            cdze019a WITH ddze019a edze019a fdze019a;
            ddze019a WITH edze019a fdze019a;
            edze019a WITH fdze019a;
          
            bdze023a WITH cdze023a ddze023a edze023a fdze023a;
            cdze023a WITH ddze023a edze023a fdze023a;
            ddze023a WITH edze023a fdze023a;
            edze023a WITH fdze023a;
          
            bdze026a WITH cdze026a ddze026a edze026a fdze026a;
            cdze026a WITH ddze026a edze026a fdze026a;
            ddze026a WITH edze026a fdze026a;
            edze026a WITH fdze026a;
            
            !Setting Item Intercepts NOT Equal Across Time
            [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
            [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
            [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i11-i15);
            [edze011a@0 edze015a edze019a edze023a edze026a] (i16-i20);
            [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i21-i25);
                 
                 ")

MODEL <- paste0(meas,"
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("trans_conf_inv.out", "trans_weak_inv.out", "trans_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats 



### 2.4 Self-enhancement values ####

SE_t2 = c("bdze012a", "bdze016a", "bdze021a", "bdze025a") # self-enhancement at Time 2
SE_t4 = c("cdze012a", "cdze016a", "cdze021a", "cdze025a")
SE_t6 = c("ddze012a", "ddze016a", "ddze021a", "ddze025a")
SE_t8 = c("edze012a", "edze016a", "edze021a", "edze025a")
SE_t10 = c("fdze012a", "fdze016a", "fdze021a", "fdze025a")


#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze012a bdze016a bdze021a bdze025a
cdze012a cdze016a cdze021a cdze025a
ddze012a ddze016a ddze021a ddze025a
edze012a edze016a edze021a edze025a
fdze012a fdze016a fdze021a fdze025a;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
            !Setting Loadings NOT Equal Across Time
            enha2 BY  bdze012a bdze016a bdze021a bdze025a (l1-l4);
            enha4 BY  cdze012a cdze016a cdze021a cdze025a (l5-l8);
            enha6 BY  ddze012a ddze016a ddze021a ddze025a (l9-l12);
            enha8 BY  edze012a edze016a edze021a edze025a (l13-l6);
            enha10 BY fdze012a fdze016a fdze021a fdze025a (l17-l20);
            

            !correlated uniqueness
            bdze012a WITH cdze012a ddze012a edze012a fdze012a;
            cdze012a WITH ddze012a edze012a fdze012a;
            ddze012a WITH edze012a fdze012a;
            edze012a WITH fdze012a;

            bdze016a WITH cdze016a ddze016a edze016a fdze016a;
            cdze016a WITH ddze016a edze016a fdze016a;
            ddze016a WITH edze016a fdze016a;
            edze016a WITH fdze016a;

            bdze021a WITH cdze021a ddze021a edze021a fdze021a;
            cdze021a WITH ddze021a edze021a fdze021a;
            ddze021a WITH edze021a fdze021a;
            edze021a WITH fdze021a;

            bdze025a WITH cdze025a ddze025a edze025a fdze025a;
            cdze025a WITH ddze025a edze025a fdze025a;
            ddze025a WITH edze025a fdze025a;
            edze025a WITH fdze025a;

            !Setting Item Intercepts NOT Equal Across Time
            [bdze012a@0 bdze016a bdze021a bdze025a] (i1-i4);
            [cdze012a@0 cdze016a cdze021a cdze025a] (i5-i8);
            [ddze012a@0 ddze016a ddze021a ddze025a] (i9-i12);
            [edze012a@0 edze016a edze021a edze025a] (i13-i16);
            [fdze012a@0 fdze016a fdze021a fdze025a] (i17-i20);
                 
                 ")

MODEL <- paste0(meas,"
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
            !Setting Loadings Equal Across Time
            enha2 BY  bdze012a bdze016a bdze021a bdze025a (l1-l4);
            enha4 BY  cdze012a cdze016a cdze021a cdze025a (l1-l4);
            enha6 BY  ddze012a ddze016a ddze021a ddze025a (l1-l4);
            enha8 BY  edze012a edze016a edze021a edze025a (l1-l4);
            enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);

           !correlated uniqueness
            bdze012a WITH cdze012a ddze012a edze012a fdze012a;
            cdze012a WITH ddze012a edze012a fdze012a;
            ddze012a WITH edze012a fdze012a;
            edze012a WITH fdze012a;
          
            bdze016a WITH cdze016a ddze016a edze016a fdze016a;
            cdze016a WITH ddze016a edze016a fdze016a;
            ddze016a WITH edze016a fdze016a;
            edze016a WITH fdze016a;
          
            bdze021a WITH cdze021a ddze021a edze021a fdze021a;
            cdze021a WITH ddze021a edze021a fdze021a;
            ddze021a WITH edze021a fdze021a;
            edze021a WITH fdze021a;
          
            bdze025a WITH cdze025a ddze025a edze025a fdze025a;
            cdze025a WITH ddze025a edze025a fdze025a;
            ddze025a WITH edze025a fdze025a;
            edze025a WITH fdze025a;
            
            !Setting Item Intercepts NOT Equal Across Time
            [bdze012a@0 bdze016a bdze021a bdze025a] (i1-i4);
            [cdze012a@0 cdze016a cdze021a cdze025a] (i5-i8);
            [ddze012a@0 ddze016a ddze021a ddze025a] (i9-i12);
            [edze012a@0 edze016a edze021a edze025a] (i13-i16);
            [fdze012a@0 fdze016a fdze021a fdze025a] (i17-i20);
                 
                 ")

MODEL <- paste0(meas,"
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
            !Setting Loadings Equal Across Time
            enha2 BY  bdze012a bdze016a bdze021a bdze025a (l1-l4);
            enha4 BY  cdze012a cdze016a cdze021a cdze025a (l1-l4);
            enha6 BY  ddze012a ddze016a ddze021a ddze025a (l1-l4);
            enha8 BY  edze012a edze016a edze021a edze025a (l1-l4);
            enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);

            !correlated uniqueness
            bdze012a WITH cdze012a ddze012a edze012a fdze012a;
            cdze012a WITH ddze012a edze012a fdze012a;
            ddze012a WITH edze012a fdze012a;
            edze012a WITH fdze012a;
          
            bdze016a WITH cdze016a ddze016a edze016a fdze016a;
            cdze016a WITH ddze016a edze016a fdze016a;
            ddze016a WITH edze016a fdze016a;
            edze016a WITH fdze016a;
          
            bdze021a WITH cdze021a ddze021a edze021a fdze021a;
            cdze021a WITH ddze021a edze021a fdze021a;
            ddze021a WITH edze021a fdze021a;
            edze021a WITH fdze021a;
          
            bdze025a WITH cdze025a ddze025a edze025a fdze025a;
            cdze025a WITH ddze025a edze025a fdze025a;
            ddze025a WITH edze025a fdze025a;
            edze025a WITH fdze025a;
            
            !Setting Item Intercepts NOT Equal Across Time
            [bdze012a@0 bdze016a bdze021a bdze025a] (i1-i4);
            [cdze012a@0 cdze016a cdze021a cdze025a] (i5-i8);
            [ddze012a@0 ddze016a ddze021a ddze025a] (i9-i12);
            [edze012a@0 edze016a edze021a edze025a] (i13-i16);
            [fdze012a@0 fdze016a fdze021a fdze025a] (i17-i20);
                 
                 ")

MODEL <- paste0(meas,"
                [enha2 enha4 enha6 enha8 enha10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("enha_conf_inv.out", "enha_weak_inv.out", "enha_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats 



### 2.5 SWB: cognitive component ####

SWB_cog_t1 <- c("bazb005a", "bazb013a", "bazb014a", "bazb015a", "bazb016a", "bazb017a", "bazb018a") # at Time 1
SWB_cog_t3 <- c("cazb005a", "cazb014a", "cazb015a", "cazb016a", "cazb017a", "cazb018a", "cazb019a")
SWB_cog_t5 <- c("dazb005a", "dazb014a", "dazb015a", "dazb016a", "dazb017a", "dazb018a", "dazb019a")
SWB_cog_t7 <- c("eazb005a", "eazb014a", "eazb015a", "eazb016a", "eazb017a", "eazb018a", "eazb019a")
SWB_cog_t9 <- c("fazb005a", "fazb014a", "fazb015a", "fazb016a", "fazb017a", "fazb018a", "fazb019a")



#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
          !Setting Loadings NOT Equal Across Time
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l8-l14);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l15-l21);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l22-l28);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l29-l35);
          
          !correlated uniqueness
          bazb005a WITH cazb005a dazb005a eazb005a fazb005a;
          cazb005a WITH dazb005a eazb005a fazb005a;
          dazb005a WITH eazb005a fazb005a;
          eazb005a WITH fazb005a;

          bazb013a WITH cazb014a dazb014a eazb014a fazb014a;
          cazb014a WITH dazb014a eazb014a fazb014a;
          dazb014a WITH eazb014a fazb014a;
          eazb014a WITH fazb014a;

          bazb014a WITH cazb015a dazb015a eazb015a fazb015a;
          cazb015a WITH dazb015a eazb015a fazb015a;
          dazb015a WITH eazb015a fazb015a;
          eazb015a WITH fazb015a;

          bazb015a WITH cazb016a dazb016a eazb016a fazb016a;
          cazb016a WITH dazb016a eazb016a fazb016a;
          dazb016a WITH eazb016a fazb016a;
          eazb016a WITH fazb016a;

          bazb016a WITH cazb017a dazb017a eazb017a fazb017a;
          cazb017a WITH dazb017a eazb017a fazb017a;
          dazb017a WITH eazb017a fazb017a;
          eazb017a WITH fazb017a;
      
          bazb017a WITH cazb018a dazb018a eazb018a fazb018a;
          cazb018a WITH dazb018a eazb018a fazb018a;
          dazb018a WITH eazb018a fazb018a;
          eazb018a WITH fazb018a;

          bazb018a WITH cazb019a dazb019a eazb019a fazb019a;
          cazb019a WITH dazb019a eazb019a fazb019a;
          dazb019a WITH eazb019a fazb019a;
          eazb019a WITH fazb019a;

          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i15-i21);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i22-i27);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i28-i35);
          
          ")

MODEL <- paste0(meas,"
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
          !Setting Loadings Equal Across Time
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
          
          !correlated uniqueness
          bazb005a WITH cazb005a dazb005a eazb005a fazb005a;
          cazb005a WITH dazb005a eazb005a fazb005a;
          dazb005a WITH eazb005a fazb005a;
          eazb005a WITH fazb005a;
          
          bazb013a WITH cazb014a dazb014a eazb014a fazb014a;
          cazb014a WITH dazb014a eazb014a fazb014a;
          dazb014a WITH eazb014a fazb014a;
          eazb014a WITH fazb014a;
          
          bazb014a WITH cazb015a dazb015a eazb015a fazb015a;
          cazb015a WITH dazb015a eazb015a fazb015a;
          dazb015a WITH eazb015a fazb015a;
          eazb015a WITH fazb015a;
          
          bazb015a WITH cazb016a dazb016a eazb016a fazb016a;
          cazb016a WITH dazb016a eazb016a fazb016a;
          dazb016a WITH eazb016a fazb016a;
          eazb016a WITH fazb016a;
          
          bazb016a WITH cazb017a dazb017a eazb017a fazb017a;
          cazb017a WITH dazb017a eazb017a fazb017a;
          dazb017a WITH eazb017a fazb017a;
          eazb017a WITH fazb017a;
          
          bazb017a WITH cazb018a dazb018a eazb018a fazb018a;
          cazb018a WITH dazb018a eazb018a fazb018a;
          dazb018a WITH eazb018a fazb018a;
          eazb018a WITH fazb018a;
          
          bazb018a WITH cazb019a dazb019a eazb019a fazb019a;
          cazb019a WITH dazb019a eazb019a fazb019a;
          dazb019a WITH eazb019a fazb019a;
          eazb019a WITH fazb019a;
          
          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i15-i21);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i22-i28);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i29-i35);
          
          ")

MODEL <- paste0(meas,"
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
          !Setting Loadings Equal Across Time
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
          
          !correlated uniqueness
          bazb005a WITH cazb005a dazb005a eazb005a fazb005a;
          cazb005a WITH dazb005a eazb005a fazb005a;
          dazb005a WITH eazb005a fazb005a;
          eazb005a WITH fazb005a;
          
          bazb013a WITH cazb014a dazb014a eazb014a fazb014a;
          cazb014a WITH dazb014a eazb014a fazb014a;
          dazb014a WITH eazb014a fazb014a;
          eazb014a WITH fazb014a;
          
          bazb014a WITH cazb015a dazb015a eazb015a fazb015a;
          cazb015a WITH dazb015a eazb015a fazb015a;
          dazb015a WITH eazb015a fazb015a;
          eazb015a WITH fazb015a;
          
          bazb015a WITH cazb016a dazb016a eazb016a fazb016a;
          cazb016a WITH dazb016a eazb016a fazb016a;
          dazb016a WITH eazb016a fazb016a;
          eazb016a WITH fazb016a;
          
          bazb016a WITH cazb017a dazb017a eazb017a fazb017a;
          cazb017a WITH dazb017a eazb017a fazb017a;
          dazb017a WITH eazb017a fazb017a;
          eazb017a WITH fazb017a;
          
          bazb017a WITH cazb018a dazb018a eazb018a fazb018a;
          cazb018a WITH dazb018a eazb018a fazb018a;
          dazb018a WITH eazb018a fazb018a;
          eazb018a WITH fazb018a;
          
          bazb018a WITH cazb019a dazb019a eazb019a fazb019a;
          cazb019a WITH dazb019a eazb019a fazb019a;
          dazb019a WITH eazb019a fazb019a;
          eazb019a WITH fazb019a;

          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
          
          ")

MODEL <- paste0(meas,"
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cogn_conf_inv.out", "cogn_weak_inv.out", "cogn_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats 



## 2.6  SWB: affective component ####

SWB_aff_t1 <- c("bazb019a", "bazb020a", "bazb021a", "bazb022a", "bazb023a", "bazb024a", "bazb025a", "bazb026a") 
SWB_aff_t3 <- c("cazb021a", "cazb022a", "cazb023a", "cazb024a", "cazb025a", "cazb026a", "cazb027a", "cazb028a")
SWB_aff_t5 <- c("dazb021a", "dazb022a", "dazb023a", "dazb024a", "dazb025a", "dazb026a", "dazb027a", "dazb028a")
SWB_aff_t7 <- c("eazb021a", "eazb022a", "eazb023a", "eazb024a", "eazb025a", "eazb026a", "eazb027a", "eazb028a")
SWB_aff_t9 <- c("fazb021a", "fazb022a", "fazb023a", "fazb024a", "fazb025a", "fazb026a", "fazb027a", "fazb028a")

#fitting different measurement models (latent state models) items and comparing their fit indices to check invariance

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a;"

## 1. latent state model: configural invariance (loadings and intercept are NOT equal across measurement time)

meas <- c("
          !Setting Loadings NOT Equal Across Time
          affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
          affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l9-l16);
          affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l17-l24);
          affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l25-l32);
          affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l33-l40);
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;

          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;

          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;

          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;

          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;

          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;

          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;

          !Setting Item Intercepts NOT Equal Across Time
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i17-i24);
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i25-i32);
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i33-i40);
          
          ")

MODEL <- paste0(meas,"
                [affe1 affe3 affe5 affe7 affe9];
               ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_conf_inv.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across measurement time)

meas <- c("
          !Setting Loadings Equal Across Time
          affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
          affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
          affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
          affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
          affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;
          
          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;
          
          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;
          
          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;
          
          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;
          
          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;
          
          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i17-i24);
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i25-i32);
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i33-i40);
          
          ")

MODEL <- paste0(meas,"
                [affe1 affe3 affe5 affe7 affe9];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_weak_inv.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across measurement time; same model as used above) 


meas <- c("
          !Setting Loadings Equal Across Time
          affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
          affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
          affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
          affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
          affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;
          
          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;
          
          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;
          
          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;
          
          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;
          
          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;
          
          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
          
          ")

MODEL <- paste0(meas,"
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_strong_inv.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("affe_conf_inv.out", "affe_weak_inv.out", "affe_strong_inv.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats 




### 3 Main Analysis: Random Intercept Cross-Lagged Panel Models (RI-CLPMs) ####


dir.create(file.path(root, "main")) # creating folder for the main analysis
setwd(file.path(root, "main"))


# Hypothesis 1: increases in the preference for openness to change values lead to increases in SWB 
# Hypothesis 2: increases in SWB lead to increases in the preference for openness to change values 


# we will create loop for the analyses, thus we need lists


### lists for MEASUREMENT models for values

meas_val <- meas_swb <-  list()

# openness to change values
meas_val[[1]] <- c("
            VAL2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
            VAL4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
            VAL6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
            VAL8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
            VAL10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
                
                bdze013a WITH cdze013a ddze013a edze013a fdze013a;
                cdze013a WITH ddze013a edze013a fdze013a;
                ddze013a WITH edze013a fdze013a;
                edze013a WITH fdze013a;
                
                bdze018a WITH cdze018a ddze018a edze018a fdze018a;
                cdze018a WITH ddze018a edze018a fdze018a;
                ddze018a WITH edze018a fdze018a;
                edze018a WITH fdze018a;
                
                bdze020a WITH cdze020a ddze020a edze020a fdze020a;
                cdze020a WITH ddze020a edze020a fdze020a;
                ddze020a WITH edze020a fdze020a;
                edze020a WITH fdze020a;
                
                bdze024a WITH cdze024a ddze024a edze024a fdze024a;
                cdze024a WITH ddze024a edze024a fdze024a;
                ddze024a WITH edze024a fdze024a;
                edze024a WITH fdze024a;
                
                bdze027a WITH cdze027a ddze027a edze027a fdze027a;
                cdze027a WITH ddze027a edze027a fdze027a;
                ddze027a WITH edze027a fdze027a;
                edze027a WITH fdze027a;
                
                
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
                
                ")


# conservation values
meas_val[[2]] <- c("
              
              VAL2 BY bdze014a bdze017a bdze022a (l6-l8);
              VAL4 BY cdze014a cdze017a cdze022a (l6-l8);
              VAL6 BY ddze014a ddze017a ddze022a (l6-l8);
              VAL8 BY edze014a edze017a edze022a (l6-l8);
              VAL10 BY fdze014a fdze017a fdze022a (l6-l8);

                
                bdze014a WITH cdze014a ddze014a edze014a fdze014a;
                cdze014a WITH ddze014a edze014a fdze014a;
                ddze014a WITH edze014a fdze014a;
                edze014a WITH fdze014a;
                
                bdze017a WITH cdze017a ddze017a edze017a fdze017a;
                cdze017a WITH ddze017a edze017a fdze017a;
                ddze017a WITH edze017a fdze017a;
                edze017a WITH fdze017a;
                
                bdze022a WITH cdze022a ddze022a edze022a fdze022a;
                cdze022a WITH ddze022a edze022a fdze022a;
                ddze022a WITH edze022a fdze022a;
                edze022a WITH fdze022a;
                
                
                [bdze014a@0 bdze017a bdze022a] (i6-i8);
                [cdze014a@0 cdze017a cdze022a] (i6-i8);
                [ddze014a@0 ddze017a ddze022a] (i6-i8);
                [edze014a@0 edze017a edze022a] (i6-i8);
                [fdze014a@0 fdze017a fdze022a] (i6-i8);
                
                ")


# self-transcendence values
meas_val[[3]] <- c("
            !Setting Loadings Equal Across Time
            VAL2 BY bdze011a bdze015a bdze019a bdze023a bdze026a  (l9-l13);
            VAL4 BY cdze011a cdze015a cdze019a cdze023a cdze026a  (l9-l13);
            VAL6 BY ddze011a ddze015a ddze019a ddze023a ddze026a  (l9-l13);
            VAL8 BY edze011a edze015a edze019a edze023a edze026a  (l9-l13);
            VAL10 BY fdze011a fdze015a fdze019a fdze023a fdze026a  (l9-l13);

            !correlated uniqueness
            bdze011a WITH cdze011a ddze011a edze011a;
            cdze011a WITH ddze011a edze011a;
            ddze011a WITH edze011a;
            
            bdze015a WITH cdze015a ddze015a edze015a;
            cdze015a WITH ddze015a edze015a;
            ddze015a WITH edze015a;
            
            bdze019a WITH cdze019a ddze019a edze019a;
            cdze019a WITH ddze019a edze019a;
            ddze019a WITH edze019a;
            
            bdze023a WITH cdze023a ddze023a edze023a;
            cdze023a WITH ddze023a edze023a;
            ddze023a WITH edze023a;
            
            bdze026a WITH cdze026a ddze026a edze026a;
            cdze026a WITH ddze026a edze026a;
            ddze026a WITH edze026a;
            
            !Setting Item Intercepts Equal Across Time
            [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i9-i13);
            [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i9-i13);
            [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i9-i13);
            [edze011a@0 edze015a edze019a edze023a edze026a] (i9-i13);
            [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i9-i13);
                 
                 ")


# self-enhancement values
meas_val[[4]] <- c("
            !Setting Loadings NOT Equal Across Time
            VAL2 BY  bdze012a bdze016a bdze021a bdze025a (l14-l17);
            VAL4 BY  cdze012a cdze016a cdze021a cdze025a (l14-l17);
            VAL6 BY  ddze012a ddze016a ddze021a ddze025a (l14-l17);
            VAL8 BY  edze012a edze016a edze021a edze025a (l14-l17);
            VAL10 BY fdze012a fdze016a fdze021a fdze025a (l14-l17);
            

            !correlated uniqueness
            bdze012a WITH cdze012a ddze012a edze012a;
            cdze012a WITH ddze012a edze012a;
            ddze012a WITH edze012a;
            
            bdze016a WITH cdze016a ddze016a edze016a;
            cdze016a WITH ddze016a edze016a;
            ddze016a WITH edze016a;
            
            bdze021a WITH cdze021a ddze021a edze021a;
            cdze021a WITH ddze021a edze021a;
            ddze021a WITH edze021a;
            
            bdze025a WITH cdze025a ddze025a edze025a;
            cdze025a WITH ddze025a edze025a;
            ddze025a WITH edze025a;
            
            !Setting Item Intercepts Equal Across Time
            [bdze012a@0 bdze016a bdze021a bdze025a] (i14-i17);
            [cdze012a@0 cdze016a cdze021a cdze025a] (i14-i17);
            [ddze012a@0 ddze016a ddze021a ddze025a] (i14-i17);
            [edze012a@0 edze016a edze021a edze025a] (i14-i17);
            [fdze012a@0 fdze016a fdze021a fdze025a] (i14-i17);
                 
                 ")


### list fo MEASUREMENT models of SWB 

# SWB cognitive component
# most labels are different at Time 1.

meas_swb[[1]] <- c("
            SWB1 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l18-l24);
            SWB3 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l18-l24);
            SWB5 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l18-l24);
            SWB7 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l18-l24);
            SWB9 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l18-l24);
            

               bazb005a WITH cazb005a dazb005a fazb005a eazb005a;
               cazb005a WITH dazb005a fazb005a eazb005a;
               dazb005a WITH fazb005a eazb005a;
               fazb005a WITH eazb005a;
               
               bazb013a WITH cazb014a dazb014a fazb014a eazb014a;
               cazb014a WITH dazb014a fazb014a eazb014a;
               dazb014a WITH fazb014a eazb014a;
               fazb014a WITH eazb014a;
               
               bazb014a WITH cazb015a dazb015a fazb015a eazb015a;
               cazb015a WITH dazb015a fazb015a eazb015a;
               dazb015a WITH fazb015a eazb015a;
               fazb015a WITH eazb015a;
               
               bazb015a WITH cazb016a dazb016a fazb016a eazb016a;
               cazb016a WITH dazb016a fazb016a eazb016a;
               dazb016a WITH fazb016a eazb016a;
               fazb016a WITH eazb016a;
               
               bazb016a WITH cazb017a dazb017a fazb017a eazb017a;
               cazb017a WITH dazb017a fazb017a eazb017a;
               dazb017a WITH fazb017a eazb017a;
               fazb017a WITH eazb017a;
               
               bazb017a WITH cazb018a dazb018a fazb018a eazb018a;
               cazb018a WITH dazb018a fazb018a eazb018a;
               dazb018a WITH fazb018a eazb018a;
               fazb018a WITH eazb018a;
               
               bazb018a WITH cazb019a dazb019a fazb019a eazb019a;
               cazb019a WITH dazb019a fazb019a eazb019a;
               dazb019a WITH fazb019a eazb019a;
               fazb019a WITH eazb019a;


               [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i18-i24);
               [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i18-i24);
               [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i18-i24);
               [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i18-i24);
               [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i18-i24);

                  ")


meas_swb[[2]] <- c("
          !Setting Loadings Equal Across Time
          SWB1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l25-l32);
          SWB3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l25-l32);
          SWB5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l25-l32);
          SWB7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l25-l32);
          SWB9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l25-l32);
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;
          
          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;
          
          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;
          
          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;
          
          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;
          
          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;
          
          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i25-i32);
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i25-i32);
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i25-i32);
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i25-i32);
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i25-i32);

")


# we need lists for variable names that we want to use in each analysis

items_val <- items_swb <-  list()

items_val[[1]] <- "usevar = 
bdze013a bdze018a bdze020a bdze024a bdze027a 
cdze013a cdze018a cdze020a cdze024a cdze027a 
ddze013a ddze018a ddze020a ddze024a ddze027a 
edze013a edze018a edze020a edze024a edze027a 
fdze013a fdze018a fdze020a fdze024a fdze027a"

items_val[[2]] <- "usevar = 
bdze014a bdze017a bdze022a 
cdze014a cdze017a cdze022a 
ddze014a ddze017a ddze022a 
edze014a edze017a edze022a 
fdze014a fdze017a fdze022a"

items_val[[3]] <- "usevar = 
bdze011a bdze015a bdze019a bdze023a bdze026a 
cdze011a cdze015a cdze019a cdze023a cdze026a 
ddze011a ddze015a ddze019a ddze023a ddze026a 
edze011a edze015a edze019a edze023a edze026a 
fdze011a fdze015a fdze019a fdze023a fdze026a"

items_val[[4]] <- "usevar = 
bdze012a bdze016a bdze021a bdze025a
cdze012a cdze016a cdze021a cdze025a
ddze012a ddze016a ddze021a ddze025a
edze012a edze016a edze021a edze025a
fdze012a fdze016a fdze021a fdze025a"


items_swb[[1]] <- "
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a         
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a;
AUXILIARY = (M) gender age edu_d;" # auxiliary variables: gender age and education (dummy for Abitur or not)

items_swb[[2]] <- "
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a;
AUXILIARY = (M) gender age edu_d;"




### list for output files
value_SWB.nam <- list()
value_SWB.nam[[1]] <- c("open_cogSWB", "open_affSWB")
value_SWB.nam[[2]] <- c("cons_cogSWB", "cons_affSWB")
value_SWB.nam[[3]] <- c("trans_cogSWB", "trans_affSWB")
value_SWB.nam[[4]] <- c("enha_cogSWB", "enha_affSWB")



### loop to run the BI-variate RI-CLPMs (Hamaker et al., 2015)
### 8 RI-CLPMs each containing 6 and 18 months time lags


for (val in 1:4){  # val = indices for value
  for (swb in 1:2){ # swb = indices for subject well-being
    
    ITEMS <- paste0(items_val[[val]], items_swb[[swb]])
      
    MODEL <- paste0(meas_val[[val]], meas_swb[[swb]], "
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                SWB1 SWB3 SWB5 SWB7 SWB9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1a);
                L_val4 (r1);
                L_val6 (r1);
                L_val8 (r1);
                L_val10 (r1);
                L_swb1 (r2a);
                L_swb3 (r2b);
                L_swb5 (r2);
                L_swb7 (r2);
                L_swb9 (r2);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1);
                L_val8 on L_val6 (ar1);
                L_val6 on L_val4 (ar1);
                L_val4 on L_val2 (ar1);
                
                L_swb9 on L_swb7 (ar2);
                L_swb7 on L_swb5 (ar2);
                L_swb5 on L_swb3 (ar2);
                L_swb3 on L_swb1 (ar2a);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1);
                L_val8  on L_swb7 (cl1);
                L_val6  on L_swb5 (cl1);
                L_val4  on L_swb3 (cl1);
                L_val2  on L_swb1 (cl1a);
                
                L_swb9 on L_val8 (cl2);
                L_swb7 on L_val6 (cl2);
                L_swb5 on L_val4 (cl2);
                L_swb3 on L_val2 (cl2a);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1b);
                L_val8  on L_swb5 (cl1b);
                L_val6  on L_swb3 (cl1b);
                L_val4  on L_swb1 (cl1b);
                
                L_swb9 on L_val6 (cl2b);
                L_swb7 on L_val4 (cl2b);
                L_swb5 on L_val2 (cl2b);
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_v var_s V2_on_S1 S2_on_V1 dom_6m
                V4_on_S1 S4_on_V1 dom_18m);

                ! first, we need the var to standardize the coefficients
                var_v = ar1**2*var_v + cl1**2*var_s + cl1b**2*var_s + r1;
                var_s = ar2**2*var_s + cl2**2*var_v + cl2b**2*var_v + r2;

                ! then, we standardize the coefficients
                V2_on_S1 = cl1*sqrt(var_s)/sqrt(var_v);
                V4_on_S1 = cl1b*sqrt(var_s)/sqrt(var_v);
                S2_on_V1 = cl2*sqrt(var_v)/sqrt(var_s);
                S4_on_V1 = cl2b*sqrt(var_v)/sqrt(var_s);
                
                ! then, we test the dif bw standardized coef
                dom_6m = V2_on_S1 - S2_on_V1;
                dom_18m = V4_on_S1 - S4_on_V1;
                ")
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_SWB.nam[[val]][swb], ".inp"), run=1, check=F)
  }
}


### 4 Robustness Check for acqiescience, scale usage, and relative importance of value ####

# first, we change working directory

dir.create(file.path(root, "rob_acq")) # creating folder for the main analysis
setwd(file.path(root, "rob_acq"))


### we ipsatize the values items for this analysis

# computing the intrapersonal means for each measurement time
mean_val_t2 <- rowMeans(data[, c(ST_t2, SE_t2, OP_t2, CO_t2)], na.rm=T) # row mean of all values at Time 2
mean_val_t4 <- rowMeans(data[, c(ST_t4, SE_t4, OP_t4, CO_t4)], na.rm=T) # row mean of all values at Time 4
mean_val_t6 <- rowMeans(data[, c(ST_t6, SE_t6, OP_t6, CO_t6)], na.rm=T) # row mean of all values at Time 6
mean_val_t8 <- rowMeans(data[, c(ST_t8, SE_t8, OP_t8, CO_t8)], na.rm=T) # row mean of all values at Time 8
mean_val_t10 <- rowMeans(data[, c(ST_t10, SE_t10, OP_t10, CO_t10)], na.rm=T) #row mean of all values at Time 10


# we create a new data frame for the ipsatized data
data_ips <- data

data_ips[, c(ST_t2, SE_t2, OP_t2, CO_t2)] <- data_ips[, c(ST_t2, SE_t2, OP_t2, CO_t2)]-mean_val_t2
data_ips[, c(ST_t4, SE_t4, OP_t4, CO_t4)] <- data_ips[, c(ST_t4, SE_t4, OP_t4, CO_t4)]-mean_val_t4
data_ips[, c(ST_t6, SE_t6, OP_t6, CO_t6)] <- data_ips[, c(ST_t6, SE_t6, OP_t6, CO_t6)]-mean_val_t6
data_ips[, c(ST_t8, SE_t8, OP_t8, CO_t8)] <- data_ips[, c(ST_t8, SE_t8, OP_t8, CO_t8)]-mean_val_t8
data_ips[, c(ST_t10, SE_t10, OP_t10, CO_t10)] <- data_ips[, c(ST_t10, SE_t10, OP_t10, CO_t10)]-mean_val_t10


### loop to run the BI-variate RI-CLPMs (Hamaker et al., 2015)
### 8 RI-CLPMs each containing 6 and 18 months time lags
### the loop is exactly the same as in the main analysis (the working directory is different, and the value items are ipsatized in the data "data_ips")

for (val in 1:4){  # val = indices for value
  for (swb in 1:2){ # swb = indices for subject well-being
    
    ITEMS <- paste0(items_val[[val]], items_swb[[swb]])
    
    MODEL <- paste0(meas_val[[val]], meas_swb[[swb]], "
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                SWB1 SWB3 SWB5 SWB7 SWB9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1a);
                L_val4 (r1);
                L_val6 (r1);
                L_val8 (r1);
                L_val10 (r1);
                L_swb1 (r2a);
                L_swb3 (r2b);
                L_swb5 (r2);
                L_swb7 (r2);
                L_swb9 (r2);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1);
                L_val8 on L_val6 (ar1);
                L_val6 on L_val4 (ar1);
                L_val4 on L_val2 (ar1);
                
                L_swb9 on L_swb7 (ar2);
                L_swb7 on L_swb5 (ar2);
                L_swb5 on L_swb3 (ar2);
                L_swb3 on L_swb1 (ar2a);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1);
                L_val8  on L_swb7 (cl1);
                L_val6  on L_swb5 (cl1);
                L_val4  on L_swb3 (cl1);
                L_val2  on L_swb1 (cl1a);
                
                L_swb9 on L_val8 (cl2);
                L_swb7 on L_val6 (cl2);
                L_swb5 on L_val4 (cl2);
                L_swb3 on L_val2 (cl2a);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1b);
                L_val8  on L_swb5 (cl1b);
                L_val6  on L_swb3 (cl1b);
                L_val4  on L_swb1 (cl1b);
                
                L_swb9 on L_val6 (cl2b);
                L_swb7 on L_val4 (cl2b);
                L_swb5 on L_val2 (cl2b);
                
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_v var_s V2_on_S1 S2_on_V1 dom_6m
                V4_on_S1 S4_on_V1 dom_18m);

                ! first, we need the var to standardize the coefficients
                var_v = ar1**2*var_v + cl1**2*var_s + cl1b**2*var_s + r1;
                var_s = ar2**2*var_s + cl2**2*var_v + cl2b**2*var_v + r2;

                ! then, we standardize the coefficients
                V2_on_S1 = cl1*sqrt(var_s)/sqrt(var_v);
                V4_on_S1 = cl1b*sqrt(var_s)/sqrt(var_v);
                S2_on_V1 = cl2*sqrt(var_v)/sqrt(var_s);
                S4_on_V1 = cl2b*sqrt(var_v)/sqrt(var_s);
                
                ! then, we test the dif bw standardized coef
                dom_6m = V2_on_S1 - S2_on_V1;
                dom_18m = V4_on_S1 - S4_on_V1;
                ")
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data_ips),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data_ips,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_SWB.nam[[val]][swb], ".inp"), run=1, check=F)
  }
}



### 5 Difference between cognitive and affective subjective well-being (exploratory analysis)  ####


dir.create(file.path(root, "cog_vs_aff")) # creating folder for the main analysis
setwd(file.path(root, "cog_vs_aff"))

# we will create loop for the analyses. We can use most lists from the main analysis

# but we need new opject for measurement models of two components of SWB (instead of list)

# SWB cognitive AND affective component
# most labels are different at Time 1.

meas_2swb <- c("
            !Cognitive SWB
            COG1 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l18-l24);
            COG3 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l18-l24);
            COG5 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l18-l24);
            COG7 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l18-l24);
            COG9 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l18-l24);
            

               bazb005a WITH cazb005a dazb005a fazb005a eazb005a;
               cazb005a WITH dazb005a fazb005a eazb005a;
               dazb005a WITH fazb005a eazb005a;
               fazb005a WITH eazb005a;
               
               bazb013a WITH cazb014a dazb014a fazb014a eazb014a;
               cazb014a WITH dazb014a fazb014a eazb014a;
               dazb014a WITH fazb014a eazb014a;
               fazb014a WITH eazb014a;
               
               bazb014a WITH cazb015a dazb015a fazb015a eazb015a;
               cazb015a WITH dazb015a fazb015a eazb015a;
               dazb015a WITH fazb015a eazb015a;
               fazb015a WITH eazb015a;
               
               bazb015a WITH cazb016a dazb016a fazb016a eazb016a;
               cazb016a WITH dazb016a fazb016a eazb016a;
               dazb016a WITH fazb016a eazb016a;
               fazb016a WITH eazb016a;
               
               bazb016a WITH cazb017a dazb017a fazb017a eazb017a;
               cazb017a WITH dazb017a fazb017a eazb017a;
               dazb017a WITH fazb017a eazb017a;
               fazb017a WITH eazb017a;
               
               bazb017a WITH cazb018a dazb018a fazb018a eazb018a;
               cazb018a WITH dazb018a fazb018a eazb018a;
               dazb018a WITH fazb018a eazb018a;
               fazb018a WITH eazb018a;
               
               bazb018a WITH cazb019a dazb019a fazb019a eazb019a;
               cazb019a WITH dazb019a fazb019a eazb019a;
               dazb019a WITH fazb019a eazb019a;
               fazb019a WITH eazb019a;


               [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i18-i24);
               [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i18-i24);
               [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i18-i24);
               [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i18-i24);
               [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i18-i24);

          !Affective SWB
          AFF1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l25-l32);
          AFF3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l25-l32);
          AFF5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l25-l32);
          AFF7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l25-l32);
          AFF9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l25-l32);
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;
          
          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;
          
          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;
          
          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;
          
          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;
          
          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;
          
          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;
          
          !Setting Item Intercepts NOT Equal Across Time
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i25-i32);
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i25-i32);
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i25-i32);
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i25-i32);
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i25-i32);

                  ")

# and we need an object with all SWB items instead of the list for SWB items

all_items_swb <- "
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a         
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a;
AUXILIARY = (M) gender age edu_d;" # auxiliary variables: gender age and education (dummy for Abitur or not)




### list for output files
value_2SWB.nam <- list()
value_2SWB.nam[[1]] <- c("open")
value_2SWB.nam[[2]] <- c("cons")
value_2SWB.nam[[3]] <- c("trans")
value_2SWB.nam[[4]] <- c("enha")


### loop to run the TRI-variate RI-CLPMs (Hamaker et al., 2015)
### 4 RI-CLPMs each containing 6 and 18 months time lags

for (val in 1:4){  # val = indices for value
    
    ITEMS <- paste0(items_val[[val]], all_items_swb)
    
    MODEL <- paste0(meas_val[[val]], meas_2swb, "
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [COG1 COG3 COG5 COG7 COG9];
                [AFF1 AFF3 AFF5 AFF7 AFF9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                COG1 COG3 COG5 COG7 COG9;
                AFF1 AFF3 AFF5 AFF7 AFF9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_cog by COG1@1 COG3@1 COG5@1 COG7@1 COG9@1;
                eta_aff by AFF1@1 AFF3@1 AFF5@1 AFF7@1 AFF9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_cog@0];
                [eta_aff@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_cog;
                eta_aff;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_cog eta_aff;
                eta_cog with eta_aff;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_cog1@0;
                eta_val with L_aff1@0;
                eta_cog with L_val2@0;
                eta_cog with L_cog1@0;
                eta_cog with L_aff1@0;
                eta_aff with L_val2@0;
                eta_aff with L_cog1@0;
                eta_aff with L_aff1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                COG1@0;
                COG3@0;
                COG5@0;
                COG7@0;
                COG9@0;
                AFF1@0;
                AFF3@0;
                AFF5@0;
                AFF7@0;
                AFF9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_cog1 by COG1@1;
                L_cog3 by COG3@1;
                L_cog5 by COG5@1;
                L_cog7 by COG7@1;
                L_cog9 by COG9@1;
                
                L_aff1 by AFF1@1;
                L_aff3 by AFF3@1;
                L_aff5 by AFF5@1;
                L_aff7 by AFF7@1;
                L_aff9 by AFF9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_cog1@0 L_cog3@0 L_cog5@0 L_cog7@0 L_cog9@0];
                [L_aff1@0 L_aff3@0 L_aff5@0 L_aff7@0 L_aff9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1a);
                L_val4 (r1);
                L_val6 (r1);
                L_val8 (r1);
                L_val10 (r1);
                L_cog1 (r2a);
                L_cog3 (r2);
                L_cog5 (r2);
                L_cog7 (r2);
                L_cog9 (r2);
                L_aff1 (r3a);
                L_aff3 (r3);
                L_aff5 (r3);
                L_aff7 (r3);
                L_aff9 (r3);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1);
                L_val8 on L_val6 (ar1);
                L_val6 on L_val4 (ar1);
                L_val4 on L_val2 (ar1);
                
                L_cog9 on L_cog7 (ar2);
                L_cog7 on L_cog5 (ar2);
                L_cog5 on L_cog3 (ar2);
                L_cog3 on L_cog1 (ar2);
                
                L_aff9 on L_aff7 (ar3);
                L_aff7 on L_aff5 (ar3);
                L_aff5 on L_aff3 (ar3);
                L_aff3 on L_aff1 (ar3);
                
                !Constrained crosslags (6 months);
                ! a = for cog SWBM; b = for aff SWB
                L_val10 on L_cog9 (cl1a);
                L_val8  on L_cog7 (cl1a);
                L_val6  on L_cog5 (cl1a);
                L_val4  on L_cog3 (cl1a);
                L_val2  on L_cog1 (cl1a);
                
                L_val10 on L_aff9 (cl1b);
                L_val8  on L_aff7 (cl1b);
                L_val6  on L_aff5 (cl1b);
                L_val4  on L_aff3 (cl1b);
                L_val2  on L_aff1 (cl1b);
                
                L_cog9 on L_val8 (cl2a);
                L_cog7 on L_val6 (cl2a);
                L_cog5 on L_val4 (cl2a);
                L_cog3 on L_val2 (cl2a);
                
                L_aff9 on L_val8 (cl2b);
                L_aff7 on L_val6 (cl2b);
                L_aff5 on L_val4 (cl2b);
                L_aff3 on L_val2 (cl2b);
                
                !Constrained crosslags (18 months);
                L_val10 on L_cog7 (cl3a);
                L_val8  on L_cog5 (cl3a);
                L_val6  on L_cog3 (cl3a);
                L_val4  on L_cog1 (cl3a);

                L_val10 on L_aff7 (cl3b);
                L_val8  on L_aff5 (cl3b);
                L_val6  on L_aff3 (cl3b);
                L_val4  on L_aff1 (cl3b);
                
                L_cog9 on L_val6 (cl4a);
                L_cog7 on L_val4 (cl4a);
                L_cog5 on L_val2 (cl4a);
                
                L_aff9 on L_val6 (cl4b);
                L_aff7 on L_val4 (cl4b);
                L_aff5 on L_val2 (cl4b);
                
                ! covariance bw cog and aff
                L_cog1 with L_aff1 L_aff3;
                L_cog3 with L_aff1 L_aff3 L_aff5;
                L_cog5 with L_aff3 L_aff5 L_aff7;
                L_cog7 with L_aff5 L_aff7 L_aff9;
                L_cog9 with L_aff7 L_aff9;
                
                !testing difference bw effecs on cog and aff SWB 
                Model Constraints: 
                NEW (var_v var_c var_a C2_on_V1 A2_on_V1 
                C4_on_V1 A4_on_V1 C_vs_A6 C_vs_A18);

                ! first, we need the var to standardize the coefficients
                var_v = ar1**2*var_v + cl1a**2*var_c + cl1b**2*var_a + r1;
                var_c = ar2**2*var_c + cl2a**2*var_v + r2;
                var_a = ar3**2*var_a + cl2b**2*var_v + r3;

                ! now we standardize the coef
                C2_on_V1 = cl2a*sqrt(var_v)/sqrt(var_c);
                A2_on_V1 = cl2b*sqrt(var_v)/sqrt(var_a);
                
                C4_on_V1 = cl4a*sqrt(var_v)/sqrt(var_c);
                A4_on_V1 = cl4b*sqrt(var_v)/sqrt(var_a);
                
                ! then, we test the dif bw standardized coef
                C_vs_A6 = C2_on_V1 - A2_on_V1;
                C_vs_A18 = C4_on_V1 - A4_on_V1;
                
                ")
    
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_2SWB.nam[[val]], ".inp"), run=1, check=F)
}


### 6: Robustness Check for age, gender, education  ####


### 6.1 Measurement invariance across age, gender, and education groups ####


### 6.1.1 Openness to Change Values ####


########## measurement invariance for age

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "inv_age")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze013a bdze018a bdze020a bdze024a bdze027a
cdze013a cdze018a cdze020a cdze024a cdze027a
ddze013a ddze018a ddze020a ddze024a ddze027a
edze013a edze018a edze020a edze024a edze027a
fdze013a fdze018a fdze020a fdze024a fdze027a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c(" 
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a;
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a;
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a;
          open8 BY edze013a edze018a edze020a edze024a edze027a;
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a;
          
          !correlated uniqueness
          bdze013a WITH cdze013a ddze013a edze013a fdze013a;
          cdze013a WITH ddze013a edze013a fdze013a;
          ddze013a WITH edze013a fdze013a;
          edze013a WITH fdze013a;
          
          bdze018a WITH cdze018a ddze018a edze018a fdze018a;
          cdze018a WITH ddze018a edze018a fdze018a;
          ddze018a WITH edze018a fdze018a;
          edze018a WITH fdze018a;
          
          bdze020a WITH cdze020a ddze020a edze020a fdze020a;
          cdze020a WITH ddze020a edze020a fdze020a;
          ddze020a WITH edze020a fdze020a;
          edze020a WITH fdze020a;
          
          bdze024a WITH cdze024a ddze024a edze024a fdze024a;
          cdze024a WITH ddze024a edze024a fdze024a;
          ddze024a WITH edze024a fdze024a;
          edze024a WITH fdze024a;
          
          bdze027a WITH cdze027a ddze027a edze027a fdze027a;
          cdze027a WITH ddze027a edze027a fdze027a;
          ddze027a WITH edze027a fdze027a;
          edze027a WITH fdze027a;
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a];
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a];
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a];
          [edze013a@0 edze018a edze020a edze024a edze027a];
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a];
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];

          !factor variances are all free
          open2 open4 open6 open8 open10;
          ")


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
          MODEL YOUNG:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL MIDDLE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l6-l10);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l6-l10);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l6-l10);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l6-l10);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l6-l10);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL OLD:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l11-l15);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l11-l15);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l11-l15);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l11-l15);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l11-l15);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i11-i15);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i11-i15);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i11-i15);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i11-i15);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i11-i15);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
          MODEL YOUNG:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL MIDDLE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL OLD:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i11-i15);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i11-i15);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i11-i15);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i11-i15);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i11-i15);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 


MODEL <- paste0(meas,"
          MODEL YOUNG:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL MIDDLE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL OLD:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("open_conf_inv_age.out", "open_weak_inv_age.out", "open_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "inv_sex")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "inv_sex"))


# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze013a bdze018a bdze020a bdze024a bdze027a
cdze013a cdze018a cdze020a cdze024a cdze027a
ddze013a ddze018a ddze020a ddze024a ddze027a
edze013a edze018a edze020a edze024a edze027a
fdze013a fdze018a fdze020a fdze024a fdze027a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
          MODEL MALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL FEMALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l6-l10);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l6-l10);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l6-l10);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l6-l10);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l6-l10);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_conf_inv_sex.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
          MODEL MALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL FEMALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
          MODEL MALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL FEMALE:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("open_conf_inv_sex.out", "open_weak_inv_sex.out", "open_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for education group

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "inv_edu")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "inv_edu"))


# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze013a bdze018a bdze020a bdze024a bdze027a
cdze013a cdze018a cdze020a cdze024a cdze027a
ddze013a ddze018a ddze020a ddze024a ddze027a
edze013a edze018a edze020a edze024a edze027a
fdze013a fdze018a fdze020a fdze024a fdze027a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
          MODEL LOW:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL HIGH:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l6-l10);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l6-l10);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l6-l10);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l6-l10);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l6-l10);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_conf_inv_edu.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
          MODEL LOW:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL HIGH:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i6-i10);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i6-i10);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i6-i10);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i6-i10);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i6-i10);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
          MODEL LOW:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          
          
          MODEL HIGH:
          !Setting Loadings
          open2 BY bdze013a bdze018a bdze020a bdze024a bdze027a (l1-l5);
          open4 BY cdze013a cdze018a cdze020a cdze024a cdze027a (l1-l5);
          open6 BY ddze013a ddze018a ddze020a ddze024a ddze027a (l1-l5);
          open8 BY edze013a edze018a edze020a edze024a edze027a (l1-l5);
          open10 BY fdze013a fdze018a fdze020a fdze024a fdze027a (l1-l5);
          
          !Setting Item Intercepts
          [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i5);
          [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i5);
          [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i5);
          [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i5);
          [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i5);
          
          !freely estimate occasion-specific grand means 
          [open2 open4 open6 open8 open10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="open_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("open_conf_inv_edu.out", "open_weak_inv_edu.out", "open_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats



### 6.1.2 Conservation Values ####


########## measurement invariance for age

# setting working directory
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze014a bdze017a bdze022a
cdze014a cdze017a cdze022a
ddze014a ddze017a ddze022a
edze014a edze017a edze022a
fdze014a fdze017a fdze022a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c("
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a;
          cons4 BY cdze014a cdze017a cdze022a;
          cons6 BY ddze014a ddze017a ddze022a;
          cons8 BY edze014a edze017a edze022a;
          cons10 BY fdze014a fdze017a fdze022a;
          
          !correlated uniqueness
          bdze014a WITH cdze014a ddze014a edze014a fdze014a;
          cdze014a WITH ddze014a edze014a fdze014a;
          ddze014a WITH edze014a fdze014a;
          edze014a WITH fdze014a;
          
          bdze017a WITH cdze017a ddze017a edze017a fdze017a;
          cdze017a WITH ddze017a edze017a fdze017a;
          ddze017a WITH edze017a fdze017a;
          edze017a WITH fdze017a;

          bdze022a WITH cdze022a ddze022a edze022a fdze022a;
          cdze022a WITH ddze022a edze022a fdze022a;
          ddze022a WITH edze022a fdze022a;
          edze022a WITH fdze022a;

          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a];
          [cdze014a@0 cdze017a cdze022a];
          [ddze014a@0 ddze017a ddze022a];
          [edze014a@0 edze017a edze022a];
          [fdze014a@0 fdze017a fdze022a];
          
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];

          !factor variances are all free
          cons2 cons4 cons6 cons8 cons10;
          ")

## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
          MODEL YOUNG:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l4-l6);
                cons4 BY cdze014a cdze017a cdze022a (l4-l6);
                cons6 BY ddze014a ddze017a ddze022a (l4-l6);
                cons8 BY edze014a edze017a edze022a (l4-l6);
                cons10 BY fdze014a fdze017a fdze022a (l4-l6);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i4-i6);
                [cdze014a@0 cdze017a cdze022a] (i4-i6);
                [ddze014a@0 ddze017a ddze022a] (i4-i6);
                [edze014a@0 edze017a edze022a] (i4-i6);
                [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL OLD:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l7-l9);
                cons4 BY cdze014a cdze017a cdze022a (l7-l9);
                cons6 BY ddze014a ddze017a ddze022a (l7-l9);
                cons8 BY edze014a edze017a edze022a (l7-l9);
                cons10 BY fdze014a fdze017a fdze022a (l7-l9);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i7-i9);
                [cdze014a@0 cdze017a cdze022a] (i7-i9);
                [ddze014a@0 ddze017a ddze022a] (i7-i9);
                [edze014a@0 edze017a edze022a] (i7-i9);
                [fdze014a@0 fdze017a fdze022a] (i7-i9);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
          MODEL YOUNG:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i1-i3);
          [ddze014a@0 ddze017a ddze022a] (i1-i3);
          [edze014a@0 edze017a edze022a] (i1-i3);
          [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                
                
          MODEL MIDDLE:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i4-i6);
          [cdze014a@0 cdze017a cdze022a] (i4-i6);
          [ddze014a@0 ddze017a ddze022a] (i4-i6);
          [edze014a@0 edze017a edze022a] (i4-i6);
          [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                
                
          MODEL OLD:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i7-i9);
          [cdze014a@0 cdze017a cdze022a] (i7-i9);
          [ddze014a@0 ddze017a ddze022a] (i7-i9);
          [edze014a@0 edze017a edze022a] (i7-i9);
          [fdze014a@0 fdze017a fdze022a] (i7-i9);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
          MODEL YOUNG:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i1-i3);
          [ddze014a@0 ddze017a ddze022a] (i1-i3);
          [edze014a@0 edze017a edze022a] (i1-i3);
          [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                
                
          MODEL MIDDLE:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i1-i3);
          [ddze014a@0 ddze017a ddze022a] (i1-i3);
          [edze014a@0 edze017a edze022a] (i1-i3);
          [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                
                
          MODEL OLD:
          !Setting Loadings
          cons2 BY bdze014a bdze017a bdze022a (l1-l3);
          cons4 BY cdze014a cdze017a cdze022a (l1-l3);
          cons6 BY ddze014a ddze017a ddze022a (l1-l3);
          cons8 BY edze014a edze017a edze022a (l1-l3);
          cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
          !Setting Item Intercepts
          [bdze014a@0 bdze017a bdze022a] (i1-i3);
          [cdze014a@0 cdze017a cdze022a] (i1-i3);
          [ddze014a@0 ddze017a ddze022a] (i1-i3);
          [edze014a@0 edze017a edze022a] (i1-i3);
          [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
          !freely estimate occasion-specific grand means 
          [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cons_conf_inv_age.out", "cons_weak_inv_age.out", "cons_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# setting working directory
setwd(file.path(root, "inv_sex"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze014a bdze017a bdze022a
cdze014a cdze017a cdze022a
ddze014a ddze017a ddze022a
edze014a edze017a edze022a
fdze014a fdze017a fdze022a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l4-l6);
                cons4 BY cdze014a cdze017a cdze022a (l4-l6);
                cons6 BY ddze014a ddze017a ddze022a (l4-l6);
                cons8 BY edze014a edze017a edze022a (l4-l6);
                cons10 BY fdze014a fdze017a fdze022a (l4-l6);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i4-i6);
                [cdze014a@0 cdze017a cdze022a] (i4-i6);
                [ddze014a@0 ddze017a ddze022a] (i4-i6);
                [edze014a@0 edze017a edze022a] (i4-i6);
                [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_conf_inv_sex.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i4-i6);
                [cdze014a@0 cdze017a cdze022a] (i4-i6);
                [ddze014a@0 ddze017a ddze022a] (i4-i6);
                [edze014a@0 edze017a edze022a] (i4-i6);
                [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cons_conf_inv_sex.out", "cons_weak_inv_sex.out", "cons_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for education group


# setting working directory
setwd(file.path(root, "inv_edu"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze014a bdze017a bdze022a
cdze014a cdze017a cdze022a
ddze014a ddze017a ddze022a
edze014a edze017a edze022a
fdze014a fdze017a fdze022a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL HIGH:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l4-l6);
                cons4 BY cdze014a cdze017a cdze022a (l4-l6);
                cons6 BY ddze014a ddze017a ddze022a (l4-l6);
                cons8 BY edze014a edze017a edze022a (l4-l6);
                cons10 BY fdze014a fdze017a fdze022a (l4-l6);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i4-i6);
                [cdze014a@0 cdze017a cdze022a] (i4-i6);
                [ddze014a@0 ddze017a ddze022a] (i4-i6);
                [edze014a@0 edze017a edze022a] (i4-i6);
                [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_conf_inv_edu.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL HIGH:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i4-i6);
                [cdze014a@0 cdze017a cdze022a] (i4-i6);
                [ddze014a@0 ddze017a ddze022a] (i4-i6);
                [edze014a@0 edze017a edze022a] (i4-i6);
                [fdze014a@0 fdze017a fdze022a] (i4-i6);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                
                
                MODEL HIGH:
                !Setting Loadings
                cons2 BY bdze014a bdze017a bdze022a (l1-l3);
                cons4 BY cdze014a cdze017a cdze022a (l1-l3);
                cons6 BY ddze014a ddze017a ddze022a (l1-l3);
                cons8 BY edze014a edze017a edze022a (l1-l3);
                cons10 BY fdze014a fdze017a fdze022a (l1-l3);
                
                !Setting Item Intercepts
                [bdze014a@0 bdze017a bdze022a] (i1-i3);
                [cdze014a@0 cdze017a cdze022a] (i1-i3);
                [ddze014a@0 ddze017a ddze022a] (i1-i3);
                [edze014a@0 edze017a edze022a] (i1-i3);
                [fdze014a@0 fdze017a fdze022a] (i1-i3);
                
                !freely estimate occasion-specific grand means 
                [cons2 cons4 cons6 cons8 cons10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cons_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cons_conf_inv_edu.out", "cons_weak_inv_edu.out", "cons_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats



### 6.1.3 Self-Transcendence Values ####


########## measurement invariance for age

# setting working directory
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze011a bdze015a bdze019a bdze023a bdze026a
cdze011a cdze015a cdze019a cdze023a cdze026a
ddze011a ddze015a ddze019a ddze023a ddze026a
edze011a edze015a edze019a edze023a edze026a
fdze011a fdze015a fdze019a fdze023a fdze026a;
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c("
          !Setting Loadings
            trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a;
            trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a;
            trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a;
            trans8 BY edze011a edze015a edze019a edze023a edze026a;
            trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a;

            !correlated uniqueness
            bdze011a WITH cdze011a ddze011a edze011a fdze011a;
            cdze011a WITH ddze011a edze011a fdze011a;
            ddze011a WITH edze011a fdze011a;
            edze011a WITH fdze011a;

            bdze015a WITH cdze015a ddze015a edze015a fdze015a;
            cdze015a WITH ddze015a edze015a fdze015a;
            ddze015a WITH edze015a fdze015a;
            edze015a WITH fdze015a;

            bdze019a WITH cdze019a ddze019a edze019a fdze019a;
            cdze019a WITH ddze019a edze019a fdze019a;
            ddze019a WITH edze019a fdze019a;
            edze019a WITH fdze019a;

            bdze023a WITH cdze023a ddze023a edze023a fdze023a;
            cdze023a WITH ddze023a edze023a fdze023a;
            ddze023a WITH edze023a fdze023a;
            edze023a WITH fdze023a;

            bdze026a WITH cdze026a ddze026a edze026a fdze026a;
            cdze026a WITH ddze026a edze026a fdze026a;
            ddze026a WITH edze026a fdze026a;
            edze026a WITH fdze026a;

            !Setting Item Intercepts
            [bdze011a@0 bdze015a bdze019a bdze023a bdze026a];
            [cdze011a@0 cdze015a cdze019a cdze023a cdze026a];
            [ddze011a@0 ddze015a ddze019a ddze023a ddze026a];
            [edze011a@0 edze015a edze019a edze023a edze026a];
            [fdze011a@0 fdze015a fdze019a fdze023a fdze026a];

            !freely estimate occasion-specific grand means 
            [trans2 trans4 trans6 trans8 trans10];
          
            !factor variances are all free
            trans2 trans4 trans6 trans8 trans10;
            ")


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l6-l10);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l6-l10);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l6-l10);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l6-l10);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l6-l10);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL OLD:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l11-l15);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l11-l15);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l11-l15);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l11-l15);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l11-l15);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i11-i15);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i11-i15);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i11-i15);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i11-i15);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i11-i15);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL OLD:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i11-i15);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i11-i15);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i11-i15);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i11-i15);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i11-i15);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL OLD:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("trans_conf_inv_age.out", "trans_weak_inv_age.out", "trans_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# setting working directory
setwd(file.path(root, "inv_sex"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze011a bdze015a bdze019a bdze023a bdze026a
cdze011a cdze015a cdze019a cdze023a cdze026a
ddze011a ddze015a ddze019a ddze023a ddze026a
edze011a edze015a edze019a edze023a edze026a
fdze011a fdze015a fdze019a fdze023a fdze026a;
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l6-l10);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l6-l10);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l6-l10);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l6-l10);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l6-l10);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_conf_inv_sex.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 


MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("trans_conf_inv_sex.out", "trans_weak_inv_sex.out", "trans_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for education group


# setting working directory
setwd(file.path(root, "inv_edu"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze011a bdze015a bdze019a bdze023a bdze026a
cdze011a cdze015a cdze019a cdze023a cdze026a
ddze011a ddze015a ddze019a ddze023a ddze026a
edze011a edze015a edze019a edze023a edze026a
fdze011a fdze015a fdze019a fdze023a fdze026a;
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL HIGH:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l6-l10);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l6-l10);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l6-l10);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l6-l10);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l6-l10);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_conf_inv_edu.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL HIGH:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i6-i10);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i6-i10);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i6-i10);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i6-i10);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i6-i10);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 


MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                
                
                MODEL HIGH:
                !Setting Loadings
                trans2 BY bdze011a bdze015a bdze019a bdze023a bdze026a (l1-l5);
                trans4 BY cdze011a cdze015a cdze019a cdze023a cdze026a (l1-l5);
                trans6 BY ddze011a ddze015a ddze019a ddze023a ddze026a (l1-l5);
                trans8 BY edze011a edze015a edze019a edze023a edze026a (l1-l5);
                trans10 BY fdze011a fdze015a fdze019a fdze023a fdze026a (l1-l5);
                
                !Setting Item Intercepts
                [bdze011a@0 bdze015a bdze019a bdze023a bdze026a] (i1-i5);
                [cdze011a@0 cdze015a cdze019a cdze023a cdze026a] (i1-i5);
                [ddze011a@0 ddze015a ddze019a ddze023a ddze026a] (i1-i5);
                [edze011a@0 edze015a edze019a edze023a edze026a] (i1-i5);
                [fdze011a@0 fdze015a fdze019a fdze023a fdze026a] (i1-i5);
                
                !freely estimate occasion-specific grand means 
                [trans2 trans4 trans6 trans8 trans10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="trans_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("trans_conf_inv_edu.out", "trans_weak_inv_edu.out", "trans_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


### 6.1.4 Self-Enhancement Values ####


########## measurement invariance for age

# setting working directory
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze012a bdze016a bdze021a bdze025a
cdze012a cdze016a cdze021a cdze025a
ddze012a ddze016a ddze021a ddze025a
edze012a edze016a edze021a edze025a
fdze012a fdze016a fdze021a fdze025a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c("
          !Setting Loadings
          enha2 BY  bdze012a bdze016a bdze021a bdze025a;
          enha4 BY  cdze012a cdze016a cdze021a cdze025a;
          enha6 BY  ddze012a ddze016a ddze021a ddze025a;
          enha8 BY  edze012a edze016a edze021a edze025a;
          enha10 BY fdze012a fdze016a fdze021a fdze025a;
          
          
          !correlated uniqueness
          bdze012a WITH cdze012a ddze012a edze012a fdze012a;
          cdze012a WITH ddze012a edze012a fdze012a;
          ddze012a WITH edze012a fdze012a;
          edze012a WITH fdze012a;
          
          bdze016a WITH cdze016a ddze016a edze016a fdze016a;
          cdze016a WITH ddze016a edze016a fdze016a;
          ddze016a WITH edze016a fdze016a;
          edze016a WITH fdze016a;
          
          bdze021a WITH cdze021a ddze021a edze021a fdze021a;
          cdze021a WITH ddze021a edze021a fdze021a;
          ddze021a WITH edze021a fdze021a;
          edze021a WITH fdze021a;
          
          bdze025a WITH cdze025a ddze025a edze025a fdze025a;
          cdze025a WITH ddze025a edze025a fdze025a;
          ddze025a WITH edze025a fdze025a;
          edze025a WITH fdze025a;
          
          !Setting Item Intercepts
          [bdze012a@0 bdze016a bdze021a bdze025a];
          [cdze012a@0 cdze016a cdze021a cdze025a];
          [ddze012a@0 ddze016a ddze021a ddze025a];
          [edze012a@0 edze016a edze021a edze025a];
          [fdze012a@0 fdze016a fdze021a fdze025a];

          !freely estimate occasion-specific grand means 
          [enha2 enha4 enha6 enha8 enha10];
          
          !factor variances are all free
          enha2 enha4 enha6 enha8 enha10;
          ")


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l5-l8);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l5-l8);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l5-l8);
                enha8 BY edze012a edze016a edze021a edze025a (l5-l8);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l5-l8);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL OLD:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l9-l12);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l9-l12);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l9-l12);
                enha8 BY edze012a edze016a edze021a edze025a (l9-l12);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l9-l12);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i9-i12);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i9-i12);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i9-i12);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i9-i12);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i9-i12);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL OLD:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i9-i12);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i9-i12);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i9-i12);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i9-i12);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i9-i12);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL MIDDLE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL OLD:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("enha_conf_inv_age.out", "enha_weak_inv_age.out", "enha_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# setting working directory
setwd(file.path(root, "inv_sex"))


# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze012a bdze016a bdze021a bdze025a
cdze012a cdze016a cdze021a cdze025a
ddze012a ddze016a ddze021a ddze025a
edze012a edze016a edze021a edze025a
fdze012a fdze016a fdze021a fdze025a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l5-l8);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l5-l8);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l5-l8);
                enha8 BY edze012a edze016a edze021a edze025a (l5-l8);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l5-l8);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_conf_inv_sex.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 


MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL FEMALE:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("enha_conf_inv_sex.out", "enha_weak_inv_sex.out", "enha_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for education group

# setting working directory
setwd(file.path(root, "inv_edu"))


# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bdze012a bdze016a bdze021a bdze025a
cdze012a cdze016a cdze021a cdze025a
ddze012a ddze016a ddze021a ddze025a
edze012a edze016a edze021a edze025a
fdze012a fdze016a fdze021a fdze025a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL HIGH:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l5-l8);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l5-l8);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l5-l8);
                enha8 BY edze012a edze016a edze021a edze025a (l5-l8);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l5-l8);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_conf_inv_edu.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL HIGH:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i5-i8);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i5-i8);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i5-i8);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i5-i8);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i5-i8);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                
                
                MODEL HIGH:
                !Setting Loadings
                enha2 BY bdze012a bdze016a bdze021a bdze025a (l1-l4);
                enha4 BY cdze012a cdze016a cdze021a cdze025a (l1-l4);
                enha6 BY ddze012a ddze016a ddze021a ddze025a (l1-l4);
                enha8 BY edze012a edze016a edze021a edze025a (l1-l4);
                enha10 BY fdze012a fdze016a fdze021a fdze025a (l1-l4);
                
                !Setting Item Intercepts
                [bdze013a@0 bdze018a bdze020a bdze024a bdze027a] (i1-i4);
                [cdze013a@0 cdze018a cdze020a cdze024a cdze027a] (i1-i4);
                [ddze013a@0 ddze018a ddze020a ddze024a ddze027a] (i1-i4);
                [edze013a@0 edze018a edze020a edze024a edze027a] (i1-i4);
                [fdze013a@0 fdze018a fdze020a fdze024a fdze027a] (i1-i4);
                
                !freely estimate occasion-specific grand means 
                [enha2 enha4 enha6 enha8 enha10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="enha_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("enha_conf_inv_edu.out", "enha_weak_inv_edu.out", "enha_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


### 6.1.5 Cognitive Component of SWB ####


########## measurement invariance for age

# setting working directory
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c("
          !Setting Loadings
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a;
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a;
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a;
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a;
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a;
          
          !correlated uniqueness
          bazb005a WITH cazb005a dazb005a eazb005a fazb005a;
          cazb005a WITH dazb005a eazb005a fazb005a;
          dazb005a WITH eazb005a fazb005a;
          eazb005a WITH fazb005a;
          
          bazb013a WITH cazb014a dazb014a eazb014a fazb014a;
          cazb014a WITH dazb014a eazb014a fazb014a;
          dazb014a WITH eazb014a fazb014a;
          eazb014a WITH fazb014a;
          
          bazb014a WITH cazb015a dazb015a eazb015a fazb015a;
          cazb015a WITH dazb015a eazb015a fazb015a;
          dazb015a WITH eazb015a fazb015a;
          eazb015a WITH fazb015a;
          
          bazb015a WITH cazb016a dazb016a eazb016a fazb016a;
          cazb016a WITH dazb016a eazb016a fazb016a;
          dazb016a WITH eazb016a fazb016a;
          eazb016a WITH fazb016a;
          
          bazb016a WITH cazb017a dazb017a eazb017a fazb017a;
          cazb017a WITH dazb017a eazb017a fazb017a;
          dazb017a WITH eazb017a fazb017a;
          eazb017a WITH fazb017a;
          
          bazb017a WITH cazb018a dazb018a eazb018a fazb018a;
          cazb018a WITH dazb018a eazb018a fazb018a;
          dazb018a WITH eazb018a fazb018a;
          eazb018a WITH fazb018a;
          
          bazb018a WITH cazb019a dazb019a eazb019a fazb019a;
          cazb019a WITH dazb019a eazb019a fazb019a;
          dazb019a WITH eazb019a fazb019a;
          eazb019a WITH fazb019a;
          
          
          !Setting Item Intercepts
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a];
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a];
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a];
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a];
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a];

          !freely estimate occasion-specific grand means 
          [cogn2 cogn4 cogn6 cogn8 cogn10];
          
          !factor variances are all free
          cogn2 cogn4 cogn6 cogn8 cogn10;
          ")


## 1. configural invariance (loadings and intercept are NOT equal across age groups)


MODEL <- paste0(meas,"
         MODEL YOUNG:
          !Setting Loadings
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);

          !Setting Item Intercepts
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);

          !freely estimate occasion-specific grand means 
          [cogn2 cogn4 cogn6 cogn8 cogn10];

          MODEL MIDDLE:
          !Setting Loadings
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l8-l14);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l8-l14);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l8-l14);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l8-l14);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l8-l14);
                
          !Setting Item Intercepts
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
          !freely estimate occasion-specific grand means 
          [cogn2 cogn4 cogn6 cogn8 cogn10];

          MODEL OLD:
          !Setting Loadings
          cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l15-l21);
          cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l15-l21);
          cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l15-l21);
          cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l15-l21);
          cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l15-l21);
                
          !Setting Item Intercepts
          [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i15-i21);
          [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i15-i21);
          [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i15-i21);
          [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i15-i21);
          [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i15-i21);
                
          !freely estimate occasion-specific grand means 
          [cogn2 cogn4 cogn6 cogn8 cogn10];
          ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL MIDDLE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL OLD:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i15-i21);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i15-i21);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i15-i21);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i15-i21);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i15-i21);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL MIDDLE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL OLD:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cogn_conf_inv_age.out", "cogn_weak_inv_age.out", "cogn_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# setting working directory
setwd(file.path(root, "inv_sex"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"

## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL FEMALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l8-l14);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l8-l14);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l8-l14);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l8-l14);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l8-l14);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_conf_inv_sex.inp", run=1, check=F)


# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL FEMALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL FEMALE:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cogn_conf_inv_sex.out", "cogn_weak_inv_sex.out", "cogn_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for education group

# setting working directory
setwd(file.path(root, "inv_edu"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"

## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL HIGH:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l8-l14);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l8-l14);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l8-l14);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l8-l14);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l8-l14);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_conf_inv_edu.inp", run=1, check=F)


# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL HIGH:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i8-i14);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i8-i14);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i8-i14);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i8-i14);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i8-i14);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                
                MODEL HIGH:
                !Setting Loadings
                cogn2 BY bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a (l1-l7);
                cogn4 BY cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a (l1-l7);
                cogn6 BY dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a (l1-l7);
                cogn8 BY eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a (l1-l7);
                cogn10 BY fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a (l1-l7);
                
                !Setting Item Intercepts
                [bazb005a@0 bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a] (i1-i7);
                [cazb005a@0 cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a] (i1-i7);
                [dazb005a@0 dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a] (i1-i7);
                [eazb005a@0 eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a] (i1-i7);
                [fazb005a@0 fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a] (i1-i7);
                
                !freely estimate occasion-specific grand means 
                [cogn2 cogn4 cogn6 cogn8 cogn10];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="cogn_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("cogn_conf_inv_edu.out", "cogn_weak_inv_edu.out", "cogn_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


### 6.1.6 Affective Component of SWB ####


########## measurement invariance for age

# setting working directory
setwd(file.path(root, "inv_age"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


meas <- c("
          !Setting Loadings
          affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a;
          affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a;
          affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a;
          affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a;
          affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a;
          
          !correlated uniqueness
          bazb019a WITH cazb021a dazb021a eazb021a fazb021a;
          cazb021a WITH dazb021a eazb021a fazb021a;
          dazb021a WITH eazb021a fazb021a;
          eazb021a WITH fazb021a;
          
          bazb020a WITH cazb022a dazb022a eazb022a fazb022a;
          cazb022a WITH dazb022a eazb022a fazb022a;
          dazb022a WITH eazb022a fazb022a;
          eazb022a WITH fazb022a;
          
          bazb021a WITH cazb023a dazb023a eazb023a fazb023a;
          cazb023a WITH dazb023a eazb023a fazb023a;
          dazb023a WITH eazb023a fazb023a;
          eazb023a WITH fazb023a;
          
          bazb022a WITH cazb024a dazb024a eazb024a fazb024a;
          cazb024a WITH dazb024a eazb024a fazb024a;
          dazb024a WITH eazb024a fazb024a;
          eazb024a WITH fazb024a;
          
          bazb023a WITH cazb025a dazb025a eazb025a fazb025a;
          cazb025a WITH dazb025a eazb025a fazb025a;
          dazb025a WITH eazb025a fazb025a;
          eazb025a WITH fazb025a;
          
          bazb024a WITH cazb026a dazb026a eazb026a fazb026a;
          cazb026a WITH dazb026a eazb026a fazb026a;
          dazb026a WITH eazb026a fazb026a;
          eazb026a WITH fazb026a;
          
          bazb025a WITH cazb027a dazb027a eazb027a fazb027a;
          cazb027a WITH dazb027a eazb027a fazb027a;
          dazb027a WITH eazb027a fazb027a;
          eazb027a WITH fazb027a;
          
          bazb026a WITH cazb028a dazb028a eazb028a fazb028a;
          cazb028a WITH dazb028a eazb028a fazb028a;
          dazb028a WITH eazb028a fazb028a;
          eazb028a WITH fazb028a;
          
          !Setting Item Intercepts
          [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a];
          [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a];
          [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a];
          [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a];
          [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a];
          
          !freely estimate occasion-specific grand means 
          [affe1 affe3 affe5 affe7 affe9];
          
          !factor variances are all free
          affe1 affe3 affe5 affe7 affe9;
          ")


## 1. configural invariance (loadings and intercept are NOT equal across age groups)


MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL MIDDLE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l9-l16);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l9-l16);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l9-l16);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l9-l16);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l9-l16);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL OLD:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l17-l24);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l17-l24);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l17-l24);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l17-l24);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l17-l24);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i17-i24);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i17-i24);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i17-i24);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i17-i24);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i17-i24);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_conf_inv_age.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL MIDDLE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL OLD:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i17-i24);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i17-i24);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i17-i24);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i17-i24);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i17-i24);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_weak_inv_age.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL YOUNG:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL MIDDLE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL OLD:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_strong_inv_age.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("affe_conf_inv_age.out", "affe_weak_inv_age.out", "affe_strong_inv_age.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats


########## measurement invariance for gender

# setting working directory
setwd(file.path(root, "inv_sex"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)


MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL FEMALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l9-l16);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l9-l16);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l9-l16);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l9-l16);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l9-l16);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_conf_inv_sex.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL FEMALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_weak_inv_sex.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 

MODEL <- paste0(meas,"
                MODEL MALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL FEMALE:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_strong_inv_sex.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("affe_conf_inv_sex.out", "affe_weak_inv_sex.out", "affe_strong_inv_sex.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats



########## measurement invariance for education group

# setting working directory
setwd(file.path(root, "inv_edu"))

# usevariable commands that is identical in all models
ITEMS <- "usevar = 
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


## 1. configural invariance (loadings and intercept are NOT equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL HIGH:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l9-l16);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l9-l16);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l9-l16);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l9-l16);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l9-l16);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")

Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_conf_inv_edu.inp", run=1, check=F)



# 2. weak invariance (loadings but not intercepts equal across age groups)

MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL HIGH:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i9-i16);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i9-i16);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i9-i16);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i9-i16);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i9-i16);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_weak_inv_edu.inp", run=1, check=F)



# 3. strong invariance (loadings and intercepts equal across age groups) 


MODEL <- paste0(meas,"
                MODEL LOW:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                
                MODEL HIGH:
                !Setting Loadings
                affe1 BY bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a (l1-l8);
                affe3 BY cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a (l1-l8);
                affe5 BY dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a (l1-l8);
                affe7 BY eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a (l1-l8);
                affe9 BY fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a (l1-l8);
                
                !Setting Item Intercepts
                [bazb019a@0 bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a] (i1-i8);
                [cazb021a@0 cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a bazb028a] (i1-i8);
                [dazb021a@0 dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a bazb028a] (i1-i8);
                [eazb021a@0 eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a bazb028a] (i1-i8);
                [fazb021a@0 fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a bazb028a] (i1-i8);
                
                !freely estimate occasion-specific grand means 
                [affe1 affe3 affe5 affe7 affe9];
                ")


Model <- mplusObject(
  VARIABLE=ITEMS,
  usevariables = names(data),
  ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
  MODEL=MODEL,rdata=data,autov=T,
  OUTPUT="STDYX;")
output <- mplusModeler(Model, modelout="affe_strong_inv_edu.inp", run=1, check=F)


## extracting results to compare fit indices to see whether setting parameters equal reduces fit significantly 

out.names <- c("affe_conf_inv_edu.out", "affe_weak_inv_edu.out", "affe_strong_inv_edu.out")

# checking model fits
fit.stats <- data.frame(matrix(NA,3,13))
colnames(fit.stats) <- c("Event Aggr","N","CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC", "Chi_square", "Parameters","Warnings", "Errors", "Non-Positive Matrix")
for (m in 1:3){ # t = trait (narc vs Mach)
  fit.stats[m,] <- as.vector(extr.Mplus.fit(out.names[m])[,2])
}
fit.stats




### 6.2 Robustness check regarding age, gender, and education groups ####

### 6.2.1 Age (Robustness Check) ####


### multiple group analysis

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "robust_age")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "robust_age"))


items_swb[[1]] <- "
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a         
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"

items_swb[[2]] <- "
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) gender edu_d;
grouping is age_c (1=YOUNG 2=MIDDLE 3=OLD);"


for (val in 1:4){  # val = indices for value
  for (swb in 1:2){ # swb = indices for subject well-being
    MODEL <- paste0(meas_val[[val]], meas_swb[[swb]], "
                               
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                SWB1 SWB3 SWB5 SWB7 SWB9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2;
                L_val4;
                L_val6;
                L_val8;
                L_val10;
                L_swb1;
                L_swb3;
                L_swb5;
                L_swb7;
                L_swb9;
                
                !AR amongst SRs;
                L_val10 on L_val8;
                L_val8 on L_val6;
                L_val6 on L_val4;
                L_val4 on L_val2;
                
                L_swb9 on L_swb7;
                L_swb7 on L_swb5;
                L_swb5 on L_swb3;
                L_swb3 on L_swb1;
                
                !Crosslags (6 months);
                L_val10 on L_swb9;
                L_val8  on L_swb7;
                L_val6  on L_swb5;
                L_val4  on L_swb3;
                L_val2  on L_swb1;
                
                L_swb9 on L_val8;
                L_swb7 on L_val6;
                L_swb5 on L_val4;
                L_swb3 on L_val2;
                
                !Crosslags (18 months);
                L_val10 on L_swb7;
                L_val8  on L_swb5;
                L_val6  on L_swb3;
                L_val4  on L_swb1;
                
                L_swb9 on L_val6;
                L_swb7 on L_val4;
                L_swb5 on L_val2;
                
                
                MODEL YOUNG:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aY);
                L_val4 (r1Y);
                L_val6 (r1Y);
                L_val8 (r1Y);
                L_val10 (r1Y);
                L_swb1 (r2aY);
                L_swb3 (r2bY);
                L_swb5 (r2Y);
                L_swb7 (r2Y);
                L_swb9 (r2Y);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1Y);
                L_val8 on L_val6 (ar1Y);
                L_val6 on L_val4 (ar1Y);
                L_val4 on L_val2 (ar1Y);
                
                L_swb9 on L_swb7 (ar2Y);
                L_swb7 on L_swb5 (ar2Y);
                L_swb5 on L_swb3 (ar2Y);
                L_swb3 on L_swb1 (ar2Ya);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1Y);
                L_val8  on L_swb7 (cl1Y);
                L_val6  on L_swb5 (cl1Y);
                L_val4  on L_swb3 (cl1Y);
                L_val2  on L_swb1 (cl1Ya);
                
                L_swb9 on L_val8 (cl2Y);
                L_swb7 on L_val6 (cl2Y);
                L_swb5 on L_val4 (cl2Y);
                L_swb3 on L_val2 (cl2Ya);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bY);
                L_val8  on L_swb5 (cl1bY);
                L_val6  on L_swb3 (cl1bY);
                L_val4  on L_swb1 (cl1bY);
                
                L_swb9 on L_val6 (cl2bY);
                L_swb7 on L_val4 (cl2bY);
                L_swb5 on L_val2 (cl2bY);
              
              
                MODEL MIDDLE:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aM);
                L_val4 (r1M);
                L_val6 (r1M);
                L_val8 (r1M);
                L_val10 (r1M);
                L_swb1 (r2aM);
                L_swb3 (r2bM);
                L_swb5 (r2M);
                L_swb7 (r2M);
                L_swb9 (r2M);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1M);
                L_val8 on L_val6 (ar1M);
                L_val6 on L_val4 (ar1M);
                L_val4 on L_val2 (ar1M);
                
                L_swb9 on L_swb7 (ar2M);
                L_swb7 on L_swb5 (ar2M);
                L_swb5 on L_swb3 (ar2M);
                L_swb3 on L_swb1 (ar2Ma);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1M);
                L_val8  on L_swb7 (cl1M);
                L_val6  on L_swb5 (cl1M);
                L_val4  on L_swb3 (cl1M);
                L_val2  on L_swb1 (cl1Ma);
                
                L_swb9 on L_val8 (cl2M);
                L_swb7 on L_val6 (cl2M);
                L_swb5 on L_val4 (cl2M);
                L_swb3 on L_val2 (cl2Ma);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bM);
                L_val8  on L_swb5 (cl1bM);
                L_val6  on L_swb3 (cl1bM);
                L_val4  on L_swb1 (cl1bM);
                
                L_swb9 on L_val6 (cl2bM);
                L_swb7 on L_val4 (cl2bM);
                L_swb5 on L_val2 (cl2bM);
                
                
                MODEL OLD:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aO);
                L_val4 (r1O);
                L_val6 (r1O);
                L_val8 (r1O);
                L_val10 (r1O);
                L_swb1 (r2aO);
                L_swb3 (r2bO);
                L_swb5 (r2O);
                L_swb7 (r2O);
                L_swb9 (r2O);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1O);
                L_val8 on L_val6 (ar1O);
                L_val6 on L_val4 (ar1O);
                L_val4 on L_val2 (ar1O);
                
                L_swb9 on L_swb7 (ar2O);
                L_swb7 on L_swb5 (ar2O);
                L_swb5 on L_swb3 (ar2O);
                L_swb3 on L_swb1 (ar2Oa);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1O);
                L_val8  on L_swb7 (cl1O);
                L_val6  on L_swb5 (cl1O);
                L_val4  on L_swb3 (cl1O);
                L_val2  on L_swb1 (cl1Oa);
                
                L_swb9 on L_val8 (cl2O);
                L_swb7 on L_val6 (cl2O);
                L_swb5 on L_val4 (cl2O);
                L_swb3 on L_val2 (cl2Oa);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bO);
                L_val8  on L_swb5 (cl1bO);
                L_val6  on L_swb3 (cl1bO);
                L_val4  on L_swb1 (cl1bO);
                
                L_swb9 on L_val6 (cl2bO);
                L_swb7 on L_val4 (cl2bO);
                L_swb5 on L_val2 (cl2bO);
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_v var_s V2_on_S1 S2_on_V1 dom_6m
                V4_on_S1 S4_on_V1 dom_18m);

                ! first, we need the var to standardize the coefficients
                var_v = ar1**2*var_v + cl1**2*var_s + cl1b**2*var_s + r1;
                var_s = ar2**2*var_s + cl2**2*var_v + cl2b**2*var_v + r2;

                ! then, we standardize the coefficients
                V2_on_S1 = cl1*sqrt(var_s)/sqrt(var_v);
                V4_on_S1 = cl1b*sqrt(var_s)/sqrt(var_v);
                S2_on_V1 = cl2*sqrt(var_v)/sqrt(var_s);
                S4_on_V1 = cl2b*sqrt(var_v)/sqrt(var_s);
                
                ! then, we test the dif bw standardized coef
                dom_6m = V2_on_S1 - S2_on_V1;
                dom_18m = V4_on_S1 - S4_on_V1;
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_vY var_sY V2_o_S1Y S2_o_V1Y dom_6mY
                V4_o_S1Y S4_o_V1Y dom_18mY
                var_vM var_sM V2_o_S1M S2_o_V1M dom_6mM
                V4_o_S1M S4_o_V1M dom_18mM
                var_vO var_sO V2_o_S1O S2_o_V1O dom_6mO
                V4_o_S1O S4_o_V1O dom_18mO
                mod6_YM mod6_YO mod6_MO
                mod18_YM mod18_YO mod18_MO);

                ! Young
                var_vY = ar1Y**2*var_vY + cl1Y**2*var_sY + cl1bY**2*var_sY + r1Y;
                var_sY = ar2Y**2*var_sY + cl2Y**2*var_vY + cl2bY**2*var_vY + r2Y;

                V2_o_S1Y = cl1Y*sqrt(var_sY)/sqrt(var_vY);
                S2_o_V1Y = cl2Y*sqrt(var_vY)/sqrt(var_sY);
                V4_o_S1Y = cl1bY*sqrt(var_sY)/sqrt(var_vY);
                S4_o_V1Y = cl2bY*sqrt(var_vY)/sqrt(var_sY);
                
                dom_6mY = V2_o_S1Y - S2_o_V1Y;
                dom_18mY = V4_o_S1Y - S4_o_V1Y;
                
                ! Middle
                var_vM = ar1M**2*var_vM + cl1M**2*var_sM + cl1bM**2*var_sM + r1M;
                var_sM = ar2M**2*var_sM + cl2M**2*var_vM + cl2bM**2*var_vM + r2M;

                V2_o_S1M = cl1M*sqrt(var_sM)/sqrt(var_vM);
                S2_o_V1M = cl2M*sqrt(var_vM)/sqrt(var_sM);
                V4_o_S1M = cl1bM*sqrt(var_sM)/sqrt(var_vM);
                S4_o_V1M = cl2bM*sqrt(var_vM)/sqrt(var_sM);
                
                dom_6mM = V2_o_S1M - S2_o_V1M;
                dom_18mM = V4_o_S1M - S4_o_V1M;
                
                ! Old
                var_vO = ar1O**2*var_vO + cl1O**2*var_sO + cl1bO**2*var_sO + r1O;
                var_sO = ar2O**2*var_sO + cl2O**2*var_vO + cl2bO**2*var_vO + r2O;

                V2_o_S1O = cl1O*sqrt(var_sO)/sqrt(var_vO);
                S2_o_V1O = cl2O*sqrt(var_vO)/sqrt(var_sO);
                V4_o_S1O = cl1bO*sqrt(var_sO)/sqrt(var_vO);
                S4_o_V1O = cl2bO*sqrt(var_vO)/sqrt(var_sO);
                
                dom_6mO = V2_o_S1O - S2_o_V1O;
                dom_18mO = V4_o_S1O - S4_o_V1O;
                
                ! testing moderation effects of age
                mod6_YM = dom_6mY - dom_6mM;
                mod6_YO = dom_6mY - dom_6mO;
                mod6_MO = dom_6mM - dom_6mO;
                mod18_YM = dom_18mY - dom_18mM;
                mod18_YO = dom_18mY - dom_18mO;
                mod18_MO = dom_18mM - dom_18mO;
                ")
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_SWB.nam[[val]][swb], ".inp"), run=1, check=F)
  }
}




### 6.2.2 Gender (Robustness Check) ####


### multiple group analysis

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "robust_sex")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "robust_sex"))


items_swb[[1]] <- "
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a         
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"

items_swb[[2]] <- "
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) age edu_d;
grouping is gender (1=MALE 2=FEMALE);"


for (val in 1:4){  # val = indices for value
  for (swb in 1:2){ # swb = indices for subject well-being
    MODEL <- paste0(meas_val[[val]], meas_swb[[swb]], "
                               
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                SWB1 SWB3 SWB5 SWB7 SWB9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2;
                L_val4;
                L_val6;
                L_val8;
                L_val10;
                L_swb1;
                L_swb3;
                L_swb5;
                L_swb7;
                L_swb9;
                
                !AR amongst SRs;
                L_val10 on L_val8;
                L_val8 on L_val6;
                L_val6 on L_val4;
                L_val4 on L_val2;
                
                L_swb9 on L_swb7;
                L_swb7 on L_swb5;
                L_swb5 on L_swb3;
                L_swb3 on L_swb1;
                
                !Crosslags (6 months);
                L_val10 on L_swb9;
                L_val8  on L_swb7;
                L_val6  on L_swb5;
                L_val4  on L_swb3;
                L_val2  on L_swb1;
                
                L_swb9 on L_val8;
                L_swb7 on L_val6;
                L_swb5 on L_val4;
                L_swb3 on L_val2;
                
                !Crosslags (18 months);
                L_val10 on L_swb7;
                L_val8  on L_swb5;
                L_val6  on L_swb3;
                L_val4  on L_swb1;
                
                L_swb9 on L_val6;
                L_swb7 on L_val4;
                L_swb5 on L_val2;
                
                
                MODEL MALE:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aM);
                L_val4 (r1M);
                L_val6 (r1M);
                L_val8 (r1M);
                L_val10 (r1M);
                L_swb1 (r2aM);
                L_swb3 (r2bM);
                L_swb5 (r2M);
                L_swb7 (r2M);
                L_swb9 (r2M);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1M);
                L_val8 on L_val6 (ar1M);
                L_val6 on L_val4 (ar1M);
                L_val4 on L_val2 (ar1M);
                
                L_swb9 on L_swb7 (ar2M);
                L_swb7 on L_swb5 (ar2M);
                L_swb5 on L_swb3 (ar2M);
                L_swb3 on L_swb1 (ar2Ma);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1M);
                L_val8  on L_swb7 (cl1M);
                L_val6  on L_swb5 (cl1M);
                L_val4  on L_swb3 (cl1M);
                L_val2  on L_swb1 (cl1Ma);
                
                L_swb9 on L_val8 (cl2M);
                L_swb7 on L_val6 (cl2M);
                L_swb5 on L_val4 (cl2M);
                L_swb3 on L_val2 (cl2Ma);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bM);
                L_val8  on L_swb5 (cl1bM);
                L_val6  on L_swb3 (cl1bM);
                L_val4  on L_swb1 (cl1bM);
                
                L_swb9 on L_val6 (cl2bM);
                L_swb7 on L_val4 (cl2bM);
                L_swb5 on L_val2 (cl2bM);
              
              
                MODEL FEMALE:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aF);
                L_val4 (r1F);
                L_val6 (r1F);
                L_val8 (r1F);
                L_val10 (r1F);
                L_swb1 (r2aF);
                L_swb3 (r2bF);
                L_swb5 (r2F);
                L_swb7 (r2F);
                L_swb9 (r2F);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1F);
                L_val8 on L_val6 (ar1F);
                L_val6 on L_val4 (ar1F);
                L_val4 on L_val2 (ar1F);
                
                L_swb9 on L_swb7 (ar2F);
                L_swb7 on L_swb5 (ar2F);
                L_swb5 on L_swb3 (ar2F);
                L_swb3 on L_swb1 (ar2Fa);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1F);
                L_val8  on L_swb7 (cl1F);
                L_val6  on L_swb5 (cl1F);
                L_val4  on L_swb3 (cl1F);
                L_val2  on L_swb1 (cl1Fa);
                
                L_swb9 on L_val8 (cl2F);
                L_swb7 on L_val6 (cl2F);
                L_swb5 on L_val4 (cl2F);
                L_swb3 on L_val2 (cl2Fa);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bF);
                L_val8  on L_swb5 (cl1bF);
                L_val6  on L_swb3 (cl1bF);
                L_val4  on L_swb1 (cl1bF);
                
                L_swb9 on L_val6 (cl2bF);
                L_swb7 on L_val4 (cl2bF);
                L_swb5 on L_val2 (cl2bF);
 
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_vM var_sM V2_o_S1M S2_o_V1M dom_6mM
                V4_o_S1M S4_o_V1M dom_18mM
                var_vF var_sF V2_o_S1F S2_o_V1F dom_6mF
                V4_o_S1F S4_o_V1F dom_18mF
                mod6_MF mod18_MF);

                ! MALE
                var_vM = ar1M**2*var_vM + cl1M**2*var_sM + cl1bM**2*var_sM + r1M;
                var_sM = ar2M**2*var_sM + cl2M**2*var_vM + cl2bM**2*var_vM + r2M;

                V2_o_S1M = cl1M*sqrt(var_sM)/sqrt(var_vM);
                S2_o_V1M = cl2M*sqrt(var_vM)/sqrt(var_sM);
                V4_o_S1M = cl1bM*sqrt(var_sM)/sqrt(var_vM);
                S4_o_V1M = cl2bM*sqrt(var_vM)/sqrt(var_sM);
                
                dom_6mM = V2_o_S1M - S2_o_V1M;
                dom_18mM = V4_o_S1M - S4_o_V1M;
                
                ! FEMALE
                var_vF = ar1F**2*var_vF + cl1F**2*var_sF + cl1bF**2*var_sF + r1F;
                var_sF = ar2F**2*var_sF + cl2F**2*var_vF + cl2bF**2*var_vF + r2F;

                V2_o_S1F = cl1F*sqrt(var_sF)/sqrt(var_vF);
                S2_o_V1F = cl2F*sqrt(var_vF)/sqrt(var_sF);
                V4_o_S1F = cl1bF*sqrt(var_sF)/sqrt(var_vF);
                S4_o_V1F = cl2bF*sqrt(var_vF)/sqrt(var_sF);
                
                dom_6mF = V2_o_S1F - S2_o_V1F;
                dom_18mF = V4_o_S1F - S4_o_V1F;
                
                ! testing moderation effects of gender
                mod6_MF = dom_6mM - dom_6mF;
                mod18_MF = dom_18mM - dom_18mF;
                ")
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_SWB.nam[[val]][swb], ".inp"), run=1, check=F)
  }
}



### 6.2.3 Education (Robustness Check) ####

### multiple group analysis

# creating working directories for output files for multiple group analysis
dir.create(file.path(root, "robust_edu")) # creating folder for the robustness analysis regarding age
setwd(file.path(root, "robust_edu"))


items_swb[[1]] <- "
bazb005a bazb013a bazb014a bazb015a bazb016a bazb017a bazb018a         
cazb005a cazb014a cazb015a cazb016a cazb017a cazb018a cazb019a
dazb005a dazb014a dazb015a dazb016a dazb017a dazb018a dazb019a
fazb005a fazb014a fazb015a fazb016a fazb017a fazb018a fazb019a
eazb005a eazb014a eazb015a eazb016a eazb017a eazb018a eazb019a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"

items_swb[[2]] <- "
bazb019a bazb020a bazb021a bazb022a bazb023a bazb024a bazb025a bazb026a
cazb021a cazb022a cazb023a cazb024a cazb025a cazb026a cazb027a cazb028a
dazb021a dazb022a dazb023a dazb024a dazb025a dazb026a dazb027a dazb028a
eazb021a eazb022a eazb023a eazb024a eazb025a eazb026a eazb027a eazb028a
fazb021a fazb022a fazb023a fazb024a fazb025a fazb026a fazb027a fazb028a
age_c;
AUXILIARY = (M) age gender;
grouping is edu_d (0=LOW 1=HIGH);"


for (val in 1:4){  # val = indices for value
  for (swb in 1:2){ # swb = indices for subject well-being
    MODEL <- paste0(meas_val[[val]], meas_swb[[swb]], "
                               
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                !factor variances are all free
                VAL2 VAL4 VAL6 VAL8 VAL10;
                SWB1 SWB3 SWB5 SWB7 SWB9;
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2;
                L_val4;
                L_val6;
                L_val8;
                L_val10;
                L_swb1;
                L_swb3;
                L_swb5;
                L_swb7;
                L_swb9;
                
                !AR amongst SRs;
                L_val10 on L_val8;
                L_val8 on L_val6;
                L_val6 on L_val4;
                L_val4 on L_val2;
                
                L_swb9 on L_swb7;
                L_swb7 on L_swb5;
                L_swb5 on L_swb3;
                L_swb3 on L_swb1;
                
                !Crosslags (6 months);
                L_val10 on L_swb9;
                L_val8  on L_swb7;
                L_val6  on L_swb5;
                L_val4  on L_swb3;
                L_val2  on L_swb1;
                
                L_swb9 on L_val8;
                L_swb7 on L_val6;
                L_swb5 on L_val4;
                L_swb3 on L_val2;
                
                !Crosslags (18 months);
                L_val10 on L_swb7;
                L_val8  on L_swb5;
                L_val6  on L_swb3;
                L_val4  on L_swb1;
                
                L_swb9 on L_val6;
                L_swb7 on L_val4;
                L_swb5 on L_val2;
                
                
                MODEL LOW:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0 L_swb1@0;
                eta_swb with L_val2@0 L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aL);
                L_val4 (r1L);
                L_val6 (r1L);
                L_val8 (r1L);
                L_val10 (r1L);
                L_swb1 (r2aL);
                L_swb3 (r2bL);
                L_swb5 (r2L);
                L_swb7 (r2L);
                L_swb9 (r2L);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1L);
                L_val8 on L_val6 (ar1L);
                L_val6 on L_val4 (ar1L);
                L_val4 on L_val2 (ar1L);
                
                L_swb9 on L_swb7 (ar2L);
                L_swb7 on L_swb5 (ar2L);
                L_swb5 on L_swb3 (ar2L);
                L_swb3 on L_swb1 (ar2La);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1L);
                L_val8  on L_swb7 (cl1L);
                L_val6  on L_swb5 (cl1L);
                L_val4  on L_swb3 (cl1L);
                L_val2  on L_swb1 (cl1La);
                
                L_swb9 on L_val8 (cl2L);
                L_swb7 on L_val6 (cl2L);
                L_swb5 on L_val4 (cl2L);
                L_swb3 on L_val2 (cl2La);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bL);
                L_val8  on L_swb5 (cl1bL);
                L_val6  on L_swb3 (cl1bL);
                L_val4  on L_swb1 (cl1bL);
                
                L_swb9 on L_val6 (cl2bL);
                L_swb7 on L_val4 (cl2bL);
                L_swb5 on L_val2 (cl2bL);
              
              
                MODEL HIGH:
                
                !freely estimate occasion-specific grand means
                [VAL2 VAL4 VAL6 VAL8 VAL10];
                [SWB1 SWB3 SWB5 SWB7 SWB9];
                
                ! BETWEEN-PERSON LEVEL
                
                !Random intercepts
                eta_val by VAL2@1 VAL4@1 VAL6@1 VAL8@1 VAL10@1;
                eta_swb by SWB1@1 SWB3@1 SWB5@1 SWB7@1 SWB9@1;
                
                !Constrain means of random intercepts
                [eta_val@0];
                [eta_swb@0];
                
                !Estimate variances of random intercept;
                eta_val;
                eta_swb;
                
                ! allowing for covariances among random intercepts
                eta_val with eta_swb;
                
                !constraining covariance among random intercepts and first residuals
                ! Mplus does allow correlations among these exogeneous variables otherwise
                eta_val with L_val2@0;
                eta_val with L_swb1@0;
                eta_swb with L_val2@0;
                eta_swb with L_swb1@0;
                
                ! WITHIN-PERSON LEVEL
                
                !Constrain observed residual variances, to identify structured residuals;
                VAL2@0;
                VAL4@0;
                VAL6@0;
                VAL8@0;
                VAL10@0;
                SWB1@0;
                SWB3@0;
                SWB5@0;
                SWB7@0;
                SWB9@0;
                
                !Estimate structured residuals
                L_val2 by VAL2@1;
                L_val4 by VAL4@1;
                L_val6 by VAL6@1;
                L_val8 by VAL8@1;
                L_val10 by VAL10@1;

                L_swb1 by SWB1@1;
                L_swb3 by SWB3@1;
                L_swb5 by SWB5@1;
                L_swb7 by SWB7@1;
                L_swb9 by SWB9@1;
                
                !Constrain means/intercepts of residuals
                [L_val2@0 L_val4@0 L_val6@0 L_val8@0 L_val10@0];
                [L_swb1@0 L_swb3@0 L_swb5@0 L_swb7@0 L_swb9@0];
                
                !Set equal the variances of the 'residuals of the residuals'
                !Freely estimate t1 structured residual.
                L_val2 (r1aH);
                L_val4 (r1H);
                L_val6 (r1H);
                L_val8 (r1H);
                L_val10 (r1H);
                L_swb1 (r2aH);
                L_swb3 (r2bH);
                L_swb5 (r2H);
                L_swb7 (r2H);
                L_swb9 (r2H);
                
                !AR amongst SRs with assumed stationarity;
                L_val10 on L_val8 (ar1H);
                L_val8 on L_val6 (ar1H);
                L_val6 on L_val4 (ar1H);
                L_val4 on L_val2 (ar1H);
                
                L_swb9 on L_swb7 (ar2H);
                L_swb7 on L_swb5 (ar2H);
                L_swb5 on L_swb3 (ar2H);
                L_swb3 on L_swb1 (ar2Ha);
                
                !Constrained crosslags (6 months);
                L_val10 on L_swb9 (cl1H);
                L_val8  on L_swb7 (cl1H);
                L_val6  on L_swb5 (cl1H);
                L_val4  on L_swb3 (cl1H);
                L_val2  on L_swb1 (cl1Ha);
                
                L_swb9 on L_val8 (cl2H);
                L_swb7 on L_val6 (cl2H);
                L_swb5 on L_val4 (cl2H);
                L_swb3 on L_val2 (cl2Ha);
                
                !Constrained crosslags (18 months);
                L_val10 on L_swb7 (cl1bH);
                L_val8  on L_swb5 (cl1bH);
                L_val6  on L_swb3 (cl1bH);
                L_val4  on L_swb1 (cl1bH);
                
                L_swb9 on L_val6 (cl2bH);
                L_swb7 on L_val4 (cl2bH);
                L_swb5 on L_val2 (cl2bH);
 
                
                !testing causal dominance
                Model Constraints: 
                NEW (var_vL var_sL V2_o_S1L S2_o_V1L dom_6mL
                V4_o_S1L S4_o_V1L dom_18mL
                var_vH var_sH V2_o_S1H S2_o_V1H dom_6mH
                V4_o_S1H S4_o_V1H dom_18mH
                mod6_LH mod18_LH);

                ! LOW
                var_vL = ar1L**2*var_vL + cl1L**2*var_sL + cl1bL**2*var_sL + r1L;
                var_sL = ar2L**2*var_sL + cl2L**2*var_vL + cl2bL**2*var_vL + r2L;

                V2_o_S1L = cl1L*sqrt(var_sL)/sqrt(var_vL);
                S2_o_V1L = cl2L*sqrt(var_vL)/sqrt(var_sL);
                V4_o_S1L = cl1bL*sqrt(var_sL)/sqrt(var_vL);
                S4_o_V1L = cl2bL*sqrt(var_vL)/sqrt(var_sL);
                
                dom_6mL = V2_o_S1L - S2_o_V1L;
                dom_18mL = V4_o_S1L - S4_o_V1L;
                
                ! HIGH
                var_vH = ar1H**2*var_vH + cl1H**2*var_sH + cl1bH**2*var_sH + r1H;
                var_sH = ar2H**2*var_sH + cl2H**2*var_vH + cl2bH**2*var_vH + r2H;

                V2_o_S1H = cl1H*sqrt(var_sH)/sqrt(var_vH);
                S2_o_V1H = cl2H*sqrt(var_vH)/sqrt(var_sH);
                V4_o_S1H = cl1bH*sqrt(var_sH)/sqrt(var_vH);
                S4_o_V1H = cl2bH*sqrt(var_vH)/sqrt(var_sH);
                
                dom_6mH = V2_o_S1H - S2_o_V1H;
                dom_18mH = V4_o_S1H - S4_o_V1H;
                
                ! testing moderation effects of education
                mod6_LH = dom_6mL - dom_6mL;
                mod18_LH = dom_18mL - dom_18mH;
                ")
    
    Model <- mplusObject(
      VARIABLE=ITEMS,
      usevariables = names(data),
      ANALYSIS="ESTIMATOR = MLR; PROCESSORS=4;",
      MODEL=MODEL,rdata=data,autov=T,
      OUTPUT="STDYX;")
    output <- mplusModeler(Model, modelout=paste0(value_SWB.nam[[val]][swb], ".inp"), run=1, check=F)
  }
}
