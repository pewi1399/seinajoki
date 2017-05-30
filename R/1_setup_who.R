library(dplyr)
library(openxlsx)
library(ggplot2)
library(data.table)

weianthro<-read.table("under_five/igrowup_R/weianthro.txt",header=T,sep="",skip=0) 
lenanthro<-read.table("under_five/igrowup_R/lenanthro.txt",header=T,sep="",skip=0) 
bmianthro<-read.table("under_five/igrowup_R/bmianthro.txt",header=T,sep="",skip=0) 
hcanthro<-read.table("under_five/igrowup_R/hcanthro.txt",header=T,sep="",skip=0) 
acanthro<-read.table("under_five/igrowup_R/acanthro.txt",header=T,sep="",skip=0) 
ssanthro<-read.table("under_five/igrowup_R/ssanthro.txt",header=T,sep="",skip=0) 
tsanthro<-read.table("under_five/igrowup_R/tsanthro.txt",header=T,sep="",skip=0) 
wflanthro<-read.table("under_five/igrowup_R/wflanthro.txt",header=T,sep="",skip=0) 
wfhanthro<-read.table("under_five/igrowup_R/wfhanthro.txt",header=T,sep="",skip=0)


source("under_five/igrowup_R/igrowup_standard.r")
source("under_five/igrowup_R/igrowup_restricted.r")

# n <- 1000
# year <- 3
# 
# 
# tmp <- data.frame(
#   sex = sample(c("m", "f"), n, replace =TRUE),
#   height = round(rnorm(n, 110, 5)),
#   weight = round(rnorm(n, 19,2)),
#   age_month = 61,#sample((year*12):((year+1)*12), n, replace = TRUE),
#   age_days = 364
# )
 

# who2007(FilePath="who2007 example",
#         FileLab="survey_who2007",mydf=tmp,
#         sex=sex,age=age_month, weight=weight,
#         height=height)
# 
# igrowup.standard(FilePath="igrowup example",
#                  FileLab="MySurvey",mydf=tmp,
#                  sex=sex,
#                  age = age_days,
#                  #age=age_month,
#                  #age.month=T,
#                  weight=weight,
#                  lenhei=height)
 

dat <- openxlsx::read.xlsx("C:/Users/perwim/Desktop/TMP/MOVE/Peter/New data/Tietopoiminta Seinäjoki.xlsx", sheet =3)
vars <- grep("dIkä|^Paino$|^Pituus$", names(dat), value = TRUE)
dat[,vars] <- lapply(dat[,vars], function(x) as.numeric(gsub(",", ".", x)))

dat$age <- dat$dIkä
dat$sex <- dat$Sukupuoli
dat$visitdate <- as.Date(dat$Käyntipäivä, format = "%d.%m.%y")
dat$visityear <- substr(dat$visitdate, 1, 4)
dat$age_class <- round(dat$dIkä*12)
dat$age_days <- round(dat$dIkä*365.24)
dat$birthdate <- dat$visitdate - dat$age_days
dat$id <- dat$HenkilötunnusHash
dat$weight <- dat$Paino
dat$height <- dat$Pituus

setDT(dat)

dat[,first_visit:=min(age_days),by = "id"]
dat[,first_age:=ifelse(first_visit == age_days, as.character(birthdate), NA),]
dat[,consensus_birth:=max(first_age, na.rm = TRUE), by = "id"]

dat$birthdate <- dat$consensus_birth

dat$approx_birthyear <- substr(as.character(dat$birthdate), 1, 4)

dat <- 
  dat %>% 
  select(id, sex, weight, height, visitdate, age, age_days, consensus_birth, approx_birthyear, visityear) %>% 
  mutate(sex = as.factor(ifelse(sex == "Mies", "m", "f")),
         rowid = 1:n())


ageinfo <- 
  dat %>% 
  select(id, rowid,  visitdate, age, consensus_birth, approx_birthyear, visityear)

saveRDS(ageinfo, "output/ageinfo.rds")

under_five <- dat %>% filter(age_days < (365.24*5) & age_days >0) %>% 
  select(sex,age_days,weight,height, id, rowid)
#under_five <- under_five[1:4000,] %>% 
#  select(sex,age_days,weight,height)

 igrowup.standard(FilePath="output",
                  FileLab="Seinajoki_five_less",mydf=under_five,
                  sex=sex,
                  age=age_days,
                  weight=weight,
                  lenhei=height)

 

#--------------------------- rita upp de äldre barnen ------------------------
 source("who2007.r")
 
 wfawho2007 <- read.table("wfawho2007.txt",header=T,sep="",skip=0) 
 hfawho2007 <- read.table("hfawho2007.txt",header=T,sep="",skip=0) 
 bfawho2007 <- read.table("bfawho2007.txt",header=T,sep="",skip=0)
 
 
over_five <- dat %>% filter(age_days > (365.24*5) & age_days <7000) %>% 
  select(sex,age_days,weight, height, id, rowid)

over_five$age_months <- over_five$age_days/(365.24/12)
# over_five <- over_five[1:4000,]
  
who2007(FilePath="output",
                 FileLab="seinajoki_over5",
                 mydf=over_five,
                 sex=sex,
                 age=age_months, 
                 weight=weight,
                 height=height)

