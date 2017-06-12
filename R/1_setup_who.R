library(dplyr)
library(openxlsx)
library(ggplot2)
library(data.table)

weianthro<-read.table("who/under_five/igrowup_R/weianthro.txt",header=T,sep="",skip=0) 
lenanthro<-read.table("who/under_five/igrowup_R/lenanthro.txt",header=T,sep="",skip=0) 
bmianthro<-read.table("who/under_five/igrowup_R/bmianthro.txt",header=T,sep="",skip=0) 
hcanthro<-read.table("who/under_five/igrowup_R/hcanthro.txt",header=T,sep="",skip=0) 
acanthro<-read.table("who/under_five/igrowup_R/acanthro.txt",header=T,sep="",skip=0) 
ssanthro<-read.table("who/under_five/igrowup_R/ssanthro.txt",header=T,sep="",skip=0) 
tsanthro<-read.table("who/under_five/igrowup_R/tsanthro.txt",header=T,sep="",skip=0) 
wflanthro<-read.table("who/under_five/igrowup_R/wflanthro.txt",header=T,sep="",skip=0) 
wfhanthro<-read.table("who/under_five/igrowup_R/wfhanthro.txt",header=T,sep="",skip=0)


source("who/under_five/igrowup_R/igrowup_standard.r")
source("who/under_five/igrowup_R/igrowup_restricted.r")

# read in seinajoki data
dat <- openxlsx::read.xlsx("C:/Users/perwim/Desktop/TMP/MOVE/Peter/data/Tietopoiminta Seinäjoki.xlsx", sheet =3)

#get primary measure vars and format these
vars <- grep("dIkä|^Paino$|^Pituus$", names(dat), value = TRUE)
dat[,vars] <- lapply(dat[,vars], function(x) as.numeric(gsub(",", ".", x)))

#translate from finnish
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


#PER: There are duplicate rows in the data which differ a little from each other because the system was updated in 10/2014. Old system version (column E in the data, version number 2, newer one is version 5) has different values in columns K (height sd), M (weight  %), P (weight bsa) and R (head sd).
#You should delete the duplicate rows (row in which the column E has value 2), if column
#-          A (encrypted ssn),
#-          G (date of entry)
#-          H (age),
#-          J (height) and
#-          L (weight) are the same.

dat$dupekey <- paste0(
                  dat$HenkilötunnusHash,
                  dat$Käyntipäivä,
                  dat$dIkä,
                  dat$Paino,
                  dat$Pituus
                  )

nrow_raw <- nrow(dat)

dat <-
  dat %>% 
    filter(!duplicated(dupekey))

nrow_dupes_removed <- nrow(dat)


#Calculate first visit and derive birthday from this date
setDT(dat)

dat[,first_visit:=min(age_days),by = "id"]
dat[,first_age:=ifelse(first_visit == age_days, as.character(birthdate), NA),]
dat[,consensus_birth:=max(first_age, na.rm = TRUE), by = "id"]

dat$birthdate <- dat$consensus_birth

dat$approx_birthyear <- substr(as.character(dat$birthdate), 1, 4)

# get rid of extra vars and recode sex
dat <- 
  dat %>% 
  select(id, sex, weight, height, visitdate, age, age_days, consensus_birth, approx_birthyear, visityear) %>% 
  mutate(sex = as.factor(ifelse(sex == "Mies", "m", "f")),
         rowid = 1:n())

# save info on ages per id 
ageinfo <- 
  dat %>% 
  select(id, rowid,  visitdate, age, consensus_birth, approx_birthyear, visityear)

saveRDS(ageinfo, "output/ageinfo.rds")

# split dataset in unde five and over five since different age profiles -
# are used to calculate BMI
under_five <- dat %>% filter(age_days <= (365.24*5) & age_days >0) %>% 
  select(sex,age_days,weight,height, id, rowid)
#under_five <- under_five[1:4000,] %>% 
#  select(sex,age_days,weight,height)

# use BMI formula from WHO
system.time({
  igrowup.standard(FilePath="output",
                    FileLab="Seinajoki_five_less",mydf=under_five,
                    sex=sex,
                    age=age_days,
                    weight=weight,
                    lenhei=height)
})
 
source("R/who2007.r")
 
wfawho2007 <- read.table("who/over_five/wfawho2007.txt",header=T,sep="",skip=0) 
hfawho2007 <- read.table("who/over_five/hfawho2007.txt",header=T,sep="",skip=0) 
bfawho2007 <- read.table("who/over_five/bfawho2007.txt",header=T,sep="",skip=0)
 
 
over_five <- dat %>% filter(age_days > (365.24*5) & age_days <7000) %>% 
  select(sex,age_days,weight, height, id, rowid)

over_five$age_months <- over_five$age_days/(365.24/12)
# over_five <- over_five[1:4000,]

system.time({  
  who2007(FilePath="output",
                   FileLab="seinajoki_over5",
                   mydf=over_five,
                   sex=sex,
                   age=age_months, 
                   weight=weight,
                   height=height)
})
