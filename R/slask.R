library(openxlsx)
library(dplyr)
library(ggplot2)
library(testthat)
library(rtf)
library(Cairo)

dat <- read.xlsx("Kasvupoiminta_Seinäjoki.xlsx")

n <- nrow(dat)

# create pseudo birthdate
dat$visitDate <- as.Date(dat$Käyntipäivä, format = "%d.%m.%y")
dat$ageDays <- as.numeric(dat$dIkä)*365.24
dat$age <- as.numeric(dat$dIkä)
dat$pseudoBirth <- dat$visitDate - dat$ageDays
dat$birthYear <- as.numeric(substr(as.character(dat$pseudoBirth), 1, 4))
dat$id <- dat$HenkilötunnusHash

# calculate bmi
dat$weight <- as.numeric(as.character(dat$Paino))
dat$height <- as.numeric(as.character(dat$Pituus))

dat$bmi <- dat$weight/(dat$height/100)^2

dat$ageclass <- cut(dat$age, 
                    breaks = quantile(dat$age, na.rm = TRUE, seq(0,1,0.05))
                    )

# classify visits
dat$beforeIntervention <- ifelse(dat$visitDate < "2009-12-31", "<2009", "2010+")

# get classification for interventionperiods lived trough by individual
tmp <- 
dat %>% 
  group_by(id) %>% 
  summarise(
    periods = sum(table(unique(beforeIntervention))),
    periodstext = paste0(beforeIntervention, collapse = ",")
    ) %>% 
  mutate(interventionClass = ifelse(periods == 2, "crossover",
                                    ifelse(grepl("<2009", periodstext), "before intervention",
                                                 "after intervention"
                                           )
                                    )
         ) %>% 
  ungroup()


# join in classification
dat <-
tmp %>% 
  select(id,interventionClass) %>% 
  merge(dat, by = "id", all.y = TRUE)

# unit test
test_that("no ids where lost", {
  expect_that(n == nrow(dat), is_true())
  })

#-------------------------------- plots and tests ------------------------------
# create before after intervention plot
bmi_all <-
dat %>% 
  group_by(ageclass) %>%
  summarise(
    avg_bmi = mean(bmi, na.rm = TRUE),
    lo = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[1]}, error = function(e){1}),
    hi = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[2]}, error = function(e){1})
  ) %>% 
  ggplot()+
  aes(x = ageclass, y = avg_bmi, group = 1, ymin = lo, ymax = hi)+
  geom_point()+
  geom_ribbon()+
  geom_line()+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))



# create before after intervention plot
bmi_comparison<-
dat %>% 
  group_by(beforeIntervention, ageclass) %>%
  summarise(
    avg_bmi = mean(bmi, na.rm = TRUE),
    lo = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[1]}, error = function(e){1}),
    hi = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[2]}, error = function(e){1})
      ) %>% 
ggplot()+
  aes(x = ageclass, y = avg_bmi, col = beforeIntervention, group = beforeIntervention)+
  geom_point()+
  geom_line()+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

# look at cases dependent on what kind of treatments 
bmi_crossover<-
dat %>% 
  group_by(interventionClass, ageclass) %>%
  summarise(
    avg_bmi = mean(bmi, na.rm = TRUE),
    lo = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[1]}, error = function(e){1}),
    hi = tryCatch({t.test(bmi, na.rm = TRUE)$conf.int[2]}, error = function(e){1})
  ) %>% 
ggplot()+
  aes(x = ageclass, y = avg_bmi, col = interventionClass, group = interventionClass)+
  geom_point()+
  geom_line()+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

# plot smoothed lines
ggplot(dat)+
  aes(x = ageclass, y = bmi, col = beforeIntervention, group = beforeIntervention)+
  geom_smooth()+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))



# show distribution of new cases by age at visits
visits <- 
dat %>% 
  filter(age < 15) %>% 
ggplot + 
  aes(x = age)+
  geom_histogram(binwidth = 1, col = "black", fill = "royalblue")+
  ylab("Number of visits")+
  xlab("Age at visit")


# show comparisons at same age
boxplotall <- 
dat %>% 
  filter(bmi<50 & birthYear > 1975) %>% 
ggplot()+
  aes(x = ageclass, y = bmi)+
  geom_boxplot()+
  facet_wrap(~birthYear, nrow = 6)+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
  

# get unique ids
persons <-
dat %>% 
  filter(!duplicated(HenkilötunnusHash))

# find out how many per birth cohort
tab <- 
persons %>% 
  group_by(birthYear) %>% 
  summarise(n = n())

table(tmp$periods, tmp$interventionclass)

write.xlsx(tab, "birthyears.xlsx")

Cairo(width =900 ,height = 500,file = "bmi_all.png",dpi = 120)
bmi_all
dev.off()

Cairo(width =900 ,height = 500,file = "bmi_2groups.png",dpi = 120)
bmi_comparison
dev.off()

Cairo(width =900 ,height = 500,file = "bmi_3groups.png",dpi = 120)
bmi_crossover
dev.off()

Cairo(width =2000 ,height = 2000,file = "bmi_boxplotall.png",dpi = 120)
boxplotall
dev.off()
