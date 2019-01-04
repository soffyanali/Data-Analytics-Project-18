set.seed(13114531)

# Directory

setwd("/Users/NORMZY/Documents/Data R/DADProject/")

library(htmltab)
url <- "https://en.wikipedia.org/wiki/List_of_cities_by_murder_rate"

htmltab(doc = url)

Homicides <- htmltab(doc=url, which= 2)

Homicides$Country <- iconv(Homicides$Country, "utf-8", "ASCII", sub="")


suicides <- read.csv("master.csv", stringsAsFactors = F, sep = ",")
survey <- read.csv("survey.csv", stringsAsFactors = F, sep = ",")


#Homicide Dataset Cleaning
Homicides <- Homicides[ , -(c(1,6))]

colnames(Homicides)[3] <- "Homicides"
colnames(Homicides)[4] <- "CityPopulation"

Homicides$Homicides  <- gsub(",","",Homicides$Homicides )
Homicides$CityPopulation  <- gsub(",","",Homicides$CityPopulation )

#Suicides Dataset cleaning
suicides <- suicides[  , -(c(2,4,7,8,9,10,11,12))]

colnames(suicides)[1] <- "Country"
colnames(suicides)[4] <- "CountryPopulation"

#Survey Dataset Cleaning
survey <- survey[ , -(c(1,3,5,7,10,11,12,14,15,17,18,21,22,23,24,25,26,27))]

colnames(survey)[5] <- "Employed"
colnames(survey)[6] <- "SocialBenefits"

#Merging all 3 datasets
a <- merge (suicides, Homicides , by = "Country" )
Fulldata <- merge (a, survey , by = "Country" )

#Removing datasets
rm(suicides)
rm(Homicides)
rm(survey)
rm(a)
rm(url)

#>>>>>>>>>>>>>>>>> BELOW CLEANING THE FullDATA <<<<<<<<<<<<<<<<<<<<

#Coverting Age into Groups for easier Groups
Fulldata$Age <- cut(Fulldata$Age, breaks=c(0,14,18,24,64,100), labels=c("Kids", "Teen", "Young Adults", "Adults", "Older People"))

#removing the special characters
Fulldata$City <- iconv(Fulldata$City, "utf-8", "ASCII", sub="")

sapply(Fulldata,function(x) sum(is.na(x)))

Fulldata <- Fulldata[!is.na(Fulldata$Employed), ]
Fulldata <- Fulldata[!is.na(Fulldata$self_employed), ]
Fulldata <- Fulldata[!is.na(Fulldata$Age), ]

str(Fulldata)

#Changing the Characters

Fulldata$Homicides <- as.numeric(Fulldata$Homicides)
Fulldata$CityPopulation <- as.numeric(Fulldata$CityPopulation)

Fulldata$sex <- factor(Fulldata$sex)
Fulldata$self_employed <- factor(Fulldata$self_employed)
Fulldata$treatment <- factor(Fulldata$treatment)
Fulldata$Employed <- factor(Fulldata$Employed)
Fulldata$SocialBenefits <- factor(Fulldata$SocialBenefits)
Fulldata$seek_help <- factor(Fulldata$seek_help)
Fulldata$mental_health_consequence <- factor(Fulldata$mental_health_consequence)
Fulldata$phys_health_consequence <- factor(Fulldata$phys_health_consequence)

str(Fulldata)

write.csv(Fulldata[1:100000, ], file = "Fulldata.csv", row.names = F)

Fulldata <- read.csv("Fulldata.csv", stringsAsFactors = F, sep = ",")

#TopTen Map reduced on Crime
topdata <-  Fulldata[,c(16)]
write.csv(topdata[1:10000 ], file = "toptencrimes.csv", row.names = F)


#>>>>>>>>>>>>>>> >>>>>>>>>>>>>>> Questions <<<<<<<<<<<<<<<<<< <<<<<<<<<<<<<<<
library(ggplot2)
library(ggthemes)

sapply(Fulldata, class)

table(Fulldata$Age)

plot(Fulldata$CountryPopulation ~ Fulldata$Crime)

ggplot(Fulldata, aes(x = CountryPopulation, fill = Crime)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Country') +
  theme_few()

plot(Fulldata$Crime, Fulldata$CountryPopulation, main="Scatterplot Of Crimes", 
    xlab="Crimes ", ylab="Country Population", pch=19)+
    abline(lm(Fulldata$CountryPopulation~Fulldata$Crime), col="red" ) # regression line (y~x) 

max(Fulldata$CountryPopulation)


#install.packages("magrittr")
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # need to load to run %>%
library(dplyr)    # alternative, this also loads %>%

#Mutated Crime coulmn was created
Fulldata <- Fulldata %>% mutate(Crime = suicides_no + Homicides)
CrimeAge <- Fulldata %>% select(sex,Crime)

write.csv(CrimeAge[1:10000, ], file = "CrimeAge.csv", row.names = F)
CrimeAge <- read.csv("CrimeAge.csv", stringsAsFactors = F, sep = ",")

#install.packages("ggpubr")
library(ggpubr)

#Mean of Males/Females in Crime
ggdensity(CrimeAge, x = "Crime",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))


#Number of Males/Females in each City
ggdotchart(Fulldata, x = "City", y = "CityPopulation",
           color = "sex",                                # Color by groups
           palette = c("#00AFBB", "#E7B800"), # Custom color palette
           sorting = "ascending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr()                        # ggplot2 theme
)

#Number of Suicidal victims had gone for treatment
ggplot(Fulldata) +
  aes(x = seek_help, y = suicides_no, colour = sex) +
  geom_point() +
  facet_grid(~ Age)

#Homicides that had treatment regarding the mental health consequences
ggplot(Fulldata) +
  aes(x = treatment, y = Homicides, colour = sex) +
  geom_point()
  #facet_grid(~ Age)



#sort(unique(Fulldata$City))

table(Fulldata$Age)

str(Fulldata)
