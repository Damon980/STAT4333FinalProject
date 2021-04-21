library(tidyverse)
library(dplyr)
library(maps)
library(countrycode)
library(rworldmap)

#Data was taken from [clinicaltrials.gov](https://clinicaltrials.gov/) using the search term COVID. 
countrydata = read.csv("rawPopData.csv", header = T)
rawData <- read_csv('SearchResults-COVID.csv', col_types = "iccccccccccccicccccDDDDDccc")
head(rawData)

df = rawData %>% filter(!is.na(Locations))
df = df %>% select(Enrollment, Locations, `Start Date`) %>% mutate(Enrollment = replace_na(Enrollment, 0)) #Ongoing studies are NA in enrollment 

splt0 <- strsplit(df$`Start Date`, " ", fixed = TRUE)

x0 = c()
f0 = unlist(splt0)
num = 0
n = nrow(df)
for (i in 1:n ) {
  num = num + length(splt0[[i]])
  x0[i] = f0[num]
}

df$`Start Date` = x0

splt <- strsplit(df$Locations, ",", fixed = TRUE)

x = c()
f = unlist(splt)
num = 0
n = nrow(df)
for (i in 1:n ) {
  num = num + length(splt[[i]])
  x[i] = f[num]
}

x = trimws(x)

x[x == "Islamic Republic of"] = "Iran"
x[x == "Republic of"] = "Republic of Korea"

df$Locations = x


df$Locations = countrycode(df$Locations, "country.name", "iso3c")

cleanDF = df %>% group_by(Locations, `Start Date`) %>%  summarise(n = n(), y = sum(Enrollment))
cleanDF19 = cleanDF %>% filter(`Start Date` == "2019")
cleanDF20 = cleanDF %>% filter(`Start Date` == "2020")
cleanDF21 = cleanDF %>% filter(`Start Date` == "2021")

cleanDFTrial = df %>% group_by(Locations, `Start Date`) %>%  summarise(y = n())
cleanDF19Trial = cleanDFTrial %>% filter(`Start Date` == "2019")
cleanDF20Trial = cleanDFTrial %>% filter(`Start Date` == "2020")
cleanDF21Trial = cleanDFTrial %>% filter(`Start Date` == "2021")

combindDf = cleanDF %>% left_join(countrydata, by = c("Locations" = "Country.Code")) 
names(combindDf)[5:6] = c("countryName", "Population")
combindDf = combindDf %>% select(-countryName)

combindDf = combindDf %>% 
  mutate(Population = replace(Population, Locations == "GUF", 294071)) %>% 
           mutate(Population = replace(Population, Locations == "MTQ", 376480)) %>% 
                    mutate(Population = replace(Population, Locations == "REU", 859959)) %>% 
                             mutate(Population = replace(Population, Locations == "TWN", 23570000))
                                    






#               catMethod = "categorical", 
#c(20, 756, 4592, 39658, 36288635) Quantiles for enrollmentSum
#for (x in splt)
#countries = tail(strsplit(splt, ',', fixed = TRUE)[[1]],1)
#tail(strsplit(splt[[4]][1], ',', fixed = TRUE)[[1]],1)