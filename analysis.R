### library
library(rstudioapi)
library(plyr)
library(dplyr)
library(rpart)
library(maps)
library(googleVis)
library(NbClust)

### load data
project_dir = dirname(rstudioapi::getSourceEditorContext()$path)
ph_dir = file.path(project_dir, 'data', 'publichealth')
covid_dir = file.path(project_dir, 'data', 'covid19')
others_dir = file.path(project_dir, 'data', 'others')

phdf1 <-
  read.csv(file.path(ph_dir, 'API_SH.DTH.COMM.ZS_DS2_en_csv_v2_3160229.csv'),
           fileEncoding = 'UTF-8-BOM')
phdf2 <-
  read.csv(file.path(ph_dir, 'API_SH.IMM.MEAS_DS2_en_csv_v2_3165446.csv'),
           fileEncoding = 'UTF-8-BOM')
phdf3 <-
  read.csv(file.path(ph_dir, 'API_SH.MED.BEDS.ZS_DS2_en_csv_v2_3160868.csv'),
           fileEncoding = 'UTF-8-BOM')
phdf4 <-
  read.csv(file.path(ph_dir, 'API_SH.MED.PHYS.ZS_DS2_en_csv_v2_3159558.csv'),
           fileEncoding = 'UTF-8-BOM')
phdf5 <-
  read.csv(file.path(ph_dir, 'API_SP.DYN.AMRT.MA_DS2_en_csv_v2_3165680.csv'),
           fileEncoding = 'UTF-8-BOM')
phdf6 <-
  read.csv(file.path(ph_dir, 'SYB64_325_202110_Expenditure on health.csv'),
           encoding = 'UTF-8')
phdf7 <- read.csv(file.path(others_dir, 'press_freedom_index.csv'),
                  encoding = 'UTF-8')
phdf8 <- read.csv(file.path(others_dir, 'democracy_score.csv'),
                  encoding = 'UTF-8')

coviddf <- read.csv(file.path(covid_dir, 'owid-covid-data.csv'),
                    encoding = 'UTF-8')

### pre-processing
phdf1 <- phdf1 %>%
  mutate(Communicable.Diseases = X2019) %>%
  select(Country.Name, Communicable.Diseases)

phdf2 <- phdf2 %>%
  mutate(Measles.Immunization = X2019) %>%
  select(Country.Name, Measles.Immunization)

phdf3 <- phdf3 %>%
  mutate(Hospital.Beds = X2011) %>%
  select(Country.Name, Hospital.Beds)

phdf4 <- phdf4 %>%
  mutate(Physicians = X2010) %>%
  select(Country.Name, Physicians)

phdf5 <- phdf5 %>%
  mutate(Mortarity.Rate = X2017) %>%
  select(Country.Name, Mortarity.Rate)

phdf6 <- phdf6 %>%
  filter(Year == 2018, Series == 'Current health expenditure (% of GDP)') %>%
  mutate(Country.Name = X, Health.Expenditure = Value) %>%
  select(Country.Name, Health.Expenditure)

phdf7 <- phdf7 %>%
  mutate(Country.Name = EN_country) %>%
  mutate(press.rank = Rank2021) %>%
  mutate(press.score = Score.A) %>%
  select(Country.Name, press.score, press.rank)

phdf8 <- phdf8 %>%
  mutate(Country.Name = X.U.FEFF.country) %>%
  mutate(democracy.score = eiu) %>%
  select(Country.Name, democracy.score)

coviddf <- coviddf %>%
  filter(date == '2021-11-01', continent != '') %>%
  mutate(Country.Name = location) %>%
  select(
    Country.Name,
    total_cases,
    total_deaths,
    total_cases_per_million,
    total_deaths_per_million
  )

covid_ph_df <-
  join_all(
    list(phdf1, phdf2, phdf3, phdf4, phdf5, phdf6, phdf7, phdf8, coviddf),
    by = 'Country.Name',
    type = 'full'
  )
covid_ph_df <- na.omit(covid_ph_df)
row.names(covid_ph_df) <- NULL


df1 <- covid_ph_df %>%
  select(
    Communicable.Diseases,
    Measles.Immunization,
    Hospital.Beds,
    Physicians,
    Mortarity.Rate
  )
PCA1 <- prcomp(df1, scale = T, center = T)

df2 <- covid_ph_df %>%
  select(press.score, democracy.score)
PCA2 <- prcomp(df2, scale = T, center = T)

covid_ph_df$health.idx <- PCA1$x[, 1]
covid_ph_df$freedom.idx <- PCA2$x[, 1]

### Scaling
covid_ph_df$Communicable.Diseases <-
  scale(covid_ph_df$Communicable.Diseases)
covid_ph_df$Measles.Immunization <-
  scale(covid_ph_df$Measles.Immunization)
covid_ph_df$Hospital.Beds <- scale(covid_ph_df$Hospital.Beds)
covid_ph_df$Physicians <- scale(covid_ph_df$Physicians)
covid_ph_df$Mortarity.Rate <- scale(covid_ph_df$Mortarity.Rate)
covid_ph_df$Health.Expenditure <-
  scale(covid_ph_df$Health.Expenditure)
covid_ph_df$press.score <- scale(covid_ph_df$press.score)
covid_ph_df$democracy.score <- scale(covid_ph_df$democracy.score)


### Anova
health_pf_df <- covid_ph_df %>%
  dplyr::select(
    Communicable.Diseases,
    Measles.Immunization,
    Hospital.Beds,
    Physicians,
    Mortarity.Rate,
    Health.Expenditure
  )
# NbClust(health_pf_df, method = "complete")$Best.nc
# n.cluster <- 3
freedom_pf_df <- covid_ph_df %>%
  dplyr::select(press.score, democracy.score)

covid_health_kmeans = kmeans(health_pf_df, centers = 3, iter.max = 10000)
covid_freedom_kmeans = kmeans(freedom_pf_df, centers = 4, iter.max = 10000)
covid_ph_df$health.cluster = as.factor(covid_health_kmeans$cluster)
covid_ph_df$freedom.cluster = as.factor(covid_freedom_kmeans$cluster)

##### anova tests (total cases)
one.way_health_covid_ph_df <-
  aov(total_cases_per_million ~ health.cluster, data = covid_ph_df)
one.way_freedom_covid_ph_df <-
  aov(total_cases_per_million ~ freedom.cluster, data = covid_ph_df)

##### anova tests (total deaths)
one.way_death_health_covid_ph_df <-
  aov(total_deaths_per_million ~ health.cluster, data = covid_ph_df)
one.way_death_freedom_covid_ph_df <-
  aov(total_deaths_per_million ~ freedom.cluster, data = covid_ph_df)


###### anova results ( total cases per million - health )
print(covid_health_kmeans$center)
print(summary(one.way_health_covid_ph_df))
print(model.tables(one.way_health_covid_ph_df, "means"), digits = 3)

###### anova results ( total cases - freedom )
print(covid_freedom_kmeans$center)
print(summary(one.way_freedom_covid_ph_df))
print(model.tables(one.way_freedom_covid_ph_df, "means"), digits = 3)

###### anova results ( total deaths per million - health )
print(covid_health_kmeans$center)
print(summary(one.way_death_health_covid_ph_df))
print(model.tables(one.way_death_health_covid_ph_df, "means"),
      digits = 3)

###### anova results ( total deaths per million - freedom )
print(covid_freedom_kmeans$center)
print(summary(one.way_death_freedom_covid_ph_df))
print(model.tables(one.way_death_freedom_covid_ph_df, "means"),
      digits = 3)

### linear regression
covid_ph_df_lm <- covid_ph_df[, !names(covid_ph_df) %in% c('Country.Name')]
lm.model <- lm(total_cases_per_million ~ . , data=covid_ph_df_lm)
print(summary(lm.model))

### recursive partitioning
rpart.model <- rpart(total_cases_per_million~Communicable.Diseases+Measles.Immunization+Hospital.Beds+Physicians+Mortarity.Rate+Health.Expenditure, data=covid_ph_df_lm)
print(summary(rpart.model))
plot(rpart.model, uniform=TRUE, main='Regression Tree for COVID-19')
text(rpart.model, use.n=TRUE, all=TRUE, cex=0.8)

### maps
column_name_for_cases <- c("Country.Name", "total_cases_per_million")
covid_ph_df_by_cases_chart <- covid_ph_df[column_name_for_cases]
covid_ph_df_by_cases <- covid_ph_df_by_cases_chart[order(covid_ph_df_by_cases_chart$total_cases_per_million, decreasing=TRUE), ]
covid_ph_df_by_cases[nrow(covid_ph_df_by_cases) + 1, ] = c("United States", 145497.47)
covid_ph_df_by_cases[nrow(covid_ph_df_by_cases) + 1, ] = c("South Korea", 8915.24)
covid_ph_df_by_cases[nrow(covid_ph_df_by_cases) + 1, ] = c("Russia", 66449.92)

covid_ph_df_by_cases$Rank <- 1:nrow(covid_ph_df_by_cases)
row.names(covid_ph_df_by_cases) <- NULL

Gl <- gvisGeoChart(covid_ph_df_by_cases, "Country.Name", "total_cases_per_million", "Rank",
    options=list(dataMode="regions", width=1500, height=1000, colorAxis="{colors:['lightgreen','red']}"))

column_name_for_cases <- c("Country.Name", "total_cases_per_million", "Rank")
covid_ph_df_by_cases_chart <- covid_ph_df_by_cases[column_name_for_cases]
GM <- gvisTable(covid_ph_df_by_cases_chart, options=list(width=400, height=1000))
G2 <- gvisMerge(Gl, GM, horizontal=TRUE)
plot(G2)

### maps democracy
column_name_for_cases <- c("Country.Name", "democracy.score")
covid_ph_df_by_cases_chart <- phdf8[column_name_for_cases]
covid_ph_df_by_cases_chart[nrow(covid_ph_df_by_cases_chart) + 1, ] = c("United Kingdom", 8.54)
covid_ph_df_by_cases <-
  covid_ph_df_by_cases_chart[order(covid_ph_df_by_cases_chart$democracy.score, decreasing =
                                     TRUE), ]

covid_ph_df_by_cases$Rank <- 1:nrow(covid_ph_df_by_cases)
row.names(covid_ph_df_by_cases) <- NULL

Gl <-
  gvisGeoChart(
    covid_ph_df_by_cases,
    "Country.Name",
    "democracy.score",
    "Rank",
    options = list(
      dataMode = "regions",
      width = 1500,
      height = 1000,
      colorAxis = "{colors:['red','lightgreen']}"
    )
  )

column_name_for_cases <-
  c("Country.Name", "democracy.score", "Rank")
covid_ph_df_by_cases_chart <-
  covid_ph_df_by_cases[column_name_for_cases]
GM <-
  gvisTable(covid_ph_df_by_cases_chart, options = list(width = 400, height =
                                                         1000))
G2 <- gvisMerge(Gl, GM, horizontal = TRUE)
plot(G2)

### linear regression with freedom
freedom_column <- c("press.score", "press.rank", "democracy.score", "total_cases_per_million")
covid_ph_df_free <- covid_ph_df[freedom_column]
lm.model <- lm(total_cases_per_million ~ press.score + press.rank + democracy.score , data=covid_ph_df_free)
print(summary(lm.model))