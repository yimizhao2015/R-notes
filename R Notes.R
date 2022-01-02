
# R notes

#-------------------------------------------
## load R packages for functions will be used
packages.need <- c("tidyverse", "readxl","PerformanceAnalytics")
if (length(setdiff(packages.need, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages.need, rownames(installed.packages())))}
library(tidyverse); library(readxl); library(PerformanceAnalytics)

#-------------------------------------------
##load multiple data files at one time
files = list.files(pattern = ".csv")
dt <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = F)))

## load a data set from R package
data(nlsyV, package = "mi")

## import spss data
dt <- foreign::read.spss("data.sav", to.data.frame = T, use.value.labels = F)
dt <- haven::read_sav("data.sav")
 #view and abstract spss lables
sjPlot::view_df(mydt)
labelled::look_for(mydt)
labelled::look_for(mydt, "onevar")

## remove dots from the column names in R
names(mydf) <- gsub("\\.", "", names(mydf))

#-------------------------------------------
## Data Manipulation
 #calculate mean of multiple columns
dt %>% mutate(LiteracyScore = rowMeans(dplyr::select(., PVLIT1:PVLIT10), na.rm = T))

 #check baseline/reference level of a factor
contrasts(gator$Lake)
 #set reference/level levels for a factor
contrasts(gator$Lake) <- contr.treatment(levels(gator$Lake), base = 2)
 #re-level a factor variable
relevel(factor(myvar), ref = "referenceLevel")

 #replace NAs with 0s in a data frame
d[is.na(d)] <- 0

 #check data set summary
summary(df)
psych::describe(df)

#sort by one column
dt[order(dt$Id),]

#replace a single value in a column
mydt$CollegeProgramCode[mydt$CFEmplid == 23310816] <- "HS"

#lookup table in r
two <- c('AA','AS')
lut <- c('AA'="American", 'AS'='Alaska','B6'='Jetblue')
(two <- lut[two])

#two ways to remove columns
mydata[, !(names(mydata) %in% remove_cols)]
mydata[, -which(names(mydata) %in% remove_cols)]

 #convert all variables into factors
for (i in 1:ncol(raw)) {
  raw[, i] <- as.factor(raw[,i])
}
 #convert multiple variables into factors
dat %>% mutate_at(c("coa", "male", "age"), as.factor)

 #reorder columns 
data[c("size", "id", "weight")] #by column name
data[, c(1,3,2)]#by index


 #unique rows
unique(dt)
dt[!duplicated(dt),]

 #select unique rows by one/multiple variable
dt[!duplicated(dt$var),]
cols <- c("vars1", "vars2", "var3")
dt[!duplicated(dt[, cols]), ]

 #identify duplicate rows
which(duplicated(dt$var) | duplicated(dt$var, fromLast = TRUE))

 #order a “mixed” vector (numbers with letters)
library(gtools)
mixedorder(); mixedsort() # sort on one variable
multi.mixedorder <- function(..., na.last = TRUE, decreasing = FALSE){
  do.call(order, c(
    lapply(list(...), function(l){
      if(is.character(l)){
        factor(l, levels=mixedsort(unique(l)))
      } else {
        l
      }
    }),
    list(na.last = na.last, decreasing = decreasing)
  ))
}

 #subset unique cases based on multiple columns
library(dplyr)
df %>% distinct(v1, v2, v3, .keep_all = T) #distinct
df %>% group_by(v1, v2, v3) %>% filter(n() > 1) #non-distinct only
df %>% group_by(v1, v2, v3) %>% filter(n() == 1) #exclude any non-distinct

 #reshape data long <-> wide
library(tidyr)
pivot_longer()#wide to long
pivot_wider()#long to wide
 #long to wide example
instr_wide <- dt %>%
  tidyr::unite("Instr",  `Instructor LastName`: `Instructor FirstName`, sep = " ", na.rm = T) %>%
  select(ClassNum = `Class#`, Instr)%>%
  distinct()%>%
  group_by(ClassNum)%>%
  mutate(instr_index = row_number(),
         instr_n = max(row_number()))%>%
  pivot_wider(names_from = instr_index, values_from = Instr, names_prefix = "Instr_")

 #cross table in a pipeline way
data(mtcars)
mtcars %>% filter(vs == 1) %>% {table(.$cyl, .$gear)}
table(mtcars$cyl, mtcars$gear, mtcars$vs)#non-pipeline way

 #compute age from data of birth
cutoff<-as.Date("01-SEP-2015",format = "%d-%B-%Y")
library(eeptools)
age_computed02<- age_calc(birth_data, enddate = cutoff, units="years", precise = T)

 #paste multiple columns
full$stopout_flag <- apply(full[, -c(1,2)], 1, paste0, collapse ="")

 #overlapping string match
library(stringr)
test <- "0011001001100111000"
searchfor <- "1[0]+1" #pattern
str_count(test, paste0("(?=",searchfor,")"))

 #split one column into multiple columns
df <- data.frame("mytext" = as.character(row.names(mtcars)))
ncols <- max(stringr::str_count(df$mytext, pattern = " ")) + 1#No.of columns after splitting by space
columns <- paste("col", 1:ncols, sep = "_")#generate necessary column names

df_new1 <- df %>% tidyr::separate(mytext, sep =" ", into = columns, remove = F) #way 1: tidyr::separate
df_new2 <- cbind(df, reshape2::colsplit(df$mytext, " ", columns)) #way 2: reshape2::colsplit
df_new3 <- cbind(df, stringr::str_split_fixed(df$mytext, " ", ncols)) #way 3: stringr::str_split_fixed

 #sub-string example:get 01,02,03,04 as a new column (split by fixed length)
example<-data.frame(cbind(index=1:4,status=c("01.graduate","02.graduate","03.graduate","04.graduate")))
example2 = cbind(example, new = substr(example$status, 1, 2))

#-------------------------------------------
## Data visualization
 #plot the data using jittered points and the the glm stat_smooth
ggplot(data = dfLong, aes(x = dose, y = mortality)) + geom_jitter(height = 0.05, width = 0.1) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"))

 #save all plots already present in the panel of Rstudio
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from = plots.png.paths, to = "path_to_your_dir")

#-------------------------------------------
# check data missingness
library(naniar)
miss_var_summary(dt)
miss_var_table(dt)
vis_miss(dt)
gg_miss_upset(dt)
gg_miss_var(dt)
gg_miss_var(dt, facet = Gender)
gg_miss_case(dt)
gg_miss_case(dt, facet = Gender)

#-------------------------------------------
## Three ways of Logistic regression in R
 # way 1: binary: y = 0 or 1
glm(y ~ x, family = "binomial")
 # way 2: Wilkinson-Rogers: cbind(success, failure)
glm(cbind(success, failure) ~ x, family = "binomial")
 # way 3: Weighted format: y = 0.3, weights = 10
glm(y ~ x, weights = weights, family = "binomial")









