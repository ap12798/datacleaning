#fin <- read.csv("Future-500.csv")
fin <- read.csv("Future-500.csv", na.strings = c(""))
fin$Inception <- factor(fin$Inception)
str(fin)
head(fin)
fin$Profit <- factor(fin$Profit)

#fin$Profit <- as.numeric(fin$Profit)

#sub() and gsub()
fin$Expenses <- gsub(" Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
#escape sequence
#also avoids factor variable trap by converting to character
fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
fin$Growth <- gsub("%","",fin$Growth)
head(fin)
str(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
summary(fin)

#Locating Missing Data
head(fin, 24)
#IS there an NA in any column?
#complete.cases(fin)
#all rows with an NA
fin[!complete.cases(fin),]

#Filtering: using which() for non-missing data
head(fin)
fin[which(fin$Revenue == 9746272),]
#Filtering: using is.na() for missing data
#Finging NAs in expenses column
fin[is.na(fin$Expenses),]

#Removing records with missing data
fin_backup <- fin
fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin <- fin[!is.na(fin$Industry),]
head(fin, 20)

#Resetting the dataframe index
rownames(fin) <- NULL
#Replacing missing data: Factual Analysis Method using another column to dictate missing data i.e. column was New York for city
#means state is NY
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"
#check:
fin[c(11,377),]
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"
#check:
fin[c(82,265),]

#Replacing Missing Data: Median Imputation Method (Part 1)
fin[!complete.cases(fin),]

med_empl_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- med_empl_retail
fin[3,]

med_empl_finserv <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] <- med_empl_finserv
fin[330,]

#Replacing Missing Data: Median Imputation Method (Part 2)
med_growth_constr <- median(fin[fin$Industry == "Construction", "Growth"], na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry == "Construction",]
fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- med_growth_constr
fin[8,]

fin[!complete.cases(fin),]

#Replacing Missing Data: Median Imputation Method (Part 3)
med_rev_constr <- median(fin[fin$Industry == "Construction", "Revenue"], na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry == "Construction",]
fin[is.na(fin$Revenue) & fin$Industry == "Construction", "Revenue"] <- med_rev_constr

fin[!complete.cases(fin),]

#Expenses:
#Be careful here. Only for certain ones
#We don't want to replace that one that's by itself (because then that row won't add up)
med_exp_constr <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry == "Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry == "Construction"& is.na(fin$Profit), "Expenses"] <- med_exp_constr

fin[!complete.cases(fin),]

#Replacing Missing Data: deriving values
#Revenue - Expenses = Profit
#Expenses = Revenue - Profit
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
p <- ggplot(data = fin)
p + geom_point(aes(x = Revenue, y = Expenses,
                   colour = Industry, size = Profit))

d <- ggplot(data = fin, aes(x = Revenue, y = Expenses,
                                colour = Industry))
d + geom_point() + 
  geom_smooth(fill = NA, size = 1.2)

f <- ggplot(data = fin, aes(x = Industry, y = Growth,
                            colour = Industry))
f + geom_boxplot(size = 1)

#Extra:
f + geom_jitter() + 
  geom_boxplot(size = 1, alpha = 0.5,
               outlier.color = NA)


