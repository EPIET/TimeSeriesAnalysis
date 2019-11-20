## Time Series Analysis module session 9 R help script

# - This session teaches participants how to perform capture recapture studies
# - There are two computational exercises:
# - Task 9.3: Calculate the sensitivity of the NSSS using the NRLS as the comparator (Salmonella notifications)
# - Task 9.5: Calculate the sensitivity of the NTSS using both the NTRL and RTSS as comparators (TB notifications)

# - In both tasks, the aim is to calculate the sensitivity of the notification system, 
# - relative to the database(s) which store the laboratory results for confirmed cases.

# - In task 9.3, the focus is on arranging the raw data to calculate specificity of one dataset compared to the other.
# - In task 9.5, preformatted data from 3 sources is provided; the focus is on using two data sources to calculate the specificity of the third.

###############################################################################################
## GETTING STARTED:

# Load relevant packages:
required_packages <- c('broom', 'car', 'ggplot2', 'haven', 
                       'ISOweek', 'lubridate', 'MASS', 'pander',  
                       'readxl', 'reshape2', 'TSA', 'zoo')
for(i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)

# Function to create Stata weekly date
stataweekdate <- function(year, week){
  (year - 1960) * 52 + week - 1
}

# Function to create Stata year and week numbers
statawofd <- function(date){
  if(!is.Date(date)) stop('date should be a Date.')
  dateposix <- as.POSIXlt(date)
  dayofyear <- dateposix$yday
  week <- floor(dayofyear/7) + 1
  week[week %in% 53] <- 52
  year <- dateposix$year + 1900
  list(year=year, week=week)
}

# Function to tidy glm regression output
glmtidy <- function(x, caption=''){ 
  pander(tidy(x, exponentiate=TRUE, conf.int=TRUE),
         caption=caption)
}

# Function to tidy glm regression statistics
glmstats <- function(x){
  pander(glance(x))
}

# Set your working directory to the sub-folder for practical session 9:
setwd("H:/06. NIS GII/08. Teaching/EPIET TSA/Practical sessions/P09 - Sensitivity and PPV")

###############################################################################################
## TASK 9.3: CALCULATING SENSITIVITY OF SALMONELLA NOTIFICATIONS

##  9.3.1 Importing the data:
# The data is provided in two worksheets in a .xlsx file, called 'NSSS' and 'NRLS'.
# To import the data into R, use the read_excel function from the readxl package:
nsss <- read_excel(path = 'salmonella.xlsx', sheet = 'NSSS')
nrls <- read_excel(path = 'salmonella.xlsx', sheet = 'NRLS')

###################################
## 9.3.2 Look at the data structure:
str(nsss)
str(nrls)

###################################
## 9.3.2 Creating a unique ID:
# To see how many cases are represented in both datasets, the datasets need to be merged.
# To facilitate the merge, create an 'id' column by pasting together name and date of birth.

# First create a date of birth column in both datasets:
nsss$dob <- as.Date(paste0(nsss$year, "-", nsss$month, "-", nsss$day), format = "%Y-%m-%d")
nrls$dob <- as.Date(paste0(nrls$year, "-", nrls$month, "-", nrls$day), format = "%Y-%m-%d")
# Note: using paste0 rather than paste ensures no spaces between the concatenated YYYY-mm-dd
# If there are spaces between the date elements the conversion will not work
# This is equivalent to paste(..., sep = "") but slightly more efficient

# Now check that the date conversions have worked correctly:
summary(nsss)
summary(nrls)

# You can see that one date in the NSSS dataset has failed to convert (there is an NA in the dob column)
# Subset the data to view the problem date:
View(subset(nsss, is.na(dob), select = c("ID-NSSS", "day", "month", "year", "dob")))

# The date has failed to convert because it does not exist
# 2010 was not a leap year so there were only 28 days in February
# Correct the date to 28 Feb 2010:
nsss$day[ is.na(nsss$dob)] <- 28

# Now repeat the date conversion and check it has worked (there should be no NAs in the dob column):
nsss$dob <- as.Date(paste0(nsss$year, "-", nsss$month, "-", nsss$day), format = "%Y-%m-%d")
summary(nsss)

# Create the unique ID in both datasets by pasting name and date of birth together:
nsss$id <- paste0(nsss$name, "_", nsss$dob)
nrls$id <- paste0(nrls$name, "_", nrls$dob)

# Check the newly created id column:
head(nsss$id)
head(nrls$id)

table(is.na(nsss$id))
table(is.na(nrls$id))

# Note: this is a very simple example of record linkage.
# For more complex cases (e.g. full names that may be spelt differently or personal identifiers that may be missing):
# - the stringi package can be used to match names with soundex codes or levenshtein distance
# - the RecordLinkage package has additional options, including probabalistic matching
# - a logical routine can be made from a combination of these functions according to what makes sense for your data sets.

###################################
## 9.3.3 Merge the two datasets based on the unique id:

# This step will merge all columns that have the same name if the cells have the same values
# To avoid unecessary column duplicates, first rename the 'case' column (column 6) in the NRLS data
# to 'labresult' as it contains different information:
colnames(nrls)[6] <- "labresult"

# Now merge the two datasets:
twosources <- merge(nsss, nrls, all = TRUE)
# Not specifying a column to merge on will merge all columns with the same name that have the same values
# Specifying all = TRUE ensures that records in NSSS that are missing in NRLS are included and vice-versa

# Optional check: calculate the number of records that are present in one dataset and not the other:

# Function to negate %in%
`%!in%` = Negate(`%in%`) 

# How many cases are notified to NSSS that don't have matching lab results in NRLS?
length(nsss$id[nsss$id %!in% nrls$id])

# How many cases have lab results in NRLS that don't have matching notifications in NSSS?
length(nrls$id[nrls$id %!in% nsss$id])

###################################
## 9.3.4 Checking for and removing duplicates:

# First sort the data set by id:
twosources <- twosources[order(twosources$id),]

# Then create a logical column to indicate if any rows are duplicates:
twosources$dup <- duplicated(twosources$id)

# Check to see if there are any duplicates:
table(twosources$dup)

# If any are identified, remove them by subsetting the data with 'dup == FALSE':
twosources <- subset(twosources, twosources$dup == FALSE)
# Note: if you have other criteria that are important in deciding which records to retain
# e.g. earliest specimen date, sort on this variable as well.

###################################
## 9.3.5 Calculating sensitivity:

# Create logical columns indicating if each record is included in one or both datasets:
twosources$nsss <- ifelse(!is.na(twosources$`ID-NSSS`), "NSSS_true", "NSSS_false")
twosources$nrls <- ifelse(!is.na(twosources$IDNRLS), "NRLS_true", "NRLS_false")

# Create the summary table (optional):
# This is easier to do if using data.table
install.packages('data.table')
library(data.table)

# Create the table with dcast:
twosources <- data.table(twosources)
my2by2 <- dcast(twosources, nsss ~ nrls, fun.aggregate = length, fill = 0, value.var = "id")

# Add row margin totals:
sumcols <- names(my2by2)[2:length(names(my2by2))] # select numeric columns only to sum
my2by2[, Total := Reduce(`+`, .SD), .SDcols = sumcols] # add them together to produce totals

# Add column margin totals:
sumcols <- names(my2by2)[2:length(names(my2by2))]
coltotals <- my2by2[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sumcols]
coltotals[, nsss := "Total"]
setcolorder(coltotals, names(my2by2))

# Append column totals to the summary table:
my2by2 <- rbind(my2by2, coltotals)

# Calculate the sensitivity of each dataset:
# Sensitivity = Records in both datasets / (Records in both datasets + Records only in the reference dataset)
both <- nrow(subset(twosources, nsss == "NSSS_true" & nrls == "NRLS_true"))
NSSSonly <- nrow(subset(twosources, nsss == "NSSS_true" & nrls == "NRLS_false"))
NRLSonly <- nrow(subset(twosources, nsss == "NSSS_false" & nrls == "NRLS_true"))

# Sensitivity of the NSSS (NRLS is the reference):
NSSS_sens <- (both/(both + NRLSonly))*100

# Sensitivity of the NRLS (NRLS is the reference):
NRLS_sens <- (both/(both + NSSSonly))*100

###############################################################################################
## TASK 9.5: CALCULATING SENSITIVITY OF TB NOTIFICATIONS

##  9.5.1 Importing the data:
# The data is provided in a STATA file.
# To import the data into R, use the read_dta function from the haven package:
threesources <- read_dta("threesources.dta")

# The STATA companion uses the package 'recap' to compare the models
# In R, an equivalent set of functions can be found in the package 'Rcapture'

# Install and load the Rcapture package:
install.packages('Rcapture')
library(Rcapture)

# Remove ID column before creating the models (input should be a matrix):
tb <- subset(threesources, select = c("ntss", "ntrl", "hospital"))

# Create the equivalent to the first model (all three are independent)
m1 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ ., mname = "All independent")
m1
summary(m1$fit)$coefficients

# Create the second model (dependency between sources A-B)
m2 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c2) + c3, mname = "AB dependent")
m2
summary(m2$fit)$coefficients

# Create the third model (dependency between sources A - C)
m3 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c3) + c2, mname = "AC dependent")
m3
summary(m3$fit)$coefficients

# Create the fourth model (dependency between sources B - C)
m4 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c2 * c3) + c1, mname = "BC dependent")
m4
summary(m4$fit)$coefficients

# Create the fifth model (dependency between sources AB and AC)
m5 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c2) + (c1 * c3), mname = "AB and AC dependent")
m5
summary(m5$fit)$coefficients

# Create the sixth model (dependency between sources AB and BC)
m6 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c2) + (c2 * c3), mname = "AB and BC dependent")
m6
summary(m6$fit)$coefficients

# Create the seventh model (dependency between sources AC and BC)
m7 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c3) + (c2 * c3), mname = "AC and BC dependent")
m7
summary(m7$fit)$coefficients

# Create the eighth model (total dependency)
m8 <- closedpCI.t(tb, dfreq = FALSE, mX = ~ (c1 * c2 * c3), mname = "All dependent")
m8
summary(m8$fit)$coefficients

# Alternatively the models can be built with a summary table using dfreq = TRUE:
# Create the summary table:
tbs <- dcast(data.table(threesources), ntss + ntrl + hospital ~ ., fun.aggregate = length, value.var = "uniqueid")
setnames(tbs, ".", "Freq")

# Get the descriptive summary:
desc <- descriptive(tbs, dfreq = TRUE)

# Create the first model (results for sm1 should be identical to m1):
sm1 <- closedpCI.t(tbs, dfreq = TRUE, mX = ~ (c1 + c2 + c3), mname = "All independent")
sm1
summary(sm1$fit)$coefficients

# The AIC and BIC values can be used to select the best model fit.
# Note: the approach taken with Rcapture is similar but not identical to the STATA recap package.
# Case population estimates will be comparable but AIC and BIC values may be different.
# Examples shown here are based on the HIV example included in the Rcapture package.

###############################################################################################


