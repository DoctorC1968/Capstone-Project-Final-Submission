#Read in consumer complaint data. 
#Original source: 
# https://catalog.data.gov/dataset/consumer-complaint-database#topic=consumer_navigation

# This will clear the whole workspace: 
#rm(list=ls())

library(dplyr) 
library(ggplot2)
install.packages("vcd") 
library(vcd)
#install.packages("Hmisc")
#library(Hmisc)
setwd("C:/Users/Owner/Documents")

consumer.complaints <- read.csv("Consumer_Complaints.csv", stringsAsFactors = FALSE)
head(consumer.complaints)
str(consumer.complaints)
max(as.Date(consumer.complaints$Date.received, format = "%m/%d/%Y"))

#Use Date.received to get year and quarter of complaint

consumer.complaints$quarter.year <- paste(quarters(as.Date(consumer.complaints$Date.received,format="%m/%d/%Y")),
                                    substr(consumer.complaints$Date.received, nchar(consumer.complaints$Date.received)-3,
                                           nchar(consumer.complaints$Date.received)),
                                    sep=" ")

#Remove records from second quarter of 2017 since we don't have all of the data for this quarter yet
consumer.complaints  <-  consumer.complaints[consumer.complaints$quarter.year != "Q2 2017", ] 

#Find frequencies of products that got complaints so that we known which products to 
#prioritize in our investigation 

(product.frequencies  <-  sort(table(consumer.complaints$Product))) 
write.csv(product.frequencies, file = "Product_freq.csv") 

#We will first focus on the product "Mortgage" since it has the most complaints
#After that I can try :
#Debt collection
#Credit reporting
#Credit card
#Bank account or service
#Consumer Loan
#Student loan
#Consumer loan

#Count the number of mortgage-related consumer complaints by counting up the number of distinct
#mortgage complaint ids per bank. Note that this method will only capture NONZERO complaint counts,
#because if there are no complaints then there will be no complaint ids. 

consumer.complaints.mortgage  <-  consumer.complaints %>%
  filter(Product == "Mortgage") 
nrow(consumer.complaints.mortgage) #225,023

#Find mortgage complaint counts per company and quarter/year
consumer.complaint.counts.mortgage  <-  consumer.complaints.mortgage %>%
  group_by(Company, quarter.year) %>%
  summarise(complaint.count = n_distinct(Complaint.ID)) 

nrow(consumer.complaint.counts.mortgage) #6640

#Find number of quarters per bank
quarter.counts  <-  consumer.complaint.counts.mortgage %>%
  group_by(Company) %>%
  summarise(number.of.quarters = n_distinct(quarter.year))
summary(quarter.counts)
#Number of quarters per bank ranges from 1 to 22, which shows that many banks have quarters 
#with no mortgage-related complaints

#For some banks and quarters, there are zero complaints. But these bank/quarter
#combinations are missing from the data. We have to put in the zero complaint counts ourselves.
#This is done below.

bank.names <- unique(consumer.complaints.mortgage$Company)
quarter.years <- sort(unique(consumer.complaints.mortgage$quarter.year))

left.set <- data.frame(Company = character(), quarter.year = character(), 
                       stringsAsFactors = FALSE)

for (Company in bank.names) {
  for (quarter.year in quarter.years) {
    newrow <- data.frame(Company = c(Company), quarter.year =c(quarter.year), 
                         stringsAsFactors = FALSE)
    left.set <- rbind(left.set, newrow)
  }
}

nrow(left.set) #27346

#Count quarters per company in left.set
quarter.counts  <-  left.set %>%
  group_by(Company) %>%
  summarise(number.of.quarters = n_distinct(quarter.year))
summary(quarter.counts)
#22 quarters for each bank

consumer.complaint.counts.mortgage.2 <-  
  left_join(left.set, consumer.complaint.counts.mortgage, by = c("Company", "quarter.year"))

#If complaint count is missing, then replace that complaint count with 0
consumer.complaint.counts.mortgage.2$complaint.count<-
   ifelse(is.na(consumer.complaint.counts.mortgage.2$complaint.count),0,
                consumer.complaint.counts.mortgage.2$complaint.count)

str(consumer.complaint.counts.mortgage.2)

quarter.counts <- consumer.complaint.counts.mortgage.2 %>%
  group_by(Company) %>%
  summarise(number.of.quarters=n_distinct(quarter.year))

summary(quarter.counts)
#all 22

summary(consumer.complaint.counts.mortgage.2$complaint.count)
#Min.     1st Qu.  Median   Mean     3rd Qu. Max. 
#0.000    0.000    0.000    8.229    0.000   4942.000 
#There are a lot of zero complaint counts, but that's because there are a lot of small banks

############################################################################################################

#Next read in the equity capital information for the banks. The purpose is to 
#assign a size to the banks so that we can scale their complaint counts accordingly

#Source: http://www.usbanklocations.com/bank-rank/total-equity-capital.html?d=2016-09-30

Ranked.Banks_12_31_11 <-   read.csv('Ranked Banks 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_12 <- read.csv('Ranked Banks 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks_6_30_12 <- read.csv('Ranked Banks 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks_9_30_12 <- read.csv('Ranked Banks 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks_12_31_12 <- read.csv('Ranked Banks 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_13 <- read.csv('Ranked Banks 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks_6_30_13 <- read.csv('Ranked Banks 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks_9_30_13 <- read.csv('Ranked Banks 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks_12_31_13 <- read.csv('Ranked Banks 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_14 <- read.csv('Ranked Banks 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks_6_30_14 <- read.csv('Ranked Banks 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks_9_30_14 <- read.csv('Ranked Banks 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks_12_31_14 <- read.csv('Ranked Banks 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_15 <- read.csv('Ranked Banks 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks_6_30_15 <- read.csv('Ranked Banks 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks_9_30_15 <- read.csv('Ranked Banks 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks_12_31_15 <- read.csv('Ranked Banks 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_16 <- read.csv('Ranked Banks 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks_6_30_16 <- read.csv('Ranked Banks 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks_9_30_16 <- read.csv('Ranked Banks 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks_12_31_16 <- read.csv('Ranked Banks 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks_3_31_17 <- read.csv('Ranked Banks 3-31-17.csv', stringsAsFactors = FALSE)
#Stack the ranked banks into one set
All.Ranked.Banks  <-  rbind(Ranked.Banks_12_31_11,
                          Ranked.Banks_3_31_12,
                          Ranked.Banks_6_30_12,
                          Ranked.Banks_9_30_12,
                          Ranked.Banks_12_31_12,
                          Ranked.Banks_3_31_13,
                          Ranked.Banks_6_30_13,
                          Ranked.Banks_9_30_13,
                          Ranked.Banks_12_31_13,
                          Ranked.Banks_3_31_14,
                          Ranked.Banks_6_30_14,
                          Ranked.Banks_9_30_14,
                          Ranked.Banks_12_31_14,
                          Ranked.Banks_3_31_15,
                          Ranked.Banks_6_30_15,
                          Ranked.Banks_9_30_15,
                          Ranked.Banks_12_31_15,
                          Ranked.Banks_3_31_16,
                          Ranked.Banks_6_30_16,
                          Ranked.Banks_9_30_16,
                          Ranked.Banks_12_31_16,
                          Ranked.Banks_3_31_17)
nrow(All.Ranked.Banks)#145667

#Get year and quarter from the dates
All.Ranked.Banks$quarter.year <- paste(quarters(as.Date(All.Ranked.Banks$Date, format="%m/%d/%Y")),
                                    substr(All.Ranked.Banks$Date, nchar(All.Ranked.Banks$Date)-3, nchar(All.Ranked.Banks$Date)),
                                    sep=" ")

#See which quarters are in All.Ranked.Banks
sort(unique(All.Ranked.Banks$quarter.year)) #Q1 2012 to Q1 2017
All.Ranked.Banks <- subset(All.Ranked.Banks, select = -Date)

str(All.Ranked.Banks)#145667 obs
write.csv(All.Ranked.Banks, file = "banks_ranked_by_equity_capital.csv")
max(All.Ranked.Banks$Rank) #7366

#Find and eliminate the banks that have more than one rank within any quarter
Ranked.Banks.With.Numranks <- All.Ranked.Banks %>% 
  group_by(Bank.Name, quarter.year) %>% 
  summarise(rank.count=n_distinct(Rank))

summary(Ranked.Banks.With.Numranks$rank.count)

nrow(Ranked.Banks.With.Numranks) #124445

(rank.distribution <- Ranked.Banks.With.Numranks %>% 
  group_by(rank.count) %>% 
  summarise(freq = n_distinct(Bank.Name,quarter.year)))

Ranked.Banks.With.Max.Ranks.Per.Qtr <- 
    Ranked.Banks.With.Numranks %>% 
    group_by(Bank.Name) %>% 
    summarise(Max.Number.Of.Ranks = max(rank.count))

summary(Ranked.Banks.With.Max.Ranks.Per.Qtr$Max.Number.Of.Ranks)

(Max.Num.Ranks.Distribution <- Ranked.Banks.With.Max.Ranks.Per.Qtr %>% 
  group_by(Max.Number.Of.Ranks) %>% 
  summarise(freq = n_distinct(Bank.Name))) #6346 banks have a a max of 1 rank in each quarter
Max.Num.Ranks.Distribution[1,2]

#Find percentage of banks that have exactly one rank in every quarter
Max.Num.Ranks.Distribution[1,2]/length(unique(All.Ranked.Banks$Bank.Name))
#94% of the banks have one rank in every quarter

#Keep only those banks with exactly one rank per quarter
Ranked.Banks.With.1.Rank.Per.Qtr <- 
  Ranked.Banks.With.Max.Ranks.Per.Qtr[Ranked.Banks.With.Max.Ranks.Per.Qtr$Max.Number.Of.Ranks==1,]

nrow(Ranked.Banks.With.1.Rank.Per.Qtr) #6346 :-)

str(Ranked.Banks.With.1.Rank.Per.Qtr)
summary(Ranked.Banks.With.1.Rank.Per.Qtr$Max.Number.Of.Ranks)

#Left join all ranked banks onto Ranked.Banks.With.1.Rank.Per.Qtr 
#(where Ranked.Banks.With.1.Rank.Per.Qtr is the left set)

All.Ranked.Banks.With.1.Rank.Per.Qtr <- 
    left_join(Ranked.Banks.With.1.Rank.Per.Qtr, All.Ranked.Banks, by = "Bank.Name")

nrow(All.Ranked.Banks.With.1.Rank.Per.Qtr)#115425
length(unique(All.Ranked.Banks.With.1.Rank.Per.Qtr$Bank.Name))#6346  :-)

#Remove max number of ranks from All.Ranked.Banks.With.1.Rank.Per.Qtr
within(All.Ranked.Banks.With.1.Rank.Per.Qtr, rm(Max.Number.Of.Ranks))

#Consider only the banks that had an equity-capital rank of 100 or less at some time. 
Maximum.Rank <- 100

Banks.Minimum.Ranks <-  All.Ranked.Banks.With.1.Rank.Per.Qtr %>%
  group_by(Bank.Name) %>%
  summarise(Minimum.Rank = min(Rank))

All.Ranked.Banks.Names.Top.100 <-  Banks.Minimum.Ranks %>% 
  filter(Minimum.Rank <= Maximum.Rank) 

summary(All.Ranked.Banks.Names.Top.100$Minimum.Rank)

nrow(distinct(All.Ranked.Banks.Names.Top.100)) #127 obs

length(unique(All.Ranked.Banks.With.1.Rank.Per.Qtr$Bank.Name)) #6346

All.Ranked.Banks.Top.100 <- 
  inner_join(All.Ranked.Banks.With.1.Rank.Per.Qtr,
             subset(All.Ranked.Banks.Names.Top.100, select=c("Bank.Name")),
             by=c("Bank.Name"))

nrow(All.Ranked.Banks.Top.100)#2384 obs. 

#Check number of ranks each bank has within each quarter: 
Number.Of.Ranks <- All.Ranked.Banks.Top.100 %>%
  group_by(quarter.year, Bank.Name) %>%
  summarise(Number.Of.Ranks = n_distinct(Rank))
range(Number.Of.Ranks$Number.Of.Ranks)
#Each bank has just one rank within each quarter

#Extract unique bank names. Bank names will be merge keys. We will manually match up bank names from the complaints
#data with bank names from the ranked banks data
Unique.Banks.From.Complaints.Dataset <- unique(consumer.complaints$Company)
length(Unique.Banks.From.Complaints.Dataset)#4200

write.csv(Unique.Banks.From.Complaints.Dataset, file="unique_banks_from_complaints.csv")

(Unique.Banks.From.Ranked.Banks.Top.100 <- unique(All.Ranked.Banks.Top.100$Bank.Name))

length(Unique.Banks.From.Ranked.Banks.Top.100)#127 banks
write.csv(Unique.Banks.From.Ranked.Banks.Top.100, file="top 100 ranked banks by equity capital.csv")

###########################################################################################################################################
# Here we standardize the bank names; that is, we 
#    1. Take the banks that ever achieved a rank in the top 50
#    2. Tweak those bank names to match the names of the banks in the complaints data
###########################################################################################################################################

for (i in 1:nrow(All.Ranked.Banks.Top.100))
{All.Ranked.Banks.Top.100$Company[i] <- ''
if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Ally Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'ALLY FINANCIAL INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Amegy Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'American Express Bank, FSB.' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'American Express Centurion Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'AMERICAN EXPRESS CENTURION BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Arvest Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'ARVEST BANK GROUP, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Associated Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'ASSOCIATED BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Astoria Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'ASTORIA BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Astoria Federal Savings and Loan Association' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Banco Popular de Puerto Rico' ){All.Ranked.Banks.Top.100$Company[i] <- 'BANCO POPULAR DE PUERTO RICO'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Banco Popular North America' ){All.Ranked.Banks.Top.100$Company[i] <- 'BANCO POPULAR NORTH AMERICA'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'BancorpSouth Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'BANCORPSOUTH BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of America' ){All.Ranked.Banks.Top.100$Company[i] <- 'BANK OF AMERICA, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of America California' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of America Oregon' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of America, Rhode Island' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of Hope' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Bank of the Ozarks' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'BankUnited' ){All.Ranked.Banks.Top.100$Company[i] <- 'BankUnited, N.A.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Barclays Bank Delaware' ){All.Ranked.Banks.Top.100$Company[i] <- 'BARCLAYS BANK DELAWARE'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Beal Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'BMO Harris Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'BMO HARRIS BANK NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'BNY Mellon' ){All.Ranked.Banks.Top.100$Company[i] <- 'BNY MELLON, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'BOKF' ){All.Ranked.Banks.Top.100$Company[i] <- 'BOK FINANCIAL CORP'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Branch Banking and Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'California Bank & Trust' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Capital One' ){All.Ranked.Banks.Top.100$Company[i] <- 'CAPITAL ONE FINANCIAL CORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Capital One Bank (USA)' ){All.Ranked.Banks.Top.100$Company[i] <- 'CAPITAL ONE FINANCIAL CORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Capmark Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Cathay Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'CATHAY BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Charles Schwab Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'CHARLES SCHWAB CORPORATION, THE'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Chase Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- 'JPMORGAN CHASE & CO.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Chemical Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'CHEMICAL FINANCIAL CORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'CIT Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'CIT BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'CIT BANK' ){All.Ranked.Banks.Top.100$Company[i] <- 'CIT BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Citibank' ){All.Ranked.Banks.Top.100$Company[i] <- 'CITIBANK, N.A.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Citizens Bank of Pennsylvania' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Comerica Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'Comerica'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Compass Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'COMPASS MORTGAGE, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Deutsche Bank Trust Company Americas' ){All.Ranked.Banks.Top.100$Company[i] <- 'Deutsche Bank'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Discover Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'DISCOVER BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'E*TRADE Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'E*TRADE BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'East West Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'EAST WEST BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Emigrant Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'EverBank' ){All.Ranked.Banks.Top.100$Company[i] <- 'EVERBANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'FIA Card Services' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Fifth Third Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIFTH THIRD FINANCIAL CORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First-Citizens Bank & Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST CITIZENS BANCSHARES, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First Hawaiian Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST HAWAIIAN, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First National Bank of Omaha' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST NATIONAL BANK OF OMAHA'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First National Bank of Pennsylvania' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST NATIONAL BANK OF PENNSYLVANIA'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First Niagara Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST NIAGARA FINANCIAL GROUP, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First Republic Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST REPUBLIC BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'First Tennessee Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRST TENNESSEE BANK NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Firstbank of Puerto Rico' ){All.Ranked.Banks.Top.100$Company[i] <- 'FirstBank of Puerto Rico'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'FirstBank Puerto Rico' ){All.Ranked.Banks.Top.100$Company[i] <- 'FirstBank of Puerto Rico'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Firstmerit Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FIRSTMERIT BANK, N.A.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Flagstar Bank, FSB' ){All.Ranked.Banks.Top.100$Company[i] <- 'FLAGSTAR BANK, FSB'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Frost Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FROST BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'GE Capital Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'GE Capital Financial Inc.' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'GE Capital Retail Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Goldman Sachs Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- 'GOLDMAN SACHS BANK USA'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Great Western Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'HSBC Bank Nevada' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'HSBC Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Hudson City Savings Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Iberiabank' ){All.Ranked.Banks.Top.100$Company[i] <- 'IBERIABANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'ING Bank, fsb' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Investors Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'INVESTORS BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'JPMorgan Bank and Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'JPMorgan Chase Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'JPMORGAN CHASE & CO.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'KeyBank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Manufacturers and Traders Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'MB Financial Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'MB FINANCIAL, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Metlife Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Morgan Stanley Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'MORGAN STANLEY & CO. LLC'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Morgan Stanley Private Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'MUFG Union Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'MUFG UNION BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'New York Community Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'NEW YORK COMMUNITY BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Old National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'OLD NATIONAL BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'OneWest Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'OneWest Bank, FSB' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Pacific Western Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'PACIFIC WESTERN BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == "People's United Bank" ){All.Ranked.Banks.Top.100$Company[i] <- "PEOPLE'S UNITED BANK, NATIONAL ASSOCIATION"}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'PNC Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'PNC Bank N.A.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Rabobank' ){All.Ranked.Banks.Top.100$Company[i] <- 'RABOBANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Raymond James Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'RAYMOND JAMES BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'RBC Bank (USA)' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'RBS Citizens' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Regions Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'REGIONS BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Sallie Mae Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Santander Bank, N.A.' ){All.Ranked.Banks.Top.100$Company[i] <- 'SANTANDER BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Scottrade Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'SCOTTRADE BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Silicon Valley Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'State Farm Bank, F.S.B.' ){All.Ranked.Banks.Top.100$Company[i] <- 'STATE FARM BANK, FSB'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Sterling National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Sterling Savings Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'SunTrust Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'SUNTRUST BANKS, INC.'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Susquehanna Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Synchrony Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'SYNCHRONY BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Synovus Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'Synovus Bank'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'TCF National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'TCF NATIONAL BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'TD Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'TD Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- 'TD BANK US HOLDING COMPANY'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Texas Capital Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The Bank of New York Mellon' ){All.Ranked.Banks.Top.100$Company[i] <- 'BNY MELLON, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The Bank of New York Mellon Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The Frost National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'FROST BANK'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The Huntington National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'HUNTINGTON NATIONAL BANK, THE'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The Northern Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- 'NORTHERN TRUST COMPANY, THE'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'The PrivateBank and Trust Company' ){All.Ranked.Banks.Top.100$Company[i] <- 'PRIVATEBANK AND TRUST COMPANY, THE'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Third Federal Savings and Loan Association of Cleveland' ){All.Ranked.Banks.Top.100$Company[i] <- 'THIRD FEDERAL SAVINGS & LOAN ASSOCIATION OF CLEVELAND'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'U.S. Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'UBS Bank USA' ){All.Ranked.Banks.Top.100$Company[i] <- 'UBS BANK USA'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'UMB Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'UMB BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Umpqua Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'UMPQUA HOLDINGS CORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'USAA Federal Savings Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'USAA Savings Bank' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Washington Federal' ){All.Ranked.Banks.Top.100$Company[i] <- 'WASHINGTON FEDERAL, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Webster Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'WEBSTER BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Wells Fargo Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'WELLS FARGO BANK, NATIONAL ASSOCIATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Wells Fargo Bank Northwest' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Wells Fargo Bank South Central' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Western Alliance Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'WESTERN ALLIANCE BANCORPORATION'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Whitney Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'Whitney Bank'}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'ZB' ){All.Ranked.Banks.Top.100$Company[i] <- ''}
else if (All.Ranked.Banks.Top.100$Bank.Name[i] == 'Zions First National Bank' ){All.Ranked.Banks.Top.100$Company[i] <- 'ZIONS BANCORPORATION'}
}

#Keep only the bank names from the ranked banks that have matches in the complaints data
All.Ranked.Banks.Top.100 <- All.Ranked.Banks.Top.100[All.Ranked.Banks.Top.100$Company != '',]
str(All.Ranked.Banks.Top.100)
str(All.Ranked.Banks.Names.Top.100)

write.csv(All.Ranked.Banks.Top.100, file = "all_ranked_banks_top100.csv")

#Next we have to merge the ranked banks data onto the complaints data by bank name and quarter

Merged.Banks.1 <- inner_join(consumer.complaint.counts.mortgage.2,
                        subset(All.Ranked.Banks.Top.100, 
                        select = c("Total.Equity.Capital","quarter.year","Company","Bank.Name")),
                        by = c("Company","quarter.year"))

#Transform units of equity capital from dollars to billions of dollars
Merged.Banks.1$Total.Equity.Capital.2 <- Merged.Banks.1$Total.Equity.Capital/10^9
str(Merged.Banks.1)#1669 obs

length(unique(Merged.Banks.1$Company)) #75 banks

range(Merged.Banks.1$Total.Equity.Capital.2)
#range: 0.192743 to 211.586000
write.csv(Merged.Banks.1, file="Merged.Banks.csv")

ggplot(data = Merged.Banks.1, aes(Merged.Banks.1$Total.Equity.Capital.2))+
  geom_histogram(bins = 20,col = "black",fill = "green")+
  labs(title = "Distribution of equity capital for all banks and quarters")+
  labs(x = "Equity Capital($ billions)",y = "Count")

#Explore distribution of complaint count 
ggplot(data = Merged.Banks.1, aes(1+Merged.Banks.1$complaint.count))+
  geom_histogram(bins = 20,col = "black",fill = "green")+
  labs(title = "Distribution of raw complaint count for all banks and quarters")+
  labs(x = "Complaint count", y = "Count")

#Adjust complaint counts here

Mortgage.Model.1 <- lm(Merged.Banks.1$complaint.count ~ Merged.Banks.1$Total.Equity.Capital.2)
summary(Mortgage.Model.1)
cor(Merged.Banks.1$complaint.count, Merged.Banks.1$Total.Equity.Capital.2) #0.76

#There is a strong, positive, linear relationship between quarterly equity capital and average 
#complaints per quarter 

#Normalize complaint counts to remove effect of bank size (as measured by equity capital)
#on counts.

Merged.Banks.1$adj.complaint.count <- (Merged.Banks.1$complaint.count - Mortgage.Model.1$coefficients[1])/
                                   Merged.Banks.1$Total.Equity.Capital.2

summary(Merged.Banks.1$adj.complaint.count)
#  Min.     1st Qu.  Median  Mean     3rd Qu. Max. 
#  0.5801   3.8180   7.5160  10.3300  11.9700 118.0000 

write.csv(Merged.Banks.1, file="Merged.Banks.csv")
cor(Merged.Banks.1$adj.complaint.count, Merged.Banks.1$Total.Equity.Capital.2)

#cor= -0.12
#Looks like we've removed the correlation between complaint count and equity capital

Mortgage.Model.2 <- lm(Merged.Banks.1$adj.complaint.count ~ Merged.Banks.1$Total.Equity.Capital.2)
summary(Mortgage.Model.2)

#R^2 and adjusted R^2 are practically zero. So hardly any variation in adjusted complaint count is due to
#the linear relationship between adjusted complaint count and equity capital. 
#between quarterly equity capital and quarterly average complaint counts. 

#Now find distributions of adjusted complaint counts for each bank separately, so we can 
#see if the distributions look like Poisson. 
bank.names <- unique(Merged.Banks.1$Company)
pvalues <- data.frame(Bank.Name = character(), quarter.year = character(), stringsAsFactors = FALSE)
plot.new()
for (Bank.Name in bank.names){
  hist(Merged.Banks.1[Merged.Banks.1$Company == Bank.Name,]$adj.complaint.count,
       main = Bank.Name, xlab = "Adj. complaint count")
  gf <-  goodfit(Merged.Banks.1[Merged.Banks.1$Company == Bank.Name,]$adj.complaint.count,
                 type = "poisson", method = "MinChisq")
  newrow <- data.frame(Bank.Name = c(Bank.Name), p = c(summary(gf)[3]), stringsAsFactors = FALSE)
  pvalues <- rbind(pvalues, newrow)
}

#In summary.goodfit(gf) : Chi-squared approximation may be incorrect
write.csv(pvalues, "p_values for testing Poisson dist of adj complaint counts.csv")

#################################################################################
# Load numbers of employees
# We will try number of employees per bank, normalized by bank size, to predict
# number of quarterly complaints
#################################################################################

Ranked.Banks.By.Num.Employees_12_31_11 <- read.csv('RankedBanksByNumEmpls 12-31-11.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_12 <- read.csv('RankedBanksByNumEmpls 3-31-12.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_6_30_12 <- read.csv('RankedBanksByNumEmpls 6-30-12.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_9_30_12 <- read.csv('RankedBanksByNumEmpls 9-30-12.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_12_31_12 <- read.csv('RankedBanksByNumEmpls 12-31-12.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_13 <- read.csv('RankedBanksByNumEmpls 3-31-13.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_6_30_13 <- read.csv('RankedBanksByNumEmpls 6-30-13.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_9_30_13 <- read.csv('RankedBanksByNumEmpls 9-30-13.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_12_31_13 <- read.csv('RankedBanksByNumEmpls 12-31-13.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_14 <- read.csv('RankedBanksByNumEmpls 3-31-14.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_6_30_14 <- read.csv('RankedBanksByNumEmpls 6-30-14.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_9_30_14 <- read.csv('RankedBanksByNumEmpls 9-30-14.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_12_31_14 <- read.csv('RankedBanksByNumEmpls 12-31-14.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_15 <- read.csv('RankedBanksByNumEmpls 3-31-15.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_6_30_15 <- read.csv('RankedBanksByNumEmpls 6-30-15.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_9_30_15 <- read.csv('RankedBanksByNumEmpls 9-30-15.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_12_31_15 <- read.csv('RankedBanksByNumEmpls 12-31-15.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_16 <- read.csv('RankedBanksByNumEmpls 3-31-16.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_6_30_16 <- read.csv('RankedBanksByNumEmpls 6-30-16.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_9_30_16 <- read.csv('RankedBanksByNumEmpls 9-30-16.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_12_31_16 <- read.csv('RankedBanksByNumEmpls 12-31-16.csv',stringsAsFactors=FALSE)
Ranked.Banks.By.Num.Employees_3_31_17 <- read.csv('RankedBanksByNumEmpls 3-31-17.csv',stringsAsFactors=FALSE)

#Stack the ranked banks into one set
All.Ranked.Banks.By.Num.Employees  <-  rbind(Ranked.Banks.By.Num.Employees_12_31_11,
                          Ranked.Banks.By.Num.Employees_3_31_12,
                          Ranked.Banks.By.Num.Employees_6_30_12,
                          Ranked.Banks.By.Num.Employees_9_30_12,
                          Ranked.Banks.By.Num.Employees_12_31_12,
                          Ranked.Banks.By.Num.Employees_3_31_13,
                          Ranked.Banks.By.Num.Employees_6_30_13,
                          Ranked.Banks.By.Num.Employees_9_30_13,
                          Ranked.Banks.By.Num.Employees_12_31_13,
                          Ranked.Banks.By.Num.Employees_3_31_14,
                          Ranked.Banks.By.Num.Employees_6_30_14,
                          Ranked.Banks.By.Num.Employees_9_30_14,
                          Ranked.Banks.By.Num.Employees_12_31_14,
                          Ranked.Banks.By.Num.Employees_3_31_15,
                          Ranked.Banks.By.Num.Employees_6_30_15,
                          Ranked.Banks.By.Num.Employees_9_30_15,
                          Ranked.Banks.By.Num.Employees_12_31_15,
                          Ranked.Banks.By.Num.Employees_3_31_16,
                          Ranked.Banks.By.Num.Employees_6_30_16,
                          Ranked.Banks.By.Num.Employees_9_30_16,
                          Ranked.Banks.By.Num.Employees_12_31_16,
                          Ranked.Banks.By.Num.Employees_3_31_17)

#Get year and quarter from the dates
All.Ranked.Banks.By.Num.Employees$quarter.year <- paste(quarters(as.Date(All.Ranked.Banks.By.Num.Employees$Date,
                                              format="%m/%d/%Y")),
                                              substr(All.Ranked.Banks.By.Num.Employees$Date,
                                              nchar(All.Ranked.Banks.By.Num.Employees$Date)-3,
                                              nchar(All.Ranked.Banks.By.Num.Employees$Date)),sep=" ")

str(All.Ranked.Banks.By.Num.Employees)
#146811 obs.

#Get rid of the commas in employee counts and convert the counts into integers.
All.Ranked.Banks.By.Num.Employees$Number.of.Employees <- 
  as.integer(gsub(",", "", All.Ranked.Banks.By.Num.Employees$Number.of.Employees))
 
summary(All.Ranked.Banks.By.Num.Employees$Number.of.Employees)

#Employee counts are very skewed to the right. And why would any of the counts be zero?

#Keep only the employees number records where the bank is in the top 100 by equity capital. 
Bank.Num.Empls.Top.100 <- All.Ranked.Banks.By.Num.Employees[All.Ranked.Banks.By.Num.Employees$Bank.Name %in%
                                        Unique.Banks.From.Ranked.Banks.Top.100,
                                        c("Number.of.Employees","Bank.Name","quarter.year")]

length(unique(Bank.Num.Empls.Top.100$Bank.Name))#127 banks (the correct number)

#Make sure the banks ranked by number of employees have just one employee count per quarter
Number.Of.Ranks.2 <- Bank.Num.Empls.Top.100 %>%
  group_by(quarter.year, Bank.Name) %>%
  summarise(Number.Of.Ranks = n_distinct(Number.of.Employees))
summary(Number.Of.Ranks.2$Number.Of.Ranks)
#They do 

#Merge employee counts onto rest of bank data
Merged.Banks.2 <- inner_join(Merged.Banks.1, subset(Bank.Num.Empls.Top.100,
                        select=c("Number.of.Employees", "Bank.Name","quarter.year"),
                        by=c("Bank.Name", "quarter.year")))
summary(Merged.Banks.2$Number.of.Employees)
sum(is.na(Merged.Banks.2$Number.of.Employees)) #Good, no missings

cor(Merged.Banks.2$Number.of.Employees, Merged.Banks.2$Total.Equity.Capital.2)
#95% 

Mortgage.Model.3 <- lm(Merged.Banks.2$Number.of.Employees ~ Merged.Banks.2$Total.Equity.Capital.2)
summary(Mortgage.Model.3)

Merged.Banks.2$Adj.Number.of.Employees <- (Merged.Banks.2$Number.of.Employees - Mortgage.Model.3$coefficients[1])/
                                         Merged.Banks.2$Total.Equity.Capital.2

cor(Merged.Banks.2$Adj.Number.of.Employees, Merged.Banks.2$Total.Equity.Capital.2) #-0.12
cor(Merged.Banks.2$Adj.Number.of.Employees, Merged.Banks.2$adj.complaint.count) #0.38; positive sign doesn't make sense

#####################################################################################################
# Let's look at return on equity (ROE). Return on equity measures a corporation's profitability by 
# revealing how much profit a company generates with the money shareholders have invested.
# Return on Equity = Net Income/Shareholder's Equity
#####################################################################################################

#Load return on equity
Ranked.Banks.By.ROE_12_31_11 <-   read.csv('RankedBanksByROE 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_12 <- read.csv('RankedBanksByROE 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_6_30_12 <- read.csv('RankedBanksByROE 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_9_30_12 <- read.csv('RankedBanksByROE 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_12_31_12 <- read.csv('RankedBanksByROE 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_13 <- read.csv('RankedBanksByROE 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_6_30_13 <- read.csv('RankedBanksByROE 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_9_30_13 <- read.csv('RankedBanksByROE 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_12_31_13 <- read.csv('RankedBanksByROE 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_14 <- read.csv('RankedBanksByROE 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_6_30_14 <- read.csv('RankedBanksByROE 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_9_30_14 <- read.csv('RankedBanksByROE 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_12_31_14 <- read.csv('RankedBanksByROE 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_15 <- read.csv('RankedBanksByROE 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_6_30_15 <- read.csv('RankedBanksByROE 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_9_30_15 <- read.csv('RankedBanksByROE 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_12_31_15 <- read.csv('RankedBanksByROE 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_16 <- read.csv('RankedBanksByROE 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_6_30_16 <- read.csv('RankedBanksByROE 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_9_30_16 <- read.csv('RankedBanksByROE 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_12_31_16 <- read.csv('RankedBanksByROE 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.ROE_3_31_17 <- read.csv('RankedBanksByROE 3-31-17.csv', stringsAsFactors = FALSE)

Ranked.Banks.By.ROE_3_31_12 <- subset(Ranked.Banks.By.ROE_3_31_12, select = -X)
str(Ranked.Banks.By.ROE_3_31_12)

#Stack the ranked banks into one set
All.Ranked.Banks.By.ROE  <-  rbind(Ranked.Banks.By.ROE_12_31_11,
                            Ranked.Banks.By.ROE_3_31_12,
                            Ranked.Banks.By.ROE_6_30_12,
                            Ranked.Banks.By.ROE_9_30_12,
                            Ranked.Banks.By.ROE_12_31_12,
                            Ranked.Banks.By.ROE_3_31_13,
                            Ranked.Banks.By.ROE_6_30_13,
                            Ranked.Banks.By.ROE_9_30_13,
                            Ranked.Banks.By.ROE_12_31_13,
                            Ranked.Banks.By.ROE_3_31_14,
                            Ranked.Banks.By.ROE_6_30_14,
                            Ranked.Banks.By.ROE_9_30_14,
                            Ranked.Banks.By.ROE_12_31_14,
                            Ranked.Banks.By.ROE_3_31_15,
                            Ranked.Banks.By.ROE_6_30_15,
                            Ranked.Banks.By.ROE_9_30_15,
                            Ranked.Banks.By.ROE_12_31_15,
                            Ranked.Banks.By.ROE_3_31_16,
                            Ranked.Banks.By.ROE_6_30_16,
                            Ranked.Banks.By.ROE_9_30_16,
                            Ranked.Banks.By.ROE_12_31_16,
                            Ranked.Banks.By.ROE_3_31_17)

All.Ranked.Banks.By.ROE$quarter.year <- paste(quarters(as.Date(All.Ranked.Banks.By.ROE$Date, format="%m/%d/%Y")),
                                      substr(All.Ranked.Banks.By.ROE$Date, nchar(All.Ranked.Banks.By.ROE$Date)-3, 
                                             nchar(All.Ranked.Banks.By.ROE$Date)),
                                      sep=" ")
All.Ranked.Banks.By.ROE$Quarterly.Return.on.Equity <- 
     as.numeric(gsub("%","",All.Ranked.Banks.By.ROE$Quarterly.Return.on.Equity))/100

str(All.Ranked.Banks.By.ROE)

#Keep only the ROE records where the bank is in the top 100 by equity capital. 
All.Ranked.Banks.By.ROE.Top.100 <- All.Ranked.Banks.By.ROE[All.Ranked.Banks.By.ROE$Bank.Name %in%
                                    Unique.Banks.From.Ranked.Banks.Top.100,
                                    c("Quarterly.Return.on.Equity","Bank.Name","quarter.year")]

length(unique(All.Ranked.Banks.By.ROE.Top.100$Bank.Name))#127 banks (the correct number)

#Make sure the banks ranked by ROE have just one value of ROE per quarter
Number.Of.Ranks3 <- All.Ranked.Banks.By.ROE.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(Number.Of.Ranks=n_distinct(Quarterly.Return.on.Equity))
summary(Number.Of.Ranks3$Number.Of.Ranks)
#They do :-)

#Merge bank ROEs onto rest of bank data
Merged.Banks.3 <- inner_join(Merged.Banks.2,subset(All.Ranked.Banks.By.ROE.Top.100,
                                              select=c("Quarterly.Return.on.Equity","Bank.Name","quarter.year"),
                                              by=c("Bank.Name","quarter.year")))
str(Merged.Banks.3)#Quarterly return on equity is a character string (e.g. "11%")
summary(Merged.Banks.3$Quarterly.Return.on.Equity)
sum(is.na(Merged.Banks.3$Number.of.Employees)) #Good, no missings

cor(Merged.Banks.3$adj.complaint.count, Merged.Banks.3$Quarterly.Return.on.Equity)
# -0.007089496 

#Next load banks ranked by total amount (secured by 1-4 family residential properties) past due 30-89 days
#Source: 
#http://www.usbanklocations.com/bank-rank/secured-by-1-4-family-residential-properties-past-due-30-89-days---pastdue3089days--pdasub_a-p3reres.html?d=2011-12-31
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_11 <- read.csv('RankedByPastDue30_89Days 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_12 <- read.csv('RankedByPastDue30_89Days 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_12 <- read.csv('RankedByPastDue30_89Days 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_12 <- read.csv('RankedByPastDue30_89Days 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_12 <- read.csv('RankedByPastDue30_89Days 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_13 <- read.csv('RankedByPastDue30_89Days 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_13 <- read.csv('RankedByPastDue30_89Days 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_13 <- read.csv('RankedByPastDue30_89Days 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_13 <- read.csv('RankedByPastDue30_89Days 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_14 <- read.csv('RankedByPastDue30_89Days 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_14 <- read.csv('RankedByPastDue30_89Days 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_14 <- read.csv('RankedByPastDue30_89Days 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_14 <- read.csv('RankedByPastDue30_89Days 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_15 <- read.csv('RankedByPastDue30_89Days 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_15 <- read.csv('RankedByPastDue30_89Days 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_15 <- read.csv('RankedByPastDue30_89Days 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_15 <- read.csv('RankedByPastDue30_89Days 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_16 <- read.csv('RankedByPastDue30_89Days 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_16 <- read.csv('RankedByPastDue30_89Days 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_16 <- read.csv('RankedByPastDue30_89Days 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_16 <- read.csv('RankedByPastDue30_89Days 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_17 <- read.csv('RankedByPastDue30_89Days 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Past.Due.30.89.Days.Amt <- rbind(Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_11,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_12 ,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_12,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_12,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_12,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_13,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_13,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_13,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_13,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_14,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_14,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_14,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_14,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_15,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_15,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_15,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_15,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_16,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_6_30_16,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_9_30_16,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_12_31_16,
Ranked.Banks.By.Past.Due.30.89.Days.Amt_3_31_17)

#Create quarters and years from dates
All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Date, nchar(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Date)-3, 
                                                nchar(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Date)),
                                                sep=" ")

names(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt)[names(All.Ranked.Banks.By.Past.Due.30.89.Days.Amt) == 
        'Secured.by.1.4.family.residential.properties..past.due.30...89.days'] <- 'Amt.Past.Due.30.89.Days'

#Have to change amounts from strings to numbers
All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Amt.Past.Due.30.89.Days <- 
    as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Amt.Past.Due.30.89.Days))

#Keep only the past due amount records where the bank is in the top 100 by equity capital. 
Banks.Past.Due.30.89.Days.Top.100 <- 
  All.Ranked.Banks.By.Past.Due.30.89.Days.Amt[All.Ranked.Banks.By.Past.Due.30.89.Days.Amt$Bank.Name %in%
                                   Unique.Banks.From.Ranked.Banks.Top.100,
                                   c("Amt.Past.Due.30.89.Days","Bank.Name","quarter.year")]
sum(is.na(Banks.Past.Due.30.89.Days.Top.100$Amt.Past.Due.30.89.Days))
#0 missings  :-)
length(unique(Banks.Past.Due.30.89.Days.Top.100$Bank.Name))#127 banks (the correct number)

#Make sure the banks ranked by past due 30-89 days have just one value of amount past due per quarter
Number.Of.Ranks4 <- Banks.Past.Due.30.89.Days.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(Number.Of.Ranks=n_distinct(Amt.Past.Due.30.89.Days))
summary(Number.Of.Ranks4$Number.Of.Ranks)
#They do :-)

#Merge amounts past due 30-89 days onto rest of bank data
Merged.Banks.4 <- inner_join(Merged.Banks.3,subset(Banks.Past.Due.30.89.Days.Top.100,
                              select=c("Amt.Past.Due.30.89.Days","Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.4)

#Next load banks ranked by total amount (secured by 1-4 family residential properties) past due 90+ days
#Source:
#http://www.usbanklocations.com/bank-rank/secured-by-1-4-family-residential-properties-past-due-90-days---pastdue90days--pdasub_b-p9reres.html

Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_11 <- read.csv('RankedByPastDue90orMoreDays 12-31-11.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_12 <- read.csv('RankedByPastDue90orMoreDays 3-31-12.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_12 <- read.csv('RankedByPastDue90orMoreDays 6-30-12.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_12 <- read.csv('RankedByPastDue90orMoreDays 9-30-12.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_12 <- read.csv('RankedByPastDue90orMoreDays 12-31-12.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_13 <- read.csv('RankedByPastDue90orMoreDays 3-31-13.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_13 <- read.csv('RankedByPastDue90orMoreDays 6-30-13.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_13 <- read.csv('RankedByPastDue90orMoreDays 9-30-13.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_13 <- read.csv('RankedByPastDue90orMoreDays 12-31-13.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_14 <- read.csv('RankedByPastDue90orMoreDays 3-31-14.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_14 <- read.csv('RankedByPastDue90orMoreDays 6-30-14.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_14 <- read.csv('RankedByPastDue90orMoreDays 9-30-14.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_14 <- read.csv('RankedByPastDue90orMoreDays 12-31-14.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_15 <- read.csv('RankedByPastDue90orMoreDays 3-31-15.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_15 <- read.csv('RankedByPastDue90orMoreDays 6-30-15.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_15 <- read.csv('RankedByPastDue90orMoreDays 9-30-15.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_15 <- read.csv('RankedByPastDue90orMoreDays 12-31-15.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_16 <- read.csv('RankedByPastDue90orMoreDays 3-31-16.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_16 <- read.csv('RankedByPastDue90orMoreDays 6-30-16.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_16 <- read.csv('RankedByPastDue90orMoreDays 9-30-16.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_16 <- read.csv('RankedByPastDue90orMoreDays 12-31-16.csv', stringsAsFactors = FALSE)
Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_17 <- read.csv('RankedByPastDue90orMoreDays 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Past.Due.90.Or.More.Days <- rbind(Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_11,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_12 ,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_12,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_12,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_12,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_13,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_13,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_13,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_13,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_14,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_14,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_14,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_14,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_15,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_15,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_15,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_15,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_16,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_6_30_16,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_9_30_16,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_12_31_16,
                                             Banks.Ranked.By.Past.Due.90.Plus.Days.Amt_3_31_17)

All.Ranked.Banks.By.Past.Due.90.Or.More.Days$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Date, nchar(All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Date)-3, 
               nchar(All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Date)),
        sep=" ")

names(All.Ranked.Banks.By.Past.Due.90.Or.More.Days)[names(All.Ranked.Banks.By.Past.Due.90.Or.More.Days) == 
                                    'Secured.by.1.4.family.residential.properties..past.due.90..days'] <- 
                                    'Amt.Past.Due.90.Or.More.Days'

#Have to change amounts from strings to numbers
All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Amt.Past.Due.90.Or.More.Days <- 
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Amt.Past.Due.90.Or.More.Days))

Banks.Past.Due.90.Or.More.Days.Top.100 <- 
  All.Ranked.Banks.By.Past.Due.90.Or.More.Days[All.Ranked.Banks.By.Past.Due.90.Or.More.Days$Bank.Name %in%
                                        Unique.Banks.From.Ranked.Banks.Top.100,
                                      c("Amt.Past.Due.90.Or.More.Days","Bank.Name","quarter.year")]

sum(is.na(Banks.Past.Due.90.Or.More.Days.Top.100$Amt.Past.Due.90.Or.More.Days))
length(unique(Banks.Past.Due.90.Or.More.Days.Top.100$Bank.Name)) #127    :-)

#Make sure the banks ranked by past due 90+ days have just one value of amount past due per quarter
Number.Of.Ranks.5 <- Banks.Past.Due.90.Or.More.Days.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(Number.Of.Ranks=n_distinct(Amt.Past.Due.90.Or.More.Days))
summary(Number.Of.Ranks.5$Number.Of.Ranks)
#They do

#Merge the 90+ day past due amounts onto merged banks
Merged.Banks.5 <- inner_join(Merged.Banks.4, subset(Banks.Past.Due.90.Or.More.Days.Top.100,
                                               select=c("Amt.Past.Due.90.Or.More.Days","Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.5)
summary(Merged.Banks.5$Amt.Past.Due.90.Or.More.Days)
sum(is.na(Merged.Banks.5$Amt.Past.Due.90.Or.More.Days)) #Good, no missings

#Next we normalize past due amounts by total mortgage loans; see
#http://www.usbanklocations.com/bank-rank/1-4-family-residential-loans---familynetloansleases--nlllt_a-lnreres.html

#Load total mortgage loans
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_11 <- read.csv('Banks ranked by 1-4 family res loans 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_12 <- read.csv('Banks ranked by 1-4 family res loans 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_6_30_12 <- read.csv('Banks ranked by 1-4 family res loans 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_9_30_12 <- read.csv('Banks ranked by 1-4 family res loans 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_12 <- read.csv('Banks ranked by 1-4 family res loans 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_13 <- read.csv('Banks ranked by 1-4 family res loans 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_6_30_13 <- read.csv('Banks ranked by 1-4 family res loans 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_9_30_13 <- read.csv('Banks ranked by 1-4 family res loans 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_13 <- read.csv('Banks ranked by 1-4 family res loans 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_14 <- read.csv('Banks ranked by 1-4 family res loans 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_6_30_14 <- read.csv('Banks ranked by 1-4 family res loans 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_9_30_14 <- read.csv('Banks ranked by 1-4 family res loans 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_14 <- read.csv('Banks ranked by 1-4 family res loans 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_15 <- read.csv('Banks ranked by 1-4 family res loans 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_6_30_15 <- read.csv('Banks ranked by 1-4 family res loans 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_9_30_15 <- read.csv('Banks ranked by 1-4 family res loans 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_15 <- read.csv('Banks ranked by 1-4 family res loans 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_16 <- read.csv('Banks ranked by 1-4 family res loans 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_6_30_16 <- read.csv('Banks ranked by 1-4 family res loans 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_9_30_16 <- read.csv('Banks ranked by 1-4 family res loans 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_12_31_16 <- read.csv('Banks ranked by 1-4 family res loans 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.1.4.Family.Res.Loans_3_31_17 <- read.csv('Banks ranked by 1-4 family res loans 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.1.4.Family.Res.Loans <- rbind(Ranked.Banks.By.1.4.Family.Res.Loans_12_31_11,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_12 ,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_6_30_12,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_9_30_12,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_12_31_12,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_13,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_6_30_13,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_9_30_13,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_12_31_13,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_14,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_6_30_14,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_9_30_14,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_12_31_14,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_15,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_6_30_15,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_9_30_15,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_12_31_15,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_16,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_6_30_16,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_9_30_16,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_12_31_16,
                                          Ranked.Banks.By.1.4.Family.Res.Loans_3_31_17)

All.Ranked.Banks.By.1.4.Family.Res.Loans$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.1.4.Family.Res.Loans$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.1.4.Family.Res.Loans$Date, nchar(All.Ranked.Banks.By.1.4.Family.Res.Loans$Date)-3, 
               nchar(All.Ranked.Banks.By.1.4.Family.Res.Loans$Date)),
        sep=" ")

str(All.Ranked.Banks.By.1.4.Family.Res.Loans)

names(All.Ranked.Banks.By.1.4.Family.Res.Loans)[names(All.Ranked.Banks.By.1.4.Family.Res.Loans) == 
        'X1.4.family.residential.loans'] <- 'Residential.Loans'

str(All.Ranked.Banks.By.1.4.Family.Res.Loans)

#Have to change amounts from strings to numbers
All.Ranked.Banks.By.1.4.Family.Res.Loans$Residential.Loans <- 
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.1.4.Family.Res.Loans$Residential.Loans))

All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100 <- 
  All.Ranked.Banks.By.1.4.Family.Res.Loans[All.Ranked.Banks.By.1.4.Family.Res.Loans$Bank.Name %in%
                                     Unique.Banks.From.Ranked.Banks.Top.100,
                                   c("Residential.Loans","Bank.Name","quarter.year")]

sum(is.na(All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100$Residential.Loans)) #none missing 

length(unique(All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100$Bank.Name)) #127
str(All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100)

#Make sure that there is just one loan amount per quarter
Number.Of.Ranks.6 <- All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(Number.Of.Ranks=n_distinct(Residential.Loans))
summary(Number.Of.Ranks.6$Number.Of.Ranks)
#there is 

Merged.Banks.6 <- inner_join(Merged.Banks.5,subset(All.Ranked.Banks.By.1.4.Family.Res.Loans.Top.100,
                                               select=c("Residential.Loans","Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.6)
nrow(Merged.Banks.6[Merged.Banks.6$Residential.Loans == 0,])
#101 zeros
sum(Merged.Banks.6$Residential.Loans == 0 & Merged.Banks.6$Amt.Past.Due.30.89.Days != 0)
#0
#So Amounts past due 30-89 days are always zero if residential loan amount is 0 (as it should be)

Merged.Banks.6$adj.Amt.Past.Due.30.89.Days <- 
  Merged.Banks.6$Amt.Past.Due.30.89.Days/(1 + Merged.Banks.6$Residential.Loans)

#Do different normalization below

Mortgage.Model.4 <- lm(Merged.Banks.6$Amt.Past.Due.30.89.Days ~ Merged.Banks.6$Total.Equity.Capital.2)
summary(Mortgage.Model.4)
Merged.Banks.6$adj.Amt.Past.Due.30.89.Days2 <- (Merged.Banks.6$Amt.Past.Due.30.89.Days - Mortgage.Model.4$coefficients[1])/
  Merged.Banks.6$Total.Equity.Capital.2

cor(Merged.Banks.6$adj.Amt.Past.Due.30.89.Days, Merged.Banks.6$adj.complaint.count)
#0.1597859
cor(Merged.Banks.6$adj.Amt.Past.Due.30.89.Days2, Merged.Banks.6$adj.complaint.count)
#0.680953!!

sum(Merged.Banks.6$Residential.Loans == 0 & Merged.Banks.6$Amt.Past.Due.90.Or.More.Days != 0)
#So amount past due 90+ days is 0 whenever loan amount is 0 (as it should be)

Merged.Banks.6$adj.Amt.Past.Due.90.Or.More.Days <- 
  Merged.Banks.6$Amt.Past.Due.90.Or.More.Days/(1 + Merged.Banks.6$Residential.Loans)

Mortgage.Model.5 <- lm(Merged.Banks.6$Amt.Past.Due.90.Or.More.Days ~ Merged.Banks.6$Total.Equity.Capital.2)
summary(Mortgage.Model.5)

Merged.Banks.6$adj.Amt.Past.Due.90.Or.More.Days2 <- (Merged.Banks.6$Amt.Past.Due.90.Or.More.Days - Mortgage.Model.5$coefficients[1])/
                                            Merged.Banks.6$Total.Equity.Capital.2
cor(Merged.Banks.6$adj.Amt.Past.Due.90.Or.More.Days, Merged.Banks.6$adj.complaint.count)
#0.1278357
cor(Merged.Banks.6$adj.Amt.Past.Due.90.Or.More.Days2, Merged.Banks.6$adj.complaint.count)
#0.5420805

#Loans Restructured in Troubled Debt Restructurings
#Not in Compliance with their modified terms
#Banks Ranked by 1-4 Family restructured loans 30-89 Day P/D
#http://www.usbanklocations.com/bank-rank/1-4-family-restructured-loans-30-89-day-p-d---loansrestructured--nlll-p3rslnfm.html?d=2015-12-31

# A restructured loan is a new loan that replaces the outstanding balance on an older loan, and is paid over 
# a longer period, usually with a lower installment amount. Loans are commonly rescheduled to accommodate a 
# borrower in financial difficulty and, thus, to avoid a default. Also called rescheduled loan.

#Next load banks ranked by total amount of restructured loans for  past due 30-89 days
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_11 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_12 <- read.csv( 'RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_12 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_12 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_12 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_13 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_13 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_13 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_13 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_14 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_14 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_14 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_14 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_15 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_15 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_15 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_15 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_16 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_16 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_16 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_16 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_17 <- read.csv('RankedByFamily1_4  Restruct Loans 30_89 days PD 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D <- rbind(Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_11,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_12 ,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_12,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_12,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_12,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_13,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_13,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_13,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_13,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_14,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_14,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_14,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_14,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_15,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_15,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_15,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_15,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_16,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_6_30_16,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_9_30_16,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_12_31_16,
                                         Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D_3_31_17)

All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Date, 
               nchar(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Date)-3, 
               nchar(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Date)),
        sep=" ")

names(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D)[names(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D) == 
          'X1.4.Family.restructured.loans.30.89.Day.P.D'] <- 'Restruct.Loans.30.89.Days.P.D'
str(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D)

#Have to change amounts from strings to numbers
All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Restruct.Loans.30.89.Days.P.D <- 
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Restruct.Loans.30.89.Days.P.D))

head(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D)

#Keep only the restructured debt amount records where the bank is in the top 100 by equity capital. 
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D.Top.100 <- 
  All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D[All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Bank.Name %in%
                                     Unique.Banks.From.Ranked.Banks.Top.100,
                                     c("Restruct.Loans.30.89.Days.P.D","Bank.Name","quarter.year")]

sum(is.na(All.Ranked.Banks.By.Family.1.4.Restruct.Loans.30.89.Days.P.D$Restruct.Loans.30.89.Days.P.D))
#none missing

length(unique(Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D.Top.100$Bank.Name ))#127 banks (the correct number)

#Make sure the banks ranked by restructured debt amount  have just one value of amount per quarter
Number.Of.Ranks.7 <- Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D.Top.100 %>%
  group_by(quarter.year, Bank.Name) %>%
  summarise(numranks=n_distinct(Restruct.Loans.30.89.Days.P.D))

summary(Number.Of.Ranks.7$numranks)
#They do
#Merge amounts of restructured debt onto rest of bank data
Merged.Banks.7 <- inner_join(Merged.Banks.6, subset(Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.30.89.Days.P.D.Top.100,
                                               select=c("Restruct.Loans.30.89.Days.P.D",
                                                        "Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))

#Normalize total restructured loan amounts by total loan amounts
sum(Merged.Banks.6$Residential.Loans == 0 & Merged.Banks.6$Restruct.Loans.30.89.Days.P.D != 0) #0 :-)

Merged.Banks.7$adj.Restruct.Loans.80.89.Days.P.D <- 
  Merged.Banks.7$Restruct.Loans.30.89.Days.P.D/(1 + Merged.Banks.7$Residential.Loans)

#Normalize total restructured loan amounts by equity capital 
Mortgage.Model.6 <- lm(Merged.Banks.7$Restruct.Loans.30.89.Days.P.D ~ Merged.Banks.7$Total.Equity.Capital.2)
summary(Mortgage.Model.6)

Merged.Banks.7$adj.Restruct.Loans.80.89.Days.P.D2 <- 
  (Merged.Banks.7$Restruct.Loans.30.89.Days.P.D - Mortgage.Model.6$coefficients[1])/
                                                                Merged.Banks.7$Total.Equity.Capital.2

cor(Merged.Banks.7$adj.Restruct.Loans.80.89.Days.P.D, Merged.Banks.7$adj.complaint.count)
#0.01629929
cor(Merged.Banks.7$adj.Restruct.Loans.80.89.Days.P.D2, Merged.Banks.7$adj.complaint.count)
#0.6404902

#Banks Ranked by 1-4 Family restructured loans 90+ Day P/D
#http://www.usbanklocations.com/bank-rank/restructured-loans-1-4-family-90-days-p-d---loansrestructured--nlll-p9rslnfm.html?d=2017-03-31
#Next load banks ranked by total amount of restructured loans for  past due 90+ days
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_11 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_12 <- read.csv( 'RankedByFamily1_4  Restruct Loans 90 days PD 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_12 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_12 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_12 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_13 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_13 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_13 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_13 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_14 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_14 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_14 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_14 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_15 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_15 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_15 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_15 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_16 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_16 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_16 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_16 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_17 <- read.csv('RankedByFamily1_4  Restruct Loans 90 days PD 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD <- rbind(Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_11,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_12 ,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_12,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_12,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_12,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_13,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_13,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_13,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_13,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_14,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_14,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_14,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_14,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_15,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_15,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_15,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_15,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_16,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_6_30_16,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_9_30_16,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_12_31_16,
                                                           Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.P.D_3_31_17)

All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Date, 
               nchar(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Date)-3, 
               nchar(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Date)),
        sep=" ")

head(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD)

names(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD)[names(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD) == 
'Restructured.Loans..1.4.Family.90..Days.P.D'] <- 'Restruct.Loans.90.Or.More.Days.P.D'

str(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD)
#Have to change amounts from strings to numbers
All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Restruct.Loans.90.Or.More.Days.P.D <- 
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Restruct.Loans.90.Or.More.Days.P.D))

head(All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD)

#Keep only the restructured debt amount records where the bank is in the top 100 by equity capital. 
Ranked.Banks.By.Family.1.4.Restruct.Loans.90.Or.More.Days.P.D.Top.100 <- 
  All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD[All.Ranked.Banks.By.Family.1.4.Res.Restruct.Loans.90.Or.More.Days.PD$Bank.Name %in%
                                                      Unique.Banks.From.Ranked.Banks.Top.100,
                                                    c("Restruct.Loans.90.Or.More.Days.P.D","Bank.Name","quarter.year")]
sum(is.na(Ranked.Banks.By.Family.1.4.Restruct.Loans.90.Or.More.Days.P.D.Top.100$Restruct.Loans.90.Or.More.Days.P.D))
#none missing

length(unique(Ranked.Banks.By.Family.1.4.Restruct.Loans.90.Or.More.Days.P.D.Top.100$Bank.Name ))#127 banks (the correct number)

#Make sure the banks ranked by restructured debt amount  have just one value of amount per quarter
Number.Of.Ranks.8 <- Ranked.Banks.By.Family.1.4.Restruct.Loans.90.Or.More.Days.P.D.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks=n_distinct(Restruct.Loans.90.Or.More.Days.P.D))
summary(Number.Of.Ranks.8$numranks)
#They do
#Merge amounts of restructured debt onto rest of bank data
Merged.Banks.8 <- inner_join(Merged.Banks.7,subset(Ranked.Banks.By.Family.1.4.Restruct.Loans.90.Or.More.Days.P.D.Top.100,
                                               select=c("Restruct.Loans.90.Or.More.Days.P.D",
                                                        "Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))

#Normalize total restructured loan amounts by equity capital 
Mortgage.Model.7 <- lm(Merged.Banks.8$Restruct.Loans.90.Or.More.Days.P.D ~ Merged.Banks.8$Total.Equity.Capital.2)
summary(Mortgage.Model.7)

Merged.Banks.8$adj.Restruct.Loans.90.Or.More.Days.P.D <- 
  (Merged.Banks.8$Restruct.Loans.90.Or.More.Days.P.D - Mortgage.Model.7$coefficients[1])/
  Merged.Banks.8$Total.Equity.Capital.2

cor(Merged.Banks.8$adj.X1.4.Family.restruct.loans.90.Day.P.D, Merged.Banks.8$adj.complaint.count)
#0.6166729

###########################################################################################################
#Next load banks ranked by charge-off percentages for loans for 1-4 family dwellings
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_11 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_12 <- read.csv( 'Pct Net Loans Charged Off 1-4 Family Res 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_12 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_12 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_12 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_13 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_13 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_13 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_13 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_14 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_14 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_14 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_14 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_15 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_15 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_15 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_15 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_16 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_16 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_16 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_16 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_17 <- read.csv('Pct Net Loans Charged Off 1-4 Family Res 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan <- rbind(Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_11,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_12 ,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_12,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_12,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_12,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_13,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_13,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_13,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_13,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_14,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_14,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_14,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_14,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_15,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_15,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_15,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_15,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_16,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_6_30_16,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_9_30_16,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_12_31_16,
                                                           Ranked.Banks.By.Pct.Net.Loans.Charged.Off.1.4.Family.Res.Loan_3_31_17)

All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Date, 
               nchar(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Date)-3, 
               nchar(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Date)),
        sep=" ")
str(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan)
names(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan)[names(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan) == 
      'Percentage.Net.Loans.Charged.off..1.4.family.residential'] <- 'Pct.Loans.Charged.Off'
str(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan)
#Remove "%" signs from percentages charged off
All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Pct.Loans.Charged.Off <-
  as.numeric(gsub("%","",All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Pct.Loans.Charged.Off))

head(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan)

#Keep only the banks that are ranked in the top 100 by equity capital
All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan.Top.100 <- 
  All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan[All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan$Bank.Name %in%
                                      Unique.Banks.From.Ranked.Banks.Top.100,
                                      c("Pct.Loans.Charged.Off","Bank.Name","quarter.year")]

length(unique(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan.Top.100$Bank.Name )) #Only 118 instead of 127

#Make sure the banks ranked by charge-off percentage have just one value of amount per quarter
Number.Of.Ranks.9 <- All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks=n_distinct(Pct.Loans.Charged.Off))
summary(Number.Of.Ranks.9$numranks)
#They do
Merged.Banks.9 <- inner_join(Merged.Banks.8, subset(All.Ranked.Banks.By.Pct.Loans.Charged.Off.1.4.Family.Res.Loan.Top.100,
                                               select=c("Pct.Loans.Charged.Off",
                                                        "Bank.Name","quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.9)
cor(Merged.Banks.9$adj.complaint.count, Merged.Banks.9$Pct.Loans.Charged.Off)
#0.1301609

#Now work on goodwill. Goodwill is defined by excess of purchase price over fair market value acquired under the purchase method
#of accounting. We will compute the ratio of Goodwill to fair market value and see how this is related
#to complaint count. We will compute fair market value as assets(excluding goodwill) minus liabilities. 
Ranked.Banks.By.Goodwill_12_31_11 <- read.csv('Banks ranked by goodwill 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_12 <- read.csv( 'Banks ranked by goodwill 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_6_30_12 <- read.csv('Banks ranked by goodwill 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_9_30_12 <- read.csv('Banks ranked by goodwill 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_12_31_12 <- read.csv('Banks ranked by goodwill 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_13 <- read.csv('Banks ranked by goodwill 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_6_30_13 <- read.csv('Banks ranked by goodwill 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_9_30_13 <- read.csv('Banks ranked by goodwill 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_12_31_13 <- read.csv('Banks ranked by goodwill 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_14 <- read.csv('Banks ranked by goodwill 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_6_30_14 <- read.csv('Banks ranked by goodwill 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_9_30_14 <- read.csv('Banks ranked by goodwill 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_12_31_14 <- read.csv('Banks ranked by goodwill 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_15 <- read.csv('Banks ranked by goodwill 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_6_30_15 <- read.csv('Banks ranked by goodwill 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_9_30_15 <- read.csv('Banks ranked by goodwill 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_12_31_15 <- read.csv('Banks ranked by goodwill 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_16 <- read.csv('Banks ranked by goodwill 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_6_30_16 <- read.csv('Banks ranked by goodwill 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_9_30_16 <- read.csv('Banks ranked by goodwill 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_12_31_16 <- read.csv('Banks ranked by goodwill 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Goodwill_3_31_17 <- read.csv('Banks ranked by goodwill 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Goodwill <- rbind(Ranked.Banks.By.Goodwill_12_31_11,
                                  Ranked.Banks.By.Goodwill_3_31_12 ,
                                  Ranked.Banks.By.Goodwill_6_30_12,
                                  Ranked.Banks.By.Goodwill_9_30_12,
                                  Ranked.Banks.By.Goodwill_12_31_12,
                                  Ranked.Banks.By.Goodwill_3_31_13,
                                  Ranked.Banks.By.Goodwill_6_30_13,
                                  Ranked.Banks.By.Goodwill_9_30_13,
                                  Ranked.Banks.By.Goodwill_12_31_13,
                                  Ranked.Banks.By.Goodwill_3_31_14,
                                  Ranked.Banks.By.Goodwill_6_30_14,
                                  Ranked.Banks.By.Goodwill_9_30_14,
                                  Ranked.Banks.By.Goodwill_12_31_14,
                                  Ranked.Banks.By.Goodwill_3_31_15,
                                  Ranked.Banks.By.Goodwill_6_30_15,
                                  Ranked.Banks.By.Goodwill_9_30_15,
                                  Ranked.Banks.By.Goodwill_12_31_15,
                                  Ranked.Banks.By.Goodwill_3_31_16,
                                  Ranked.Banks.By.Goodwill_6_30_16,
                                  Ranked.Banks.By.Goodwill_9_30_16,
                                  Ranked.Banks.By.Goodwill_12_31_16,
                                  Ranked.Banks.By.Goodwill_3_31_17)

All.Ranked.Banks.By.Goodwill$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Goodwill$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Goodwill$Date, 
               nchar(All.Ranked.Banks.By.Goodwill$Date)-3, 
               nchar(All.Ranked.Banks.By.Goodwill$Date)),
        sep=" ")


str(All.Ranked.Banks.By.Goodwill)

#Remove commas and dollar signs from goodwill amounts
All.Ranked.Banks.By.Goodwill$Goodwill <- as.numeric(gsub("[$, ]","",All.Ranked.Banks.By.Goodwill$Goodwill))
str(All.Ranked.Banks.By.Goodwill)

#Keep only the banks that are ranked in the top 100 by equity capital
All.Ranked.Banks.By.Goodwill.Top.100 <- 
  All.Ranked.Banks.By.Goodwill[All.Ranked.Banks.By.Goodwill$Bank.Name %in%
                                                   Unique.Banks.From.Ranked.Banks.Top.100,
                                                 c("Goodwill","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.Goodwill.Top.100$Bank.Name))) #127  :-)

#Make sure the banks ranked by goodwill have just one value of goodwill per quarter
Number.Of.Ranks10 <- All.Ranked.Banks.By.Goodwill.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks=n_distinct(Goodwill))
summary(Number.Of.Ranks10$numranks)
#They do

#Now load banks ranked by assets
Ranked.Banks.By.Total.Assets_12_31_11 <- read.csv('Banks ranked by total assets 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_12 <- read.csv( 'Banks ranked by total assets 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_6_30_12 <- read.csv('Banks ranked by total assets 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_9_30_12 <- read.csv('Banks ranked by total assets 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_12_31_12 <- read.csv('Banks ranked by total assets 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_13 <- read.csv('Banks ranked by total assets 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_6_30_13 <- read.csv('Banks ranked by total assets 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_9_30_13 <- read.csv('Banks ranked by total assets 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_12_31_13 <- read.csv('Banks ranked by total assets 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_14 <- read.csv('Banks ranked by total assets 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_6_30_14 <- read.csv('Banks ranked by total assets 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_9_30_14 <- read.csv('Banks ranked by total assets 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_12_31_14 <- read.csv('Banks ranked by total assets 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_15 <- read.csv('Banks ranked by total assets 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_6_30_15 <- read.csv('Banks ranked by total assets 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_9_30_15 <- read.csv('Banks ranked by total assets 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_12_31_15 <- read.csv('Banks ranked by total assets 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_16 <- read.csv('Banks ranked by total assets 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_6_30_16 <- read.csv('Banks ranked by total assets 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_9_30_16 <- read.csv('Banks ranked by total assets 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_12_31_16 <- read.csv('Banks ranked by total assets 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Assets_3_31_17 <- read.csv('Banks ranked by total assets 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Assets <- rbind(Ranked.Banks.By.Total.Assets_12_31_11,
                                  Ranked.Banks.By.Total.Assets_3_31_12 ,
                                  Ranked.Banks.By.Total.Assets_6_30_12,
                                  Ranked.Banks.By.Total.Assets_9_30_12,
                                  Ranked.Banks.By.Total.Assets_12_31_12,
                                  Ranked.Banks.By.Total.Assets_3_31_13,
                                  Ranked.Banks.By.Total.Assets_6_30_13,
                                  Ranked.Banks.By.Total.Assets_9_30_13,
                                  Ranked.Banks.By.Total.Assets_12_31_13,
                                  Ranked.Banks.By.Total.Assets_3_31_14,
                                  Ranked.Banks.By.Total.Assets_6_30_14,
                                  Ranked.Banks.By.Total.Assets_9_30_14,
                                  Ranked.Banks.By.Total.Assets_12_31_14,
                                  Ranked.Banks.By.Total.Assets_3_31_15,
                                  Ranked.Banks.By.Total.Assets_6_30_15,
                                  Ranked.Banks.By.Total.Assets_9_30_15,
                                  Ranked.Banks.By.Total.Assets_12_31_15,
                                  Ranked.Banks.By.Total.Assets_3_31_16,
                                  Ranked.Banks.By.Total.Assets_6_30_16,
                                  Ranked.Banks.By.Total.Assets_9_30_16,
                                  Ranked.Banks.By.Total.Assets_12_31_16,
                                  Ranked.Banks.By.Total.Assets_3_31_17)

All.Ranked.Banks.By.Assets$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Assets$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Assets$Date, 
               nchar(All.Ranked.Banks.By.Assets$Date)-3, 
               nchar(All.Ranked.Banks.By.Assets$Date)),
        sep=" ")

str(All.Ranked.Banks.By.Assets)

#Remove commas and dollar signs from asset amounts
All.Ranked.Banks.By.Assets$Total.Assets <- as.numeric(gsub("[$, ]","",All.Ranked.Banks.By.Assets$Total.Assets))
str(All.Ranked.Banks.By.Assets)

#Keep only the banks that are ranked in the top 100 by equity capital
All.Ranked.Banks.By.Assets.Top.100 <- 
  All.Ranked.Banks.By.Assets[All.Ranked.Banks.By.Assets$Bank.Name %in%
                             Unique.Banks.From.Ranked.Banks.Top.100,
                           c("Total.Assets","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.Assets.Top.100$Bank.Name))) #127  :-)

#Make sure the banks ranked by assets have just one value of assets per quarter
Number.Of.Ranks.10 <- All.Ranked.Banks.By.Assets.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks=n_distinct(Total.Assets))
summary(Number.Of.Ranks.10$numranks)

#Now load banks ranked by liabilities
Ranked.Banks.By.Total.Liab_12_31_11 <- read.csv('Banks ranked by total liab 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_12 <- read.csv( 'Banks ranked by total liab 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_6_30_12 <- read.csv('Banks ranked by total liab 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_9_30_12 <- read.csv('Banks ranked by total liab 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_12_31_12 <- read.csv('Banks ranked by total liab 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_13 <- read.csv('Banks ranked by total liab 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_6_30_13 <- read.csv('Banks ranked by total liab 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_9_30_13 <- read.csv('Banks ranked by total liab 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_12_31_13 <- read.csv('Banks ranked by total liab 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_14 <- read.csv('Banks ranked by total liab 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_6_30_14 <- read.csv('Banks ranked by total liab 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_9_30_14 <- read.csv('Banks ranked by total liab 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_12_31_14 <- read.csv('Banks ranked by total liab 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_15 <- read.csv('Banks ranked by total liab 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_6_30_15 <- read.csv('Banks ranked by total liab 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_9_30_15 <- read.csv('Banks ranked by total liab 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_12_31_15 <- read.csv('Banks ranked by total liab 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_16 <- read.csv('Banks ranked by total liab 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_6_30_16 <- read.csv('Banks ranked by total liab 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_9_30_16 <- read.csv('Banks ranked by total liab 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_12_31_16 <- read.csv('Banks ranked by total liab 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Total.Liab_3_31_17 <- read.csv('Banks ranked by total liab 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Liab <- rbind(Ranked.Banks.By.Total.Liab_12_31_11,
                                Ranked.Banks.By.Total.Liab_3_31_12 ,
                                Ranked.Banks.By.Total.Liab_6_30_12,
                                Ranked.Banks.By.Total.Liab_9_30_12,
                                Ranked.Banks.By.Total.Liab_12_31_12,
                                Ranked.Banks.By.Total.Liab_3_31_13,
                                Ranked.Banks.By.Total.Liab_6_30_13,
                                Ranked.Banks.By.Total.Liab_9_30_13,
                                Ranked.Banks.By.Total.Liab_12_31_13,
                                Ranked.Banks.By.Total.Liab_3_31_14,
                                Ranked.Banks.By.Total.Liab_6_30_14,
                                Ranked.Banks.By.Total.Liab_9_30_14,
                                Ranked.Banks.By.Total.Liab_12_31_14,
                                Ranked.Banks.By.Total.Liab_3_31_15,
                                Ranked.Banks.By.Total.Liab_6_30_15,
                                Ranked.Banks.By.Total.Liab_9_30_15,
                                Ranked.Banks.By.Total.Liab_12_31_15,
                                Ranked.Banks.By.Total.Liab_3_31_16,
                                Ranked.Banks.By.Total.Liab_6_30_16,
                                Ranked.Banks.By.Total.Liab_9_30_16,
                                Ranked.Banks.By.Total.Liab_12_31_16,
                                Ranked.Banks.By.Total.Liab_3_31_17)

All.Ranked.Banks.By.Liab$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Liab$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Liab$Date, 
               nchar(All.Ranked.Banks.By.Liab$Date)-3, 
               nchar(All.Ranked.Banks.By.Liab$Date)),
        sep=" ")

str(All.Ranked.Banks.By.Liab)

#Remove commas and dollar signs from asset amounts
All.Ranked.Banks.By.Liab$Total.Liabilities <- as.numeric(gsub("[$, ]","",All.Ranked.Banks.By.Liab$Total.Liabilities))
str(All.Ranked.Banks.By.Liab)

#Keep only the banks that are ranked in the top 100 by equity capital
All.Ranked.Banks.By.Liab.Top.100 <- 
  All.Ranked.Banks.By.Liab[All.Ranked.Banks.By.Liab$Bank.Name %in%
                           Unique.Banks.From.Ranked.Banks.Top.100,
                         c("Total.Liabilities","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.Liab.Top.100$Bank.Name))) #127

#Now make sure that the banks ranked by liabilities have just one liability value per quarter
Number.Of.Ranks.11 <- All.Ranked.Banks.By.Liab.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks=n_distinct(Total.Liabilities))
summary(Number.Of.Ranks.11$numranks)
#They do

#Now join goodwill, assets and liabilities to data

Merged.Banks.10 <- inner_join(Merged.Banks.9,subset(All.Ranked.Banks.By.Goodwill.Top.100, select=c("Goodwill", "Bank.Name", "quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.10)
cor(Merged.Banks.10$adj.complaint.count, Merged.Banks.10$Goodwill)
#-0.1037175. Good, we would expect complaints to decrease as goodwill increases. 

Merged.Banks.11 <- inner_join(Merged.Banks.10,subset(All.Ranked.Banks.By.Assets.Top.100, select=c("Total.Assets", "Bank.Name", "quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.11)
cor(Merged.Banks.11$adj.complaint.count, Merged.Banks.11$Total.Assets)
#-0.1260982

Merged.Banks.12 <- inner_join(Merged.Banks.11, subset(All.Ranked.Banks.By.Liab.Top.100, select=c("Total.Liabilities", "Bank.Name", "quarter.year"),
                                               by=c("Bank.Name","quarter.year")))
str(Merged.Banks.12)
cor(Merged.Banks.12$adj.complaint.count, Merged.Banks.12$Total.Liabilities)
#-0.1247867

Merged.Banks.12$Goodwill.Pct <- 
  Merged.Banks.12$Goodwill/(Merged.Banks.12$Total.Assets - Merged.Banks.12$Goodwill - Merged.Banks.12$Total.Liabilities)
cor(Merged.Banks.12$Goodwill.Pct, Merged.Banks.12$adj.complaint.count)
#-0.2120867 

#See how well complaint count predicts Goodwill percent
GW.model <- lm(adj.complaint.count ~ Goodwill.Pct, data = Merged.Banks.12)
summary(GW.model)
#Not too well. Adjusted R^2 = 0.04437. 

#Next look at return on assets
#Return on assets (ROA) is an indicator of how profitable a company is relative to its total assets. 
#ROA gives an idea as to how efficient management is at using its assets to generate earnings. Calculated 
#by dividing a company's annual earnings by its total assets, ROA is displayed as a percentage. Sometimes 
#this is referred to as "return on investment".

#The formula for return on assets is:
#Return On Assets (ROA) = Net income/Total assets

Ranked.Banks.By.Rtn.On.Assets_12_31_11 <- read.csv('Banks ranked by rtn on assets 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_12 <- read.csv( 'Banks ranked by rtn on assets 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_6_30_12 <- read.csv('Banks ranked by rtn on assets 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_9_30_12 <- read.csv('Banks ranked by rtn on assets 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_12_31_12 <- read.csv('Banks ranked by rtn on assets 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_13 <- read.csv('Banks ranked by rtn on assets 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_6_30_13 <- read.csv('Banks ranked by rtn on assets 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_9_30_13 <- read.csv('Banks ranked by rtn on assets 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_12_31_13 <- read.csv('Banks ranked by rtn on assets 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_14 <- read.csv('Banks ranked by rtn on assets 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_6_30_14 <- read.csv('Banks ranked by rtn on assets 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_9_30_14 <- read.csv('Banks ranked by rtn on assets 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_12_31_14 <- read.csv('Banks ranked by rtn on assets 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_15 <- read.csv('Banks ranked by rtn on assets 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_6_30_15 <- read.csv('Banks ranked by rtn on assets 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_9_30_15 <- read.csv('Banks ranked by rtn on assets 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_12_31_15 <- read.csv('Banks ranked by rtn on assets 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_16 <- read.csv('Banks ranked by rtn on assets 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_6_30_16 <- read.csv('Banks ranked by rtn on assets 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_9_30_16 <- read.csv('Banks ranked by rtn on assets 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_12_31_16 <- read.csv('Banks ranked by rtn on assets 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.Rtn.On.Assets_3_31_17 <- read.csv('Banks ranked by rtn on assets 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Rtn.On.Assets <- rbind(Ranked.Banks.By.Rtn.On.Assets_12_31_11,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_12 ,
                              Ranked.Banks.By.Rtn.On.Assets_6_30_12,
                              Ranked.Banks.By.Rtn.On.Assets_9_30_12,
                              Ranked.Banks.By.Rtn.On.Assets_12_31_12,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_13,
                              Ranked.Banks.By.Rtn.On.Assets_6_30_13,
                              Ranked.Banks.By.Rtn.On.Assets_9_30_13,
                              Ranked.Banks.By.Rtn.On.Assets_12_31_13,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_14,
                              Ranked.Banks.By.Rtn.On.Assets_6_30_14,
                              Ranked.Banks.By.Rtn.On.Assets_9_30_14,
                              Ranked.Banks.By.Rtn.On.Assets_12_31_14,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_15,
                              Ranked.Banks.By.Rtn.On.Assets_6_30_15,
                              Ranked.Banks.By.Rtn.On.Assets_9_30_15,
                              Ranked.Banks.By.Rtn.On.Assets_12_31_15,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_16,
                              Ranked.Banks.By.Rtn.On.Assets_6_30_16,
                              Ranked.Banks.By.Rtn.On.Assets_9_30_16,
                              Ranked.Banks.By.Rtn.On.Assets_12_31_16,
                              Ranked.Banks.By.Rtn.On.Assets_3_31_17)

All.Ranked.Banks.By.Rtn.On.Assets$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Rtn.On.Assets$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Rtn.On.Assets$Date, 
               nchar(All.Ranked.Banks.By.Rtn.On.Assets$Date)-3, 
               nchar(All.Ranked.Banks.By.Rtn.On.Assets$Date)),
        sep=" ")

str(All.Ranked.Banks.By.Rtn.On.Assets)

#Remove "%" signs from return on assets
All.Ranked.Banks.By.Rtn.On.Assets$Return.on.Assets <-
  as.numeric(gsub("%", "", All.Ranked.Banks.By.Rtn.On.Assets$Return.on.Assets))/100

str(All.Ranked.Banks.By.Rtn.On.Assets)

All.Ranked.Banks.By.Rtn.On.Assets.Top.100 <- 
  All.Ranked.Banks.By.Rtn.On.Assets[All.Ranked.Banks.By.Rtn.On.Assets$Bank.Name %in%
                         Unique.Banks.From.Ranked.Banks.Top.100,
                       c("Return.on.Assets","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.Rtn.On.Assets.Top.100$Bank.Name))) #127

#Now make sure that the banks ranked by return on assets have just one value of ROA per quarter
Number.Of.Ranks.13 <- All.Ranked.Banks.By.Rtn.On.Assets.Top.100 %>%
  group_by(quarter.year,Bank.Name) %>%
  summarise(numranks = n_distinct(Return.on.Assets))
summary(Number.Of.Ranks.13$numranks)
#They do

Merged.Banks.13 <- inner_join(Merged.Banks.12,
                            subset(All.Ranked.Banks.By.Rtn.On.Assets.Top.100, select = c("Return.on.Assets", "Bank.Name", "quarter.year"),
                            by=c("Bank.Name","quarter.year")))
str(Merged.Banks.13)

cor(Merged.Banks.13$adj.complaint.count, Merged.Banks.13$Return.on.Assets)
#-0.05736703
##########################################################################################################################
# Next merge on nonaccrual loans
#
#  Source: http://www.usbanklocations.com/bank-rank/nonaccrual-restructured-1-4-family-loans---loansrestructured--nlll-narslnfm.html
#
# What is  a 'Nonaccrual Loan'?
#
#A nonaccrual loan is a nonperforming loan that is not generating its stated interest rate because of 
#nonpayment from the borrower. Nonaccrual loans are more likely to default, meaning that the lender will not 
#receive its principal and interest unless the lender has adequate collateral to cover the loan. Because 
#these loans can have interest credited only when the borrower makes a payment, the interest on a nonaccrual 
#loan is recorded as earned income.
#BREAKING DOWN 'Nonaccrual Loan'
#After 90 days of nonpayment, a loan is placed on nonaccrual status, and interest stops accumulating. The 
#bank classifies the loan as substandard and reports the change to the credit reporting agencies, which 
#lowers the borrower's credit score. The lender changes its allowance for the potential loan loss, sets aside
#a reserve to protect the bank's financial interests and may take legal action against the borrower.
#
#Restructuring a Loan
#
#After entering nonaccrual status, the borrower typically works with the lender in determining a plan for 
#paying off the debt. After reviewing the borrower's income and expense status, the lender may create a 
#troubled debt restructure (TDR). The TDR may erase part of the loan's principal or interest payments, lower 
#the interest rate, allow interest-only payments or modify repayment terms in other ways. Lower debt payments 
#may be made until the borrower's monetary circumstances improve. The lender may recoup at least its principal 
#rather than losing its entire investment.
##########################################################################################################################
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_11 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-11.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_12 <- read.csv( 'Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_12 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 6-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_12 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 9-30-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_12 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-12.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_13 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_13 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 6-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_13 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 9-30-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_13 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-13.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_14 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_14 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 6-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_14 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 9-30-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_14 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-14.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_15 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_15 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 6-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_15 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 9-30-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_15 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-15.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_16 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_16 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 6-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_16 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 9-30-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_16 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 12-31-16.csv', stringsAsFactors = FALSE)
Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_17 <- read.csv('Ranked by Nonaccrual Restruct 1-4 Family Loans 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans <- rbind(Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_11,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_12 ,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_12,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_12,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_12,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_13,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_13,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_13,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_13,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_14,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_14,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_14,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_14,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_15,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_15,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_15,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_15,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_16,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_6_30_16,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_9_30_16,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_12_31_16,
                                     Ranked.Banks.By.NonAcc.Restruct.1.4.Family.Res.Loans_3_31_17)

All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Date, 
               nchar(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Date)-3, 
               nchar(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Date)),
        sep=" ")

str(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans)

#Remove "$" and ","
All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$NonaccRestruct1_4FamilyLoans <-
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Nonaccrual.Restructured.1.4.Family.Loans))
str(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans)

All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans.Top.100 <- 
  All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans[All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans$Bank.Name %in%
                                Unique.Banks.From.Ranked.Banks.Top.100,
                              c("NonaccRestruct1_4FamilyLoans","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans.Top.100$Bank.Name))) #127

#Now make sure that the banks ranked by nonaccrual assets have just one value of nonaccrual assets per quarter
Number.Of.Ranks.14 <- All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans.Top.100 %>%
  group_by(quarter.year, Bank.Name) %>%
  summarise(numranks = n_distinct(NonaccRestruct1_4FamilyLoans))
summary(Number.Of.Ranks.14$numranks)
#They do

Merged.Banks.14 <- inner_join(Merged.Banks.13,
                            subset(All.Ranked.Banks.By.Non.Accr.Restruct.1.4.Family.Res.Loans.Top.100, 
                                   select=c("NonaccRestruct1_4FamilyLoans", "Bank.Name", "quarter.year"),
                                   by=c("Bank.Name","quarter.year")))
#Adjust (normalize) amount of nonaccrual loans

Mortgage.Model.8 <- lm(Merged.Banks.14$NonaccRestruct1_4FamilyLoans ~ Merged.Banks.14$Total.Equity.Capital.2)
summary(Mortgage.Model.8)

Merged.Banks.14$adj.Nonaccr.Restr.Loans <- 
  (Merged.Banks.14$NonaccRestruct1_4FamilyLoans - Mortgage.Model.8$coefficients[1])/
  Merged.Banks.14$Total.Equity.Capital.2
str(Merged.Banks.14)
cor(Merged.Banks.14$adj.complaint.count, Merged.Banks.14$adj.Nonaccr.Restr.Loans)
#0.71795

####################################################################################################################################################
# Next merge on credit card loans
# Source: http://www.usbanklocations.com/bank-rank/credit-card-loans---loansleases--nll-lncrcd.html
####################################################################################################################################################
Banks.ranked.by.cc.loans_12_31_11 <- read.csv('Ranked by cc loans 12-31-11.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_12 <- read.csv( 'Ranked by cc loans 3-31-12.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_6_30_12 <- read.csv('Ranked by cc loans 6-30-12.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_9_30_12 <- read.csv('Ranked by cc loans 9-30-12.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_12_31_12 <- read.csv('Ranked by cc loans 12-31-12.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_13 <- read.csv('Ranked by cc loans 3-31-13.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_6_30_13 <- read.csv('Ranked by cc loans 6-30-13.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_9_30_13 <- read.csv('Ranked by cc loans 9-30-13.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_12_31_13 <- read.csv('Ranked by cc loans 12-31-13.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_14 <- read.csv('Ranked by cc loans 3-31-14.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_6_30_14 <- read.csv('Ranked by cc loans 6-30-14.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_9_30_14 <- read.csv('Ranked by cc loans 9-30-14.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_12_31_14 <- read.csv('Ranked by cc loans 12-31-14.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_15 <- read.csv('Ranked by cc loans 3-31-15.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_6_30_15 <- read.csv('Ranked by cc loans 6-30-15.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_9_30_15 <- read.csv('Ranked by cc loans 9-30-15.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_12_31_15 <- read.csv('Ranked by cc loans 12-31-15.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_16 <- read.csv('Ranked by cc loans 3-31-16.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_6_30_16 <- read.csv('Ranked by cc loans 6-30-16.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_9_30_16 <- read.csv('Ranked by cc loans 9-30-16.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_12_31_16 <- read.csv('Ranked by cc loans 12-31-16.csv', stringsAsFactors = FALSE)
Banks.ranked.by.cc.loans_3_31_17 <- read.csv('Ranked by cc loans 3-31-17.csv', stringsAsFactors = FALSE)

All.Ranked.Banks.By.CC.Loans <- rbind(Banks.ranked.by.cc.loans_12_31_11,
                                                 Banks.ranked.by.cc.loans_3_31_12 ,
                                                 Banks.ranked.by.cc.loans_6_30_12,
                                                 Banks.ranked.by.cc.loans_9_30_12,
                                                 Banks.ranked.by.cc.loans_12_31_12,
                                                 Banks.ranked.by.cc.loans_3_31_13,
                                                 Banks.ranked.by.cc.loans_6_30_13,
                                                 Banks.ranked.by.cc.loans_9_30_13,
                                                 Banks.ranked.by.cc.loans_12_31_13,
                                                 Banks.ranked.by.cc.loans_3_31_14,
                                                 Banks.ranked.by.cc.loans_6_30_14,
                                                 Banks.ranked.by.cc.loans_9_30_14,
                                                 Banks.ranked.by.cc.loans_12_31_14,
                                                 Banks.ranked.by.cc.loans_3_31_15,
                                                 Banks.ranked.by.cc.loans_6_30_15,
                                                 Banks.ranked.by.cc.loans_9_30_15,
                                                 Banks.ranked.by.cc.loans_12_31_15,
                                                 Banks.ranked.by.cc.loans_3_31_16,
                                                 Banks.ranked.by.cc.loans_6_30_16,
                                                 Banks.ranked.by.cc.loans_9_30_16,
                                                 Banks.ranked.by.cc.loans_12_31_16,
                                                 Banks.ranked.by.cc.loans_3_31_17)

All.Ranked.Banks.By.CC.Loans$quarter.year <- 
  paste(quarters(as.Date(All.Ranked.Banks.By.CC.Loans$Date, format="%m/%d/%Y")),
        substr(All.Ranked.Banks.By.CC.Loans$Date, 
               nchar(All.Ranked.Banks.By.CC.Loans$Date)-3, 
               nchar(All.Ranked.Banks.By.CC.Loans$Date)),
        sep=" ")

str(All.Ranked.Banks.By.CC.Loans)

#Remove "$" and ","
All.Ranked.Banks.By.CC.Loans$CC.Loans <-
  as.numeric(gsub("[$, ]", "", All.Ranked.Banks.By.CC.Loans$Credit.card.loans))
str(All.Ranked.Banks.By.CC.Loans$CC.Loans)

All.Ranked.Banks.By.CC.Loans.Top.100 <- 
  All.Ranked.Banks.By.CC.Loans[All.Ranked.Banks.By.CC.Loans$Bank.Name %in%
                                            Unique.Banks.From.Ranked.Banks.Top.100,
                                          c("CC.Loans","Bank.Name","quarter.year")]
(length(unique(All.Ranked.Banks.By.CC.Loans.Top.100$Bank.Name))) #127

str(All.Ranked.Banks.By.CC.Loans.Top.100)
#Now make sure that the banks ranked by nonaccrual assets have just one value of nonaccrual assets per quarter
Number.Of.Ranks.15 <- All.Ranked.Banks.By.CC.Loans.Top.100 %>%
  group_by(quarter.year, Bank.Name) %>%
  summarise(numranks = n_distinct(CC.Loans))
summary(Number.Of.Ranks.15$numranks)
#They do

Merged.Banks.15 <- inner_join(Merged.Banks.14,
                              subset(All.Ranked.Banks.By.CC.Loans.Top.100, 
                                     select=c("CC.Loans", "Bank.Name", "quarter.year"),
                                     by=c("Bank.Name","quarter.year")))

#Adjust (normalize) amount of credit card loans

Mortgage.Model.9 <- lm(Merged.Banks.15$CC.Loans ~ Merged.Banks.15$Total.Equity.Capital.2)
summary(Mortgage.Model.9)

Merged.Banks.15$adj.CC.Loans <- 
  (Merged.Banks.15$CC.Loans - Mortgage.Model.9$coefficients[1])/
  Merged.Banks.15$Total.Equity.Capital.2

str(Merged.Banks.15)
cor(Merged.Banks.15$adj.CC.Loans, Merged.Banks.15$adj.complaint.count)
#-0.05014103
######################################################################################################
#Now for the modeling part
######################################################################################################

#Make training and test sets
Merged.Banks.15$train <- (runif(nrow(Merged.Banks.15)) <= 0.7)
Merged.Banks.15$test <- !Merged.Banks.15$train
sum(Merged.Banks.15$train)
sum(Merged.Banks.15$test)
str(Merged.Banks.15)

fit1 <- glm(adj.complaint.count ~ 
                               adj.Amt.Past.Due.30.89.Days2 +
                               adj.Amt.Past.Due.90.Or.More.Days2 +
                               adj.Restruct.Loans.80.89.Days.P.D2 +
                               adj.Restruct.Loans.90.Or.More.Days.P.D +
                               Pct.Loans.Charged.Off +
                               adj.Nonaccr.Restr.Loans +
                               adj.CC.Loans ,
             data = Merged.Banks.15[Merged.Banks.15$train,], 
             family = poisson())
summary(fit1)
str(summary(fit1))
#Get rid of adj.Restruct.Loans.80.89.Days.P.D2 because p-value is too big
#Amount past due 90+ days has to go because the negative sign of the coefficient doesn't make sense. 

fit2 <- glm(adj.complaint.count ~ 
              adj.Amt.Past.Due.30.89.Days2 +
              adj.Restruct.Loans.90.Or.More.Days.P.D +
              Pct.Loans.Charged.Off +
              adj.Nonaccr.Restr.Loans +
              adj.CC.Loans ,
            data = Merged.Banks.15[Merged.Banks.15$train,], 
            family = poisson())
summary(fit2)

#Generate model predictions on test set
predictions <- predict(fit2, newdata = Merged.Banks.15[Merged.Banks.15$test,],type="response")
head(predictions)
summary(predictions)

Actuals.And.Predictions <- as.data.frame(cbind(predictions, Merged.Banks.15[Merged.Banks.15$test,]$adj.complaint.count))
str(Actuals.And.Predictions)

write.csv(Actuals.And.Predictions, file = "ActualsAndPredictions.csv")

#Divide model scores into quartiles 
(predictionQuartiles <- quantile(Actuals.And.Predictions$predictions)[c("25%","50%","75%")])
Actuals.And.Predictions$predictionQuartile <- 
   ifelse(Actuals.And.Predictions$predictions <= predictionQuartiles[1], 1, 
   ifelse(Actuals.And.Predictions$predictions <= predictionQuartiles[2], 2,
   ifelse(Actuals.And.Predictions$predictions <= predictionQuartiles[3], 3, 4)))

#Make sure that each quartile has 25% of the scores
for (i in c(1:4))
 print(sum(Actuals.And.Predictions$predictionQuartile == i)/nrow(Actuals.And.Predictions))

#Make lift chart of model predictions
(Lift_chart <- Actuals.And.Predictions %>% 
  group_by(predictionQuartile) %>% 
  summarise(meancomplaint.count = sum(V2)))

g <- ggplot(Actuals.And.Predictions, aes(predictionQuartile))
g + geom_bar(aes(weight = V2)) + 
  labs(title = "Lift chart for predicted adj. mortgage complaint counts") + 
  labs(x = "Model score quartile", y = "Sum of adjusted complaint count")

#Below we find the distribution of the multiplicative weights from the Poisson model "fit2", where
#the weights are calculated using the observations from the test data

(PD.30.89 <- summary(exp(summary(fit2)[12]$coefficients[2]*Merged.Banks.15[Merged.Banks.15$test,]$adj.Amt.Past.Due.30.89.Days2)))
(RL.90 <- summary(exp(summary(fit2)[12]$coefficients[3]*Merged.Banks.15[Merged.Banks.15$test,]$adj.Restruct.Loans.90.Or.More.Days.P.D)))
(PCO <- summary(exp(summary(fit2)[12]$coefficients[4]*Merged.Banks.15[Merged.Banks.15$test,]$Pct.Loans.Charged.Off)))
(NRL <- summary(exp(summary(fit2)[12]$coefficients[5]*Merged.Banks.15[Merged.Banks.15$test,]$adj.Nonaccr.Restr.Loans)))
(CC <- summary(exp(summary(fit2)[12]$coefficients[6]*Merged.Banks.15[Merged.Banks.15$test,]$adj.CC.Loans)))

(PD.30.89.distr <- c(PD.30.89[[1]], PD.30.89[[2]], PD.30.89[[3]], PD.30.89[[5]], PD.30.89[[6]]))
(RL.90.distr <- c(RL.90[[1]], RL.90[[2]], RL.90[[3]], RL.90[[5]], RL.90[[6]]))
(PCO.distr <- c(PCO[[1]], PCO[[2]], PCO[[3]], PCO[[5]], PCO[[6]]))
(NRL.distr <- c(NRL[[1]], NRL[[2]], NRL[[3]], NRL[[5]], NRL[[6]]))
(CC.distr <- c(CC[[1]], CC[[2]], CC[[3]], CC[[5]], CC[[6]]))

(All.weight.distr <- rbind(PD.30.89.distr, RL.90.distr, PCO.distr, NRL.distr, CC.distr))
write.csv(All.weight.distr, "AllWeightDistr.csv")

#Below we graph the effects of the predictor variables upon predicted complaint counts 
#as one predictor variable varies while the others are held fixed
start =  quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Amt.Past.Due.30.89.Days2)[2]
finish = quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Amt.Past.Due.30.89.Days2)[4]
x.axis <- seq(from = start, to = finish, by = (finish - start)/20)
y.axis <- 100*exp(summary(fit2)[12]$coefficients[2]*x.axis)
(PD.30.89.graph <- as.data.frame(cbind(x.axis, y.axis)))
ggplot(data = PD.30.89.graph, aes(x.axis, y.axis)) + geom_point() +
  theme(text = element_text(size = 30)) +
  xlab("Amount past due 30-89 days") + ylab("Predicted complaint count")

start =  quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Restruct.Loans.90.Or.More.Days.P.D)[2]
finish = quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Restruct.Loans.90.Or.More.Days.P.D)[4]
x.axis <- seq(from = start, to = finish, by = (finish - start)/20)
y.axis <- 100*exp(summary(fit2)[12]$coefficients[3]*x.axis)
(Restr.90.graph <- as.data.frame(cbind(x.axis, y.axis)))
ggplot(data = Restr.90.graph, aes(x.axis, y.axis)) + geom_point() +
  theme(text = element_text(size = 30)) +
  xlab("Restr. loans 90+ days past due") + ylab("Predicted complaint count")

start =  quantile(Merged.Banks.15[Merged.Banks.15$test, ]$Pct.Loans.Charged.Off)[2]
finish = quantile(Merged.Banks.15[Merged.Banks.15$test, ]$Pct.Loans.Charged.Off)[4]
x.axis <- seq(from = start, to = finish, by = (finish - start)/20)
y.axis <- 100*exp(summary(fit2)[12]$coefficients[4]*x.axis)
(Pct.Charged.Off.graph <- as.data.frame(cbind(x.axis, y.axis)))
ggplot(data = Pct.Charged.Off.graph , aes(x.axis, y.axis)) + geom_point() +
  theme(text = element_text(size = 30)) +
  xlab("Pct. charged off") + ylab("Predicted complaint count")

start =  quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Nonaccr.Restr.Loans)[2]
finish = quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.Nonaccr.Restr.Loans)[4]
x.axis <- seq(from = start, to = finish, by = (finish - start)/20)
y.axis <- 100 * exp(summary(fit2)[12]$coefficients[5]*x.axis)
(Nonaccr.graph <- as.data.frame(cbind(x.axis, y.axis)))
ggplot(data = Nonaccr.graph , aes(x.axis, y.axis)) + geom_point() +
  theme(text = element_text(size = 30)) +
  xlab("Nonaccrual restructured loan amount") + ylab("Predicted complaint count")

start =  quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.CC.Loans)[2]
finish = quantile(Merged.Banks.15[Merged.Banks.15$test, ]$adj.CC.Loans)[4]
x.axis <- seq(from = start, to = finish, by = (finish - start)/20)
y.axis <- 100*exp(summary(fit2)[12]$coefficients[6]*x.axis)
(CC.graph <- as.data.frame(cbind(x.axis, y.axis)))
ggplot(data = CC.graph , aes(x.axis, y.axis)) + geom_point() +
  theme(text = element_text(size = 30)) +
  xlab("Credit card loans") + ylab("Predicted complaint count")