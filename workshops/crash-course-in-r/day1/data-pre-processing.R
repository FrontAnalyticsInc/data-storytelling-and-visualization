#Data Steps
# load data from web.
# 'https://opendata.utah.gov/api/views/2evu-rdpv/rows.csv'

tmp <- read.csv('https://opendata.utah.gov/api/views/88ws-42u9/rows.csv',header=TRUE, sep=',', na.strings = c("*", "**", "#") )

# Data Preprocessing

mycolnames <- names(tmp)

## Area has one level, I am going to drop it and it's not helpful (same with st)
drop_columns <- c(which(mycolnames == 'AREA')
,which(mycolnames == 'ST')
,which(mycolnames == 'Location.1')
,which(mycolnames == 'Occupation.title')
)
## usable data copy

utah <- tmp[, -drop_columns]

rm(tmp, mycolnames, drop_columns)

#Clean the factor up
utah$Mean.Annual.Wage <- as.numeric(utah$Mean.Annual.Wage)
utah$Mean.Hourly.Wage <- as.numeric(utah$Mean.Hourly.Wage)
utah$Annual.median.wage..or.the.50th.percentile. <- as.numeric(utah$Annual.median.wage..or.the.50th.percentile.)
utah$TOT_EMP <- as.numeric(utah$TOT_EMP)
utah$Occupation.Title <- as.character(utah$Occupation.Title)
utah$industry <- as.character(substr(as.character(utah$Occupation.Code), 1,2))
utah$code <- as.character(substr(utah$Occupation.Code, 4, 7) )

utah <- utah[ , - which(names(utah) == 'Occupation.Code')]
nases <- which(is.na(utah$Mean.Annual.Wage) )

length(nases)
# Only 15, not really worth it.
# utah[nases, c(1:5,12, 16, 19,20)]
#
# utah$AnnualizedWages <- utah$Mean.Annual.Wage
# utah$AnnualizedWages[nases] <- round( utah$Mean.Hourly.Wage[nases] * 2000, -1)

utah <- utah[!is.na(utah$Mean.Annual.Wage), ]

# and to make it more concise I am dropping all that have hourly wages
(drop_columns <- which(grepl("^Hourly", names(utah), perl=TRUE) ) )

#sort by?  What?
utah  <- utah[order(utah$Annual.25th.percentile.wage), -drop_columns ]
utah$[which(utah$industry == 51 & utah$code == '0000')]
# This is the save to binary
# Rename the columns
(mycolnames <- names(utah))
colnames(utah)[1] <- "title_name"
colnames(utah)[2] <- "total_empl"
colnames(utah)[3] <- "stnd_error"
colnames(utah)[4] <- "jobs_per_K"

colnames(utah)[5] <- "loc_quotient"
colnames(utah)[6] <- "hourlywage"
colnames(utah)[7] <- "annualwage"
colnames(utah)[8] <- "mean_stnd_error"

colnames(utah)[9]  <- "wage_D1"
colnames(utah)[10] <- "wage_Q1"
colnames(utah)[11] <- "wage_Q2"
colnames(utah)[12] <- "wage_Q3"
colnames(utah)[13] <- "wage_D9"

save(utah, file =  "utah.Rdata")


#industry dataset
industry <- aggregate(cbind(total_empl,annualwage,wage_D1, wage_Q1, wage_Q2, wage_Q3, wage_D9)~industry+code+title_name, data=utah, sum, na.rm=TRUE)
industry$centered_D9 <- industry$wage_D9 - industry$wage_Q2

# Creating Lists for other portions of the app
idx_majors <- which(industry$code == "0000")
industry$industry[idx_majors]

majors   <- industry[idx_majors, ]
industry <- industry[-idx_majors,]

industry$centered_D9 <- industry$wage_D9 - industry$wage_Q2


save(industry, file =  "industry.Rdata")
save(majors, file =  "majors.Rdata")
save(idx_majors, file =  "idx_majors.Rdata")


# New Ordering




