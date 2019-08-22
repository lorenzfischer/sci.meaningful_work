#
# In this script, we try to find evidence for whether people mostly want
# "meaningful" work or whether they don't really care about aspects of
# meaning or fulfillment in their professional life. As a proxy for this
# I look at the data of the World Value Survey and, specificially, at the 
# answers given to the question:
# 
# "Here are some more aspects of a job that people say are important. 
# Please look at them and tell me which ones you personally think are 
# important in a job?"
# (used until 2004, http://www.worldvaluessurvey.org/WVSDocumentationWV4.jsp)
#
# From the answers, we check how many people selected one of:
#  A job respected by people in general
#  An opportunity to use initiative
#  A job you feel you can achieve something
#  A job that is interesting
#  A job that meets one's abilities
#
# The reasoning is, that if a person answers yes to one or more of these
# then, fulfillment in a job is imporant. If someone does not say yes to
# any of these, the person is likely to be happy with a mundane/repetitive
# job.


#install.packages("plyr")
library(plyr)
#install.packages("data.table")   # install only needed once
library(data.table)   
install.packages("ggplot2")
library(ggplot2)
library(reshape)

setwd("~/projects/sci.meaningful_work")

# load world value survey data
#
# cite as: 
# --------
# WVS (2015). World Value Survey 1981-2015 official aggregate v.20150418, 2015. World Values Survey 
# Association (www.worldvaluessurvey.org). Aggregate File Producer: JDSystems, Madrid.
#
# You can get the data from here: http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
#
data = readRDS("F00008390-WVS_Longitudinal_1981_2016_r_v20180912.rds")
country_codes = read.csv("country_codes.csv", sep=",", header=TRUE, strip.white=TRUE)
wave_codes = read.csv("wave_codes.csv", sep=",", header=TRUE, strip.white=TRUE)

#
# we are looking for all answers to these
# V216 Good pay 1 
# V217 Pleasant people to work with 2 
# V218 Not too much pressure 3 
# V219 Good job security 4 
# V220 Good chances for promotion 5 
# A V221 A job respected by people in general 6  ->>>> C014
# V222 Good hours 7 
# A V223 An opportunity to use initiative 8 ->>> C016
# V224 A useful job for society 9 
# V225 Generous holidays 1 
# V226 Meeting people 2 
# A V227 A job you feel you can achieve something  ->>>> C018
# V228 A responsible job 4 
# A V229 A job that is interesting 5   ->>>> C020
# A V230 A job that meets one's abilities 6 ->>> C021
# V231 None of these
#                                         
questions = data[c("S002", "S003", "S012", "C014", "C016", "C018", "C020", "C021")]
names(questions)[names(questions)=="S002"] <- "wave_code"
names(questions)[names(questions)=="S003"] <- "country_code"
names(questions)[names(questions)=="S012"] <- "interview_date"
names(questions)[names(questions)=="C014"] <- "respected_job"
names(questions)[names(questions)=="C016"] <- "use_initiative"
names(questions)[names(questions)=="C018"] <- "achieve_something"
names(questions)[names(questions)=="C020"] <- "interesting_job"
names(questions)[names(questions)=="C021"] <- "meets_abilities"
h_questions = merge(x = questions, y = country_codes, by = "country_code", all.x = TRUE)
h_questions = merge(x = h_questions, y = wave_codes, by = "wave_code", all.x = TRUE) 
h_questions = h_questions[, c("wave", "country", "respected_job", "use_initiative", "achieve_something", "interesting_job", "meets_abilities")]
#pos_questions = h_questions[h_questions$respected_job >= 0 && h_questions$use_initiative >= 0 && h_questions$achieve_something >= 0 && h_questions$interesting_job >= 0 && h_questions$meets_abilities >= 0,]
pos_questions = h_questions
pos_questions = pos_questions[pos_questions$respected_job >= 0,]
pos_questions = pos_questions[pos_questions$use_initiative >= 0,]
pos_questions = pos_questions[pos_questions$achieve_something >= 0,]
pos_questions = pos_questions[pos_questions$interesting_job >= 0,]
pos_questions = pos_questions[pos_questions$meets_abilities >= 0,]
pos_questions[,"sum"] = pos_questions[,"respected_job"] + pos_questions[,"use_initiative"] + pos_questions[,"achieve_something"] + pos_questions[,"interesting_job"] + pos_questions[,"meets_abilities"]
sum_questions = pos_questions[c("wave", "country", "sum")]

# histogram over all of the data
hist(sum_questions[,"sum"])


# remove country for by year count
no_country = sum_questions[ , !(names(sum_questions) %in% c("country"))]
zero_counts = count(no_country[no_country$sum == 0,])
names(zero_counts) = c("wave", "val", "zero_count")
non_zeros = no_country[no_country$sum > 0,]
non_zeros$val = 1
non_zeros = non_zeros[ , !(names(non_zeros) %in% c("sum"))]
non_zero_counts = count(non_zeros)
names(non_zero_counts) = c("wave", "val", "non_zero_count")
merged_counts = merge(x = zero_counts, y = non_zero_counts, by = "wave", all.x = TRUE)
merged_counts$zero_ratio = merged_counts$zero_count / merged_counts$non_zero_count
non_zero_ratios = merged_counts[,(names(merged_counts) %in% c("wave", "zero_ratio"))]
plot(non_zero_ratios)


# GDP per capita https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp=read.csv("API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_41408.csv", sep=",", header=TRUE, strip.white=TRUE, comment.char = "#", skip = 4)  # the first 4 lines contain notting, John Snow!
names(gdp)[names(gdp)=="Country.Name"] <- "country"
# compute average GDP for wave years 1989-1993 1994-1998 1999-2004 2005-2009 2010-2014 (there is no gdp data for 1981-1984)
gdp = setDT(gdp)
gdp[, ":="("1989-1993" = rowMeans(.SD)), by = country, .SDcols = c("X1990", "X1991", "X1992", "X1993")]
gdp[, ":="("1994-1998" = rowMeans(.SD)), by = country, .SDcols = c("X1994", "X1995", "X1996", "X1997", "X1998")]
gdp[, ":="("1999-2004" = rowMeans(.SD)), by = country, .SDcols = c("X1999", "X2000", "X2001", "X2002", "X2003", "X2004")]
gdp[, ":="("2005-2009" = rowMeans(.SD)), by = country, .SDcols = c("X2005", "X2006", "X2007", "X2008", "X2009")]
gdp[, ":="("2010-2014" = rowMeans(.SD)), by = country, .SDcols = c("X2010", "X2011", "X2012", "X2013", "X2014")]
gdp = gdp[, .SD, .SDcols=c("country", "1989-1993", "1994-1998", "1999-2004","2005-2009", "2010-2014")]
gdp = melt(gdp, id=c("country"))
setnames(gdp, "variable", "wave")
setnames(gdp, "value", "gdp")

questions_gdp = merge(x = sum_questions, y = gdp, by = c("country", "wave"), all.x = TRUE)
questions_gdp = questions_tax[! is.na(questions_tax$tax_rate),]
questions_gdp = setDT(questions_gdp)
setnames(questions_gdp, "sum", "sum_a_questions")
aggregated = ddply(questions_gdp, .(wave, country), numcolwise(mean))

plot_data = aggregated # [aggregated$sum_a_questions >= 0 ,] # & aggregated$tax_rate <= 100
plt_aq_gdp_agg = ggplot(plot_data, aes(sum_a_questions, gdp)) +
  geom_point() +
  labs(title = "A-Questions vs. GDP (by country/wave)") +
  geom_smooth(method=lm) 
plot(plt_aq_gdp_agg)

# non-a ratio plot
oecd_countries = read.csv("oecd_countries.csv", sep=",", header=TRUE, strip.white=TRUE, comment.char = "#")

zero_counts = count(sum_questions[sum_questions$sum == 0,])
names(zero_counts) = c("wave", "country", "val", "zero_count")
non_zeros = sum_questions[sum_questions$sum > 0,]
non_zeros$val = 1
non_zeros = non_zeros[ , !(names(non_zeros) %in% c("sum"))]
non_zero_counts = count(non_zeros)
names(non_zero_counts) = c("wave", "country", "val", "non_zero_count")
merged_counts = merge(x = zero_counts, y = non_zero_counts, by = c("wave", "country"), all.x = TRUE)
merged_counts$zero_ratio = merged_counts$zero_count / merged_counts$non_zero_count
non_zero_ratios = merged_counts[,(names(merged_counts) %in% c("wave", "country", "zero_ratio"))]
# there is something wrong with finlands numbers, need to look at this manana ;-)
non_zero_ratios_no_finland = non_zero_ratios[non_zero_ratios$country != "Finland", ]
non_zero_ratios_oecd_no_finland = non_zero_ratios_no_finland[!(non_zero_ratios_no_finland$country %in% oecd_countries),]
ggplot(data=non_zero_ratios_oecd_no_finland, aes(x=reorder(country, -zero_ratio), y=zero_ratio, fill=wave)) +
  geom_bar(stat="identity", position = "dodge") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#
aggregated[aggregated$country == "Switzerland", ]

