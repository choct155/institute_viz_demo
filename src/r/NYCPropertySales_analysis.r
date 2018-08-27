## Chen Zhao
## Last update: August 24, 2018
## This code analyzes New York City Sales Data from April 2017 to March 2018
## Data downloaded from: http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page
## Data accessed: May 8, 2018

######################## Preliminaries ########################
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)

setwd("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/data/NYC Raw Data")

##### Plot.ly API settings
Sys.setenv("plotly_username"="chendzhao")
Sys.setenv("plotly_api_key"="GFvoAFqKT3W0DjesCDxx")

######################## Ingest data ########################
# combine data from five boroughs into one dataframe
files = list.files(pattern = "*.xls")
read_files <- function(x) {
  temp <- read_excel(x, col_types = c("numeric","text","text","text","numeric","numeric","numeric",
                                      "text","text","text","numeric","numeric","numeric","numeric", 
                                      "numeric","numeric","numeric","text","text","numeric","date"), 
                     skip = 4)
  return(temp)
}
allboroughs = do.call(rbind, lapply(files, read_files))

#write_csv(allboroughs, "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/data/NYC Raw Data/allboroughs.csv", na = "NA", append = FALSE, col_names = TRUE) 

######################## Clean data ########################
# rename columns using all lower case and no spaces
colnames(allboroughs) = tolower(names(allboroughs))
colnames(allboroughs) = sapply(colnames(allboroughs), function(x)gsub(" ", "_", x))

# rename "ease-ment" column
colnames(allboroughs)[colnames(allboroughs)=="ease-ment"] <- "easement"

# replace borough numbers with names of boroughs for ease of interpretation
allboroughs$borough[allboroughs$borough == 1] = "Manhattan"
allboroughs$borough[allboroughs$borough == 2] = "Bronx"
allboroughs$borough[allboroughs$borough == 3] = "Brooklyn"
allboroughs$borough[allboroughs$borough == 4] = "Queens"
allboroughs$borough[allboroughs$borough == 5] = "Staten Island"

# remove duplicate rows
allboroughs_dedup <- unique(allboroughs) #83,183 unique rows out of 83,851 total rows

######################## Create sample of interest ########################

##### find actual sales
# remove rows with missing sales price or sales price = 0 (indicates a transfer)
allboroughs_sales <- filter(allboroughs_dedup, sale_price != 0 & !is.na(allboroughs_dedup$sale_price)) #59,441 rows

##### find residential sales
# filter out tax class = 4 
# tax class 4 includes all other properties not included in class 1, 2, and 3, such as offices, factories, warehouses, garage buildings, etc.
taxclass <- count(allboroughs_sales,tax_class_at_present) #2,457 have tax class = 4
taxclass_sale <- count(allboroughs_sales,tax_class_at_time_of_sale) #2,475 have tax class = 4
allboroughs_res = allboroughs_sales %>% filter(tax_class_at_present != 4 & tax_class_at_time_of_sale != 4) #56,705 rows

# filter out building class codes that do not appear to be residential
buildingclass <- count(allboroughs_res,building_class_category) #remove "05 TAX CLASS 1 VACANT LAND" 497 rows
buildingclass_present <- count(allboroughs_res,building_class_at_present) #remove "G0" 45 rows, "V2" 3 rows, "Z0" 8 rows
buildingclass_sale <- count(allboroughs_res,building_class_at_time_of_sale) #remove "G0" 44 rows, "V2" 3 rows, "Z0" 7 rows
allboroughs_res2 = allboroughs_res %>% filter(building_class_category !=  '05 TAX CLASS 1 VACANT LAND' & 
                                              building_class_at_present !=  'G0' & #Garage; Residential Tax Class 1
                                              building_class_at_present !=  'V2' & #Zoned Commercial Adjacent to Class 1 Dwellng: Not Manhattan
                                              building_class_at_present !=  'Z0' & #Tennis court, pool, shed, etc.
                                              building_class_at_time_of_sale != 'G0' & #Garage; Residential Tax Class 1
                                              building_class_at_time_of_sale != 'V2' & #Zoned Commercial Adjacent to Class 1 Dwellng: Not Manhattan
                                              building_class_at_time_of_sale != 'Z0') #Tennis court, pool, shed, etc.
                                              #56,157 rows

##### find primary residence sales
# Filter out sales with too many units to be a primary residence
totalunits <- count(allboroughs_res2,total_units)
residentialunits <- count(allboroughs_res2,residential_units) #should remove more than 2 residential units
commertialunits <- count(allboroughs_res2,commercial_units) #should remove 1 or more commercial units
allboroughs_res3 = allboroughs_res2 %>% filter(residential_units <= 2 & commercial_units == 0) #50,784 rows

# inspect other categorical variables
boroughs <- count(allboroughs_res3,borough)
blocks <- count(allboroughs_res3,block)
lots <- count(allboroughs_res3,lot)
neighborhoods <- count(allboroughs_res3,neighborhood)
zipcode <- count(allboroughs_res3,zip_code)
easement <- count(allboroughs_res3,easement) #all null

# inspect all variables
summary(allboroughs_res3)

######################## Analysis ########################

##### create variables useful for analysis
# indicator variable for above $750,000 (= 1) vs below (= 0)
# indicator variable for above $937,500 (= 1) vs below (= 0)
# month and year variable
# indicator variable for after tax law takes effect (after Jan. 1, 2018)
allboroughs_an1 = mutate(allboroughs_res3, bet1m750k = ifelse(sale_price > 750000 & sale_price < 1000000, "between 1m and 750k", "not between 1m and 750k"),
                                           above750k = ifelse(sale_price > 750000, "above 750k", "below 750k"),
                                           above937k = ifelse(sale_price > 937500, "above 937k", "below 937k"),
                                           bet1m750k_dum = ifelse(sale_price > 750000 & sale_price < 1000000, 1, 0),
                                           above750k_dum = ifelse(sale_price > 750000, 1, 0),
                                           above937k_dum = ifelse(sale_price > 937500, 1, 0),
                                           month = format(allboroughs_res3$sale_date,"%Y-%m"),
                                           afterlaw = ifelse(month %in% c("2018-01","2018-02","2018-03"), 1, 0))
count(allboroughs_an1,bet1m750k) #7,868 = above 750k and below 1m, 42,916 = not
count(allboroughs_an1,bet1m750k_dum) #7,868 = above 750k and below 1m, 42,916 = not
count(allboroughs_an1,above750k) #19,459 = above 750k, 31,325 = below 750k
count(allboroughs_an1,above750k_dum) #19,459 = 1, 31,325 = 0
count(allboroughs_an1,above937k) #13,586 = above 937k, 37,198 = below 937k
count(allboroughs_an1,above937k_dum) #13,586 = 1, 37,198 = 0
count(allboroughs_an1,month) #3,903 in 01-2018, 3,239 in 02-2018, and 2,666 in 03-2018
count(allboroughs_an1,afterlaw) #9,808 = 1, 40,976 = 0

##### look at summary statistics
# find number of sales and dollar amount of sales
# collapsing data by:
# 1. month and above/below 750k
# 2. month and above/below 937k
# 3. borough, month, and above/below 750k
# 4. borough, month, and above/below 937k
bymonth7501m_sales <- allboroughs_an1 %>% group_by(month, bet1m750k) %>% summarise(numsales = n(), dollarsales = median(sale_price))
bymonth750_sales <- allboroughs_an1 %>% group_by(month, above750k) %>% summarise(numsales = n(), dollarsales = median(sale_price))
bymonth937_sales <- allboroughs_an1 %>% group_by(month, above937k) %>% summarise(numsales = n(), dollarsales = median(sale_price))
bybormonth7501m_sales <- allboroughs_an1 %>% group_by(borough, month, bet1m750k) %>% summarise(numsales = n(), dollarsales = median(sale_price))
bybormonth750_sales <- allboroughs_an1 %>% group_by(borough, month, above750k) %>% summarise(numsales = n(), dollarsales = median(sale_price))
bybormonth937_sales <- allboroughs_an1 %>% group_by(borough, month, above937k) %>% summarise(numsales = n(), dollarsales = median(sale_price))

# plot the number of sales and dollar amount of sales by month
## by between 750k and 1m vs. not
numsales_7501m <- ggplot(bymonth7501m_sales, aes(x=month, y=numsales, group=bet1m750k)) + 
                  geom_line(aes(color=bet1m750k)) +
                  geom_point(aes(color=bet1m750k)) +
                  labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales between $750,000 and $1,000,000 vs Not") +
                  theme(legend.position = "bottom", legend.title=element_blank())

numsales_7501m <- ggplotly(numsales_7501m)
api_create(numsales_7501m, "Number Sales by Month for $750K to $1m v Not")

dollarsales_7501m <- ggplot(bymonth7501m_sales, aes(x=month, y=dollarsales, group=bet1m750k)) + 
                   geom_line(aes(color=bet1m750k)) +
                   geom_point(aes(color=bet1m750k)) +
                   labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Dollar Amount of Sales by Month for Sales between $750,000 and $1,000,000 vs Not") +
                   theme(legend.position = "bottom", legend.title=element_blank())

dollarsales_7501m <- ggplotly(dollarsales_7501m)
api_create(dollarsales_7501m, "Dollar Sales by Month for $750K to $1m v Not")

## by above and below 750k
numsales_750 <- ggplot(bymonth750_sales, aes(x=month, y=numsales, group=above750k)) + 
                geom_line(aes(color=above750k)) +
                geom_point(aes(color=above750k)) +
                labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales Above and Below $750,000") +
                theme(legend.position = "bottom", legend.title=element_blank())
dollarsales_750 <- ggplot(bymonth750_sales, aes(x=month, y=dollarsales, group=above750k)) + 
                   geom_line(aes(color=above750k)) +
                   geom_point(aes(color=above750k)) +
                   labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Dollar Amount of Sales by Month for Sales Above and Below $750,000") +
                   theme(legend.position = "bottom", legend.title=element_blank())

## by above and below 937k
numsales_937 <- ggplot(bymonth937_sales, aes(x=month, y=numsales, group=above937k)) + 
                geom_line(aes(color=above937k)) +
                geom_point(aes(color=above937k)) +
                labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales Above and Below $937,500") +
                theme(legend.position = "bottom", legend.title=element_blank())
dollarsales_937 <- ggplot(bymonth937_sales, aes(x=month, y=dollarsales, group=above937k)) + 
                   geom_line(aes(color=above937k)) +
                   geom_point(aes(color=above937k)) +
                   labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Dollar Amount of Sales by Month for Sales Above and Below $937,500") +
                   theme(legend.position = "bottom", legend.title=element_blank())

# plot the number of sales and dollar amount of sales by month and borough
## by between 750k and 1m vs. not
numsales_7501m_bor <- ggplot(bybormonth7501m_sales, aes(x=month, y=numsales, group=bet1m750k)) + 
                    geom_line(aes(color=bet1m750k)) +
                    geom_point(aes(color=bet1m750k)) +
                    facet_wrap(~ borough) + 
                    labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales between $750,000 and $1,000,000 vs Not") +
                    theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())

dollarsales_7501m_bor <- ggplot(bybormonth7501m_sales, aes(x=month, y=dollarsales, group=bet1m750k)) + 
                       geom_line(aes(color=bet1m750k)) +
                       geom_point(aes(color=bet1m750k)) +
                       facet_wrap(~ borough) + 
                       labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Median Dollar Amount of Sales by Month for Sales between $750,000 and $1,000,000 vs Not") +
                       theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())

## by above and below 750k
numsales_750_bor <- ggplot(bybormonth750_sales, aes(x=month, y=numsales, group=above750k)) + 
                    geom_line(aes(color=above750k)) +
                    geom_point(aes(color=above750k)) +
                    facet_wrap(~ borough) + 
                    labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales Above and Below $750,000") +
                    theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())

dollarsales_750_bor <- ggplot(bybormonth750_sales, aes(x=month, y=dollarsales, group=above750k)) + 
                       geom_line(aes(color=above750k)) +
                       geom_point(aes(color=above750k)) +
                       facet_wrap(~ borough) + 
                       labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Median Dollar Amount of Sales by Month for Sales Above and Below $750,000") +
                       theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())

## by above and below 937k
numsales_937_bor <- ggplot(bybormonth937_sales, aes(x=month, y=numsales, group=above937k)) + 
                    geom_line(aes(color=above937k)) +
                    geom_point(aes(color=above937k)) +
                    facet_wrap(~ borough) + 
                    labs(x = "Month of Sale", y = "Number of Sales", title = "Number of Sales by Month for Sales Above and Below $937,500") +
                    theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())

dollarsales_937_bor <- ggplot(bybormonth937_sales, aes(x=month, y=dollarsales, group=above937k)) + 
                       geom_line(aes(color=above937k)) +
                       geom_point(aes(color=above937k)) +
                       facet_wrap(~ borough) + 
                       labs(x = "Month of Sale", y = "Median Dollar Amount of Sales", title = "Median Dollar Amount of Sales by Month for Sales Above and Below $937,500") +
                       theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = "bottom", legend.title=element_blank())


# save plots
ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_7501m.jpg",
       plot = numsales_7501m, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_7501m.jpg",
       plot = dollarsales_7501m, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_750.jpg",
       plot = numsales_750, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_750.jpg",
       plot = dollarsales_750, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_937.jpg",
       plot = numsales_937, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_937.jpg",
       plot = dollarsales_937, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_7501m_bor.jpg",
       plot = numsales_7501m_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_7501m_bor.jpg",
       plot = dollarsales_7501m_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_750_bor.jpg",
       plot = numsales_750_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_750_bor.jpg",
       plot = dollarsales_750_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/numsales_937_bor.jpg",
       plot = numsales_937_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)

ggsave("/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/dollarsales_937_bor.jpg",
       plot = dollarsales_937_bor, device = NULL, path = NULL, scale = 1, width = 8, height = 4)


##### run difference-in-differences OLS regressions
# above and below 750k
reg_750k1m <- lm(sale_price ~ bet1m750k_dum + afterlaw + bet1m750k_dum*afterlaw, data = allboroughs_an1)
summary(reg_750k1m)
## controlling for borough
reg_750k1m_bor <- lm(sale_price ~ bet1m750k_dum + afterlaw + bet1m750k_dum*afterlaw + borough, data = allboroughs_an1)
summary(reg_750k1m_bor)

# above and below 750k
reg_750k <- lm(sale_price ~ above750k_dum + afterlaw + above750k_dum*afterlaw, data = allboroughs_an1)
summary(reg_750k)
## controlling for borough
reg_750k_bor <- lm(sale_price ~ above750k_dum + afterlaw + above750k_dum*afterlaw + borough, data = allboroughs_an1)
summary(reg_750k_bor)

# above and below 937k
reg_937k <- lm(sale_price ~ above937k_dum + afterlaw + above937k_dum*afterlaw, data = allboroughs_an1)
summary(reg_937k)
## controlling for borough
reg_937k_bor <- lm(sale_price ~ above937k_dum + afterlaw + above937k_dum*afterlaw + borough, data = allboroughs_an1)
summary(reg_937k_bor)

# save regression output
write.csv(tidy(reg_750k1m), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k1m_coefs.csv")
write.csv(glance(reg_750k1m), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k1m_an.csv")

write.csv(tidy(reg_750k), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k_coefs.csv")
write.csv(glance(reg_750k), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k_an.csv")

write.csv(tidy(reg_937k), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_937k_coefs.csv")
write.csv(glance(reg_937k), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_937k_an.csv")

write.csv(tidy(reg_750k1m_bor), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k1m_bor_coefs.csv")
write.csv(glance(reg_750k1m_bor), "/Users/ChenZhao/Google Drive/JPMorgan/Plotly Test/reg_750k1m_bor_an.csv")

write.csv(tidy(reg_750k_bor), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k_bor_coefs.csv")
write.csv(glance(reg_750k_bor), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_750k_bor_an.csv")

write.csv(tidy(reg_937k_bor), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_937k_bor_coefs.csv")
write.csv(glance(reg_937k_bor), "/Users/ChenZhao/Documents/GitHub/institute_viz_demo/out/reg_937k_bor_an.csv")