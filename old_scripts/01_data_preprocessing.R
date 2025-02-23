library(tidyverse)
library(tidyquant)
library(zoo)
#library(ioanalysis)
library(haven)
library(lubridate)

### Computations of interest
# Change of Upstreamness and price volat between two periods

# Denseness of trade network in extensive margin
# Spread of trade intensive margin

# Is the distribution of input share approx equal across countries
# Is the distribution of input shares across sector sums approx equal across years


################################################################################
######################## Calculate price volatility ############################
################################################################################

############################### Merge PPi data #################################

# Set base path
base_path <- "base/ALS_replication/analysis/PPI/MatlabFiles/"

# Define empty datasets for looping
ppi_17sec_df <- tibble(year = numeric(),
                       month = numeric(),
                       country = character(),
                       country_ind = character(),
                       chng_ppi = numeric())
ppi_18sec_df <- tibble(year = numeric(),
                       month = numeric(),
                       country = character(),
                       country_ind = character(),
                       chng_ppi = numeric())

# Loop over years and months
for (iter in 1995:2011){
  for (m in 1:12){
    
    # Generate year-month indicator
    yrm <- paste0(iter,m)
    
    # Read 17 sec PPI file for yrm and merge on table
    iter1_df <- read_csv(paste0(base_path,"PPI_",yrm,".csv")) %>% 
      select(year,month,country,country_ind,chng_ppi) 
    ppi_17sec_df <- bind_rows(ppi_17sec_df, iter1_df)
    
    # Read 18 sec PPI file for yrm and merge on table
    iter2_df <- read_csv(paste0(base_path,"PPI_",yrm,"_31cty18sec_srest_agg.csv")) %>% 
      select(year,month,country,country_ind,chng_ppi) 
    ppi_18sec_df <- bind_rows(ppi_18sec_df, iter2_df)
  }

}

# Pivot longer tables into form
ppi_17sec_wide <- pivot_wider(ppi_17sec_df,names_from = year, values_from = chng_ppi,
                    names_prefix = "ppi_change_")
ppi_18sec_wide <- pivot_wider(ppi_18sec_df,names_from = year, values_from = chng_ppi,
                            names_prefix = "ppi_change_")

# Generate datetime year-month variable for long format
ppi_18sec_df <- ppi_18sec_df %>% 
  unite(yrmon,year,month,sep = "-") %>% 
  mutate(yrmon = as.yearmon(yrmon))
ppi_17sec_df <- ppi_17sec_df %>% 
  unite(yrmon,year,month,sep = "-") %>% 
  mutate(yrmon = as.yearmon(yrmon))

# Save results
write_csv(ppi_17sec_df,"data/analytics/ppi_sec17_long.csv")
write_csv(ppi_18sec_df,"data/analytics/ppi_sec18_long.csv")
write_csv(ppi_17sec_wide,"data/analytics/ppi_sec17_wide.csv")
write_csv(ppi_18sec_wide,"data/analytics/ppi_sec18_wide.csv")

########################### Calculate volatility ###############################

# 17 Sec: Generate year-month variable and calculate volatility
ppi_17sec_volat <- ppi_17sec_df %>% 
  mutate(year = year(yrmon)) %>% 
  group_by(country, country_ind,year) %>% 
  summarise(volat_ppi = sd(chng_ppi))

# 18 Sec: Generate year-month variable and calculate volatility
ppi_18sec_volat <- ppi_18sec_df %>% 
  mutate(year = year(yrmon)) %>% 
  group_by(country, country_ind,year) %>% 
  summarise(volat_ppi = sd(chng_ppi))

# Save results
write_csv(ppi_17sec_volat,"data/analytics/ppi_sec17_volat.csv")
write_csv(ppi_18sec_volat,"data/analytics/ppi_sec18_volat.csv")


################################################################################
############################ Total Output Weights ##############################
################################################################################

total_output_df <- tibble(year=numeric(),
                          country = character(),
                          country_ind = character(),
                          total_output = numeric())

for (iter in 1995:2011){
  # Generate path to read in datafile
  base_path <- "base/ALS_replication/analysis/WIOD/totalOutput/Matlabfiles/WIOD_totalOutput_"
  path <- paste0(base_path,iter,".csv")
  
  # Print loop indicator
  print(iter)
  
  # Read datafile, calculate upstreamness and merge on table
  total_output_df <- read_csv(path) %>% 
    bind_rows(total_output_df)
}  

# Save result
write_csv(total_output_df,"data/analytics/total_output_weights.csv")

################################################################################
########################### Final Country-sector Info ##########################

### Load empirical price changes
ppi_changes <- read_csv("data/analytics/ppi_sec17_long.csv") %>% 
  mutate(year = year(as.yearmon(yrmon))) 

### Compute yearly volatlities
ppi_volat <- ppi_changes %>% 
  group_by(year,country,country_ind) %>% 
  summarise(emp_ppi_volat1 = sd(chng_ppi),
            emp_ppi_volat2 = sd(chng_ppi*100),
            emp_ppi_volat3 = var(chng_ppi),
            emp_ppi_volat4 = var(chng_ppi*100))

### Load data on production network position
prod_pos_df <- read_csv("output/production_network_position.csv")

### Load yearly variable table
yearly_vars <- read_csv("output/yearly_input_variables.csv")

### Join tables together
cs_info <- ppi_volat %>% 
  left_join(prod_pos_df,by=c("year","country","country_ind"))

### Write table to file
write_csv(cs_info,"output/countrysector_information.csv")

