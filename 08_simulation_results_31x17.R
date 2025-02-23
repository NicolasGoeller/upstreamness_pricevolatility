
### Load required packages
library(tidyverse)

########################### Load country-sector info ###########################

cs_info_df <- read_csv("output/countrysector_information.csv") %>% 
  filter(country != "ROW")

########################### Country-Sector Only Shocks #########################

#c_level3 <- read_csv("output/cost_levels_31x17.csv")
#d_level3 <- read_csv("output/demand_changes_31x17.csv")

idiosyn_cost_df <- read_csv("output/results_cost_shock_31x17.csv") %>% 
  filter(c_shock_var %in% c("normal-0-0-0.05","normal-0-0-0.1","normal-0-0-0.15")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-d_shock_var) %>%
  rename(c(shock_var = c_shock_var,c_ppi_volat = price_volat))

idiosyn_demand_df <- read_csv("output/results_demand_shock_31x17.csv") %>% 
  filter(d_shock_var %in% c("normal-0-0-0.05","normal-0-0-0.1","normal-0-0-0.15")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-c_shock_var) %>%
  rename(c(shock_var = d_shock_var,d_ppi_volat = price_volat))

idiosyn_df <- idiosyn_cost_df %>% 
  left_join(idiosyn_demand_df,by=c("year","country","country_ind","rs_ind","shock_var","p_shock_var")) %>% 
  left_join(cs_info_df,by=c("year","country","country_ind"))

####

### Intuitive plots for clear correlation
idiosyn_df %>% 
  filter(year==2011, p_shock_var == "N(0.05, 0.05)") %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=c_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages",
       title="BWD Linkages with country-sector-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_bwd_idio_cppi.png", width = 20, height=25, units="cm")

idiosyn_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=d_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated demand-shock price volatility",x="Total FWD Linkages",
       title="BWD Linkages with country-sector-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_fwd_idio_dppi.png", width = 20, height=25, units="cm")


### Reverse the x-axis variables
#idiosyn_df %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=tot_fwd,y=c_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))

#idiosyn_df %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=tot_bwd,y=d_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(y="Simulated demand-shock price volatility",x="Total BWD Linkages",
#       title="BWD Linkages with country-sector-shocked volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_tot_bwd_idio_dppi.png", width = 20, height=25, units="cm")

#### Fit of simulated prices against observed prices

#idiosyn_df %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(y=c_ppi_volat,x=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(y="Simulated cost-shock price volatility",x="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_idio_cppi.png", width = 20, height=25, units="cm")
#
#idiosyn_df %>% 
#  #mutate(d_ppi_volat = if_else(d_ppi_volat == 0,10**-10,d_ppi_volat),
#  #       c_ppi_volat = if_else(c_ppi_volat == 0,10**-10,c_ppi_volat),
#  #       emp_ppi_volat = if_else(emp_ppi_volat == 0,10**-10,emp_ppi_volat)) %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(y=d_ppi_volat,x=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(y="Simulated demand-shock price volatility",x="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_idio_dppi.png", width = 20, height=25, units="cm")

############################## Country Only Shocks #############################

country_cost_df <- read_csv("output/results_cost_shock_31x17.csv") %>% 
  filter(c_shock_var %in% c("normal-0.05-0-0","normal-0.1-0-0","normal-0.15-0-0")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-d_shock_var) %>%
  rename(c(shock_var = c_shock_var,c_ppi_volat = price_volatility))

country_demand_df <- read_csv("output/results_demand_shock_31x17.csv") %>% 
  filter(d_shock_var %in% c("normal-0.05-0-0","normal-0.1-0-0","normal-0.15-0-0")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-c_shock_var) %>%
  rename(c(shock_var = d_shock_var,d_ppi_volat = price_volatility))

country_df <- country_cost_df %>% 
  left_join(country_demand_df,by=c("year","country","country_ind","rs_ind","shock_var")) %>% 
  left_join(cs_info_df,by=c("year","country","country_ind"))

#### Fit of simulated prices against observed prices

#country_df %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=c_ppi_volat,y=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(x="Simulated cost-shock price volatility",y="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_country_cppi.png", width = 20, height=25, units="cm")

#country_df %>% 
#  #mutate(d_ppi_volat = if_else(d_ppi_volat == 0,10**-10,d_ppi_volat),
#  #       c_ppi_volat = if_else(c_ppi_volat == 0,10**-10,c_ppi_volat),
#  #       emp_ppi_volat = if_else(emp_ppi_volat == 0,10**-10,emp_ppi_volat)) %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=d_ppi_volat,y=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(x="Simulated demand-shock price volatility",y="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_country_dppi.png", width = 20, height=25, units="cm")

country_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=d_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated demand-shock price volatility",x="Total FWD Linkages",
       title="FWD Linkages with country-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_fwd_country_dppi.png", width = 20, height=25, units="cm")


country_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_bwd,y=c_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages",
       title="BWD Linkages with country-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_bwd_country_cppi.png", width = 20, height=25, units="cm")

############################## Sector Only Shocks #############################

sector_cost_df <- read_csv("output/results_cost_shock_31x17.csv") %>% 
  filter(c_shock_var %in% c("normal-0-0.05-0","normal-0-0.1-0","normal-0-0.15-0")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-d_shock_var) %>%
  rename(c(shock_var = c_shock_var,c_ppi_volat = price_volatility))

sector_demand_df <- read_csv("output/results_demand_shock_31x17.csv") %>% 
  filter(d_shock_var %in% c("normal-0-0.05-0","normal-0-0.1-0","normal-0-0.15-0")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-c_shock_var) %>%
  rename(c(shock_var = d_shock_var,d_ppi_volat = price_volatility))

sector_df <- sector_cost_df %>% 
  left_join(sector_demand_df,by=c("year","country","country_ind","rs_ind","shock_var")) %>% 
  left_join(cs_info_df,by=c("year","country","country_ind"))

sector_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=d_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated demand-shock price volatility",x="Total FWD Linkages",
       title="FWD Linkages with sector-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_fwd_sector_dppi.png", width = 20, height=25, units="cm")


sector_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_bwd,y=c_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages",
       title="BWD Linkages with sector-shocked volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_bwd_sector_cppi.png", width = 20, height=25, units="cm")


#sector_df %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=c_ppi_volat,y=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(x="Simulated cost-shock price volatility",y="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_sector_cppi.png", width = 20, height=25, units="cm")
#
#sector_df %>% 
#  #mutate(d_ppi_volat = if_else(d_ppi_volat == 0,10**-10,d_ppi_volat),
#  #       c_ppi_volat = if_else(c_ppi_volat == 0,10**-10,c_ppi_volat),
#  #       emp_ppi_volat = if_else(emp_ppi_volat == 0,10**-10,emp_ppi_volat)) %>% 
#  filter(year==2011) %>% 
#  mutate(shock_var = factor(shock_var),
#         rs_ind = factor(rs_ind)) %>% 
#  ggplot(aes(x=d_ppi_volat,y=emp_ppi_volat))+
#  geom_point()+
#  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
#  labs(x="Simulated demand-shock price volatility",y="Observed price volatility")+
#  theme(strip.text.x = element_text(size = 12),
#        strip.text.y = element_text(size = 12))
#ggsave("graphs/scatter_emp_ppi_sector_dppi.png", width = 20, height=25, units="cm")

########## Are volatilities different by shock origin ################

compare_cost_df <- read_csv("output/results_cost_shock_31x17.csv") %>% 
  filter(c_shock_var %in% c("normal-0.04-0-0.01","normal-0.025-0-0.025","normal-0.01-0-0.04")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-d_shock_var) %>%
  rename(c(shock_var = c_shock_var,c_ppi_volat = price_volatility))

compare_demand_df <- read_csv("output/results_demand_shock_31x17.csv") %>% 
  filter(d_shock_var %in% c("normal-0.04-0-0.01","normal-0.025-0-0.025","normal-0.01-0-0.04")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-c_shock_var) %>%
  rename(c(shock_var = d_shock_var,d_ppi_volat = price_volatility))

compare_df <- compare_cost_df %>% 
  left_join(compare_demand_df,by=c("year","country","country_ind","rs_ind","shock_var")) %>% 
  left_join(cs_info_df,by=c("year","country","country_ind"))



compare_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=c_ppi_volat,y=emp_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(x="Simulated cost-shock price volatility",y="Observed price volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_emp_ppi_origin_cppi.png", width = 20, height=25, units="cm")

compare_df %>% 
  #mutate(d_ppi_volat = if_else(d_ppi_volat == 0,10**-10,d_ppi_volat),
  #       c_ppi_volat = if_else(c_ppi_volat == 0,10**-10,c_ppi_volat),
  #       emp_ppi_volat = if_else(emp_ppi_volat == 0,10**-10,emp_ppi_volat)) %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=d_ppi_volat,y=emp_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(x="Simulated demand-shock price volatility",y="Observed price volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_emp_ppi_origin_dppi.png", width = 20, height=25, units="cm")

test <- read_csv("output/results_cost_shock_31x17.csv")

compare_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_bwd,y=c_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_bwd_idio_cppi.png", width = 20, height=25, units="cm")

compare_df %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=d_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated demand-shock price volatility",x="Total FWD Linkages")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

################# Mixed - Demand & Cost shocks ##################
mixed_shocks_df <- read_csv("output/results_mixed_shock_31x17.csv") %>% 
  #filter(d_shock_var %in% c("normal-0.04-0-0.01","normal-0.025-0-0.025","normal-0.01-0-0.04")) %>% 
  mutate(country = str_sub(country_sector,1,3),
         country_ind = str_sub(country_sector,5)) %>% 
  filter(country != "ROW") %>% 
  select(-country_sector,-c_shock_var) %>%
  rename(c(shock_var = d_shock_var,m_ppi_volat = price_volatility))

mixed_df <- mixed_shocks_df %>% 
  left_join(cs_info_df,by=c("year","country","country_ind"))

mixed_df %>% 
  filter(shock_var %in% c("normal-0-0-0.025","normal-0-0-0.05","normal-0-0-0.075")) %>% 
  #mutate(d_ppi_volat = if_else(d_ppi_volat == 0,10**-10,d_ppi_volat),
  #       c_ppi_volat = if_else(c_ppi_volat == 0,10**-10,c_ppi_volat),
  #       emp_ppi_volat = if_else(emp_ppi_volat == 0,10**-10,emp_ppi_volat)) %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=m_ppi_volat,y=emp_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(x="Simulated demand-shock price volatility",y="Observed price volatility")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_emp_ppi_idio_mppi.png", width = 20, height=25, units="cm")


mixed_df %>% 
  filter(shock_var %in% c("normal-0-0-0.025","normal-0-0-0.05","normal-0-0-0.075")) %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_bwd,y=m_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_bwd_idio_mppi.png", width = 20, height=25, units="cm")

mixed_df %>% 
  filter(shock_var %in% c("normal-0-0-0.025","normal-0-0-0.05","normal-0-0-0.075")) %>% 
  filter(year==2011) %>% 
  mutate(shock_var = factor(shock_var),
         rs_ind = factor(rs_ind)) %>% 
  ggplot(aes(x=tot_fwd,y=m_ppi_volat))+
  geom_point()+
  facet_grid(rows=vars(rs_ind),cols=vars(shock_var))+
  labs(y="Simulated cost-shock price volatility",x="Total BWD Linkages")+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("graphs/scatter_tot_fwd_idio_mppi.png", width = 20, height=25, units="cm")

### Findings:
## 1. Demand shocks cause negative correlation with FWD linkages, 0-correlation with BWD (normally distributed along linkages)
## 2. Cost shocks cause negative correlation with BWD linkages, 0-correlation with FWD
## 3. Volatilities from cost shocks are bigger in magnitude than from demand
## 4. Volatility differences from RS are bigger for demand shocks than for cost
## 5. Volatility differences over shock variances scale approximately, but not exactly
## 6. Shock origins matter little for linkage-volatility correlation
## 7. Mixed shocks (cost+demand) cause weak neg cor with BWD, 0-cor with FWD

### Potential Add-Ons:
## 1. Specific shock to a country or sector only and map effects on price changes
## 2. Explore heterogenous RS assignments (based on sectors in country)
## 3. Check effects of added information (distance/trade costs, price stickiness, pass-through)
