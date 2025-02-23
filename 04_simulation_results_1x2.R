library(tidyverse)
library(ggpubr)

#### Cost shocks 1x2 Model #####################################################

c_shock1 <- read_csv("output/results_cost_shock_1x2.csv") 

c_scenario <- c_shock1 %>% 
  select(scenario,rs_ind,country_sector) %>% 
  mutate(rs_scen = str_c(country_sector,rs_ind,sep=": ")) %>% 
  group_by(scenario) %>% 
  summarise(rs_comb = toString(rs_scen))

######## Cost shock for High Openness ##########################################
c0_setting = 0

c_label0 <- c_shock1 %>% 
  distinct(year,country_sector,tot_bwd) %>% 
  mutate(tot_bwd= round(tot_bwd,3),
         label = str_c(country_sector,": Downstr. ",tot_bwd)) %>% 
  filter(year == c0_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

c_caption01 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."
#c_caption2 <- paste(c_caption12," Returns to scale are: ",rs_scenario,".",sep = "")

cplot_0_1 <- c_shock1 %>% 
  left_join(c_scenario,by="scenario") %>% 
  filter(rs_comb %in% c("s1: 0.8, s2: 0.8","s1: 0.9, s2: 0.9",
                        "s1: 1, s2: 1","s1: 1.1, s2: 1.1",
                        "s1: 1.2, s2: 1.2"),
         year == c0_setting) %>%
  mutate(shock_var = str_sub(c_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption01)+
  theme(plot.caption = element_text(hjust = 0))


rs_scenario <- "s1: 1, s2: 1"
c_caption02 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."
c_caption02 <- paste(c_caption01," Returns to scale are: ",rs_scenario,".",sep = "")

cplot_0_2 <-c_shock1 %>% 
  left_join(c_scenario,by="scenario") %>% 
  filter(rs_comb == rs_scenario,year==c0_setting) %>% 
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_var = str_sub(c_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption02)+
  theme(plot.caption = element_text(hjust = 0))


cplot_0 <- ggarrange(cplot_0_1,cplot_0_2,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/1x2_scenarioplot_cost_shocks_1.0-1.0_year0.png",
       height = 26,width = 21,units ="cm")

######## Cost shock for Low Openness ###########################################
c1_setting = 1

c_label1 <- c_shock1 %>% 
  distinct(year,country_sector,tot_bwd) %>% 
  mutate(tot_bwd= round(tot_bwd,3),
         label = str_c(country_sector,": Downstr. ",tot_bwd)) %>% 
  filter(year == c1_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

c_caption11 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."
#c_caption2 <- paste(c_caption12," Returns to scale are: ",rs_scenario,".",sep = "")

cplot_1_1 <- c_shock1 %>% 
  left_join(c_scenario,by="scenario") %>% 
  filter(rs_comb %in% c("s1: 0.8, s2: 0.8","s1: 0.9, s2: 0.9",
                        "s1: 1, s2: 1","s1: 1.1, s2: 1.1",
                        "s1: 1.2, s2: 1.2"),
         year == c1_setting) %>%
  #filter(stage < 4) %>% 
  mutate(shock_var = str_sub(c_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption11)+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/1x2_firststage_cost_shocks_year1.png",
       height = 12,width = 16,units ="cm")

rs_scenario <- "s1: 1, s2: 1"
c_caption12 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."
c_caption12 <- paste(c_caption12," Returns to scale are: ",rs_scenario,".",sep = "")

cplot_1_2 <- c_shock1 %>% 
  left_join(c_scenario,by="scenario") %>% 
  filter(rs_comb == rs_scenario,year==c1_setting) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_var = str_sub(c_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption12)+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/1x2_multistage_cost_shocks_1.0-1.0_year1.png",
       height = 12,width = 16,units ="cm")

cplot_1 <- ggarrange(cplot_1_1,cplot_1_2,
                     labels = c("a", "b"),
                    ncol = 1, nrow = 2)
ggsave("graphs/1x2_scenarioplot_cost_shocks_1.0-1.0_year1.png",
       height = 26,width = 21,units ="cm")


#### Demand shocks 1x2 Model ###################################################

d_shock1 <- read_csv("output/results_demand_shock_1x2.csv") 

d_scenario <- d_shock1 %>% 
  select(scenario,rs_ind,country_sector) %>% 
  mutate(rs_scen = str_c(country_sector,rs_ind,sep=": ")) %>% 
  group_by(scenario) %>% 
  summarise(rs_comb = toString(rs_scen))

######## Demand shock for High Openness ########################################
d0_setting = 0

d_label0 <- d_shock1 %>% 
  distinct(year,country_sector,tot_fwd) %>% 
  mutate(tot_fwd= round(tot_fwd,3),
         label = str_c(country_sector,": Upstr. ",tot_fwd)) %>% 
  filter(year == d0_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

d_caption01 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."

dplot_0_1 <- d_shock1 %>% 
  left_join(d_scenario,by="scenario") %>% 
  filter(rs_comb %in% c("s1: 0.8, s2: 0.8","s1: 0.9, s2: 0.9",
                        "s1: 1, s2: 1","s1: 1.1, s2: 1.1",
                        "s1: 1.2, s2: 1.2"),
         year == d0_setting) %>%
  mutate(shock_var = str_sub(d_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage upstream Propagation of Demand Shocks - High Openness",
       caption = d_caption01)+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/1x2_scenarioplot_demand_shocks_stage1_year0.png",height = 12,width = 21,units = "cm")


rs_scenario <- "s1: 0.8, s2: 0.8"
d_caption02 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."
d_caption02 <- paste(d_caption01," Returns to scale are: ",rs_scenario,".")

dplot_0_2 <- d_shock1 %>% 
  left_join(d_scenario,by="scenario") %>% 
  filter(rs_comb == rs_scenario,year == d0_setting) %>% 
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_var = str_sub(d_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage upstream Propagation of Demand Shocks - High Openness",
       caption = d_caption02)+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/1x2_scenarioplot_demand_shocks_0.8-0.8_year0.png",height = 12,width = 21,units = "cm")


dplot_0 <- ggarrange(dplot_0_1,dplot_0_2,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/1x2_scenarioplot_demand_shocks_0.8-0.8_year0.png",
       height = 26,width = 21,units ="cm")

######## Demand shock for Low Openness #########################################
d1_setting = 1

d_label1 <- d_shock1 %>% 
  distinct(year,country_sector,tot_fwd) %>% 
  mutate(tot_fwd= round(tot_fwd,3),
         label = str_c(country_sector,": Upstr. ",tot_fwd)) %>% 
  filter(year == d1_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

d_caption11 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."

dplot_1_1 <- d_shock1 %>% 
  left_join(d_scenario,by="scenario") %>% 
  filter(rs_comb %in% c("s1: 0.8, s2: 0.8","s1: 0.9, s2: 0.9",
                        "s1: 1, s2: 1","s1: 1.1, s2: 1.1",
                        "s1: 1.2, s2: 1.2"),
         year == d1_setting) %>% 
  mutate(shock_var = str_sub(d_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage upstream Propagation of Demand Shocks - Low Openness",
       caption = d_caption11)+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/1x2_firststage_demand_shocks_year1.png",height = 12,width = 16,units = "cm")


rs_scenario <- "s1: 0.8, s2: 0.8"
d_caption12 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."
d_caption12 <- paste(d_caption12," Returns to scale are: ",rs_scenario,".")

dplot_1_2 <- d_shock1 %>% 
  left_join(d_scenario,by="scenario") %>% 
  filter(rs_comb == rs_scenario,year == d1_setting) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_var = str_sub(d_shock_var,start=12),
         shock_label = str_c("N(0, ",shock_var,")")) %>% 
  #filter(stage < 3) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Sector",breaks=c("s1", "s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage upstream Propagation of Demand Shocks - Low Openness",
       caption = d_caption12)+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/1x2_multistage_demand_shocks_0.8-0.8_year1.png",height = 12,width = 16,units = "cm")


dplot_1 <- ggarrange(dplot_1_1,dplot_1_2,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/1x2_scenarioplot_demand_shocks_0.8-0.8_year1.png",
       height = 26,width = 21,units ="cm")
