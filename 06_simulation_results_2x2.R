
library(tidyverse)

shock_type <- function(df,shock_var){
  type <- df %>% 
    select({{shock_var}}) %>% 
    separate_wider_delim({{shock_var}},delim="-",
                         names=c("dist","c_var","s_var","i_var")) %>% 
    mutate(type = case_when(
             ((c_var == 0) & (s_var == 0) & (i_var == 0)) ~ "NULL",
             ((c_var != 0) & (s_var == 0) & (i_var == 0)) ~ "C",
             ((c_var == 0) & (s_var != 0) & (i_var == 0)) ~ "S",
             ((c_var == 0) & (s_var == 0) & (i_var != 0)) ~ "I",
             ((c_var != 0) & (s_var == 0) & (i_var != 0)) ~ "CI",
             ((c_var == 0) & (s_var != 0) & (i_var != 0)) ~ "SI",
             ((c_var != 0) & (s_var != 0) & (i_var == 0)) ~ "CS",
             ((c_var != 0) & (s_var != 0) & (i_var != 0)) ~ "CSI",
             .default = NA)) %>% 
    select(type) %>% 
    as.vector(.) %>% 
    .[[1]]
  
  return(type)
}

shock_labels <- function(df,shock_var,shock_type){
  shock_label <- df %>% 
    select({{shock_var}}) %>% 
    separate_wider_delim({{shock_var}},delim="-",
                         names=c("dist","c_var","s_var","i_var")) %>% 
    mutate(type = shock_type,
           dist_lab = case_when(dist == "normal" ~ "N",
                                dist == "uniform" ~ "U",
                                .default = NA),
           across(ends_with("_var"),.fns= ~str_c(dist_lab,"(0, ",.x,")")),
           shock_lab = case_when(
             type == "C" ~ str_c("Country ~ ",i_var),
             type == "S" ~ str_c("Sector ~ ",i_var),
             type == "I" ~ str_c("CS ~ ",i_var),
             type == "CI" ~ str_c("Country ~ ",c_var,"\n CS ~ ",i_var),
             type == "SI" ~ str_c("Sector ~ ",s_var,"\n CS ~ ",i_var),
             type == "CS" ~ str_c("Country ~ ",c_var,"\n Sector ~ ",s_var),
             type == "CSI" ~ str_c("Country ~ ",c_var,"\n Sector ~ ",s_var,"\n CS ~ ",i_var),
             .default = NA)
           ) %>% 
    select(shock_lab) %>% 
    as.vector(.) %>% 
    .[[1]]
  
  return(shock_label)
}


#### Cost Shocks 2x2 Model #####################################################

c_shock2 <- read_csv("output/results_cost_shock_2x2.csv") %>% 
  mutate(shock_type = shock_type(.,c_shock_var))

######## Cost shock for High Openness ##########################################
c0_setting = 0

c_label0 <- c_shock2 %>% 
  distinct(year,country_sector,tot_bwd) %>% 
  mutate(tot_bwd= round(tot_bwd,3),
         label = str_c(country_sector,": Downstr. ",tot_bwd)) %>% 
  filter(year == c0_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

## Plots for stage 1, across all RS values

c_caption_0_1 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."

cplot_0_1 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == c0_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_i_cost_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


cplot_0_2 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == c0_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_s_cost_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


cplot_0_3 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == c0_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_c_cost_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


cplot_0_4 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == c0_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_cost_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


cplot_0_5 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == c0_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption= c_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")

## Plots for all stages, one RS value

c_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero. Constant Returns-to-Scale (1.0) holds for all sectors."

cplot_0_6 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == c0_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_i_cost_shocks_0.8-0.8_year0.png",height = 14,width = 21,units ="cm")


cplot_0_7 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == c0_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_s_cost_shocks_0.8-0.8_year0.png",height = 14,width = 21,units ="cm")


cplot_0_8 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == c0_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_c_cost_shocks_0.8-0.8_year0.png",height = 14,width = 21,units ="cm")


cplot_0_9 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == c0_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_scenarioplot_si_cost_shocks_0.8-0.8_year0.png",height = 14,width = 21,units ="cm")


cplot_0_10 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == c0_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - High Openness",
       caption=c_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_0.8-0.8_year0.png",height = 14,width = 21,units ="cm")

cplot_1 <- ggarrange(cplot_0_1,cplot_0_6,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_i_cost_shocks_RS1.0_year0.png",
       height = 28,width = 21,units ="cm")

cplot_2 <- ggarrange(cplot_0_2,cplot_0_7,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_s_cost_shocks_RS1.0_year0.png",
       height = 28,width = 21,units ="cm")

cplot_2 <- ggarrange(cplot_0_3,cplot_0_8,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_c_cost_shocks_RS1.0_year0.png",
       height = 28,width = 21,units ="cm")

cplot_2 <- ggarrange(cplot_0_4,cplot_0_9,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_si_cost_shocks_RS1.0_year0.png",
       height = 28,width = 21,units ="cm")

cplot_2 <- ggarrange(cplot_0_5,cplot_0_10,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_RS1.0_year0.png",
       height = 28,width = 21,units ="cm")

######## Cost shock for Low Openness ###########################################
c1_setting = 1

c_label1 <- c_shock2 %>% 
  distinct(year,country_sector,tot_bwd) %>% 
  mutate(tot_bwd= round(tot_bwd,3),
         label = str_c(country_sector,": Downstr. ",tot_bwd)) %>% 
  filter(year == c1_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

## Plots for stage 1, across all RS values

c_caption_1_1 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero."

cplot_1_1 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == c1_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_i_cost_shocks_year1.png",height = 14,width = 21,units ="cm")


cplot_1_2 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == c1_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_s_cost_shocks_year1.png",height = 14,width = 21,units ="cm")


cplot_1_3 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == c1_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_c_cost_shocks_year1.png",height = 14,width = 21,units ="cm")


cplot_1_4 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == c1_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_cost_shocks_stage1_year1.png",height = 14,width = 21,units ="cm")


cplot_1_5 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == c1_setting) %>% 
  mutate(shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption= c_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_stage1_year1.png",height = 14,width = 21,units ="cm")

## Plots for all stages, one RS value

c_caption_1_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of cost shocks. \n All distributions are normal with mean zero. Constant Returns-to-Scale (1.0) holds for all sectors."

cplot_1_6 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == c1_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_i_cost_shocks_RS1.0_year1.png",height = 14,width = 21,units ="cm")


cplot_1_7 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == c1_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_s_cost_shocks_RS1.0_year1.png",height = 14,width = 21,units ="cm")


cplot_1_8 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == c1_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_c_cost_shocks_RS1.0_year1.png",height = 14,width = 21,units ="cm")


cplot_1_9 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == c1_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_cost_shocks_0.8-0.8_year1.png",height = 14,width = 21,units ="cm")


cplot_1_10 <- c_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == c1_setting, rs_ind == 1) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,c_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=c_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Downstream Propagation of Labor Cost Shocks - Low Openness",
       caption=c_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_0.8-0.8_year1.png",height = 14,width = 21,units ="cm")

cplot_6 <- ggarrange(cplot_1_1,cplot_1_6,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_i_cost_shocks_RS1.0_year1.png",
       height = 28,width = 21,units ="cm")

cplot_7 <- ggarrange(cplot_1_2,cplot_1_7,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_s_cost_shocks_RS1.0_year1.png",
       height = 28,width = 21,units ="cm")

cplot_8 <- ggarrange(cplot_1_3,cplot_1_8,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_c_cost_shocks_RS1.0_year1.png",
       height = 28,width = 21,units ="cm")

cplot_9 <- ggarrange(cplot_1_4,cplot_1_9,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_si_cost_shocks_RS1.0_year1.png",
       height = 28,width = 21,units ="cm")

cplot_10 <- ggarrange(cplot_1_5,cplot_1_10,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_ci_cost_shocks_RS1.0_year1.png",
       height = 28,width = 21,units ="cm")

#### Demand Shock 2x2 Model ####################################################

d_shock2 <- read_csv("output/results_demand_shock_2x2.csv") %>% 
  mutate(shock_type = shock_type(.,d_shock_var))

######## Demand shock for High Openness ########################################
d0_setting = 0

d_label0 <- d_shock2 %>% 
  distinct(year,country_sector,tot_fwd) %>% 
  mutate(tot_fwd= round(tot_fwd,3),
         label = str_c(country_sector,": Upstr. ",tot_fwd)) %>% 
  filter(year == d0_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

## Plots for stage 1, across all RS values

d_caption_0_1 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."

dplot_0_1 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == d0_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - High Openness",
       caption= d_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_i_demand_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


dplot_0_2 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == d0_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - High Openness",
       caption= d_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_s_demand_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


dplot_0_3 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == d0_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - High Openness",
       caption= d_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_c_demand_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


dplot_0_4 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == d0_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - High Openness",
       caption= d_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_demand_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


dplot_0_5 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == d0_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - High Openness",
       caption= d_caption_0_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_stage1_year0.png",height = 14,width = 21,units ="cm")


## Plots for all stages, one RS value

d_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_0_6 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == d0_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - High Openness",
       caption=d_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_i_demand_shocks_RS0.8_year0.png",height = 14,width = 21,units ="cm")


d_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_0_7 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == d0_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - High Openness",
       caption=d_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_s_demand_shocks_RS0.8_year0.png",height = 14,width = 21,units ="cm")


d_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_0_8 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == d0_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - High Openness",
       caption=d_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_c_demand_shocks_RS0.8_year0.png",height = 14,width = 21,units ="cm")


d_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_0_9 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == d0_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - High Openness",
       caption=d_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_demand_shocks_RS0.8_year0.png",height = 14,width = 21,units ="cm")


d_caption_0_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_0_10 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == d0_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label0)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - High Openness",
       caption=d_caption_0_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_RS0.8_year0.png",height = 14,width = 21,units ="cm")

dplot_1 <- ggarrange(dplot_0_1,dplot_0_6,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_i_demand_shocks_RS0.8_year0.png",
       height = 28,width = 21,units ="cm")

dplot_2 <- ggarrange(dplot_0_2,dplot_0_7,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_s_demand_shocks_RS0.8_year0.png",
       height = 28,width = 21,units ="cm")

dplot_3 <- ggarrange(dplot_0_3,dplot_0_8,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_c_demand_shocks_RS0.8_year0.png",
       height = 28,width = 21,units ="cm")

dplot_4 <- ggarrange(dplot_0_4,dplot_0_9,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_si_demand_shocks_RS0.8_year0.png",
       height = 28,width = 21,units ="cm")

dplot_5 <- ggarrange(dplot_0_5,dplot_0_10,
                      labels = c("a", "b"),
                      ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_RS0.8_year0.png",
       height = 28,width = 21,units ="cm")

######## Demand shock for Low Openness #########################################
d1_setting = 1

d_label1 <- d_shock2 %>% 
  distinct(year,country_sector,tot_fwd) %>% 
  mutate(tot_fwd= round(tot_fwd,3),
         label = str_c(country_sector,": Upstr. ",tot_fwd)) %>% 
  filter(year == d1_setting) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

## Plots for stage 1, across all RS values

d_caption_1_1 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero."

dplot_1_1 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == d1_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption= d_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_i_demand_shocks_year1.png",height = 14,width = 21,units ="cm")


dplot_1_2 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == d1_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption= d_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_s_demand_shocks_year1.png",height = 14,width = 21,units ="cm")


dplot_1_3 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == d1_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption= d_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_firststage_c_demand_shocks_year1.png",height = 14,width = 21,units ="cm")


dplot_1_4 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == d1_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption= d_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_demand_shocks_stage1_year1.png",height = 14,width = 21,units ="cm")


dplot_1_5 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == d1_setting) %>% 
  mutate(shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat_1,x=rs_ind,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Returns-To-Scale",y="Price Volatility",
       title="One-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption= d_caption_1_1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_stage1_year1.png",height = 14,width = 21,units ="cm")


## Plots for all stages, one RS value

d_caption_1_2 <- "Explanation: Plot rows are different variances of initial price changes. Plot columns are different variances of demand shocks. \n All distributions are normal with mean zero. Decreasing Returns-To-Scale (0.8) holds for all sectors."

dplot_1_6 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","I"),
         year == d1_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption=d_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_i_demand_shocks_RS0.8_year1.png",height = 14,width = 21,units ="cm")


dplot_1_7 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","S"),
         year == d1_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption=d_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_s_demand_shocks_RS0.8_year1.png",height = 14,width = 21,units ="cm")


dplot_1_8 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","C"),
         year == d1_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption=d_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/2x2_multistage_c_demand_shocks_RS0.8_year1.png",height = 14,width = 21,units ="cm")


dplot_1_9 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","SI"),
         year == d1_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption=d_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_si_demand_shocks_RS0.8_year1.png",height = 14,width = 21,units ="cm")


dplot_1_10 <- d_shock2 %>% 
  filter(shock_type %in% c("NULL","CI"),
         year == d1_setting, rs_ind == 0.8) %>%
  pivot_longer(cols = starts_with("price_volat"),
               names_to ="stage",values_to="price_volat") %>% 
  mutate(stage = as.numeric(str_sub(stage,start=-1)),
         shock_label = shock_labels(.,d_shock_var,"CSI"),
         p_shock_var = str_c("P ~ ",p_shock_var)) %>% 
  ggplot(aes(y=price_volat,x=stage,color=country_sector))+
  geom_line(stat = "identity")+
  facet_grid(rows=vars(p_shock_var),cols=vars(shock_label))+
  scale_colour_discrete(name="Country-Sector",
                        breaks=c("A_s1","A_s2","B_s1","B_s2"),
                        labels=d_label1)+
  labs(x="Value Chain Stage Propagation",y="Price Volatility",
       title="Multi-stage Upstream Propagation of Demand Shocks - Low Openness",
       caption=d_caption_1_2)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_RS0.8_year1.png",height = 14,width = 21,units ="cm")

dplot_6 <- ggarrange(dplot_1_1,dplot_1_6,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_i_demand_shocks_RS0.8_year1.png",
       height = 28,width = 21,units ="cm")

dplot_7 <- ggarrange(dplot_1_2,dplot_1_7,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_s_demand_shocks_RS0.8_year1.png",
       height = 28,width = 21,units ="cm")

dplot_8 <- ggarrange(dplot_1_3,dplot_1_8,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_c_demand_shocks_RS0.8_year1.png",
       height = 28,width = 21,units ="cm")

dplot_9 <- ggarrange(dplot_1_4,dplot_1_9,
                     labels = c("a", "b"),
                     ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_si_demand_shocks_RS0.8_year1.png",
       height = 28,width = 21,units ="cm")

dplot_10 <- ggarrange(dplot_1_5,dplot_1_10,
                      labels = c("a", "b"),
                      ncol = 1, nrow = 2)
ggsave("graphs/2x2_scenarioplot_ci_demand_shocks_RS0.8_year1.png",
       height = 28,width = 21,units ="cm")


