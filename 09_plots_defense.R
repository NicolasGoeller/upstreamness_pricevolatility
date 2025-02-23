library(tidyverse)
library(tidyquant)

cs_info_df <- read_csv("output/countrysector_information.csv") %>%
  mutate(tot_gvc = tot_fwd + tot_bwd)

cs_info_df %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarise(fwd_final = cor(tot_fwd,final_share),
            bwd_va = cor(tot_bwd,va_share)) %>% 
  ungroup() %>% 
  pivot_longer(2:3, names_to = "corr",values_to = "value") %>% 
  ggplot(aes(x=year,y=value,color=corr))+
  geom_line()+
  scale_color_manual(labels=c("Upstreamness-Final share","Downstreamness - VA share"),
                     values=c(1:2))+
  labs(title= "Correlations between GVC position and direct exposure share",
       color='Observed Correlation',
       y="Pearson Correlation Coefficient",x="Year",
       caption="Pearson Correlation Coefficient for country-sector values computed for each year. The black line shows the correlation \n between upstremaness and the share of final demand. The red line shows the correlation of downstreamness with \n value added share (primary input share).")+
  theme_light()+
  theme(legend.position = "bottom",plot.caption = element_text(hjust = 0))
ggsave("graphs/shares_pos.png",width=16,height=12,units="cm")

cs_info_df %>% 
  ggplot(aes(x=country,y=ppi_volat_std))+
  geom_violin()+
  labs(x="Country",y="Empirical Price Volatility",
       title="Country-wise distributions of country-sector price volatilities")+
  #coord_flip()+
  theme_light()
ggsave("graphs/volat_country_dist_flip.png", width = 25, height=15, units="cm")


