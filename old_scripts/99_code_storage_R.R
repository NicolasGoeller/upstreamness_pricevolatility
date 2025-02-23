
library(tidyverse)
library(reshape2)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)

##################### Create categories for subsetting #########################

upstream_17sec_df <- read_csv("data/analytics/upstream_sec17_long.csv")

sectors <- unique(upstream_17sec_df$country_ind)
countries <- unique(upstream_17sec_df$country)
countries <- countries[countries != "ROW"]

#sector_color_pal <- c("#F0E442","#D55E00","#E69F00","#CC79A7","#FF0000","#A020F0",
#                      "#00CED1","#56B4E9","#00008B","#00FF00","#009E73","#E9967A",
#                      "#000000","#999999","#ADFF2F","#B0E0E6","#EEE8AA")
#sector_color_pal <- c("tomato2","green","darkgreen","lightblue","darkblue","purple",
#                      "orange","yellow","turquoise","grey","black","saddlebrown",
#                      "khaki","pink","violet","darkred","greenyellow")
sector_color_pal <- c("green","yellow","lightblue","red","orange","magenta","pink",
                      "darkgreen","khaki","blue","darkred","goldenrod4","purple","deeppink",
                      "grey","black","turquoise")

################################################################################
#################### Univariate plots price volatility #########################
################################################################################

ppi_sec17_volat <- read_csv("data/analytics/ppi_sec17_volat.csv")

violin_ppi <- ppi_sec17_volat %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=country,y=volat_ppi))+
  geom_boxplot()
ggsave("graphs/ppivolat_country_violin.png",height = 9*2, width=16*2, units="cm")

violin_ppi2 <- ppi_sec17_volat %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=country_ind,y=volat_ppi))+
  geom_boxplot()
ggsave("graphs/ppivolat_sector_violin.png",height = 9*2, width=16*2, units="cm")

### Build in y axis limits
ppivolat_line1 <- ppi_sec17_volat %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=year,y=volat_ppi,color=country_ind))+
  geom_line()+
  facet_wrap(~country)+
  scale_color_manual(values = sector_color_pal)+
  ylim(NA, 0.15)
ggsave("graphs/ppivolat_line_country.png",width = 16*2, height=16*2, units="cm")

ppivolat_line2 <- ppi_sec17_volat %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=year,y=volat_ppi,color=country))+
  geom_line()+
  facet_wrap(~country_ind)+
  ylim(NA, 0.15)
ggsave("graphs/ppivolat_line_sector.png",width = 16*2, height=16*2, units="cm")

################################################################################
###################### Univariate plots upstreamness ###########################
################################################################################

upstream_full_df <- read_csv("data/analytics/upstream_full_long.csv")

violin_upstream1 <- upstream_full_df %>% 
  filter(country %in% countries) %>% 
  filter(country_ind %in% sectors) %>% 
  ggplot(aes(x=country,y=upstream))+
  geom_violin()
ggsave("graphs/upstreamness_country_violin.png",height = 9*2, width=16*2, units="cm")

violin_upstream2 <- upstream_full_df %>% 
  filter(country %in% countries) %>%
  filter(country_ind %in% sectors) %>% 
  ggplot(aes(x=country_ind,y=upstream))+
  geom_violin()
ggsave("graphs/upstreamness_sector_violin.png",height = 9*2, width=16*2, units="cm")

### Build in y axis limits
upstream_line1 <- upstream_full_df %>% 
  filter(country_ind %in% sectors) %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x=year,y=upstream,color=country_ind))+
  geom_line()+
  facet_wrap(~country)+
  scale_color_manual(values = sector_color_pal)#+ylim(NA, 15)
ggsave("graphs/upstream17sec_line_country.png",width = 16*2, height=16*2, units="cm")

upstream_line2 <- upstream_full_df %>% 
  filter(country_ind %in% sectors) %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x=year,y=upstream,color=country))+
  geom_line()+
  facet_wrap(~country_ind)#+ylim(NA, 15)
ggsave("graphs/upstream17sec_line_sector.png",width = 16*2, height=16*2, units="cm")

################################################################################
################# Bivariate plots upstreamness-volatility ######################
################################################################################

upstream_volat_17_df <- read_csv("data/analytics/upstream_ppivolat_sec17.csv")

upstream_volat_17_df %>% 
  filter(country == "CHN",year == 2010) %>% 
  ggplot(aes(x=upstream,y=volat_ppi))+
  geom_point()


volatup_scatter1 <- upstream_volat_17_df %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=upstream,y=volat_ppi,color=country))+
  geom_point()+
  facet_wrap(~country_ind)
ggsave("graphs/ppivolat_upstream_sector_scatter.png",width = 9*2, height=16*2, units="cm")

volatup_scatter2 <- upstream_volat_17_df %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=upstream,y=volat_ppi,color=country_ind))+
  geom_point()+
  facet_wrap(~country)+
  scale_color_manual(values = sector_color_pal)
ggsave("graphs/ppivolat_upstream_country_scatter.png",width = 9*2, height=16*2, units="cm")

dvolatup_scatter1 <- upstream_volat_17_df %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=d_upstream,y=d_volatppi,color=country_ind))+
  geom_point()+
  facet_wrap(~country)+
  scale_color_manual(values = sector_color_pal)
ggsave("graphs/d_ppivolat_d_upstream_country_scatter.png",width = 9*2, height=16*2, units="cm")

dvolatup_scatter2 <- upstream_volat_17_df %>% 
  filter(country != "ROW") %>% 
  ggplot(aes(x=d_upstream,y=d_volatppi,color=country))+
  geom_point()+
  facet_wrap(~country_ind)
ggsave("graphs/d_ppivolat_d_upstream_sector_scatter.png",width = 9*2, height=16*2, units="cm")

################################################################################
################# Upstreamness-volatility correlations #########################
################################################################################

total_output_df <- read_csv("data/analytics/total_output_weights.csv")

########################### Data preparation ###################################

upstream_volat_ccor <- upstream_volat_17_df %>% 
  left_join(total_output_df, by=c("year","country","country_ind")) %>% 
  group_by(country,country_ind) %>% 
  mutate(sector_ouput = sum(total_output)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(sector_share = sector_ouput/sum(total_output),
         weight = total_output/sum(total_output)) %>% 
  summarise(hhi = sum(unique(sector_share)^2),
            hhi_norm = (hhi - 1/n_distinct(country_ind))/ (1 - 1/n_distinct(country_ind)),
            corr = cor(volat_ppi,upstream),
            adj_corr = cov.wt(data.frame(volat_ppi,upstream), weight,
                              cor = TRUE)$cor[1,2],
            check = sum(weight),
            diff = corr - adj_corr)

upstream_volat_scor <- upstream_volat_17_df %>% 
  left_join(total_output_df, by=c("year","country","country_ind")) %>% 
  group_by(country_ind) %>% 
  mutate(weight = total_output/sum(total_output)) %>% 
  summarise(corr = cor(volat_ppi,upstream),
            adj_corr = cov.wt(data.frame(volat_ppi,upstream), weight,
                              cor = TRUE)$cor[1,2],
            check = sum(weight))

upstream_volat_cscor <- upstream_volat_17_df %>% 
  left_join(total_output_df, by=c("year","country","country_ind")) %>% 
  group_by(country,country_ind) %>% 
  mutate(weight = total_output/sum(total_output)) %>% 
  summarise(corr = cor(volat_ppi,upstream),
            adj_corr = cov.wt(data.frame(volat_ppi,upstream), weight,
                              cor = TRUE)$cor[1,2],
            check = sum(weight))

################################# Plotting #####################################

corr_country_violin <- upstream_volat_ccor %>% 
  ggplot(aes(x=country,y=adj_corr))+
  geom_violin()
ggsave("graphs/ppivolat_upstream_countryviolin.png", width = 16*2, height=9*2, units="cm")

corr_sector_violin <- upstream_volat_scor %>% 
  ggplot(aes(x=country_ind,y=adj_corr))+
  geom_violin()
ggsave("graphs/ppivolat_upstream_sectorviolin.png", width = 16*2, height=9*2, units="cm")

# Some country-sector values are not depicted
# because all price data is imputed, hence 
# constant and with 0 std dev
corr_heatmap <- upstream_volat_cscor %>% 
  ggplot(aes(x=country,y=country_ind))+
  geom_tile(aes(fill=adj_corr))
ggsave("graphs/ppivolat_upstream_heatmap.png", width = 16*2, height=9*2, units="cm")

################################################################################
##################### Country-specific visualisations #########################
################################################################################

country_viz <- function(data, cor_data, country_select){
  country_scatter <- data %>% 
    filter(country == country_select) %>% 
    ggplot(aes(x=upstream,y=volat_ppi,color=country_ind))+
    geom_point()+
    scale_color_manual(values = sector_color_pal)
  
  ppivolat_line <- data %>% 
    filter(country == country_select) %>% 
    ggplot(aes(x=year,y=upstream,color=country_ind))+
    geom_line()+
    scale_color_manual(values = sector_color_pal)
  
  upstream_line <- data %>% 
    filter(country == country_select) %>% 
    ggplot(aes(x=year,y=volat_ppi,color=country_ind))+
    geom_line()+
    scale_color_manual(values = sector_color_pal)
  
  corr <- cor_data %>% 
    filter(country == country_select) %>% 
    mutate(adj_corr = round(adj_corr,3),
           corr = round(corr,3)) %>% 
    select(corr, adj_corr) 
  
  titletext <- paste0("Upstreamness-Volatility: ",country_select," (Correlation: ",corr[[1]],", Adj. Correlation: ",corr[[2]],")")
  plot_final <- grid.arrange(grobs= list(upstream_line,
                                         ppivolat_line,
                                         country_scatter), 
                             nrow = 1, ncol = 3, layout_matrix= rbind(c(1,2,3)),
                             top = titletext)
  name <- paste0("graphs/ppivolat_upstream_",tolower(country_select),".png")
  ggsave(name, plot_final, width = 3*16, height=13, units="cm")
  
}

### Low correlation
country_viz(upstream_volat_17_df,upstream_volat_ccor,"JPN")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"USA")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"DEU")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"SWE")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"SVN")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"CHN")

### High correlation
country_viz(upstream_volat_17_df,upstream_volat_ccor,"RUS")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"MEX")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"CAN")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"AUS")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"NLD")

country_viz(upstream_volat_17_df,upstream_volat_ccor,"GBR")

up_volat <- upstream_volat_17_df %>% 
  ggplot(aes(x=upstream,y=volat_ppi))+
  geom_point()
ggsave("graphs/ppivolat_upstream_total.png", width = 16, height=9, units="cm")


up_volat2000 <- upstream_volat_17_df %>% 
  filter(year == 2000) %>% 
  ggplot(aes(x=upstream,y=volat_ppi))+
  geom_point()
ggsave("graphs/ppivolat_upstream_2000.png", width = 16, height=9, units="cm")

up_volat2010 <- upstream_volat_17_df %>% 
  filter(year == 2010) %>% 
  ggplot(aes(x=upstream,y=volat_ppi))+
  geom_point()
ggsave("graphs/ppivolat_upstream_2010.png", width = 16, height=9, units="cm")


## rs_ind constant across sectors
d_shock2 %>% 
  #filter(run %in% c(0:4,9:13,18:22)) %>% 
  #filter(d_shock_var == "normal-0-0-0.05") %>% 
  filter(d_shock_var %in% c("normal-0-0-0.05","normal-0-0-0.1","normal-0-0-0.15")) %>%
  ggplot(aes(y=price_volat,x=rs_ind,color=country_sector))+
  geom_point()+
  facet_grid(rows=vars(p_shock_var),cols=vars(d_shock_var))
#facet_wrap(~p_shock_var)
ggsave("graphs/demand_shock_volatility_2x2.png",width = 16, height=9, units="cm")

c_shock2 %>% 
  filter(run %in% c(0:4,9:13,18:22)) %>% 
  ggplot(aes(y=price_volatility,x=rs_ind,color=country_sector))+
  geom_line()+
  facet_wrap(~c_shock_var)
ggsave("graphs/cost_shock_volatility_2x2.png",width = 16, height=9, units="cm")


## rs_ind variable across sectors
c_shock2 %>% 
  filter(!(run %in% c(0:4,9:13,18:22))) %>% 
  mutate(case = case_when(
    run %in% c(5,14,23) ~ "upstream RS=0.9",
    run %in% c(6,15,24) ~ "upstream RS=0.8",
    run %in% c(7,16,25) ~ "upstream RS=1.1",
    run %in% c(8,17,26) ~ "upstream RS=1.2",
  )) %>% 
  ggplot(aes(y=price_volatility,x=rs_ind,color=case)) +
  geom_point()+
  facet_wrap(~c_shock_var)
ggsave("graphs/cost_shock_volatility_2x2_scale.png",width = 16, height=9, units="cm")


d_shock2 %>% 
  filter(!(run %in% c(0:4,9:13,18:22))) %>% 
  mutate(case = case_when(
    run %in% c(5,14,23) ~ "upstream RS=0.9",
    run %in% c(6,15,24) ~ "upstream RS=0.8",
    run %in% c(7,16,25) ~ "upstream RS=1.1",
    run %in% c(8,17,26) ~ "upstream RS=1.2",
  )) %>% 
  ggplot(aes(y=price_volatility,x=rs_ind,color=case)) +
  geom_point()+
  facet_wrap(~d_shock_var)


#### Plot with downstreamness/ upstreamness

d_shock2 %>% 
  #filter(d_shock_var %in% c("normal-0-0-0.05","normal-0-0-0.1","normal-0-0-0.15")) %>%
  #filter(d_shock_var %in% c("normal-0.04-0-0.01","normal-0.025-0-0.025","normal-0.01-0-0.04")) %>% 
  filter(d_shock_var %in% c("normal-0-0.04-0.01", "normal-0-0.025-0.025","normal-0-0.01-0.04")) %>% 
  ggplot(aes(y=price_volat,x=tot_fwd,
             color=factor(rs_ind))) +
  geom_line()+
  facet_grid(row=vars(p_shock_var),col=vars(d_shock_var))

c_shock2 %>% 
  filter(!(run %in% c(0:4,9:13,18:22))) %>% 
  mutate(case = case_when(
    run %in% c(5,14,23) ~ "upstream RS=0.9",
    run %in% c(6,15,24) ~ "upstream RS=0.8",
    run %in% c(7,16,25) ~ "upstream RS=1.1",
    run %in% c(8,17,26) ~ "upstream RS=1.2",
  )) %>% 
  ggplot(aes(y=price_volatility,x=downstreamness,
             color=factor(rs_ind))) +
  geom_point()+
  facet_grid(row=vars(case),col=vars(c_shock_var))
ggsave("graphs/cost_upstreamness_volatility_2x2.png",width = 16, height=9, units="cm")

