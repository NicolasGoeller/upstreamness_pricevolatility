
library(tidyverse)
library(tidyquant)
library(fixest)
library(modelsummary)
library(ggpubr)
library(ggridges)

### Read in data and transform with logs
cs_info_df <- read_csv("output/countrysector_information.csv") %>%
  mutate(tot_gvc = tot_fwd + tot_bwd)

################### Run regression to determine fixed effects ##############

### Run regressions with various fixed effects
colnames <- c("(1)","(2)","(3)","(4)","(5)","(6)")#,"(7)","(8)")
rownames <- c("tot_bwd"="Downstreamness",
              "tot_fwd"="Upstreamness",
              "tot_gvc"="GVC length")
gm <- data.frame("raw"= c("FE: country","FE: country_ind","FE: factor(year)",
                          "FE: country^factor(year)","FE: country_ind^factor(year)",
                          "FE: country_sector","nobs","r.squared","adj.r.squared",
                          "df.residual"),
                 "clean"=c("FE: Country","FE: Sector","FE: Year","FE: Country x Year",
                           "FE: Sector x Year","FE: Country x Sector",
                           "Num.Obs.","R2","Adj. R2","DF Residual"),
                 "fmt" = c(0,0,0,0,0,0,0,3,3,0))
lhs <- c("ppi_volat_var","ppi_volat_std")
rhs <- c("tot_bwd","tot_fwd","tot_gvc")

### RHS: tot_fwd, tot_bwd
m1 <- feols(c(ppi_volat_std,ppi_volat_var) ~ tot_bwd + tot_fwd | 
              csw0(country,country_ind,factor(year),country_sector), 
            data = cs_info_df)
m2 <- feols(c(ppi_volat_std,ppi_volat_var) ~ tot_bwd + tot_fwd | 
              country^factor(year) + country_ind^factor(year), 
            data = cs_info_df)
#m3 <- feols(c(ppi_volat_var,ppi_volat_std) ~ tot_bwd + tot_fwd | 
#              csw(country_sector, factor(year)), data = cs_info_df)
#m4 <- feols(c(ppi_volat_var,ppi_volat_std) ~ tot_bwd + tot_fwd | 
#              csw(country, country_ind,country_sector, factor(year)), 
#            data = cs_info_df)


### RHS: several combinations of the above
m5 <- feols(ppi_volat_std ~ sw(tot_bwd,tot_fwd,tot_gvc) | 
              csw0(country,country_ind,factor(year),country_sector),
            data = cs_info_df)
#m6 <- feols(c(ppi_volat_var,ppi_volat_std) ~ sw(tot_bwd,tot_fwd,tot_gvc) | 
#              country^factor(year) + country_ind^factor(year), data = cs_info_df)
#m7 <- feols(c(ppi_volat_var,ppi_volat_std) ~ sw(tot_bwd,tot_fwd,tot_gvc) | 
#              csw(country_sector, factor(year)), data = cs_info_df)
#m8 <- feols(c(ppi_volat_var,ppi_volat_std) ~ sw(tot_bwd,tot_fwd,tot_gvc) | 
#              csw(country, country_ind,country_sector,factor(year)), data = cs_info_df)


### Year-wise sample split
m9 <- feols(ppi_volat_std ~ tot_bwd + tot_fwd | 
              csw0(country, country_ind),
            fsplit = ~factor(year), data = cs_info_df)

### Create tables with regression results - baseline
base_models <- c(
  m1[lhs = "ppi_volat_std"],
  m2[lhs = "ppi_volat_std"]
  ) %>% list_flatten()
names(base_models) <- colnames
out_name1 = "tables/reg_ppi_volat_std_base.tex"
modelsummary(base_models,stars=TRUE,vcov="robust",
             coef_rename=rownames,gof_map=gm,
             output = "latex_tabular",
             coef_omit = "Intercept")

### tables for singualr variables
m5_1 <- c(m5[rhs = "tot_fwd"]) %>%  list_flatten()
names(m5_1) <- colnames[1:5]
out_name2 = "tables/reg_ppi_volat_std_tot_fwd.tex"
modelsummary(m5_1, vcov="robust", stars=TRUE,
             coef_omit = "Intercept",
             gof_map=gm[grep("FE",gm$raw),],
             output = "latex_tabular",
             coef_rename = rownames)

m5_2 <- c(m5[rhs = "tot_bwd"]) %>%  list_flatten()
names(m5_2) <- colnames[1:5]
out_name3 = "tables/reg_ppi_volat_std_tot_bwd.tex"
modelsummary(m5_2, vcov="robust", stars=TRUE,
             coef_omit = "Intercept",
             gof_map=gm[grep("FE",gm$raw),],
             output = "latex_tabular",
             coef_rename = rownames)

m5_3 <- c(m5[rhs = "tot_gvc"]) %>%  list_flatten()
names(m5_3) <- colnames[1:5]
out_name4 = "tables/reg_ppi_volat_std_tot_gvc.tex"
modelsummary(m5_3, vcov="robust", stars=TRUE,
             coef_omit = "Intercept",
             gof_map=gm[grep("FE",gm$raw),],
             output = "latex_tabular",
             coef_rename = rownames)

### Tables for year-wise regressions
split <- c("Full sample",as.character(1995:2011))
fes <- c("No FE","FE: Country","FE: Country + Sector")
year_labels <- expand.grid(split,fes) %>% 
  arrange(Var1) %>% 
  mutate(label = str_c(Var1,Var2,sep="; ")) %>% 
  select(label) %>% 
  as.vector(.) %>% 
  .[[1]]

m9_1 <- c(m9[sample=1:9]) %>% list_flatten()
names(m9_1) <- year_labels[1:27]
out_name5 = "tables/reg_ppi_volat_std_year1995-2002.tex"
modelsummary(m9_1, vcov="robust", stars=TRUE,
             shape= model ~ term + statistic, 
             coef_omit = "Intercept",
             output = "latex_tabular",
             coef_rename = rownames)

m9_2 <- c(m9[sample=10:18]) %>% list_flatten()
names(m9_2) <- year_labels[28:54]
out_name6 = "tables/reg_ppi_volat_std_year2003-2011.tex"
modelsummary(m9_2, vcov="robust", stars=TRUE,
             shape= model ~ term + statistic, 
             coef_omit = "Intercept",
             output = "latex_tabular",
             coef_rename = rownames)

################### Exposure and Production chain position ##############################

cor_plot1 <- cs_info_df %>% 
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

cor_plot2 <- cs_info_df %>% 
  na.omit() %>% 
  group_by(year) %>% 
  summarise(link_cor = cor(tot_fwd,tot_bwd),
            share_cor = cor(final_share,va_share)) %>% 
  ungroup() %>% 
  pivot_longer(2:3, names_to = "corr",values_to = "value") %>% 
  ggplot(aes(x=year,y=value,color=corr))+
  geom_line()+
  scale_color_manual(labels=c("Downstreamness-Upstreamness","VA share - Final share"),
                     values=c(1:2))+
  labs(title= "Correlations between different exposure measures",
       color='Observed Correlation:',
       y="Pearson Correlation Coefficient",x="Year",
       caption="Pearson Correlation Coefficient for country-sector values computed for each year. The black line shows the correlation \n between upstremaness and downstreamness. The red line shows the correlation of final demand share with \n value added share (primary input share).")+
  theme_light()+
  theme(legend.position="bottom",plot.caption = element_text(hjust = 0))

shares_streamness <- ggarrange(cor_plot1,cor_plot2,
                               labels = c("a", "b"),
                               ncol = 2, nrow = 1)
ggsave("graphs/shares_stream.png",width=32,height=12,units="cm")

#cs_info_df %>% 
#  #select(year,sd,dir_bwd,dir_fwd,tot_bwd,tot_fwd) %>% 
#  group_by(year) %>% 
#  summarise(across(.cols = c("dir_bwd","dir_fwd","tot_bwd","tot_fwd"),
#                   .fns= ~cor(.x,resid_year), .names = "cor_{.col}_sd")) %>% 
#  pivot_longer(cols=2:5,values_to = "corr",names_to = "var") %>% 
#  ggplot(aes(x=year,y=corr,color=var))+
#  geom_line()
#


######################## Distribution of price changes ########################

ppi_changes <-read_csv("output/ppi_changes.csv") %>% 
  mutate(yrmon = str_c(year,month,sep = "-"),
         country_sector = str_c(country,country_ind,sep = "_"))

### Price change distribution over country-sector

cs_ppidist <- ppi_changes %>% 
  group_by(country,country_ind) %>% 
  summarise(min = min(ppi_chng),
            pctl25 = quantile(ppi_chng,0.25),
            mean = mean(ppi_chng),
            median = median(ppi_chng),
            pctl75 = quantile(ppi_chng,0.75),
            max = max(ppi_chng),
            sd = sd(ppi_chng), 
            skew = skewness(ppi_chng),
            kurt = kurtosis(ppi_chng,method="moment"))

#cs_ppidist %>% 
#  pivot_longer(cols=3:7,values_to="value",names_to="stat") %>% 
#  filter(stat == "mean") %>% 
#  ggplot(aes(x=value))+
#    geom_histogram()
#
#cs_ppidist %>% 
#  pivot_longer(cols=3:7,values_to="value",names_to="stat") %>% 
#  filter(stat %in% c("pctl25","median","pctl75")) %>% 
#  ggplot(aes(x=value,colour= stat))+
#  geom_density()

ppi_chng_cs_density <- ppi_changes %>% 
  ggplot()+
  geom_density(aes(x=ppi_chng,group=country_sector),
               alpha=0.4,color="grey")+
  stat_function(fun = dnorm, n = 527,  color="red",
                args = list(mean = mean(cs_ppidist$mean), 
                            sd = sd(cs_ppidist$sd)))+
  labs(y="Density in Country-Sector",x="Price Change",
       title="Density plots of price changes per Country-Sector",
       caption="Density plots of producer price changes within each country-sector. The plot was truncated on \n the x-axis at [-0.1,0.1] and at [0,1000] on the y-axis, leading to the exclusion of 195 \n observations. The red curve represents a normal distribution with mean and standard deviation \n equal to the sample values.")+
  xlim(-0.1,0.1)+
  ylim(0,1000)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
#ggsave("graphs/ppi_chng_cs_density_trunc.png",height=9,width=16,units="cm")

### Price Change distribution over Year-month

month_ppidist <- ppi_changes %>% 
  group_by(year,month) %>% 
  summarise(min = min(ppi_chng),
            pctl25 = quantile(ppi_chng,0.25),
            mean = mean(ppi_chng),
            median = median(ppi_chng),
            pctl75 = quantile(ppi_chng,0.75),
            max = max(ppi_chng),
            sd = sd(ppi_chng), 
            skew = skewness(ppi_chng),
            kurt = kurtosis(ppi_chng,method="moment"))

#month_ppidist %>% 
#  pivot_longer(cols=3:7,values_to="value",names_to="stat") %>% 
#  filter(stat %in% c("pctl25","median","pctl75")) %>% 
#  ggplot(aes(x=value,colour= stat))+
#  geom_density()

ppi_chng_mon_density <- ppi_changes %>% 
  ggplot()+
  geom_density(aes(x=ppi_chng,group=yrmon),
               alpha=0.4,color="grey")+
  stat_function(fun = dnorm, n = 204, color="red",
                args = list(mean = mean(month_ppidist$mean), 
                            sd = sd(month_ppidist$sd)))+
  labs(y="Density in Year-Month",x="Price Change",
       title="Density plots of price changes per Year-Month",
       caption="Density plots of producer price changes within each year-month. The plot was truncated on \n the x-axis at [-0.1,0.1], leading to the exclusion of 195 observations. The red curve represents \n a normal distribution with mean and standard deviation equal to the sample values.")+
  xlim(-0.1,0.1)+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))

ppi_chng_density <- ggarrange(ppi_chng_mon_density,
                              ppi_chng_cs_density,
                              labels = c("a", "b"),
                              ncol = 2, nrow = 1)

ggsave("graphs/ppi_chng_density_trunc.png",height=9,width=25,units="cm")



ppi_changes %>% 
  filter(ppi_chng > 0) %>% 
  ggplot()+
  geom_density(aes(x=ppi_chng,group=country_sector),
               alpha=0.4,color="grey")+
  #stat_function(fun = dnorm, n = 527,  color="red",
  #              args = list(mean = mean(cs_ppidist$mean), 
  #                          sd = sd(cs_ppidist$sd)))+
  scale_y_continuous(trans = "log")+
  scale_x_continuous(trans = "log")
  
ppi_changes %>% 
  filter(yrmon == "2008-10") %>%
  #filter(year == 1995) %>% 
  #filter(country_sector == "CHN_e") %>% 
  #filter(country == "RUS") %>% 
  mutate(ppi_chng = abs(ppi_chng)) %>% 
  ggplot()+
  geom_density(aes(x=ppi_chng,group=yrmon),
               alpha=0.4,color="grey")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks()  


######################### Empirical plots about volatility ######################

cs_info_df %>% 
  ggplot(aes(x=factor(year),y=ppi_volat_std))+
  geom_violin()+
  labs(x="Year",y="Empirical Price Volatility",
       title="Year-wise distributions of country-sector price volatilities")+
  theme_light()
ggsave("graphs/volat_year_dist.png", width = 16, height=9, units="cm")

cs_info_df %>% 
  ggplot(aes(x=description,y=ppi_volat_std))+
  geom_violin()+
  labs(x="Sector",y="Empirical Price Volatility",
       title="Sector-wise distributions of country-sector price volatilities")+
  coord_flip()+
  theme_light()
ggsave("graphs/volat_sector_dist.png", width = 20, height=20, units="cm")

cs_info_df %>% 
  ggplot(aes(x=country,y=ppi_volat_std))+
  geom_violin()+
  labs(x="Country",y="Empirical Price Volatility",
       title="Country-wise distributions of country-sector price volatilities")+
  coord_flip()+
  theme_light()
ggsave("graphs/volat_country_dist.png", width = 15, height=20, units="cm")
  
#### Macro fact: volatility-prod pos consistent over time
cs_info_df %>% 
  #select(year,sd,dir_bwd,dir_fwd,tot_bwd,tot_fwd) %>% 
  group_by(year) %>% 
  summarise(across(.cols = c("tot_bwd","tot_fwd"),
                   .fns= ~cor(.x,ppi_volat_std), .names = "cor_{.col}_std")) %>% 
  pivot_longer(cols=2:3,values_to = "corr",names_to = "var") %>% 
  ggplot(aes(x=year,y=corr,color=var))+
  geom_line()+
  scale_color_manual(labels=c("Downstreamness","Upstreamness"),
                     values=c(1:2))+
  labs(color='Correlated with',y="Pearson Correlation Coefficient",x="Year",
       title="Year-wise correlation of price volatility with production chain positions",
       caption="Pearson Correlation Coefficient for country-sector values computed for each year. The black line shows \n the correlation between producer price volatility and downstreamness. The red line shows the correlation \n of producer price volatility with upstreamness.")+
  theme_light()+
  theme(plot.caption = element_text(hjust = 0))
ggsave("graphs/macro_correlations.png", width = 16, height=9, units="cm")





### Aggregate values
vals = final_df %>% 
  group_by(year) %>% 
  filter(country != "ROW") %>% 
  summarise(yr_up_cor = cor(sd,tot_fwd),
            yr_down_cor = cor(sd,tot_bwd)) %>% 
  mutate(year = factor(year))

#### But, there is heterogeneity across countries
final_df %>%
  group_by(year,country) %>% 
  summarise(up_cor = cor(sd,tot_fwd)) %>% 
  ggplot(aes(x=factor(year),y=up_cor))+
  geom_violin()+
  geom_point(data=vals,aes(x=year,y=yr_up_cor))+
  labs(x="Year",y="Correlation: Price volatility - Total FWD",
       title="Distribution of country-individual correlations by year")
#ggsave("graphs/dist_tot_fwd_countrycorrelations.png", width = 16, height=9, units="cm")


final_df %>%
  group_by(year,country) %>% 
  summarise(down_cor = cor(sd,tot_bwd)) %>% 
  ggplot(aes(x=factor(year),y=down_cor))+
  geom_violin()+
  geom_point(data=vals,aes(x=year,y=yr_down_cor))+
  labs(x="Year",y="Correlation: Price volatility - Total BWD",
       title="Distribution of country-individual correlations by year")
#ggsave("graphs/dist_tot_bwd_countrycorrelations.png", width = 16, height=9, units="cm")

#### But, there is heterogeneity across sectors
final_df %>%
  filter(country !="ROW") %>% 
  group_by(year,country_ind) %>% 
  summarise(up_cor = cor(sd,tot_fwd)) %>% 
  ggplot(aes(x=factor(year),y=up_cor))+
  geom_violin()+
  geom_point(data=vals,aes(x=year,y=yr_up_cor))+
  labs(x="Year",y="Correlation: Price volatility - Total FWD",
       title="Distribution of sector-individual correlations by year")
#ggsave("graphs/dist_tot_fwd_sectorcorrelations.png", width = 16, height=9, units="cm")

final_df %>%
  filter(country != "ROW") %>% 
  group_by(year,country_ind) %>% 
  summarise(down_cor = cor(sd,tot_bwd)) %>% 
  ggplot(aes(x=factor(year),y=down_cor))+
  geom_violin()+
  geom_point(data=vals,aes(x=year,y=yr_down_cor))+
  labs(x="Year",y="Correlation: Price volatility - Total BWD",
       title="Distribution of sector-individual correlations by year")
#ggsave("graphs/dist_tot_bwd_sectorcorrelations.png", width = 16, height=9, units="cm")





### Over time, are sectors constant??
final_df %>% 
  filter(country != "ROW") %>% 
  group_by(country,country_ind) %>% 
  summarise(cs_iqr = IQR(sd),
            cs_range = max(sd) - min(sd),
            cs_mean = mean(sd)) %>% 
  ggplot(aes(x=country, y=cs_iqr))+
  geom_violin()

final_df %>% 
  filter(country != "ROW") %>% 
  group_by(country,country_ind) %>% 
  summarise(cs_iqr = IQR(sd),
            cs_range = max(sd) - min(sd),
            cs_mean = mean(sd)) %>% 
  ggplot(aes(x=country_ind, y=cs_iqr))+
  geom_violin()

final_df %>% 
  filter(country != "ROW") %>% 
  group_by(country,country_ind) %>% 
  summarise(cs_iqr = IQR(sd),
            cs_range = max(sd) - min(sd),
            cs_mean = mean(sd)) %>% 
  ggplot(aes(x=cs_mean, y=cs_iqr))+
    geom_point()

final_df %>% 
  select(year,country,country_ind,sd,tot_bwd,tot_fwd) %>% 
  filter(country != "ROW") %>% 
  arrange(country,country_ind) %>% 
  group_by(country,country_ind) %>% 
  mutate(sd_diff = sd - lag(sd),
         tot_bwd_diff = tot_bwd - lag(tot_bwd),
         tot_fwd_diff = tot_fwd - lag(tot_fwd)) %>% 
  group_by(year,country_ind) %>% 
  summarise(cor_fwd = cor(sd_diff,tot_fwd_diff),
            cor_bwd = cor(sd_diff,tot_bwd_diff)) %>% 
  #filter(year==2000) %>% 
  ggplot(aes(x=factor(year),y=cor_fwd))+
  geom_violin()

ppi_changes %>% 
  filter(country %in% c("AUS","USA","DEU","FRA","ITA"),
         country_ind %in% c("atb")) %>% 
  ggplot(aes(x=yrmon,y=chng_ppi,color=country))+
  geom_line()

ppi_changes %>% 
  filter(country == "AUS",country_ind == "atb") %>% 
  ggplot(aes(x=yrmon,y=chng_ppi))+
  geom_line()


final_df %>% 
  group_by(year,country_sector,country,country_ind) %>% 
  summarise(sd = sd(chng_ppi), skew = skewness(chng_ppi),
            kurt = kurtosis(chng_ppi,method="excess"),
            down = mean(tot_bwd),
            up = mean(tot_fwd)) %>% 
  filter(year == 2010,country=="DEU") %>% 
  ggplot(aes(x=down,y=sd,color=country_ind))+
  geom_point()

### Clear pos corr upstream-volat, 
### nega or near zero downstream-volat
### very strong negative cor of upstream/downstream with final_share/labor_share
### Direct linkages fwd & bwd explain most of total linkages in sd-corr

## What is the impact of first-order inputs - what about concentration
## Do I find the mean-volatility and mean-skewness relations in the pure crossections?? 
## can I establish a relation to average upstreamness

ppi_changes %>% 
  filter(year == 2011) %>% 
  ggplot(aes(x=factor(yrmon),y=chng_ppi))+
  geom_violin()
  
ppi_changes %>% 
  group_by(yrmon,country_ind) %>% 
  summarise(mean = mean(chng_ppi),
            skew = skewness(chng_ppi),
            sd = sd(chng_ppi)) %>% 
  ggplot(aes(x=mean,y=skew))+
  geom_point()


final_df %>% 
  filter(year == 2011) %>% 
  group_by(country,country_ind) %>% 
  summarise(sd = sd(chng_ppi), skew = skewness(chng_ppi),
            kurt = kurtosis(chng_ppi,method="excess"),
            down = mean(tot_bwd),
            up = mean(tot_bwd)) %>% 
  ungroup(country_ind) %>% 
  mutate(sd_rank = rank(sd),
         up_rank = rank(up),
         down_rank = rank(down)) %>% 
  ungroup() %>% 
  filter(!(country %in% c("ROM","ROW"))) %>% 
  summarise(up_cor_rr = cor(sd_rank,up_rank),
            down_cor_rr = cor(sd_rank,down_rank),
            up_cor_vr = cor(sd,up_rank),
            down_cor_vr = cor(sd,down_rank),
            up_cor_rv = cor(sd_rank,up),
            down_cor_rv = cor(sd_rank,down),
            up_cor_vv = cor(sd,up),
            down_cor_vv = cor(sd,down))

test = final_df %>% 
  filter(year == 2010) %>% 
  group_by(country,country_ind) %>% 
  summarise(sd = sd(chng_ppi), skew = skewness(chng_ppi),
            kurt = kurtosis(chng_ppi,method="excess"),
            down = mean(tot_bwd),
            up = mean(tot_fwd)) %>% 
  ungroup(country_ind) %>% 
  mutate(sd_rank = rank(sd),
         up_rank = rank(up),
         down_rank = rank(down)) %>% 
  summarise(up_cor_r = cor(sd_rank,up),
            down_cor_r = cor(sd_rank,down),
            up_cor_v = cor(sd,up),
            down_cor_v = cor(sd,down))
  ggplot(aes(x=up, y=sd_rank))+
  geom_point()#+
  scale_color_manual(values = sector_color_pal)
  

