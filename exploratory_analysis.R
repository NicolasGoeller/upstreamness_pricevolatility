library(tidyverse)
library(tidyquant)


df %>% 
  mutate(a = if_else(nchar(as.character(a)) == 1, paste0("0", as.character(a)), as.character(a)),
         a = if_else(nchar(as.character(b)) == 1, paste0("0", as.character(b)), as.character(b)),
         date = paste0("2010", a, b),
         date2 = as.Date(date, format= "%Y%m%d"))


test <- read_csv("Data/CNR_QJE_handm.csv")

glimpse(test)

test %>% 
  select(id, country, price) %>% 
  group_by(country, id, )

#### Create plot for PPP data
ppp <- read_csv("Data/pricestats_bpp_series.csv")

ppp_volat <- ppp %>% 
  filter(country %in% c("CHINA_S","BRAZIL","SOUTHAFRICA","ARGENTINA",
                        "GERMANY", "USA","UK","JAPAN")) %>% 
  group_by(country) %>% 
  mutate(ps_diff = annualPS - lag(annualPS,n=1),
         ps_change = ps_diff/lag(annualPS,n=1)) %>% 
  filter(!is.na(annualPS))

Sys.setlocale("LC_ALL","English")
plot_names <- c('CHINA_S' = "China",
                'BRAZIL' = "Brazil",
                'SOUTHAFRICA' = "South Africa",
                'ARGENTINA' = "Argentina",
                'GERMANY' = "Germany",
                'USA' = "USA",
                'UK' = "United Kingdom",
                'JAPAN' = "Japan")

 ppp %>% 
  filter(country %in% c("CHINA_S","BRAZIL","SOUTHAFRICA","ARGENTINA",
                        "GERMANY", "USA","UK","JAPAN")) %>% 
  mutate(date = as.Date(date, format= "%d%b%Y")) %>% 
  group_by(country) %>% 
  mutate(ps_diff = annualPS - lag(annualPS,n=1),
         ps_change = ps_diff/lag(annualPS,n=1),
         pc_2 = annualPS/lag(annualPS, n=1) - 1) %>% 
  filter(!is.na(annualPS))
  ggplot(aes(x=date, y=ps_diff))+
  geom_line()+
  facet_wrap(~country, nrow= 2, labeller = as_labeller(plot_names))+
  labs(title="Online CPI Index - First Difference",
       x= "Date", y= "Online CPI")+
  theme_light()
ggsave("country_agg_pricevolatility.png", device="png", 
       width= 16, height= 9, units= "cm")
  
#### Create plot for apple data
apple_data <- read_csv("Data/CNR_QJE_apple.csv")
head(apple_data)

apple_data %>% 
  filter(country %in% c("de","fr","at","be","it")) %>% 
  mutate(date = as.Date(date, format= "%d %b %y")) %>% 
  select(country, date, price) %>% 
  group_by(country) %>% 
  summarise(m = mean(price))
# %>% 
  ggplot(aes(x=date, y=price, color=country))+
  geom_line(show.legend = FALSE)

price_volat <- apple_data %>% 
  select(country, id, price) %>% 
  filter(country != "ph") %>% 
  group_by(country,id) %>% 
  mutate(price_diff = price - lag(price, n=1),
         price_change = price_diff/lag(price, n=1),
         pc_2 = price/lag(price,n=1)) %>% 
  summarise(price_sd = sd(price_change, na.rm = T))

price_volat %>% 
  ggplot(aes(x=country, y=price_sd))+
  geom_boxplot()+
  labs(title="Std. Dev of percent price change of products (Apple)",
       x= "Country", y= "Std. Dev of products")+
  theme_light()


ggsave("country_product_pricevolatility.png", device="png", 
       width= 16, height= 9, units= "cm")

agg_price_volat <- apple_data %>% 
  group_by(country, id) %>% 
  mutate(price_diff = price - Lag(price, k=1)[,1]) %>% 
  summarise(sd_price = sd(price),
            sd_pricediff = sd(price_diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  summarise(p_sd_mean = mean(sd_price, na.rm=TRUE),
            p_sd_median = median(sd_price, na.rm=TRUE),
            p_sd_sd = sd(sd_price, na.rm=TRUE),
            p_sd_min = min(sd_price, na.rm=TRUE),
            p_sd_max = max(sd_price, na.rm=TRUE),
            pd_sd_mean = mean(sd_pricediff, na.rm=TRUE),
            pd_sd_median = median(sd_pricediff, na.rm=TRUE),
            pd_sd_sd = sd(sd_pricediff, na.rm=TRUE),
            pd_sd_min = min(sd_pricediff, na.rm=TRUE),
            pd_sd_max = max(sd_pricediff, na.rm=TRUE))
  


glimpse(apple_data)

test = apple_data %>% 
  group_by(country, id) %>% 
  mutate(price_diff = price - Lag(price, k=1)[,1]) %>% 
  summarise(sd_price = sd(price),
            sd_pricediff = sd(price_diff, na.rm = TRUE)) %>% 
  ungroup() 

apple_data %>% 
  filter(country == "uk") %>% 
  mutate(date = as.Date(date, format= "%d %b %y")) %>% 
  select(id, date, price) %>% 
  ggplot(aes(x=date, y=price, color=id))+
  geom_line(show.legend = FALSE)

apple_data %>% 
  filter(id == "FB133") %>% 
  mutate(date = as.Date(date, format= "%d %b %y")) %>% 
  select(country, date, price) %>% 
  ggplot(aes(x=date, y=price, color=country))+
  geom_line(show.legend = FALSE)



apple_data %>% 
  filter(country == "us") %>% 
  select(price, id)



# %>% 
  arrange(desc(sd_price)) %>% 
  ggplot(aes(x=country, y=sd_price, ))+
  geom_boxplot()

apple_data %>% 
  group_by(id) %>% 
  summarise(n_countries = n_distinct(country)) %>% 
  filter(n_countries > 10) %>% 
  arrange(n_countries)


apple_data %>% 
  group_by(country) %>% 
  summarise(n_products= n_distinct(id)) %>% 
  arrange(n_products)

apple_data %>% 
  summarise(n_products = n_distinct(id)) 

apple_data %>% 
  select(country) %>% 
  distinct() %>% 
  tail()


cn <- ppp[ppp$country == "CHINA_S",]
uk <- ppp[ppp$country == "UK",]
za <- ppp[ppp$country == "SOUTHAFRICA",]
de <- ppp[ppp$country == "GERMANY",]
us <- ppp[ppp$country == "USA",]
bz <- ppp[ppp$country == "BRAZIL",]

plot(cn$annualCPI)

plot(de$annualCPI)

summary(za$annualCPI)
summary(bz$annualCPI)

colnames(ppp)
