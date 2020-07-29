library(tidyverse)
#part1
Morocco <- DEVECON_fulldataset %>% filter(countrycode == "MAR")
Morocco_now <- Morocco %>% filter(year == 2018)
MAR_income <- Morocco_now$WB_ny_gdp_pcap_kd
MAR_pop <- Morocco_now$WB_sp_pop_totl

#neighbors are north african countries excluding Libya because of civil war
neighbors <- DEVECON_fulldataset %>% 
  filter(countrycode == "MAR"|countrycode == "EGY" | countrycode =="TUN" |countrycode =="DZA") 


#Criteria for similarity: gdp per capita within 1000 USD, population above 10 million, less than 240 million
similar_dev1 <- DEVECON_fulldataset %>% filter(year == 2018) %>%
  filter(WB_ny_gdp_pcap_kd <= MAR_income +1000) %>% filter(WB_ny_gdp_pcap_kd >= MAR_income - 1000) %>%
  filter(WB_sp_pop_totl <= MAR_pop + 2e+08 ) %>% filter(WB_sp_pop_totl >= MAR_pop - 2.5e+07 )
similar_dev1 <- list(similar_dev1$countrycode)

similar_dev <- DEVECON_fulldataset %>% 
  filter(countrycode == "AGO"| countrycode == "BOL"| countrycode == "EGY"| countrycode == "GTM"| countrycode == "LKA"| 
           countrycode == "MAR"| countrycode == "NGA"| countrycode == "PHL"| countrycode == "UKR")

#plots of development for neighbors
neighbors_gdp_percap_g <- neighbors %>% select(WB_ny_gdp_pcap_kd_zg, year, countrycode)
neighbors_gdp_percap_g <- melt(neighbors_gdp_percap_g, id.vars = c("year", "countrycode"))

plot_neighbor_gdp_percap_g <- ggplot(neighbors_gdp_percap_g) +geom_line(aes(year, value, colour = countrycode)) +
  scale_x_continuous(limits = c(1960, 2020)) + ylab("GDP per capita growth rate") + xlab("Year") +
  labs(title = "GDP per capita Growth Rate vs Neighbors, 1960-", color = "Countries \n")+
  theme_bw()

plot_neighbor_gdp_percap_g1 <- ggplot(neighbors_gdp_percap_g) +geom_line(aes(year, value, colour = countrycode)) +
  scale_x_continuous(limits = c(1995, 2020)) + ylab("GDP per capita growth rate") + xlab("Year") +
  labs(title = "GDP per capita Growth Rate vs Neighbors, 1995-", color = "Countries \n")+ 
  scale_y_continuous(limits = c(-10, 15))+
  theme_bw()
  

#plots of development for peers
similar_dev_gdp_percap_g <- similar_dev %>% select(WB_ny_gdp_pcap_kd_zg, year, countrycode)
similar_dev_gdp_percap_g <- melt(similar_dev_gdp_percap_g, id.vars = c("year", "countrycode"))

plot_similar_gdp_percap_g <- ggplot(similar_dev_gdp_percap_g) +geom_line(aes(year, value, colour = countrycode)) +
  scale_x_continuous(limits = c(1960, 2020)) + ylab("GDP per capita growth rate") + xlab("Year") +
  labs(title = "GDP per capita Growth Rate vs Peers, 1960-", color = "Countries \n")+
  theme_bw()

plot_similar_gdp_percap_g1 <- ggplot(similar_dev_gdp_percap_g) +geom_line(aes(year, value, colour = countrycode)) +
  scale_x_continuous(limits = c(1995, 2020)) + ylab("GDP per capita growth rate") + xlab("Year") +
  labs(title = "GDP per capita Growth Rate vs Peers, 1995-", color = "Countries \n")+ 
  scale_y_continuous(limits = c(-15, 15))+
  theme_bw()


#As benchmark I select Argentina, Turkey, Chile, Spain of varying degrees of development
benchmark <- DEVECON_fulldataset %>% 
  filter(countrycode == "MAR"| countrycode == "ARG"| countrycode == "TUR"| countrycode == "CHL"| countrycode == "ESP") 

benchmark_gdp_percap <- benchmark %>% select(WB_ny_gdp_pcap_cd, year, countrycode)
benchmark_gdp_percap <- melt(benchmark_gdp_percap, id.vars = c("year", "countrycode"))

benchmark_gdp_percap_PPP <- benchmark %>% select(WB_ny_gdp_mktp_pp_cd, year, countrycode)
benchmark_gdp_percap_PPP <- melt(benchmark_gdp_percap_PPP, id.vars = c("year", "countrycode"))

benchmark_gdp_percap_g  <- benchmark %>% select(WB_ny_gdp_pcap_kd_zg, year, countrycode)
benchmark_gdp_percap_g  <- melt(benchmark_gdp_percap_g, id.vars = c("year", "countrycode"))

#plot of benchmark gdp percap
plot_benchmark_gdp_percap <- ggplot(benchmark_gdp_percap) + geom_line(aes(year, value, colour = countrycode))+
  ylab("GDP per capita, current USD") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "GDP per capita vs Benchmarks, current USD", color = "Countries \n") +
  theme_bw()

#plot of benchmark gdp percap, PPP
plot_benchmark_gdp_percap_PPP <- ggplot(benchmark_gdp_percap_PPP) + geom_line(aes(year, value, colour = countrycode))+
  ylab("GDP per capita, PPP") + xlab("Year")+ scale_x_continuous(limits = c(1990, 2020))+
  labs(title = "GDP per capita vs Benchmarks, PPP", color = "Countries \n") +
  theme_bw()

#plot of benchmark gdp percap growth
plot_benchmark_gdp_percap_g <- ggplot(benchmark_gdp_percap_g) + geom_line(aes(year, value, colour = countrycode))+
  ylab("GDP per capita Growth ") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "GDP per capita growth vs Benchmarks, 1960-", color = "Countries \n") +
  theme_bw()

plot_benchmark_gdp_percap_g1 <- ggplot(benchmark_gdp_percap_g) + geom_line(aes(year, value, colour = countrycode))+
  ylab("GDP per capita Growth ") + xlab("Year")+ scale_x_continuous(limits = c(1990, 2020))+
  labs(title = "GDP per capita growth vs Benchmarks, 1990-", color = "Countries \n") +
  theme_bw()

#part2
Morocco$adj_age_dep_ratio = (Morocco$WB_sp_pop_0014_to + 4*Morocco$WB_sp_pop_65up_to)/Morocco$WB_sp_pop_1564_to
plot_dem_opp <- ggplot(Morocco, aes(year, adj_age_dep_ratio)) + geom_line() + 
  ylab("Age dependency ratio, adjusted") + xlab("Year") + scale_x_continuous(limits = c(1960, 2020))+
  ggtitle("Morocco's Demographic Window of Oppurtunity") +
  theme_bw()

library(reshape2)
Morocco_female <- Morocco %>% 
  select(WB_sl_tlf_cact_fe_ne_zs, WB_sl_tlf_cact_fe_zs, year)

Morocco_female <- melt(Morocco_female, id.vars = "year")

#plot of female labor force participation rate
plot_female <- ggplot(Morocco_female) +geom_line(aes(year, value, colour = variable)) +
  scale_color_manual(labels = c("National Estimate", "ILO Estimate"), values = c("blue", "red"))+
  scale_x_continuous(limits = c(1990, 2020))+ scale_y_continuous(limits = c(15, 35))+
  labs(color = "Female Labour Force Participation Rate \n")+
  ylab("Labour Participation Rate, Female") + xlab("Year") +
  ggtitle("Morocco: Female Labour Force Participation Rate, 1990-")+
  theme_bw()


#plot of fertility rate
plot_fertility <- ggplot(Morocco) +geom_line(aes(year, WB_sp_dyn_tfrt_in)) +
  ylab("Fertility rate")+ xlab("Year") + scale_x_continuous(limits = c(1960, 2020))+
  ggtitle("Morocco Fertility Rate") +theme_bw()

#part3
plot_capital_stock <- ggplot(Morocco) +geom_line(aes(year, PW_cn)) +
  ylab("Capital stock at current PPPs (in mil. 2011US$)")+ xlab("Year") +
  scale_x_continuous(limits = c(1960, 2020))+
  ggtitle("Morocco: Capital Stock") +theme_bw()

#capital stock relative to neighbors
neighbors_cap_stock <- neighbors %>% select(PW_cn, year, countrycode)
neighbors_cap_stock<- melt(neighbors_cap_stock, id.vars = c("year", "countrycode"))

plot_neighbors_cap_stock <- ggplot(neighbors_cap_stock) + geom_line(aes(year, value, colour = countrycode))+
  ylab("Capital stock at current PPPs (in mil. 2011US$)") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "Capital Stock vs Neighbors", color = "Countries \n") +
  theme_bw()

#capital stok relative to peers
similar_dev_cap_stock <- similar_dev %>% select(PW_cn, year, countrycode)
similar_dev_cap_stock<- melt(similar_dev_cap_stock, id.vars = c("year", "countrycode"))

plot_similar_dev_cap_stock <- ggplot(similar_dev_cap_stock) + geom_line(aes(year, value, colour = countrycode))+
  ylab("Capital stock at current PPPs (in mil. 2011US$)") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "Capital Stock vs Peers", color = "Countries \n") +
  theme_bw()

plot_benchmark_cap_stock <- ggplot(benchmark_cap_stock) + geom_line(aes(year, value, colour = countrycode))+
  ylab("Capital stock at current PPPs (in mil. 2011US$)") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "Capital Stock vs Benchmarks", color = "Countries \n") +
  theme_bw()

#capital stock relative to benchmarks
benchmark_cap_stock <- benchmark %>% select(PW_cn, year, countrycode)
benchmark_cap_stock<- melt(benchmark_cap_stock, id.vars = c("year", "countrycode"))

plot_benchmark_cap_stock <- ggplot(benchmark_cap_stock) + geom_line(aes(year, value, colour = countrycode))+
  ylab("Capital stock at current PPPs (in mil. 2011US$)") + xlab("Year")+ scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "Capital Stock vs Benchmarks", color = "Countries \n") +
  theme_bw()

#part4
#plot years of schooling
plot_years_school<- ggplot(Morocco) +geom_point(aes(year,BL_yr_sch15_99MF ))  +
  scale_y_continuous(limits = c(0, 10))+
  ylab("Years of Schooling")+ xlab("Year") +
  ggtitle("Morocco: Schooling") +theme_bw()

#school enrollment
Morocco_enrollment <- Morocco %>% select(year,WB_se_prm_enrr,WB_se_sec_enrr ,WB_se_ter_enrr)
Morocco_enrollment <- melt(Morocco_enrollment, id.vars = "year")

plot_enrollment<- ggplot(Morocco_enrollment) + geom_line(aes(year, value, colour = variable)) +
  ylab("School Enrollment, Gross") + xlab("Years") + scale_x_continuous(limits = c(1960, 2020))+
  labs(title = "Morocco: Gross Schooling Enrollment", color = "Schooling") +
  scale_color_manual(labels = c("Primary", "Secondary", "Tertiary"), values = c("blue", "red", "green"))+
  theme_bw()