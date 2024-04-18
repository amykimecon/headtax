#_____________________________________________________________
# FILE DESCRIPTION: Descriptive statistics -- summary stats and graphing raw trends
# CREATED BY: Amy Kim
# CREATED ON: July 2023
# LAST MODIFIED: April 2024
#_____________________________________________________________

#_____________________________________________________________
# MISC STATISTICS ----
#_____________________________________________________________
# percentage of chinese immigrants arriving in either vancouver or victoria
# 98.16%
print(nrow(filter(reg_chi, str_detect(ARR_PORT, "(Vancouver|Victoria|Vancvouver)")))/nrow(reg_chi))

# share from guangdong
# 95.98%
# guangdong counties:
jiangmen <- c("Taishan", "Xinhui", "Kaiping", "Enping", "Heshan")
guangzhou <- c("Panyu","Zengcheng", "Guangzhou")
foshan <- c("Nanhai", "Shunde","Sanshui","Gaoming","Foshan")
other <- c("Guangdong", "Zhongshan", "Dongguan", "Yangjiang","Gaoyao","Meizhou",
           "Chaozhou","Huizhou","Yingde","Zhaoqing","Boluo","Maoming")
print(nrow(filter(reg_chi, NEW_COUNTY %in% c(jiangmen, guangzhou, foshan, other)))/nrow(reg_chi))


# percentage of chinese immigrants arriving via ship from hong kong -- NOT FINISHED
hk_ships <- c("Empress Of China", "Empress Of Japan", "Empress Of India", "Empress of Russia", "Empress of Asia",
              "Parthia","Abyssinia", "Batavia", "Port Fairy", "Port Augusta", "Tacoma", "Mogul",
              "Victoria", "Tacoma","Sikh","Zambesi","Phra Nang","Loo Sok","Devawongse","Flintshire",
              "Belle Isle", "Sussex", "City of Peking","Sussex", "Mongkut", "City of Rio de Janeiro")

## est. between 1886-1896 on ships from hk (manual calc): 15261 out of 16935 total

# chicago maru -- hong kong to tacoma or yokohama to victoria? (1910-1930)
# antilochus (1907-1914)

#_____________________________________________________________
# SUMMARY STATS TABLE ----
#_____________________________________________________________
## Chinese Register Data ----
# Register variables: sex, year of imm, age at imm, laborer status, height
# x100 for stats that are being output as percentages
varlist_reg = c("MALE", "PRE1886", "YRIMM18861895", "YRIMM18961905", "YRIMM19061915", "POST1916", 
                "UNDER18","AGE1825","AGE2535","AGE35PLUS","WHIPPLE","LABOR", "HEIGHT")
reg_chi_summ <- reg_chi %>% mutate(LABOR = ifelse(AGE < 18, NA, LABOR)*100,
                                   UNDER18 = ifelse(AGE < 18, 1, 0)*100,
                                   AGE1825 = ifelse(AGE >= 18 & AGE < 25, 1, 0)*100,
                                   AGE2535 = ifelse(AGE >= 25 & AGE < 35, 1, 0)*100,
                                   AGE35PLUS = ifelse(AGE >= 35, 1, 0)*100,
                                   PRE1886 = ifelse(YRIMM < 1886, 1, 0)*100,
                                   YRIMM18861895 = ifelse(YRIMM >= 1886 & YRIMM < 1896, 1, 0)*100,
                                   YRIMM18961905 = ifelse(YRIMM >= 1896 & YRIMM < 1906, 1, 0)*100,
                                   YRIMM19061915 = ifelse(YRIMM >= 1906 & YRIMM < 1916, 1, 0)*100,
                                   POST1916 = ifelse(YRIMM >= 1916, 1, 0)*100,
                                   MALE = MALE*100) %>% 
  filter(YRIMM >= 1886 & YRIMM <= 1923)

## Canadian Census Data ----
# Census variables: sex, year of imm, age at imm, marital status, literacy, laborer status, earnings
varlist_cen <- c("MALE", "MAR", "PRE1886", "YRIMM18861895", "YRIMM18961905", "YRIMM19061915", "POST1916", 
  "UNDER18","AGE1825","AGE2535","AGE35PLUS", "CANREAD", "WHIPPLE", "LABOR", "EARN")
can_imm_summ <- can_imm %>% mutate(AGEATIMM = AGE - (YEAR-YRIMM),
                                   UNDER18 = ifelse(AGEATIMM < 18, 1, 0)*100,
                                   AGE1825 = ifelse(AGEATIMM >= 18 & AGEATIMM < 25, 1, 0)*100,
                                   AGE2535 = ifelse(AGEATIMM >= 25 & AGEATIMM < 35, 1, 0)*100,
                                   AGE35PLUS = ifelse(AGEATIMM >= 35, 1, 0)*100,
                                   PRE1886 = ifelse(YRIMM < 1886, 1, 0)*100,
                                   YRIMM18861895 = ifelse(YRIMM >= 1886 & YRIMM < 1896, 1, 0)*100,
                                   YRIMM18961905 = ifelse(YRIMM >= 1896 & YRIMM < 1906, 1, 0)*100,
                                   YRIMM19061915 = ifelse(YRIMM >= 1906 & YRIMM < 1916, 1, 0)*100,
                                   POST1916 = ifelse(YRIMM >= 1916, 1, 0)*100,
                                   LABOR = LABOR*100,
                                   CANREAD = CANREAD*100,
                                   MALE = MALE*100,
                                   MAR = MAR*100)

## Summarizing and binding by group ----
summ_stats_df <- bind_rows(summstats(can_imm_summ, varlist_cen),
                           summstats(can_imm_summ %>% filter(BPL == "Japan"), varlist_cen),
                           summstats(can_imm_summ %>% filter(BPL == "China"), varlist_cen),
                           summstats(reg_chi_summ %>% filter(FEES != 0), varlist_reg),
                           summstats(reg_chi_summ %>% filter(FEES == 0), varlist_reg)) %>%
  select(c(-OBS, OBS)) %>%
  arrange(across(c(group, source)))

summ_stats_out <- t(summ_stats_df) %>% as.data.frame()

## Writing to table ----
# variable names for table
summ_cats <- c("\\% Male", "\\% Married*", "Year of Immigration (\\%)", "\\;\\; Before 1886", 
               "\\;\\; 1886-1895", "\\;\\; 1896-1905", "\\;\\; 1906-1915", "\\;\\;After 1916", 
               "Age at Immigration (\\%)", "\\;\\; Under 18", "\\;\\; 18-24","\\;\\; 25-34", 
               "\\;\\; Over 35", "\\% Literate*", "Whipple Index", "\\% Laborers**", "Mean Annual Earnings**", "Height (cm)**")
# opening table connection
summtex <- file(glue("{tabs}/summstats.tex"), open = "w")

# writing table header
writeLines(c("\\begin{tabular}{lcccccc}", 
             "\\hhline{=======}", 
             "& \\multicolumn{3}{c}{Canadian Census} & & \\multicolumn{2}{c}{Chinese Register} \\\\ ", 
             "\\hhline{~---~--}", "& (1) & (2) & (3) & & (4) & (5) \\\\ ",
              "& All Foreign-Born. & Japanese Imm. & Chinese Imm. & & Head Tax & No Head Tax \\\\ ", " \\hhline{-------}"), summtex)

# iterating through lines
ind = 1
for (i in 1:length(summ_cats)){
  # skip category rownames
  if(summ_cats[i] == "Age at Immigration (\\%)" | summ_cats[i] == "Year of Immigration (\\%)"){
    writeLines(glue_collapse(c(summ_cats[i],rep("&",6), "\\\\")), summtex)
  }else{
    means <- ifelse(is.na(summ_stats_out[2*ind+1,]) | as.numeric(summ_stats_out[2*ind+1,])==0, "-", formatC(as.numeric(summ_stats_out[2*ind+1,]), digits = 4))
    writeLines(c(paste(summ_cats[i], "&", glue_collapse(c(means[1:3],"",means[4:5]), sep = "&", last = ""), "\\\\ ")), summtex) 
    ind = ind + 1
  }
}

# writing number of observations for each sample
obs <- ifelse(is.na(summ_stats_out[2*ind+1,]), "-", formatC(as.numeric(summ_stats_out[2*ind+1,]), format = "d", big.mark = ","))
writeLines(c("Obs.", "&", glue_collapse(c(obs[1:3],"",obs[4:5]), sep = "&", last = ""), "\\\\ "), summtex)
writeLines(c("\\hhline{-------}","\\end{tabular}"), summtex)
close(summtex)


########################################################################
### FIGURE 1: AVG TAX PAID BY CHINESE IMM BY YEAR
########################################################################
tax_by_year_reg <- reg_chi %>% filter(REG_Year != 0) %>% group_by(REG_Year) %>% 
  summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE), 
            taxpayers = sum(ifelse(FEES > 0, 1, 0), na.rm=TRUE)/n) %>% rename(YEAR = REG_Year) %>%
  mutate(CAT = "Year Registered")

tax_by_year_arriv <- reg_chi %>% group_by(YEAR_ARRIV) %>% summarize(n=n(), tax = mean(ifelse(FEES > 0, FEES, NA), na.rm=TRUE)) %>% rename(YEAR = YEAR_ARRIV) %>%
  mutate(CAT = "Year Arrived")

fig1_taxespaid <- ggplot(tax_by_year_arriv %>% filter(YEAR <= 1923), aes(x = YEAR, y = tax)) + geom_line() +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = ht) +
  geom_text(aes(x = yrs, y = 400, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = -1.2, size = 4, color = ht) + expand_limits(y = 0) +
  xlab("Year of Arrival") + ylab("Average Tax Paid Among Tax Payers") + theme_minimal()

ggsave(glue("{git}/figs/fig1_taxespaid.png"), fig1_taxespaid, height = 5, width = 9)

fig1_taxespaid_slides <-  ggplot(tax_by_year_arriv %>% filter(YEAR <= 1923), aes(x = YEAR, y = tax)) + geom_line(linewidth = 1) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = ht) +
  geom_text(aes(x = yrs, y = 400, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = -1.2, size = 4, color = ht) + expand_limits(y = 0) +
  xlab("Year of Arrival") + ylab("Average Tax Paid Among Tax Payers") + theme_minimal() + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/figs/slides/fig1_taxespaid.png"), fig1_taxespaid_slides, height = 5, width = 8)

########################################################################
### FIGURE 1: NUMBER OF CHINESE IMMIGRANTS: INFLOW BY YEAR AND DATA SOURCE
########################################################################
# number of chinese immigrants by year from chinese register data
yrimm_reg <- reg_chi %>%
  group_by(YRIMM, tax) %>% 
  summarize(CHIFLOW_REGISTER = n()) %>%
  arrange(YRIMM)

# number of immigrants by year and by birthplace from ca census data
yrimm_census <- can_imm %>% group_by(YEAR, YRIMM, BPL, tax) %>% 
  summarize(FLOW = sum(WEIGHT)) %>%
  filter((YEAR == 1901 & YRIMM < 1901) | 
           (YEAR == 1911 & YRIMM < 1911 & YRIMM >= 1901) | 
           (YEAR == 1921 & YRIMM < 1921 & YRIMM >= 1911)) # only taking YRIMM from most recent census (lowest rate of loss to outmigration)

# register data for chinese immigrants; canadian govt stats for all
yrimm_flow_graph <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigrants") %>%
                            select(c(YRIMM, FLOW, variable)),
                          immflow %>% mutate(YRIMM = Year, FLOW = IMMFLOW_NATL/50000, variable = "All Immigrants") %>%
                            select(c(YRIMM, FLOW, variable))) %>%
  filter(YRIMM >= 1886 & YRIMM <= 1930)

# key graph: inflow of chinese/all immigrants into canada using migration data
fig1_immflow <- ggplot(data = yrimm_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')
# annotate("rect", xmin = 1893, xmax = 1897, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3) +
# annotate("rect", xmin = 1899.5, xmax = 1901, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
# annotate("rect", xmin = 1902.75, xmax = 1904.75, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
# annotate("rect", xmin = 1907.5, xmax = 1908.5, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
# annotate("rect", xmin = 1910, xmax = 1912, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
# annotate("rect", xmin = 1913, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)

ggsave(glue("{git}/figs/fig1_immflow.png"), fig1_immflow, height = 4, width = 7)

fig1_immflow_slides_chionly <- ggplot(data = yrimm_flow_graph %>% filter(variable == "Chinese Immigrants") %>%
                                        mutate(variable = ifelse(variable == "Chinese Immigrants", "Chinese Immigrants (Register)", variable)), 
                              aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line(linewidth = 1) +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants (Register)"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants (Register)"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3, linewidth = 0.8) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)") + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/figs/slides/fig1_immflow_chionly.png"), fig1_immflow_slides_chionly, height = 5, width = 9)

fig1_immflow_slides <- ggplot(data = yrimm_flow_graph %>% mutate(variable = ifelse(variable == "Chinese Immigrants", "Chinese Immigrants (Register)", variable)), 
                              aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line(linewidth = 1) +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants (Register)"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants (Register)"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3, linewidth = 0.8) +
  geom_text(aes(x = yrs, y = 7, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/figs/slides/fig1_immflow.png"), fig1_immflow_slides, height = 5, width = 9)

# key inflow graph using census data rather than migration data
yrimm_flow_census_graph <- rbind(yrimm_census %>% filter(BPL == "China") %>% 
                                   mutate(FLOW = FLOW/1000, variable = "Chinese Immigrants") %>%
                                   select(c(YRIMM, FLOW, variable)),
                                 yrimm_census %>% ungroup() %>% group_by(YRIMM) %>% summarize(FLOW = sum(FLOW)) %>% 
                                   mutate(FLOW = FLOW/50000, variable = "All Immigrants") %>%
                                   select(c(YRIMM, FLOW, variable))) %>%
  filter(YRIMM >= 1880 & YRIMM <= 1920)

fig2_census_flow <- ggplot(data = yrimm_flow_census_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 3, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_census_flow.png"), fig2_census_flow, height = 4, width = 7)

fig2_census_flow_slides <- ggplot(data = yrimm_flow_census_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line(linewidth = 1) +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c2,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3, linewidth = 1) +
  geom_text(aes(x = yrs, y = 3, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(glue("{git}/figs/slides/fig2_census_flow.png"), fig2_census_flow_slides, height = 5, width = 9)

## chinese emigration to canada and total chinese emigration
yrem_flow_graph <- hk_departure %>% filter(YEAR > 1880) %>% mutate(EMIG_CA = EMIG_CA/1000, EMIG_TOT = EMIG_TOT/10000) %>%
  select(-EMIG_US) %>% pivot_longer(cols = starts_with("EMIG"), names_to = "variable",
                                    names_prefix = "EMIG_", values_to = "EMIG") %>%
  mutate(YRIMM = YEAR, FLOW = EMIG, variable = ifelse(variable == "CA", "Emigration from Hong Kong to Canada", "Total Emigration from Hong Kong"))

fig2_flow_hkemig <- ggplot(data = yrem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(hk2,hk4)) +
  scale_linetype_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 13, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("HK Emigration Outflow to Canada (Thous.)", sec.axis = sec_axis(~ . *10, name = "Total HK Emigration Outflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow_hkemig.png"), fig2_flow_hkemig, height = 4, width = 7)

fig2_flow_hkemig_slides <- ggplot(data = yrem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line(linewidth = 1) +
  scale_color_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(hk2,hk4)) +
  scale_linetype_manual(breaks = c("Total Emigration from Hong Kong", "Emigration from Hong Kong to Canada"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3, linewidth = 1) +
  geom_text(aes(x = yrs, y = 13, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  scale_y_continuous("HK Emigration Outflow to Canada (Thous.)", sec.axis = sec_axis(~ . *10, name = "Total HK Emigration Outflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(glue("{git}/figs/slides/fig2_flow_hkemig.png"), fig2_flow_hkemig_slides, height = 5, width = 9)

## chinese emigration to canada in hk harbourmaster reports vs. immigration in register
yrimmandem_flow_graph <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigration to Canada") %>%
                                 select(c(YRIMM, FLOW, variable)),
                               hk_departure %>% mutate(variable = "Canadian Emigration from Hong Kong", FLOW = EMIG_CA/1000, YRIMM = YEAR) %>%
                                 select(c(FLOW, YRIMM, variable))) %>%
  filter(YRIMM >= 1880 & YRIMM <= 1930)

fig2_flow_immandem <- ggplot(data = yrimmandem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(hk4,c4)) +
  scale_linetype_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(4, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 10, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  labs(x = "Year of Immigration", y = "Number of Migrants (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom')

ggsave(glue("{git}/figs/fig2_flow_immandem.png"), fig2_flow_immandem, height = 4, width = 7)

fig2_flow_immandem_slides <- ggplot(data = yrimmandem_flow_graph, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line(linewidth = 1) +
  scale_color_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(hk4,c4)) +
  scale_linetype_manual(breaks = c("Canadian Emigration from Hong Kong", "Chinese Immigration to Canada"), values = c(4, 1)) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3, linewidth = 1) +
  geom_text(aes(x = yrs, y = 10, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  labs(x = "Year of Immigration", y = "Number of Migrants (Thous.)", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') + 
  theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(glue("{git}/figs/slides/fig2_flow_immandem.png"), fig2_flow_immandem_slides, height = 5, width = 8)
  



########################################################################
### FIGURE 3: US VS CANADA
########################################################################
yrimm_us <- us_imm %>% 
  filter((YEAR == 1900 & YRIMM < 1900) | (YEAR == 1910 & YRIMM < 1910 & YRIMM >= 1900) | (YEAR == 1920 & YRIMM < 1920 & YRIMM >= 1910)) %>% # only taking YRIMM from most recent census (lowest rate of loss to outmigration)
  group_by(YEAR, YRIMM) %>% 
  summarize(IMMFLOW = n(), CHIFLOW = sum(BORNCHI)) 

yrimm_flow_graph_all <- rbind(yrimm_reg %>% mutate(FLOW = CHIFLOW_REGISTER/1000, variable = "Chinese Immigrants") %>%
                                select(c(YRIMM, FLOW, variable)),
                              immflow %>% mutate(YRIMM = Year, FLOW = IMMFLOW_NATL/50000, variable = "All Immigrants") %>%
                                select(c(YRIMM, FLOW, variable))) %>%
  mutate(source = "Canada") %>%
  rbind(yrimm_us %>% mutate(FLOW = IMMFLOW/50000, variable = "All Immigrants", source = "US") %>% select(c(YRIMM, FLOW, variable, source)),
        yrimm_us %>% mutate(FLOW = CHIFLOW/1000, variable = "Chinese Immigrants", source = "US") %>% select(c(YRIMM, FLOW, variable, source))) %>%
  filter(YRIMM >= 1870 & YRIMM <= 1930)

ggplot(data = yrimm_flow_graph_all, aes(x = YRIMM, y = FLOW, color = variable, linetype = variable)) + geom_line() +
  scale_color_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(c1,c4)) +
  scale_linetype_manual(breaks = c("All Immigrants", "Chinese Immigrants"), values = c(2, 1)) +
  geom_vline(aes(xintercept = yrs), data = rbind(headtaxcuts,data.frame(yrs = 1882,labs="US Chinese Excl. Act")), show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 11, label = labs), data = rbind(headtaxcuts,data.frame(yrs = 1882,labs="US Chinese Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') +
  facet_wrap(~source, ncol = 1)

ggsave(glue("{git}/figs/fig3_us_can.png"), height = 5, width = 7)

# sep 6
ggplot(data = yrimm_flow_graph_all %>% filter(variable == "All Immigrants"), aes(x = YRIMM, y = FLOW, color = source)) + geom_line() +
  scale_y_continuous("Chinese Immigrant Inflow (Thous.)", sec.axis = sec_axis(~ . *50, name = "Total Immigrant Inflow (Thous.)")) + 
  labs(x = "Year of Immigration", linetype = "", color = "") + theme_minimal() + theme(legend.position='bottom') +
  annotate("rect", xmin = 1893, xmax = 1897, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 1899.5, xmax = 1901, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1902.75, xmax = 1904.75, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  annotate("rect", xmin = 1907.5, xmax = 1908.5, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) + 
  annotate("rect", xmin = 1910, xmax = 1912, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.1) +
  annotate("rect", xmin = 1913, xmax = 1915, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)

ggsave(glue("{git}/figs/6sep23/immflow.png"), height = 4, width = 7)


# 
# fig3_us_can <- ggplot(data = yrimm_us_can %>% filter(YRIMM >= 1870), aes(x = YRIMM, y = POP, color = source, linetype = IMM)) + geom_line() +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts[1:3,], show.legend = FALSE) +
#   geom_text(aes(x = yrs, y = 7000, label = labs), data = headtaxcuts[1:3,], inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
#   geom_vline(aes(xintercept = x), data = data.frame(x = c(1882)), show.legend = FALSE, color = "dark blue") +
#   geom_text(aes(x = x, y = 7000, label = labs), data = data.frame(x = c(1882), labs = c("US Chi. Excl. Act")), inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 4) +
#   #scale_color_manual(breaks=c("CA Census", "US Census"), values=c("black","blue")) +
#   scale_linetype_manual(breaks=c("Chinese", "Japanese"), values=c(1,3)) +
#   labs(x = "Year of Immigration", y = "Inflow of Chinese Immigrants", linetype = "Data Source", color = "Immigrant Group") + theme_minimal() + theme(legend.position='bottom')
# 
# 
# ggsave(glue("{git}/figs/fig3_us_can.png"), fig3_us_can, height = 5, width = 9)
# 