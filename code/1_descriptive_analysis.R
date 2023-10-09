## DESCRIPTIVE ANALYSIS 

########## MISC STATS #############
# percentage of chinese immigrants arriving in either vancouver or victoria
print(nrow(filter(reg_chi, str_detect(ARR_PORT, "(Vancouver|Victoria|Vancvouver)")))/nrow(reg_chi))

# percentage of chinese immigrants arriving via ship from hong kong
hk_ships <- c("Empress Of China", "Empress Of Japan", "Empress Of India", "Empress of Russia", "Empress of Asia",
              "Parthia","Abyssinia", "Batavia", "Port Fairy", "Port Augusta", "Tacoma", "Mogul",
              "Victoria", "Tacoma","Sikh","Zambesi","Phra Nang","Loo Sok","Devawongse","Flintshire",
              "Belle Isle", "Sussex", "City of Peking","Sussex", "Mongkut", "City of Rio de Janeiro")

## est. between 1886-1896 on ships from hk (manual calc): 15261 out of 16935 total

# chicago maru -- hong kong to tacoma or yokohama to victoria? (1910-1930)
# antilochus (1907-1914)

########################################################################
### TABLE 1: SUMMARY STATS BY DATA SOURCE & GROUP (1900-1920)
########################################################################
# # us -- entire population
# us_summ <- read_csv(glue("{dbox}/cleaned/us_all_summ.csv"))
# 
# # canada -- entire population
# can_summ <- read_csv(glue("{dbox}/cleaned/can_all_summ.csv"))

# computing and binding all summary stats for other groups
summ_stats_df <- bind_rows(#us_summ, #can_summ, 
                           summstats(us_imm),
                           summstats(us_chi),
                           summstats(us_jap),
                           summstats(can_imm),
                           summstats(can_chi),
                           summstats(can_jap),
                           summstats(reg_chi, vars = c("MALE", "AGE", "LABOR", "YRIMM"))) %>%
  select(c(-OBS, OBS)) %>%
  arrange(across(c(group, source)))

summ_stats_out <- t(summ_stats_df) %>% as.data.frame()

# writing
summ_cats <- c("\\% Male", "\\% Married*", "Age", "\\% Literate*", "\\% Laborers*", "Earnings","Year of Imm.")
summtex <- file(glue("{git}/figs/summstats.tex"), open = "w")
writeLines(c("\\begin{tabular}{lccccccccc}", 
             "\\hhline{==========}", 
             "& \\multicolumn{2}{c}{All Foreign-Born} & & \\multicolumn{3}{c}{Chinese Immigrants} & & \\multicolumn{2}{c}{Japanese Immigrants} \\\\ ", 
             "\\hhline{~--~---~--}", "& (1) & (2) & & (3) & (4) & (5) & & (6) & (7) \\\\ ",
              "& CA Census & US Census & & CA Census & US Census & Chinese Reg. & & CA Census & US Census \\\\ ", " \\hhline{----------}"), summtex)
for (i in 1:length(summ_cats)){
  means <- ifelse(is.na(summ_stats_out[2*i+1,]), "-", summ_stats_out[2*i+1,])
  sds <- ifelse(is.na(summ_stats_out[2*i+2,]), "", paste0("(",round(as.numeric(summ_stats_out[2*i+2,]),4),")"))
  writeLines(c(paste(summ_cats[i], "&", glue_collapse(c(means[1:2],"",means[3:5],"",means[6:7]), sep = "&", last = ""), "\\\\ "), 
               paste("&", glue_collapse(c(sds[1:2],"",sds[3:5],"",sds[6:7]), sep = "&", last = ""), "\\\\ ")), summtex)
}
obs <- ifelse(is.na(summ_stats_out[2*(length(summ_cats)+1)+1,]), "-", round(as.numeric(summ_stats_out[2*(length(summ_cats)+1)+1,]), 0))
writeLines(c("Obs. (Thousands)", "&", glue_collapse(c(obs[1:2],"",obs[3:5],"",obs[6:7]), sep = "&", last = ""), "\\\\ "), summtex)
writeLines(c("\\hhline{----------}","\\end{tabular}"), summtex)
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





