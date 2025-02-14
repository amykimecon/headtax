# outcome/selection analysis 

#_____________________________________________________________
# Height Analysis ----------------------------------
#_____________________________________________________________

## FIGURE 2: Raw height plots from register data ----
# all men age 23-50 
height_plot <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE), 
            medheight = median(ifelse(HEIGHT == 0, NA, HEIGHT), na.rm=TRUE),
            height25 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.25,na.rm=TRUE),
            height75 = quantile(ifelse(HEIGHT==0,NA,HEIGHT),0.75,na.rm=TRUE),
            n= n())
# by taxpayer status
height_plot2 <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(taxgrp = ifelse(FEES == 0, "nofee", "fee")) %>%
  group_by(YRIMM, taxgrp) %>%
  summarize(height = mean(ifelse(HEIGHT==0,NA,HEIGHT), na.rm=TRUE),
            n= n()) %>% filter(n > 1)

# mean heights by tax paid
taxmeanheights = summarize(group_by(height_plot, tax), meanheight = weighted.mean(height, n))$meanheight

# plot for paper
fig_height <- ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  #geom_segment(aes(y = taxmeanheights[1], yend = taxmeanheights[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[2], yend = taxmeanheights[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[3], yend = taxmeanheights[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  #geom_segment(aes(y = taxmeanheights[4], yend = taxmeanheights[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanheights[4] - 0.2, label = "Mean Height Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# of Chinese Immigrants")

# saving plot
ggsave(glue("{git}/output/paper/figures/height_selection.png"), fig_height, height = 4, width = 7)

# plot for slides
fig_height_slides <- ggplot(height_plot, aes(x = YRIMM, y = height)) + 
  geom_segment(aes(y = taxmeanheights[1], yend = taxmeanheights[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[2], yend = taxmeanheights[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[3], yend = taxmeanheights[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanheights[4], yend = taxmeanheights[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanheights[4] - 0.2, label = "Interval Mean"), inherit.aes=FALSE, color = c3, size = 5) +
  geom_point(aes(size = n), color = c4, alpha = 0.9) + 
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", size = "# of Chinese Immigrants") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

# saving plot
ggsave(glue("{git}/output/slides/height_selection.png"), fig_height_slides, height = 5, width = 8)


# # plot by taxpayer group (deprecated)
# ggplot(height_plot2, aes(x = YRIMM, y = height, color = taxgrp)) +
#   geom_smooth(method = "lm", mapping = aes(weight = n)) +
#   geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
#   geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
#   geom_point(aes(size = n), alpha = 0.9) + theme_minimal() + theme(legend.position='bottom') +
#   labs(x = "Year of Immigration", y = "Mean Height of Chinese Immigrants (cm)", size = "# Chinese Immigrants")

## SLIDES FIGURE: Comparison by YRIMM ----
# crosswalk from chinese height file source to label
height_file_name_crosswalk <- china_height %>% group_by(file_name) %>% summarize(source = first(source))
                                                    
# combining register with chinese height data (to get predicted heights from various sources)                             
height_plot_compare <- reg_chi %>% filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_height %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "height_raw"), 
            by = c("year")) %>%
  group_by(YRIMM, tax) %>%
  summarize(Actual = mean(HEIGHT, na.rm=TRUE), 
            across(ends_with(".csv"), function(.x) mean(.x, na.rm=TRUE)),
            n= n()) %>%
  pivot_longer(Actual:sth_unskilled_rlwy_plot.csv, names_to = "var_raw", values_to = "height") %>%
  left_join(height_file_name_crosswalk, by = c("var_raw" = "file_name")) %>%
  mutate(var = ifelse(var_raw == "Actual", "Actual", source))

# comparison plot with ALL sources (for slides)
fig_height_compare_slides <- ggplot(
  height_plot_compare %>% filter(var != "Actual"), aes(x = YRIMM, y = height, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 168.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_line(aes(linetype = var, color = var)) + 
  scale_color_manual(breaks = c("AUS Melb/Vic Migrants",
                                "Chinese Railway Workers",
                                "AUS QLD Migrants",
                                "US Migrants",
                                "US Prisoners",
                                "AUS NT Migrants",
                                "AUS Prisoners",
                                "Indonesia Migrants"),
                     values = colorRampPalette(colors = c(c3, c5))(8)) +
  scale_shape_manual(breaks = c("AUS Melb/Vic Migrants",
                                "Chinese Railway Workers",
                                "AUS QLD Migrants",
                                "US Migrants",
                                "US Prisoners",
                                "AUS NT Migrants",
                                "AUS Prisoners",
                                "Indonesia Migrants"),
                     values = c(3, 4, 2, 18, 10, 0, 8, 15)) +
  geom_point(data = height_plot_compare %>% filter(var == "Actual"), aes(x = YRIMM, y = height, size = n), shape = 16, color = c1) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", 
       size = "", color = "", shape = "", linetype = "Comparison Population") + guides(color = "none") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/height_compare_selection_all.png"), fig_height_compare_slides, height = 5, width = 8)

# comparison plot with only AUS NT migrants (for slides)
fig_height_compare_ausnt_slides <- ggplot(
  height_plot_compare %>% filter(var %in% c("Actual", "AUS NT Migrants")), 
  aes(x = YRIMM, y = height, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 167.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_point(aes(size = n), alpha = 0.9) + 
  scale_color_manual(breaks = c("AUS NT Migrants", "Actual"), values = c(c4,c2)) +
  scale_shape_manual(breaks = c("AUS NT Migrants", "Actual"), values = c(18,16)) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Avg. Height of Chinese Imm. (cm)", size = "", color = "", shape = "") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/height_compare_selection_ausnt.png"), fig_height_compare_ausnt_slides, height = 5, width = 8)

## EXTRA FIGURES: Comparison by Birth Cohort ----
# merging register with chinese height data by birth cohort -- separately by tax group (CAN)
height_compare_bc <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  group_by(year, tax) %>%
  summarize(height = mean(HEIGHT), n = n()) %>%
  mutate(dataseries = glue("CA Tax = {tax}")) %>%
  select(c(year, height, dataseries, n)) 

# recreating original graph
height_bybc_original <- ggplot(china_height %>% rename(height = height_raw, dataseries = source),
                               aes(x = year, y = height, color = dataseries, shape = dataseries)) +
  geom_point() + geom_line()

# plotting register data on top of original graph
height_plot_path <- glue("{dbox}/raw/china_height_plot_data")
height_csvs <- list.files(height_plot_path, pattern = ".*csv$")

ggplot(rbind(height_compare_bc %>% filter(n > 100), china_height %>% rename(height = height_raw, dataseries = file_name) %>% select(-c(yr_raw))), 
       aes(x = year, y = height, color = dataseries, shape = dataseries)) + geom_point(aes(size = n)) + 
  scale_color_manual(name = "dataseries", breaks = c(c(height_csvs),"CA Tax = 0", "CA Tax = 1496.19", "CA Tax = 2992.61", "CA Tax = 14115.7"),
                     values = c(rep("grey",8), c5, c4, c2, c1)) + theme_minimal() + theme(legend.position='bottom') + geom_line(alpha = 0.5) 

## REGRESSIONS ----
height_reg_data <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 50 & YRIMM > 1879 & YRIMM < 1924 & HEIGHT > 100 & 
           MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_height %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "height_raw"), 
            by = c("year")) %>%
  mutate(height_diff = HEIGHT - mig_au_melbvic_plot.csv)

height_reg1 <- lm(data = height_reg_data, HEIGHT ~ factor(tax) + AGE)
coeftest(height_reg1, vcov. = vcovHC(height_reg1, vcov = "HC1"))

height_reg2 <- lm(data = height_reg_data, height_diff ~ factor(tax) + AGE)
coeftest(height_reg2, vcov. = vcovHC(height_reg2, vcov = "HC1"))

height_reg3 <- lm(data = height_reg_data, HEIGHT ~ factor(tax) + factor(year))
coeftest(height_reg3, vcov. = vcovHC(height_reg3, vcov = "HC1"))

stargazer(height_reg1, height_reg2, height_reg3,
          dep.var.labels = c("Height (cm)", "Height Diff from Melb/Vic", "Height"),
          keep = c("factor\\(tax\\)*", "AGE"),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax", "Age"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("5-yr Birth Cohort FE", "No", "No", "Yes")))


#_____________________________________________________________
# Age Heaping Analysis ----------------------------------
#_____________________________________________________________
## FIGURE ?: Raw whipple plots from register data ----
# all men age 23-50 
whipple_plot <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = n()) %>% filter(whipple >= 100)

# from census
whipple_plot_cen <- can_imm %>% filter(BPL == "China" & AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  group_by(YRIMM, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = sum(WEIGHT))

# mean whipple by tax paid
taxmeanwhipple = summarize(group_by(whipple_plot, tax), meanwhipple = weighted.mean(whipple, n))$meanwhipple

# plot for paper
fig_whipple <- ggplot(whipple_plot %>% filter(whipple >= 100), aes(x = YRIMM, y = whipple)) + 
  geom_segment(aes(y = taxmeanwhipple[1], yend = taxmeanwhipple[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[2], yend = taxmeanwhipple[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[3], yend = taxmeanwhipple[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[4], yend = taxmeanwhipple[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 160, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanwhipple[4] - 2, label = "Whipple Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), color = c1, alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Immigrants", size = "# of Chinese Immigrants")

# saving plot
ggsave(glue("{git}/output/paper/figures/whipple_selection.png"), fig_whipple, height = 4, width = 7)

# plot using census data
ggplot(rbind(whipple_plot %>% mutate(source = "Reg"), whipple_plot_cen %>% mutate(source = "Cen")), 
       aes(x = YRIMM, y = whipple, color = source)) + 
  geom_segment(aes(y = taxmeanwhipple[1], yend = taxmeanwhipple[1], x = 1881, xend = 1885), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[2], yend = taxmeanwhipple[2], x = 1885, xend = 1900), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[3], yend = taxmeanwhipple[3], x = 1900, xend = 1903), inherit.aes = FALSE, color = c3) +
  geom_segment(aes(y = taxmeanwhipple[4], yend = taxmeanwhipple[4], x = 1903, xend = 1923), inherit.aes = FALSE, color = c3) +
  geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 160, label = labs), data = headtaxcuts, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 3, color = "#808080") +
  geom_text(aes(x = 1919, y = taxmeanwhipple[4] - 2, label = "Whipple Over Interval"), inherit.aes=FALSE, color = c3, size = 2.5) +
  geom_point(aes(size = n), alpha = 0.9) + 
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Immigrants", size = "# of Chinese Immigrants")


## SLIDES FIGURE: Comparison by YRIMM ----
# crosswalk from chinese age file source to label
age_file_name_crosswalk <- china_age %>% group_by(file_name) %>% summarize(source = first(source))

# combining register with chinese height data (to get predicted heights from various sources)                             
whipple_plot_compare <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1885 & YRIMM < 1924 & MALE == 1 & FEES > 0) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_age %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "age_raw"), 
            by = c("year")) %>%
  group_by(YRIMM, tax) %>%
  summarize(Actual = mean(WHIPPLE, na.rm=TRUE), 
            across(ends_with(".csv"), function(.x) mean(.x, na.rm=TRUE)),
            n= n()) %>%
  pivot_longer(Actual:pris_au_plot.csv, names_to = "var_raw", values_to = "whipple") %>%
  left_join(age_file_name_crosswalk, by = c("var_raw" = "file_name")) %>%
  mutate(var = ifelse(var_raw == "Actual", "Actual", source)) %>%
  filter(whipple < 200)

# comparison plot with ALL sources (for slides)
fig_whipple_compare_slides <- ggplot(
  whipple_plot_compare %>% filter(var != "Actual" & var != "Beijing Military"), aes(x = YRIMM, y = whipple, color = var, shape = var)) + 
  geom_vline(aes(xintercept = yrs), data = headtaxcuts_slides, show.legend = FALSE, color = "#808080", linetype = 3) +
  geom_text(aes(x = yrs, y = 168.5, label = labs), data = headtaxcuts_slides, inherit.aes = FALSE, angle = 90, nudge_x = 0.8, size = 5, color = "#808080") +
  geom_line(alpha = 0.7) +
  scale_color_manual(breaks = c("AUS Prisoners",
                                "Indonesia Migrants",
                                "US Migrants",
                                "AUS Syd Migrants",
                                "AUS Melb/Bris/Darw Migrants",
                                "Chinese 1953 Census"),
                     values = colorRampPalette(colors = c(c3, c5))(6)) +
  scale_shape_manual(breaks = c("AUS Prisoners",
                                "Indonesia Migrants",
                                "US Migrants",
                                "AUS Syd Migrants",
                                "AUS Melb/Bris/Darw Migrants",
                                "Chinese 1953 Census"),
                     values = c(3, 4, 2, 18, 10, 0)) +
  geom_point(data = whipple_plot_compare %>% filter(var == "Actual" & whipple >= 100), aes(x = YRIMM, y = whipple, size = n), shape = 16, color = c1) +
  guides(size = "none", color = guide_legend(override.aes = list(size=7))) +
  #geom_errorbar(mapping = aes(ymin = height10, ymax = height90)) +
  theme_minimal() + theme(legend.position='bottom') + 
  labs(x = "Year of Immigration", y = "Whipple Index of Chinese Imm.", size = "", color = "", shape = "") +
  theme(text = element_text(size=18), axis.text = element_text(size = 14))

ggsave(glue("{git}/output/slides/whipple_compare_selection_all.png"), fig_whipple_compare_slides, height = 5, width = 8)

## EXTRA FIGURES: Comparison by Birth Cohort ----
# merging register with chinese age data by birth cohort -- separately by tax group (CAN)
whipple_compare_bc <- reg_chi %>% 
  filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  group_by(year, tax) %>%
  summarize(whipple = mean(WHIPPLE), n = n()) %>%
  mutate(dataseries = glue("CA Tax = {tax}")) %>%
  select(c(year, whipple, dataseries, n)) 

# recreating original graph
whipple_bybc_original <- ggplot(china_age %>% rename(whipple = age_raw, dataseries = source),
                               aes(x = year, y = whipple, color = dataseries, shape = dataseries)) +
  geom_point() + geom_line()

# plotting register data on top of original graph
whipple_plot_path <- glue("{dbox}/raw/china_age_plot_data")
whipple_csvs <- list.files(whipple_plot_path, pattern = ".*csv$")

ggplot(rbind(whipple_compare_bc %>% filter(whipple >= 100), 
             china_age %>% rename(whipple = age_raw, dataseries = file_name) %>% select(-c(yr_raw)) %>% filter(year <= 1900)), 
       aes(x = year, y = whipple, color = dataseries, shape = dataseries)) + geom_point(aes(size = n)) + 
  scale_color_manual(name = "dataseries", breaks = c(c(whipple_csvs),"CA Tax = 0", "CA Tax = 1496.19", "CA Tax = 2992.61", "CA Tax = 14115.7"),
                     values = c(rep("grey",7), c5, c4, c2, c1)) + theme_minimal() + geom_line(alpha = 0.5) 

## REGRESSIONS ----
whipple_reg_data <- reg_chi %>% filter(AGE >= 23 & AGE <= 62 & YRIMM > 1879 & YRIMM < 1924 & 
                                         MALE == 1) %>%
  mutate(BIRTHYR = ifelse(YRIMM != 0, YRIMM - AGE, NA),
         year = 5*round(BIRTHYR/5)) %>%
  left_join(china_age %>% pivot_wider(id_cols = year, names_from = "file_name", values_from = "age_raw"), 
            by = c("year")) %>%
  mutate(whipplediff = WHIPPLE - mig_au_melb_b_d_plot.csv)

whipple_reg1 <- lm(data = whipple_reg_data, WHIPPLE ~ factor(tax) + AGE)
coeftest(whipple_reg1, vcov. = vcovHC(whipple_reg1, vcov = "HC1"))

whipple_reg2 <- lm(data = whipple_reg_data, whipplediff ~ factor(tax) + AGE)
coeftest(whipple_reg2, vcov. = vcovHC(whipple_reg2, vcov = "HC1"))

whipple_reg3 <- lm(data = whipple_reg_data, WHIPPLE ~ factor(tax) + factor(year))
coeftest(whipple_reg3, vcov. = vcovHC(whipple_reg3, vcov = "HC1"))


stargazer(whipple_reg1, whipple_reg2, whipple_reg3,
          dep.var.labels = c("Whipple", "Whipple Diff from Melb/Vic", "Whipple"),
          keep = c("factor\\(tax\\)*", "AGE"),
          covariate.labels = c("\\$50 Tax", "\\$100 Tax", "\\$500 Tax", "Age"),
          keep.stat=c("n","adj.rsq"),
          add.lines = list(c("5-yr Birth Cohort FE", "No", "No", "Yes")))

# MORE REGS
age_reg <- lm(data = reg_chi %>% filter(YRIMM > 1885 & YRIMM < 1924 & MALE == 1 & FEES != 0), 
              AGE ~ factor(tax) + YRIMM + I(YRIMM^2))
coeftest(age_reg, vcov. = vcovHC(age_reg, vcov = "HC1"))

age_grp <- reg_chi %>% filter(YRIMM < 1924 & YRIMM > 1885 & MALE == 1 & FEES != 0) %>% group_by(YRIMM) %>% summarize(AGE = mean(AGE))
ggplot(age_grp, aes(x = YRIMM, y = AGE)) + geom_line() + geom_vline(aes(xintercept = yrs), data = headtaxcuts, show.legend = FALSE, color = "#808080", linetype = 3) 

for (bw in 3:10){
  print(paste0("BW:", bw))
  yrmin = 1900-bw
  yrmax = 1904+bw
  x1 <- breakpoints(CHIFLOW_REGISTER ~ YRIMM, 
                    data = flow_regress %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3, breaks = 2)
  print(filter(flow_regress, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x1$breakpoints])
  
  x2 <- breakpoints(AGE ~ YRIMM, 
                    data = age_grp %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3, breaks = 2)
  print(filter(age_grp, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x2$breakpoints])
  
  x3 <- breakpoints(height ~ YRIMM, 
                    data = height_plot %>% filter(YRIMM >= yrmin & YRIMM <= yrmax),
                    h = 3)
  print(filter(height_plot, YRIMM >= yrmin & YRIMM <= yrmax)$YRIMM[x3$breakpoints])
}

test1 <- flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1923)

sctest(CHIFLOW_REGISTER ~ YRIMM + IMMFLOW_NATL, 
       data = test1, type = "Chow", point = 15)

sctest(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), 
       data = test1, type = "supF", plot = TRUE)

sctest(CHIFLOW_REGISTER ~ YRIMM, 
       data = test1 %>% filter(YRIMM >= 1890 & YRIMM <= 1903), type = "Chow", point = 10, plot = TRUE)

#reg1 <- lm(CHIFLOW_REGISTER ~ IMMFLOW_NATL + log(POPSTOCKLAG_China) + EMIG_TOT, data = chowdata)
#chowdata$resid <- resid(reg1)

chowgraphdata <- function(df, k=2, controls = ""){
  n = nrow(df)
  fstat <- numeric(n)
  pval <- numeric(n)
  critval <- numeric(n)
  yrs <- numeric(n)
  for (i in (k+1):(n-k-1)){
    #print(df$YRIMM[i])
    chowtest <- sctest(as.formula(paste0("CHIFLOW_REGISTER ~ YRIMM",controls)), 
                       data = df, type = "Chow", point = i)
    yrs[i] <- df$YRIMM[i]
    fstat[i] <- chowtest$statistic
    pval[i] <- chowtest$p.value
    critval[i] <- qf(0.95, k, n-(2*k))
    #print(chowtest$statistic)
    #print(chowtest$p.value)
  }
  chow_out <- data.frame(fstat = fstat, pval = pval, critval = critval, YRIMM = yrs) %>% filter(YRIMM > 0)
  return(chow_out)
}

chowdata_all <- flow_regress %>% filter(YRIMM >= 1886 & YRIMM <= 1921)
chowdata_pre <- flow_regress %>% filter(YRIMM >= 1894 & YRIMM <= 1903)
chowdata_post <- flow_regress %>% filter(YRIMM >= 1901 & YRIMM <= 1910)

chowdata_out <- bind_rows(list(chowgraphdata(chowdata_all, k = 5, controls = "+ IMMFLOW_NATL + log(POPSTOCKLAG_China) + EMIG_TOT") %>% 
                                 mutate(type = "all", subtype = "all"),
                               chowgraphdata(chowdata_pre) %>% mutate(type = "subset", subtype = "pre"),
                               chowgraphdata(chowdata_post) %>% mutate(type = "subset", subtype = "post")))
ggplot(chowdata_out, aes(x = YRIMM, y = fstat, color = subtype)) + 
  geom_point() + geom_line(aes(y = critval)) + facet_wrap(~type, ncol = 1, scales = "free")
#ggplot(chow_out, aes(x = YRIMM, y = pval)) + geom_point()


sctest(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + log(POPSTOCKLAG_China), 
       data = test1, type = "Chow", point = 16)


breakpoints(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), 
       data = test1, breaks = 1, h = 8)

supmz(CHIFLOW_REGISTER ~ YRIMM + EMIG_TOT + IMMFLOW_NATL + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2), data = test1)
changepoints(segment(test1$CHIFLOW_REGISTER, method = "pelt"))


x2 <- breakpoints(AGE ~ YRIMM, 
                  data = age_grp %>% filter(YRIMM >= 1895 & YRIMM <= 1908),
                  h = 3)
summary(x2)

x3 <- breakpoints(HEIGHT ~ YRIMM, 
                  data = height_plot %>% filter(YRIMM >= 1895 & YRIMM <= 1908),
                  h = 3)
summary(x2)


x2 <- breakpoints(AGE ~ rand, 
                  data = age_grp %>% filter(YRIMM >= 1895 & YRIMM <= 1908) %>%
                    group_by(row_number()) %>%
                    mutate(rand = runif(1)),
                  h = 3)
summary(x2)

flowreg1 <- lm(data = flow_regress %>% filter(YRIMM >= regstart & YRIMM <= regend), 
               CHIFLOW_REGISTER ~ EMIG_TOT + IMMFLOW_NATL + factor(tax) + POPSTOCKLAG_China + I(POPSTOCKLAG_China^2))

#_____________________________________________________________
# DiD Regressions: Can Imm control --------------------------
#_____________________________________________________________
did_data <- can_imm %>% filter(YEAR >= 1901 & YRIMM >= 1882 & YRIMM <= 1913) %>% #only keeping years with earnings/yrimm data
  mutate(LABOR = ifelse(OCC1950 %in% c(970, 650, 820, 830), 1, 0), #labor: includes general laborer, mine laborer, farm laborer
         BORNCHI = ifelse(BPL == "China", 1, 0),
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         BORNCHI_notax = BORNCHI * ifelse(tax == 0, 1, 0),
         AGEATIMM = AGE - (YEAR - YRIMM),
         ageheap = ifelse(AGE %% 5 == 0, 1, 0),
         twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
                                 YRIMM %in% c(1901,1902) ~ "post100",
                                 YRIMM %in% c(1904,1905) ~ "post500",
                                 TRUE ~ "_notpost"),
         oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
                                 YRIMM == 1901 ~ "post100",
                                 YRIMM == 1904 ~ "post500",
                                 TRUE ~ "_notpost"),
         BORNCHI_twoyearpost50 = BORNCHI*ifelse(twoyearpost == "_post50", 1, 0),
         BORNCHI_twoyearpost100 = BORNCHI*ifelse(twoyearpost == "post100", 1, 0),
         BORNCHI_twoyearpost500 = BORNCHI*ifelse(twoyearpost == "post500", 1, 0))

did_data_japan <- did_data %>% filter(BORNCHI == 1 | BPL == "Japan") %>% filter(YRIMM >= 1890 & YRIMM <= 1907)# & YEAR != 1921)

# # test
# test <- did_data %>% group_by(BORNCHI, YRIMM) %>% 
#   summarize(n = sum(WEIGHT),
#             across(c(LABOR, SPEAKENG, CANREAD, EARN, EMPLOYED, EMPLOYEE), function(.x) weighted.mean(.x, WEIGHT, na.rm=TRUE)))
# 
# ggplot(test, aes(x = YRIMM, y = LABOR, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = SPEAKENG, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = CANREAD, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EARN, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EMPLOYED, color = factor(BORNCHI), size = n))+ geom_point()
# ggplot(test, aes(x = YRIMM, y = EMPLOYEE, color = factor(BORNCHI), size = n))+ geom_point()

# normal regs
didlab <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                    data = did_data %>% filter(MALE == 1 & AGE >= 18), #%>% filter(OCC1950 != 290 & OCC1950 != 983), 
                    weights = WEIGHT)

dideng <- lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
                       data = did_data %>% filter(AGE >= 6 & MALE == 1), 
                       weights = WEIGHT)

didread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                      data = did_data %>% filter(AGE >= 18 & MALE == 1), weights = WEIGHT)

didwhip <- lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
                      data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT)

didearn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                   data = did_data %>% filter(MALE == 1 & AGE >= 18), weights = WEIGHT)

# japan
didjlab <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
             data = did_data_japan %>% filter(MALE == 1 & AGE >= 18), #%>% filter(OCC1950 != 290 & OCC1950 != 983), 
             weights = WEIGHT)

didjeng <- lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
             data = did_data_japan %>% filter(AGE >= 6 & MALE == 1), 
             weights = WEIGHT)

didjread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
              data = did_data_japan %>% filter(AGE >= 18 & MALE == 1), weights = WEIGHT)

didjwhip <- lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
              data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT)

didjearn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, 
              data = did_data_japan %>% filter(MALE == 1 & AGE >= 18), weights = WEIGHT)

# summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
# summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
# 
# summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1), weights = WEIGHT))
# summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1), weights = WEIGHT))
# 
# summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
# summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#            data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
# 
# 
# did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                       data = did_data, weights = WEIGHT)
# did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                    data = did_data, weights = WEIGHT)
# did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
#                        data = did_data, weights = WEIGHT)
# 
# # regs w/ two year post
# did_reg_labor <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500 +
#                       BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                     data = did_data, weights = WEIGHT)
# did_reg_canread <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                         BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                       data = did_data, weights = WEIGHT)
# did_reg_earn <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                      BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                    data = did_data, weights = WEIGHT)
# did_reg_houseown <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500+
#                          BORNCHI_twoyearpost50 + BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                        data = did_data, weights = WEIGHT)

# 
# # normal japan regs
# did_reg_labor_japan <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                           data = did_data_japan, weights = WEIGHT)
# did_reg_canread_japan <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                             data = did_data_japan, weights = WEIGHT)
# did_reg_earn_japan <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                          data = did_data_japan, weights = WEIGHT)
# did_reg_houseown_japan <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
#                              data = did_data_japan, weights = WEIGHT)

# # japan regs w/ two year post
# did_reg_labor_japan <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500 +
#                             BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                           data = did_data_japan, weights = WEIGHT)
# did_reg_canread_japan <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                               BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                             data = did_data_japan, weights = WEIGHT)
# did_reg_earn_japan <- lm(EARN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                            BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                          data = did_data_japan, weights = WEIGHT)
# did_reg_houseown_japan <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500+
#                                BORNCHI_twoyearpost100 + BORNCHI_twoyearpost500, 
#                              data = did_data_japan, weights = WEIGHT)

# dep var means & ses
# did_data_means <- c(weighted.mean(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
#                     weighted.mean(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
#                     weighted.mean(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
# did_data_japan_means <- c(weighted.mean(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
#                           weighted.mean(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
#                           weighted.mean(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))
# did_data_ses <- c(wtd_se(filter(did_data, !is.na(LABOR))$LABOR,filter(did_data, !is.na(LABOR))$WEIGHT),
#                     wtd_se(filter(did_data, !is.na(CANREAD))$CANREAD,filter(did_data, !is.na(CANREAD))$WEIGHT),
#                     wtd_se(filter(did_data, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data, !is.na(HOUSEOWN))$WEIGHT))
# did_data_japan_ses <- c(wtd_se(filter(did_data_japan, !is.na(LABOR))$LABOR,filter(did_data_japan, !is.na(LABOR))$WEIGHT),
#                           wtd_se(filter(did_data_japan, !is.na(CANREAD))$CANREAD,filter(did_data_japan, !is.na(CANREAD))$WEIGHT),
#                           wtd_se(filter(did_data_japan, !is.na(HOUSEOWN))$HOUSEOWN,filter(did_data_japan, !is.na(HOUSEOWN))$WEIGHT))

did_data_means_chi <- c(weighted.mean(filter(did_data, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$LABOR,
                                      filter(did_data, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$CANREAD,
                                  filter(did_data, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$SPEAKENG,
                                  filter(did_data, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WHIPPLE,
                                  filter(did_data, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WEIGHT),
                    weighted.mean(filter(did_data, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$EARN,
                                  filter(did_data, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT)
)

did_data_means_chi_jap <- c(weighted.mean(filter(did_data_japan, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$LABOR,
                                      filter(did_data_japan, !is.na(LABOR) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                        weighted.mean(filter(did_data_japan, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$CANREAD,
                                      filter(did_data_japan, !is.na(CANREAD) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT),
                        weighted.mean(filter(did_data_japan, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$SPEAKENG,
                                      filter(did_data_japan, !is.na(SPEAKENG) & BORNCHI == 1 & MALE == 1 & AGE >= 6)$WEIGHT),
                        weighted.mean(filter(did_data_japan, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WHIPPLE,
                                      filter(did_data_japan, !is.na(WHIPPLE) & BORNCHI == 1 & MALE == 1 & AGE >= 23 & AGE <= 52)$WEIGHT),
                        weighted.mean(filter(did_data_japan, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$EARN,
                                      filter(did_data_japan, !is.na(EARN) & BORNCHI == 1 & MALE == 1 & AGE >= 18)$WEIGHT)
)
# stargazer(did_reg_labor, did_reg_canread, did_reg_houseown,did_reg_labor_japan, did_reg_canread_japan, did_reg_houseown_japan,
#           out = glue("{git}/figs/selection.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           column.labels = c("All Immigrants (1880-1913)","Chinese \\& Japanese Imm. (1890-1907)"),
#           column.separate = c(3,3),
#           dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$",
#                              "$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$","$\\mathbb{P}[\\text{Owns House}]$"),
#           keep = c("BORNCHI*"),
#           covariate.labels = c("$\\hat{\\beta}_{1}$ ($BORNCHI$)", 
#                                "$\\hat{\\gamma}_{50}^{DD}$ ($BORNCHI \\times$ \\$50 Tax)", 
#                                "$\\hat{\\gamma}_{100}^{DD}$ ($BORNCHI \\times$ \\$100 Tax)", 
#                                "$\\hat{\\gamma}_{500}^{DD}$ ($BORNCHI \\times$ \\$500 Tax)"),
#           keep.stat=c("n","adj.rsq"),
#           table.layout = "=cld#-ta-s-",
#           add.lines = list(c("Dep. Var. Mean (SE)", formatC(did_data_means, format = "f"), formatC(did_data_japan_means, format = "f"), "\\\\"),
#                            c("", paste0("(",c(formatC(did_data_ses, format = "f"), formatC(did_data_japan_ses, format = "f")), ")"), "\\\\")))

# slides: all immig
stargazer(didlab, didread, dideng, didwhip, didearn,
          out = glue("{git}/output/slides/selection_all.tex"), float = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$",
                             "$\\mathbb{P}[\\text{Speaks English}]$", "Age Heaping", "Earnings"),
          keep = c("BORNCHI*"), 
          covariate.labels = c("$\\hat{\\beta}_{1}$ (Born in China)", 
                               "$\\hat{\\gamma}_{50}^{DD}$ ($C_i \\times$ \\$50 Tax)", 
                               "$\\hat{\\gamma}_{100}^{DD}$ ($C_i \\times$ \\$100 Tax)", 
                               "$\\hat{\\gamma}_{500}^{DD}$ ($C_i \\times$ \\$500 Tax)"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "d-t-as",
          add.lines = list(c("Dep. Var. Mean (Chinese)", formatC(did_data_means_chi, format = "f"))))
                             
   
#slides: japanese immig
stargazer(didjlab, didjread, didjeng, didjwhip, didjearn,
          out = glue("{git}/output/slides/selection_japan.tex"), float = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          dep.var.labels = c("$\\mathbb{P}[\\text{Laborer}]$","$\\mathbb{P}[\\text{Literate}]$",
                             "$\\mathbb{P}[\\text{Speaks English}]$", "Age Heaping", "Earnings"),
          keep = c("BORNCHI*"), 
          covariate.labels = c("$\\hat{\\beta}_{1}$ (Born in China)", 
                               "$\\hat{\\gamma}_{100}^{DD}$ ($C_i \\times$ \\$100 Tax)", 
                               "$\\hat{\\gamma}_{500}^{DD}$ ($C_i \\times$ \\$500 Tax)"),
          keep.stat=c("n","adj.rsq"),
          table.layout = "d-t-as",
          add.lines = list(c("Dep. Var. Mean (Chinese)", formatC(did_data_means_chi_jap, format = "f"))))


#_____________________________________________________________
# DiD Regressions: US Imm control --------------------------
#_____________________________________________________________
did_data_us <- us_imm %>% filter(YEAR >= 1900 & YRIMM >= 1880) %>% #& YRIMM <= 1913) %>% #only keeping years with earnings/yrimm data
  mutate(LABOR = ifelse(OCC1950 %in% c(970, 650, 820, 830), 1, 0), #labor: includes general laborer, mine laborer, farm laborer
         BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
         BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
         BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
         BORNCHI_notax = BORNCHI * ifelse(tax == 0, 1, 0),
         AGEATIMM = AGE - (YEAR - YRIMM),
         twoyearpost = case_when(YRIMM %in% c(1886, 1887) ~ "_post50",
                                 YRIMM %in% c(1901,1902) ~ "post100",
                                 YRIMM %in% c(1904,1905) ~ "post500",
                                 TRUE ~ "_notpost"),
         oneyearpost = case_when(YRIMM == 1886 ~ "_post50",
                                 YRIMM == 1901 ~ "post100",
                                 YRIMM == 1904 ~ "post500",
                                 TRUE ~ "_notpost"),
         BORNCHI_twoyearpost50 = BORNCHI*ifelse(twoyearpost == "_post50", 1, 0),
         BORNCHI_twoyearpost100 = BORNCHI*ifelse(twoyearpost == "post100", 1, 0),
         BORNCHI_twoyearpost500 = BORNCHI*ifelse(twoyearpost == "post500", 1, 0),
         WEIGHT = 1,
         US = 1) # %>%
  #filter(MALE == 1 & AGEATIMM >= 18) 

did_data_compare <- bind_rows(did_data_us, did_data %>% mutate(US = 0))
  
# test
test <- did_data_compare %>% filter(BORNCHI == 1) %>% group_by(US, YRIMM) %>% 
  summarize(n = sum(WEIGHT),
            across(c(LABOR, CANREAD, EARN), function(.x) weighted.mean(.x, WEIGHT, na.rm=TRUE)))

ggplot(test, aes(x = YRIMM, y = LABOR, color = factor(US), size = n))+ geom_point()
ggplot(test, aes(x = YRIMM, y = CANREAD, color = factor(US), size = n))+ geom_point()
ggplot(test, aes(x = YRIMM, y = EARN, color = factor(US), size = n))+ geom_point()

# normal regs
did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500, 
                    data = did_data_us %>% filter(OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1), weights = WEIGHT)

did_reg_labor_compare <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + factor(US)*factor(tax), 
                       data = did_data_compare %>% filter(YRIMM < 1913 & YRIMM >= 1886 & OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1 & BORNCHI == 1), weights = WEIGHT)

did_reg_canread_compare <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + factor(US)*factor(tax), 
                            data = did_data_compare %>% filter(YRIMM < 1913 & OCC1950 != 290 & OCC1950 != 983 & AGE >= 18 & MALE == 1 & BORNCHI == 1), weights = WEIGHT)


summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))
summary(lm(SPEAKENG ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1 & AGE > 23 & !(OCC1950 %in% c(290, 983))), weights = WEIGHT))

summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1), weights = WEIGHT))
summary(lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1), weights = WEIGHT))

summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax50 + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))
summary(lm(WHIPPLE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500,
           data = did_data_japan %>% filter(MALE == 1 & AGE >= 23 & AGE <= 52), weights = WEIGHT))


# ########################################################################
# ### TABLE 4: US VS CAN REGRESSIONS
# ########################################################################
# did_data_us <- us_imm %>% filter(YEAR >= 1900 & YRIMM >= 1890 & YRIMM <= 1906 & YEAR != 1920) %>% #only keeping years with earnings/yrimm data
#   mutate(BORNCHI_tax50 = BORNCHI*ifelse(tax == 1496.19, 1, 0),
#          BORNCHI_tax100 = BORNCHI*ifelse(tax == 2992.61, 1, 0),
#          BORNCHI_tax500 = BORNCHI*ifelse(tax == 14115.70, 1, 0),
#          AGEATIMM = AGE - (YEAR - YRIMM),
#          ERSCORE = ifelse(EARN > 100, NA, EARN)) %>%
#   filter(AGEATIMM >= 18 & MALE == 1) %>% filter(BORNCHI == 1 | BORNJAP == 1)
# 
# did_reg_labor_us <- lm(LABOR ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_canread_us <- lm(CANREAD ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_earn_us <- lm(ERSCORE ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# did_reg_houseown_us <- lm(HOUSEOWN ~ factor(YRIMM) + factor(YEAR) + AGE + BORNCHI + BORNCHI_tax100 + BORNCHI_tax500, data = did_data_us)
# 
# stargazer(did_reg_labor_us, did_reg_canread_us, did_reg_earn_us, did_reg_houseown_us, 
#           out = glue("{git}/figs/8aug23/outcome_regs_us.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           keep = c("BORNCHI*"),
#           single.row = TRUE,
#           covariate.labels = c("$BORNCHI$", "$BORNCHI \\times$ \\$50 Tax", "$BORNCHI \\times$ \\$100 Tax", "$BORNCHI \\times$ \\$500 Tax"),
#           keep.stat=c("n","adj.rsq"),
#           table.layout = "=cd#-ta-s-",
#           add.lines = list(c("Dep. Var. Mean (SE)", "0.219 (0.000)", "0.884 (0.000)", "48.3 (0.007)", "0.332 (0.000)")))
# 
# mean(filter(did_data_us, !is.na(LABOR))$LABOR)
# sd(filter(did_data_us, !is.na(LABOR))$LABOR)/sqrt(nrow(filter(did_data_us, !is.na(LABOR))))
# 
# mean(filter(did_data_us, !is.na(CANREAD))$CANREAD)
# sd(filter(did_data_us, !is.na(CANREAD))$CANREAD)/sqrt(nrow(filter(did_data_us, !is.na(CANREAD))))
# 
# mean(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)
# sd(filter(did_data_us, !is.na(ERSCORE))$ERSCORE)/sqrt(nrow(filter(did_data_us, !is.na(ERSCORE))))
# 
# mean(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)
# sd(filter(did_data_us, !is.na(HOUSEOWN))$HOUSEOWN)/sqrt(nrow(filter(did_data_us, !is.na(HOUSEOWN))))
# 
# did_data_us_can <- bind_rows(did_data_japan %>% mutate(CAN = 1), did_data_us %>% mutate(CAN = 0))
# # run regressions -- triple diff with us and canada
# did_reg_uscan_labor <- lm(LABOR ~ factor(YRIMM)*CAN + factor(YEAR) +  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# did_reg_uscan_canread <- lm(CANREAD ~ factor(YRIMM)*CAN + factor(YEAR)+  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# did_reg_uscan_houseown <- lm(HOUSEOWN  ~ factor(YRIMM)*CAN + factor(YEAR)+  BORNCHI_tax100*CAN + BORNCHI_tax500*CAN, data = did_data_us_can, weights = WEIGHT)
# 
# stargazer(did_reg_uscan_labor, did_reg_uscan_canread, did_reg_uscan_houseown,
#           out = glue("{git}/figs/uscan_regs.tex"), float = FALSE, 
#           intercept.bottom =FALSE,
#           dep.var.labels = c("Laborer", "Literate", "Owns Home"),
#           keep = c(31, 62, 63, 64, 67, 68),
#           covariate.labels = c("$BORNCHI \\times US$", 
#                                "$BORNCHI \\times CANADA $",
#                                "$BORNCHI \\times US \\times $ \\$100 Tax",
#                                "$BORNCHI \\times US \\times $ \\$500 Tax",
#                                "$BORNCHI \\times CANADA \\times $ \\$100 Tax",
#                                "$BORNCHI \\times CANADA \\times $ \\$500 Tax"),
#           keep.stat=c("n","adj.rsq"),
#           add.lines = list(c("Includes Year $\\times$ Country FE", rep("Yes", 3))),
#           table.layout = "=cld#-ta-s-")
