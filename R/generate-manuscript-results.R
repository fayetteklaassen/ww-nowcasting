
# Colors, libraries, sources -----------------------------------------
library(RColorBrewer)
library(tidyverse)
require(sf)

source("R/wis.R")

brewer.pal(6, "Dark2")
shedCols <- c("MSD01" = "#1B9E77",
              "MSD02" = "#D95F02",
              "MSD03" = "#7570B3",
              "MSD04" = "#E7298A",
              "MSD05" = "#66A61E")


Col2 <- brewer.pal(3, "PuOr")
Col2 <- c(Col2[1], Col2[3])

# Reading in data files ---------------------------------------------------


shedpop <- read_csv("data-products/shedPop.csv")
shedpop <- rbind(shedpop, 
                 shedpop %>% summarize(shedID = "Total", pop = sum(pop)))
# shedInfo <- read_csv("data-products/sewershed-info.csv")
ww_by_week <- read_csv("data-products/cases-deaths-wastewater-rr-by-week.csv") %>%
  mutate(
    shedName = case_when(shedID == "total" ~ "Total",
                         TRUE~ paste0("MSD0", shedID))
  )
ww_by_date <- read_csv("data-products/wastewater-by-date.csv") %>%
  mutate(
    shedName = case_when(shedID == "total" ~ "Total",
                         TRUE~ paste0("MSD0", shedID))
  )

covidestim_input <- read_csv("data-products/covidestim-dataframe.csv") %>%
  mutate(cases = cases/pop) %>%
  mutate(
    shedName = case_when(shedID == "total" ~ "Total",
                         TRUE~ paste0("MSD0", shedID)),
    date = date -1)

covidestim_output <- read_csv("result/result.csv") %>%
  mutate(model = substr(model, 1, nchar(model)-8),
         config = substr(config, 1, nchar(config)-4),
         shedID = sub("shed", "", id),
         shedName = case_when(shedID == "total" ~ "Total",
                              TRUE~ paste0("MSD0", shedID)),
         shedID = case_when(shedID == "total" ~ "Total",
                            TRUE ~ shedID) ,
         ww_var = sub("ww\\-(.*)\\-.*\\-.*", "\\1", config),
         casesObs = case_when(grepl("case", model) ~ TRUE,
                              TRUE~FALSE),
         wwObs = case_when(grepl("ww", model) ~ TRUE,
                           TRUE ~ FALSE),
         month = as.numeric(sub("ww\\-.*\\-months(.*)\\-.*", "\\1", config)),
         Model = case_when(model == "cases-deaths" ~ "Cases-Deaths Model",
                           model == "cases-deaths-ww" ~ "Additive Model",
                           model == "deaths-ww" ~ "Substitutive Model",
                           TRUE ~ "Deaths-Only Model"),
         Model = factor(Model, levels = c(
           "Cases-Deaths Model", "Additive Model",
           "Substitutive Model", "Deaths-Only Model"))
  ) %>%
  left_join(shedpop, by = "shedID") %>%
  mutate(infections = infections/pop,
                  infections_p2_5 = infections_p2_5/pop,
                  infections_p97_5 = infections_p97_5/pop
  ) %>%
  mutate(date = case_when(shedID %in% c("Total", "1","2") ~ date,
                          TRUE ~ date + 7)) %>%
  mutate(casesObs = case_when(casesObs == TRUE ~ "Cases included",
                              TRUE ~ "No cases included")) %>%
  mutate(wwObs = case_when(wwObs == TRUE ~ "Wastewater included",
                           TRUE ~ "No wastewater included")) 


covidestim_output_deaths <- read_csv("result-deaths/result.csv") %>%
  mutate(model = substr(model, 1, nchar(model)-8),
         config = substr(config, 1, nchar(config)-4),
         shedID = sub("shed", "", id),
         shedName = case_when(shedID == "total" ~ "Total",
                              TRUE~ paste0("MSD0", shedID)),
         shedID = case_when(shedID == "total" ~ "Total",
                            TRUE ~ shedID) ,
         ww_var = sub("ww\\-(.*)\\-.*\\-.*", "\\1", config),
         casesObs = case_when(grepl("case", model) ~ TRUE,
                              TRUE~FALSE),
         wwObs = case_when(grepl("ww", model) ~ TRUE,
                           TRUE ~ FALSE),
         month = as.numeric(sub("ww\\-.*\\-months(.*)\\-.*", "\\1", config)),
         Model = case_when(model == "cases-deaths" ~ "Cases-Deaths Model",
                           model == "cases-deaths-ww" ~ "Additive Model",
                           model == "deaths-ww" ~ "Substitutive Model",
                           TRUE ~ "Deaths-Only Model"),
         Model = factor(Model, levels = c(
           "Cases-Deaths Model", "Additive Model",
           "Substitutive Model", "Deaths-Only Model"))
  ) %>%
  left_join(shedpop, by = "shedID") %>%
  mutate( infections = infections/pop,
                  infections_p2_5 = infections_p2_5/pop,
                  infections_p97_5 = infections_p97_5/pop
  ) %>%
  mutate(date = case_when(shedID %in% c("Total", "1","2") ~ date,
                          TRUE ~ date + 7)) %>%
  mutate(casesObs = case_when(casesObs == TRUE ~ "Cases included",
                              TRUE ~ "No cases included")) %>%
  mutate(wwObs = case_when(wwObs == TRUE ~ "Wastewater included",
                           TRUE ~ "No wastewater included")) 

samples_output <- read_csv("result/result-ss.csv") %>%
  mutate(model = substr(model, 1, nchar(model)-8),
         config = substr(config, 1, nchar(config)-4),
         shedID = sub("shed", "", id),
         ww_var = sub("ww\\-(.*)\\-.*\\-.*", "\\1", config),
         month = as.numeric(sub("ww\\-.*\\-months(.*)\\-.*", "\\1", config))) 

sero <- read_csv("data-products/serosurvey-by-date.csv") %>%
  filter(shedCluster != "Other")

cluster <- read_csv("data-products/sewershed-info.csv") %>% 
  transmute(id = paste0("shed", shedID), 
            shedCluster) %>%
  filter(shedCluster != "Other") %>%
  rbind(data.frame(id = "shedtotal", shedCluster = "Total")) 

jhu <- read_csv("data-products/jhu-estimates.csv") %>%
  filter(fips == "21111") %>%
  filter(date < as.Date("2021-05-01")) %>%
  transmute(date, infections, Rt, cum.incidence) 

# Figure 1: sewershed map --------------------------------------------------
fgdb <- "data-sources/shapefilezip/p12/sewer_zones.gdb"

fc <- sf::st_read(fgdb) %>%
  mutate(Sewershed = factor(paste0("MSD0", ZoneNUM)))
map_data("county") %>% 
  filter(region == "kentucky" & subregion == "jefferson") -> ctt

ggplot() +
  geom_sf(data = fc %>% filter(ZoneNUM < 6), aes(fill = Sewershed)) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  geom_polygon(data = ctt %>%
                 rename(Longitude = long,
                        Latitude = lat),
               aes(Longitude, Latitude,group=group), fill = NA, color = "black") +
  geom_sf_label(data = fc %>% filter(ZoneNUM < 6), aes(label = Sewershed,size ='10pt')) +
  scale_fill_manual("",values = shedCols) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none", text = element_text(family = "Times New Roman"),
        axis.text.x = element_blank(),
              axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
              axis.ticks = element_blank(),
              rect = element_blank()) -> fig1
tiff("figures/Fig1-map.tiff", 
     units = "in", 
     res = 300,
     height = 3.54,
     width = 3.54,
     type = "cairo")
fig1
dev.off()

# Figure S1: wastewater data by week, with/without outliers -----------------

twoCol <- c("N1" = Col2[1],
            "N1_ts" = Col2[2])

twoVar <- c("N1" = "All observations",
            "N1_ts" = "Outliers removed")

ww_by_week %>% 
  transmute(shedName, weekstart, N1, N1_ts) %>%
  pivot_longer(cols = c("N1", "N1_ts"),
               names_to = "outliers",
               values_to = "N1") %>% 
  filter(weekstart > as.Date("2020-08-16")) %>%
  ggplot(aes(x = weekstart, y = N1)) +
  geom_point(aes(col = outliers), size = 1) +
  scale_x_date("")+
  geom_line(aes(col = outliers)) + 
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,400))+ 
  geom_ribbon(data = covidestim_output %>%
                filter(date > as.Date("2020-08-10")) %>%
                filter(ww_var %in% c("N1", "N1_ts")) %>%
                filter(shedID != "Total") %>%
                filter(model %in% c("cases-deaths-ww")) %>%
                group_by(shedID) %>%
                filter(month == max(month)) %>%
                ungroup() %>%
                mutate(weekstart = date, outliers = ww_var,
                       shedName = case_when(shedID == "Total"~ "Total",
                                            TRUE ~paste0("MSD0", shedID))),
              aes(x= weekstart, y = fitted_wastewater_prvl, ymin = fitted_wastewater_prvl_p2_5,
                  ymax = fitted_wastewater_prvl_p97_5, fill = outliers), alpha = .2) +
  facet_grid(~shedName) +
  scale_fill_manual("95% credible interval of model estimates", values = twoCol, labels = twoVar) +
  scale_color_manual("Input data", values = twoCol, labels = twoVar) +
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)", limits = c(0,400)) +
  scale_x_date("", date_labels = "%b'%y", minor_breaks = '1 month') -> sup1
sup1

tiff("figures/supFig1-outlier.tiff", 
     units = "in", 
     res = 1000,
     height = 4.48,
     width = 10.48,
     type = "cairo")
sup1
dev.off()
# Table S2: descriptives of how many outliers for each sewershed rem --------
### Required dataframe, date, shedID, N1, N1 removed
ww_by_date %>%
  group_by(shedID) %>%
  drop_na(N1) %>%
  summarize(Nout = sum(is.na(N1_ts) & !is.na(N1), na.rm = TRUE),
            Nobs = sum(!is.na(N1), na.rm = TRUE))

# Figure 2: timeseries ww versus infections, Rt --------
infscale <- 10000
casscale <- 50000

covidestim_output %>% 
  filter(model == "cases-deaths") %>%
  group_by(shedID) %>%
  filter(month == max(month)) %>%
  ungroup() %>%
  transmute(shedName, date, infections, 
            inf100k = infections*100000,
            inf_scaled = infections*10000,
            r_t,
            rt_scaled = (r_t-1) * 500) %>%
  left_join(covidestim_input %>% 
              transmute(shedName, date, N1, N1_diff1, cases, N1_ts,
                        ratio,N1_by_flow,
                        N1_ts_diff1,
                        cas100k = cases*100000,
                        cases_scaled = cases*50000),
            by = c("shedName", "date")) -> input_output

# Figure 2a: Infections timeseries ---------------------------------------------------
twoCol <- c("inf_scaled" = Col2[1],
            "N1" = Col2[2])

twoVar <- c("inf_scaled" = "Estimated infections per 100K",
            "N1" = "Wastewater SARS-CoV-2 (N1)")
input_output %>%
  pivot_longer(cols = c("inf_scaled", "N1"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, col = variable)) +
  facet_wrap(~shedName) +
  geom_line(aes()) +
  geom_point(aes(),size = 0.5) +
  scale_color_manual("",
                     values = twoCol,
                     labels = twoVar)+
  theme_bw(base_size = 10)+
  theme(legend.position = "top", text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 8)) +
  scale_x_date("") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)",    
                     sec.axis = sec_axis(~ .*10 , name = "Estimated infections per 100K")) ->p2a

# Figure 2b: Rt timeseries ------------------------------------------------
twoCol <- c("rt_scaled" = Col2[1],
            "N1_diff1" = Col2[2])

twoVar <- c("rt_scaled" = "Estimated Rt",
            "N1_diff1" = "d/dt (Wastewater SARS-CoV-2 (N1))")

input_output %>%
  pivot_longer(cols = c("rt_scaled", "N1_diff1"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, col = variable)) +
  facet_wrap(~shedName) + 
  geom_line() +
  scale_color_manual("",
                     values = twoCol,
                     labels = twoVar)+
  geom_point(size = 0.5) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 8))+
  scale_x_date("") + 
  scale_y_continuous("d/dt SARS-CoV-2 (N1) (copies/ml)",    
                     sec.axis = sec_axis(~ ./500+1 , name = "Estimated Rt")) -> p2b
# Figure 2cd: scatterplot of ww versus, infections,rt -----------------

ggplot(input_output %>%
         filter(shedName != "Total"), 
       aes(y = N1, x = infections*100000)) +
  geom_smooth(method = "lm", col = "black", fill = "black") + 
  geom_point(aes(col = shedName)) +
  scale_color_manual("", values = shedCols) +
  # guides(col = "none") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)") +
  scale_x_continuous("Estimated infections per 100K", limits = c(0, 3000))+
  theme_bw(base_size = 10) +
  theme(text = element_text(family = "Times New Roman"), legend.position = "bottom",
        legend.text = element_text(size= 6),
        axis.title = element_text(size = 8))-> p2c

ggplot(input_output %>% filter(shedName != "Total") %>%
         drop_na(N1), aes(y = N1_diff1, x = r_t)) +
  geom_smooth(method = "lm", col = "black", fill = "black") + 
  geom_point(aes(col = shedName)) +
  scale_color_manual("", values = shedCols) + 
  scale_y_continuous("d/dt SARS-CoV-2 (N1) (copies/ml)") +
  scale_x_continuous("Estimated Rt")+
  # guides(col = "none") + 
  theme_bw(base_size = 10)+
  theme(text = element_text(family = "Times New Roman"), legend.position ="bottom",
        legend.text=element_text(size=6),
        axis.title = element_text(size = 8)) -> p2d

gridExtra::grid.arrange(p2a,p2b,p2c,p2d,
                        layout_matrix = matrix(c(1,2,1,2,3,4,3,4),
                                               nrow = 4, byrow = TRUE))
tiff("figures/Fig2-grid.tiff", 
     units = "in", 
     res = 1000,
     height = 7.48,
     width = 7.48,
     type = "cairo")
ggpubr::ggarrange(p2a, p2b, p2c, p2d,
                  nrow = 2, ncol =2, labels = "auto"
)
dev.off()


# Supplemenatry Figure 2 --------------------------------------------------

# Figure sup2a: Cases timeseries ---------------------------------------------------
twoCol <- c("cases_scaled" = Col2[1],
            "N1" = Col2[2])

twoVar <- c("cases_scaled" = "Cases per 100K",
            "N1" = "Wastewater SARS-CoV-2 (N1)")
input_output %>%
  pivot_longer(cols = c("cases_scaled", "N1"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, col = variable)) +
  facet_wrap(~shedName) +
  geom_line(aes()) +
  geom_point(aes(),size = 0.5) +
  scale_color_manual("",
                     values = twoCol,
                     labels = twoVar)+
  theme_bw(base_size = 10)+
  theme(legend.position = "top", text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 8)) +
  scale_x_date("") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)",    
                     sec.axis = sec_axis(~ .*2 , name = "Cases per 100K")) ->ps2a

# Figure sup2b: ratio timeseries ---------------------------------------------------
twoCol <- c("inf100k" = Col2[1],
            "ratio" = Col2[2])

twoVar <- c("inf100k" = "Estimated infections per 100K",
            "ratio" = "Wastewater SARS-CoV-2 (N1) divided by PMMoV")
input_output %>%
  mutate(inf100k = inf100k/500000) %>%
  filter(ratio < .01) %>%
  pivot_longer(cols = c("inf100k", "ratio"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, col = variable)) +
  facet_wrap(~shedName) +
  geom_line(aes()) +
  geom_point(aes(),size = 0.5) +
  scale_color_manual("",
                     values = twoCol,
                     labels = twoVar)+
  theme_bw(base_size = 10)+
  theme(legend.position = "top", text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 6)) +
  scale_x_date("") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml) divided by PMMoV (copies/ml)",    
                     sec.axis = sec_axis(trans = ~.*500000 , name = "Estimated infections per 100K")) ->ps2b

# Figure sup2c: N1/flow timeseries ---------------------------------------------------
twoCol <- c("inf100k" = Col2[1],
            "N1_by_flow" = Col2[2])

twoVar <- c("inf100k" = "Estimated infections per 100K",
            "N1_by_flow" = "Wastewater SARS-CoV-2 (N1) divided by flow")
input_output %>%
  mutate(inf100k = inf100k/50) %>%
  filter(ratio < .01) %>%
  pivot_longer(cols = c("inf100k", "N1_by_flow"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = date, y = value, col = variable)) +
  facet_wrap(~shedName) +
  geom_line(aes()) +
  geom_point(aes(),size = 0.5) +
  scale_color_manual("",
                     values = twoCol,
                     labels = twoVar)+
  theme_bw(base_size = 10)+
  theme(legend.position = "top", text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 6)) +
  scale_x_date("") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml) divided by flow (MGD)",    
                     sec.axis = sec_axis(trans = ~.*50 , name = "Estimated infections per 100K")) ->ps2c
ps2c

## Cases for supplement
ggplot(input_output %>%
         filter(shedName != "Total"), 
       aes(y = N1, x = cases*100000)) +
  geom_smooth(method = "lm", col = "black", fill = "black") + 
  geom_point(aes(col = shedName)) +
  scale_color_manual("", values = shedCols) +
  # guides(col = "none") + 
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)") +
  scale_x_continuous("Cases per 100K", limits = c(0, 800))+
  theme_bw(base_size = 10) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        axis.title = element_text(size = 8))-> ps2d

## Ratio for supplement
ggplot(input_output %>%
         filter(ratio < .01) %>%
         filter(shedName != "Total"), 
       aes(y = ratio, x = inf100k)) +
  geom_smooth(method = "lm", col = "black", fill = "black") + 
  geom_point(aes(col = shedName)) +
  scale_color_manual("", values = shedCols) +
  scale_y_continuous("SARS-CoV-2 (N1) (cp./ml) divided by PMMoV (cp./ml)") +
  scale_x_continuous("Estimated infections per 100K")+
  theme_bw(base_size = 10) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        axis.title = element_text(size = 8))-> ps2e
## Flow for supplement
ggplot(input_output %>%
         filter(shedName != "Total"), 
       aes(y = N1_by_flow, x = inf100k)) +
  geom_smooth(method = "lm", col = "black", fill = "black") + 
  geom_point(aes(col = shedName)) +
  scale_color_manual("", values = shedCols) +
  scale_y_continuous("SARS-CoV-2 (N1) divided by flow (MGD)") +
  scale_x_continuous("Estimated infections per 100K")+
  theme_bw(base_size = 10) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        axis.title = element_text(size = 8))-> ps2f
ps2f
tiff("figures/FigS2-grid.tiff", 
     units = "in", 
     res = 1000,
     height = 7.48,
     width = 14.48,
     type = "cairo")
ggpubr::ggarrange(ps2a,ps2b,ps2c,ps2d,ps2e,ps2f,
                  nrow = 2, ncol =3, labels = "auto"
)
dev.off()
# Analysis: correlation and regression ww versus infection, cases, Rt ---------------------

cor.test(input_output$inf100k, input_output$N1)
cor.test(input_output$cas100k, input_output$N1)
cor.test(input_output$r_t, input_output$N1_diff1)
cor.test(input_output$cas100k, input_output$N1)
cor.test(input_output$inf100k, input_output$ratio)
cor.test(input_output$inf100k, input_output$N1_by_flow)

# Table 1: Predictive performance within models ---------------------------
samples_output %>%
  filter(ww_var == "N1") %>%
  filter(shedID == "total") %>%
  filter(par == "infections") -> samples_inft

samples_inft %>%
  group_by(month) %>%
  filter(week == max(week)) %>%
  ungroup() %>%
  pivot_longer(cols = -c("week", "month", "id", "model",
                         "ww_var",
                         "shedID", "par", "config"),
               names_to = "iter",
               values_to = "inft") %>%
  transmute(month, shedID, model, week,
            ww_var, inft, iter) -> samples_x

endWeeks <- unique(samples_x$week)

samples_inft %>%
  filter(month == max(month)) %>%
  pivot_longer(cols = -c("week", "month", "id", "model",
                         "ww_var", "shedID", "par", "config"),
               names_to = "iter",
               values_to = "inft_final") %>%
  transmute(month, shedID, model, week, 
            ww_var, inft_final, iter) -> samples_y

mods <- unique(samples_x$model)
# within model comparison -------------------------------------------------


out_within <- data.frame()
for(i in mods){
  for(w in endWeeks){
    x <- samples_x %>% 
      filter(week == w) %>% 
      filter(model == i) %>%
      pull(inft) %>% log()
    y <- samples_y %>%
      filter(week == w) %>%
      filter(model == i) %>%
      pull(inft_final) %>% log()
    
    tmp <- wis(x, y)
    tmpdf <- tmp$df
    out_within <- rbind(out_within, tmpdf %>% mutate(week = w,
                                                     model = i))
  }
}

out_within
out_within %>%
  filter(week > 9) %>%
  filter(week != 31) %>%
  # filter(!(model == "deaths-ww" & week == 17)) %>%
  # filter(!(model == "cases-deaths-ww" & week %in% c(17,21,29))) %>%
  # filter(!(model == "deaths" & week == 33)) %>%
  group_by(model) %>%
  summarize(WIS = mean(WIS),
            coverage95= mean(coverage95),
            bias = mean(bias),
            bias_std = mean(bias_std),
            width = mean(width), widthstd = mean(widthstd))


# historic reconstruction, from the final run ----------------------------
out_hist <- data.frame()
for(i in mods){
  for(w in unique(samples_y$week)){
    x <- samples_y %>% 
      filter(week == w) %>% 
      filter(model == i) %>%
      pull(inft_final) %>% log()
    y <- samples_y %>%
      filter(week == w) %>%
      filter(model == mods[1]) %>%
      pull(inft_final) %>% log()
    
    tmp <- wis(x,y)
    tmpdf <- tmp$df
    out_hist <- rbind(out_hist, tmpdf %>% mutate(week = w,
                                                 model = i))
  }
}

out_hist %>%
  filter(week > 9) %>%
  filter(week != 31) %>%
  group_by(model) %>%
  summarize(WIS = mean(WIS),
            bias = mean(bias),
            bias_std = mean(bias_std),
            stdy = mean(stdy),
            dev = mean(dev),
            coverage95 = mean(coverage95),
            width = mean(width),
            widthstd = mean(widthstd),
            .groups = 'drop')

out_hist %>%
  filter(week == max(week))


# between model comparison, predictive performance --------------------------------------
out_between <- data.frame()
for(i in mods){
  for(w in endWeeks){
    x <- samples_x %>% 
      filter(week == w) %>% 
      filter(model == i) %>%
      pull(inft) %>% log()
    y <- samples_x %>%
      filter(week == w) %>%
      filter(model == mods[1]) %>%
      pull(inft) %>% log()
    
    tmp <- wis(x,y)
    tmpdf <- tmp$df
    out_between <- rbind(out_between, tmpdf %>% mutate(week = w,
                                                       model = i))
    # print(paste("Week", w, "model", i, "width", tmpdf$width))
  }
}

out_between %>%
  filter(week > 9) %>%
  group_by(model) %>%
  summarize(WIS = mean(WIS),
            bias = mean(bias),
            coverage95 = mean(coverage95),
            width = mean(width),
            widthstd = mean(widthstd),
            .groups = 'drop')

out_hist %>%
  filter(week == max(week))


# Table 2: between location comparison ------------------------------------

samples_output %>%
  filter(ww_var == "N1") %>%
  filter(model %in% c("cases-deaths", "cases-deaths-ww")) %>%
  filter(par == "infections") -> samples_inft_loc

samples_inft_loc %>%
  group_by(shedID) %>%
  filter(month == max(month)) %>%
  ungroup() %>%
  pivot_longer(cols = -c("week", "month", "id", "model",
                         "ww_var",
                         "shedID", "par", "config"),
               names_to = "iter",
               values_to = "inft") %>%
  transmute(month, 
            shedID = case_when(shedID == "total" ~ "Total",
                               TRUE ~ shedID), 
            model, week,
            # week = case_when(shedID %in% c("Total", "1","2") ~ week,
            #                  TRUE~ week + 2),
            ww_var, inft, iter) %>%
  left_join(shedpop, by = "shedID") %>%
  mutate(inft = inft/pop*100000) -> samples_y_loc

modsY <- unique(samples_y_loc$model)
# between model and location comparison, historic --------------------------------------

out_hist_loc <- data.frame()
for(i in modsY){
  print(i)
  for(s in 1:5){
    print(s)
    weeks <- samples_y_loc %>% filter(shedID == paste(s)) %>%
      pull(week) %>% unique()
    for(w in weeks){
      print(w)
      x <- samples_y_loc %>% 
        filter(shedID == paste(s)) %>%
        filter(week == w) %>% 
        filter(model == i) %>%
        pull(inft) %>% log()
      y <- samples_y_loc %>%
        filter(shedID == "Total") %>%
        filter(week == w) %>%
        filter(model == i) %>%
        pull(inft) %>% log()
      
      tmp <- wis(x,y)
      tmpdf <- tmp$df
      out_hist_loc <- rbind(out_hist_loc, tmpdf %>% mutate(week = w,
                                                           model = i,
                                                           shedID = s))
    }
  }
}

out_hist_loc %>%
  filter(week > 9) %>%
  group_by(model, shedID) %>%
  summarize(WIS = mean(WIS),
            bias = mean(bias),
            coverage95 = mean(coverage95),
            width = mean(width),
            widthstd = mean(widthstd),
            .groups = 'drop')
out_hist_loc2 <- data.frame()
for(i in modsY){
  print(i)
  for(s in 1:5){
    print(s)
    weeks <- samples_y_loc %>% filter(shedID == paste(s)) %>%
      pull(week) %>% unique()
    for(w in weeks){
      print(w)
      x <- samples_y_loc %>% 
        filter(shedID == paste(s)) %>%
        filter(week == w) %>% 
        filter(model == i) %>%
        pull(inft) %>% log()
      y <- samples_y_loc %>%
        filter(shedID == "Total") %>%
        filter(week == w) %>%
        filter(model == i) %>%
        pull(inft) %>% log()
      
      tmp <- wis(y,x)
      tmpdf <- tmp$df
      out_hist_loc2 <- rbind(out_hist_loc2, tmpdf %>% mutate(week = w,
                                                             model = i,
                                                             shedID = s))
    }
  }
}

out_hist_loc2 %>%
  filter(week > 9) %>%
  group_by(model, shedID) %>%
  summarize(WIS = mean(WIS),
            bias = mean(bias),
            coverage95 = mean(coverage95),
            width = mean(width),
            widthstd = mean(widthstd),
            .groups = 'drop')

out_hist %>%
  filter(week == max(week))


# Figure 3: total sewershed timeseries estimates of the last date ----------
twoCol <- c("No wastewater included" = Col2[1],
            "Wastewater included" = Col2[2])

twoVar <- c("inf_scaled" = "Estimated infections",
            "N1" = "Wastewater SARS-CoV-2 (N1) (copies/ml)")
covidestim_output %>% 
  filter(shedID == "Total") %>%
  filter(month == max(month)) %>%
  filter(ww_var == "N1") %>%
  filter(date > as.Date("2020-08-16")) %>%
  ggplot(aes(x = date, y = infections*100000, col = wwObs)) +
  geom_line() +
  geom_ribbon(aes(ymin = infections_p2_5*100000,
                  ymax = infections_p97_5*100000,
                  fill = wwObs),
              alpha = .2, linetype = 2) +
  theme_bw(base_size = 10) +
  theme(text = element_text(family = 'Times New Roman'), legend.position = "top") + 
  scale_y_continuous("Estimated infections per 100K") +
  scale_x_date("", minor_breaks = '1 month',
               date_labels = "%b'%y") +
  scale_color_manual("",
                     values = twoCol) + 
  scale_fill_manual("",
                    values = twoCol) + 
  facet_wrap(~casesObs) -> p3

tiff("figures/Fig3-inf.tiff", 
     units = "in", 
     res = 1000,
     height = 5.48,
     width = 7.48,
     type = "cairo")
p3
dev.off()


# Figure 4: between location comparison -----------------------------------

shedCols <- c("MSD01" = "#1B9E77",
              "MSD02" = "#D95F02",
              "MSD03" = "#7570B3",
              "MSD04" = "#E7298A",
              "MSD05" = "#66A61E",
              "Total" = "#E6AB02")
shedFills <- c("MSD01"= "white",
               "MSD02" = "white",
               "MSD03" = "white",
               "MSD04" = "white",
               "MSD05" = "white",
               "Total" = "#E6AB02")


covidestim_output %>%
  filter(ww_var == "N1") %>%
  filter(date > as.Date("2020-08-10"))  %>%
  filter(model != "deaths") %>%
  filter(model != "deaths-ww") %>%
  mutate(
    infections_p2_5 = case_when(shedID == "Total" ~ infections_p2_5),
    infections_p97_5 = case_when(shedID == "Total" ~infections_p97_5)) %>%
  group_by(shedID) %>%
  filter(month == max(month)) %>% 
  ungroup() %>%
  ggplot(aes(x = date, y = infections*100000, col = shedName)) +
  geom_line() +
  geom_ribbon(aes(ymin = infections_p2_5*100000, 
                  ymax = infections_p97_5*100000,
                  fill = shedName),
              col = NA, 
              alpha = .2) +
  theme_bw(base_size = 10) + 
  theme(text = element_text(family = "Times New Roman")) + 
  scale_x_date("", date_labels = "%b'%y") +
  scale_y_continuous("Estimated infections per 100K") + 
  scale_color_manual("", values = shedCols) + 
  scale_fill_manual("", values = shedFills)+
  theme(legend.position = "none") +
  facet_grid(~Model) -> p4a

covidestim_output %>%
  filter(ww_var == "N1") %>%
  filter(date > as.Date("2020-08-10"))  %>%
  filter(model != "deaths") %>%
  filter(model != "deaths-ww") %>%
  mutate(
    r_t_p2_5 = case_when(shedID == "Total" ~ r_t_p2_5),
    r_t_p97_5 = case_when(shedID == "Total" ~r_t_p97_5)) %>%
  group_by(shedID) %>%
  filter(month == max(month)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = r_t, col = shedName)) +
  geom_line() +
  geom_ribbon(aes(ymin = r_t_p2_5, 
                  ymax = r_t_p97_5,
                  fill = shedName),
              col = NA,
              alpha = .2) +
  theme_bw(base_size = 10) + 
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom", legend.box = "horizontal") + 
  scale_color_manual("Sewershed",values = shedCols) + 
  scale_fill_manual("Sewershed",values = shedFills) + 
  scale_y_continuous("Estimated Rt", limits = c(0.5,2)) +
  scale_x_date("", minor_breaks = '1 month',
               date_labels = "%b'%y")+
  facet_grid(~Model) ->p4b

gridExtra::grid.arrange(p4a,p4b, nrow = 2)
tiff("figures/Fig4-sewersheds.tiff", 
     units = "in", 
     res = 1000,
     height = 7.48,
     width = 7.48,
     type = "cairo")
ggpubr::ggarrange(p4a, p4b,
                  nrow = 2, ncol =1
)
dev.off()

# Figure 5: serosurvey comparison -----------------------------------------
fipspop <- covidestim::get_pop("21111")

modCols <- c("Cases-Deaths Model" = "#1B9E77",
              "Additive Model" = "#D95F02",
              "Substitutive Model" = "#7570B3",
              "Deaths-Only Model" = "#E7298A",
              "Cases-Deaths, JHU data" = "#66A61E",
              "Serosurvey data with errorbar" = "black")

covidestim_output %>%
  filter(ww_var == "N1") %>%
  left_join(cluster, 
            by = c("id")) %>%
  group_by(shedID) %>%
  filter(month == max(month)) %>%
  ungroup() %>%
  group_by(date, Model, shedCluster) %>%
  summarize(infections_cumulative_pct = sum(infections_cumulative)/sum(unique(pop))*100,
            infections_cumulative_pct2_5 = sum(infections_cumulative_p2_5)/sum(unique(pop))*100,
            infections_cumulative_pct97_5= sum(infections_cumulative_p97_5)/sum(unique(pop))*100,
            .groups = 'drop') %>%
  rbind(jhu %>% transmute(infections_cumulative_pct = cum.incidence/fipspop*100,
                          infections_cumulative_pct2_5= NA,
                          infections_cumulative_pct97_5 = NA, 
                          shedCluster = "MSD01",
                          date,
                          Model = "Cases-Deaths, JHU data")) %>%
  rbind(jhu %>% transmute(infections_cumulative_pct = cum.incidence/fipspop*100,
                          infections_cumulative_pct2_5= NA,
                          infections_cumulative_pct97_5 = NA, 
                          shedCluster = "MSD02",
                          date,
                          Model = "Cases-Deaths, JHU data")) %>%
  rbind(jhu %>% transmute(infections_cumulative_pct = cum.incidence/fipspop*100,
                          infections_cumulative_pct2_5= NA,
                          infections_cumulative_pct97_5 = NA, 
                          shedCluster = "MSD03-05",
                          date,
                          Model = "Cases-Deaths, JHU data")) %>%
  rbind(jhu %>% transmute(infections_cumulative_pct = cum.incidence/fipspop*100,
                          infections_cumulative_pct2_5= NA,
                          infections_cumulative_pct97_5 = NA, 
                          shedCluster = "Total",
                          date,
                          Model = "Cases-Deaths, JHU data")) %>%
  rbind(sero %>% transmute(infections_cumulative_pct = seropos,
                           infections_cumulative_pct2_5 = seropos_low,
                           infections_cumulative_pct97_5 = seropos_high,
                           shedCluster, date, Model = "Serosurvey data with errorbar")) %>%

  ggplot(aes(x = date, col = Model)) +
  scale_y_continuous(limits = c(0,100)) + 
  geom_line(data = . %>% filter(Model != "Serosurvey data with errorbar"), 
            aes( y = infections_cumulative_pct,  col = Model)) + 
  geom_point(data = . %>% filter(Model == "Serosurvey data with errorbar"), aes(x = date, y = infections_cumulative_pct))+
  geom_errorbar(data = . %>% filter(Model == "Serosurvey data with errorbar"), aes(ymin = infections_cumulative_pct2_5,
                                 ymax = infections_cumulative_pct97_5),
                fill = NA, linetype = 2, col = "black") + 
  scale_color_manual("", values = modCols) + 
  facet_wrap(~shedCluster) +
  scale_y_continuous("Seroprevalence / estimated cumulative indidence (%)")+
  theme_bw(base_size = 10) + 
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom") + 
  scale_x_date("", date_labels = "%b'%y", minor_breaks = '1 month') -> p5

p5
tiff("figures/Fig5-sero.tiff", 
     units = "in", 
     res = 1000,
     height = 7.48,
     width = 7.48,
     type = "cairo")
p5
dev.off()

# Figure S3: predictive power ----------------------------------------------
monthCol <- brewer.pal(7, "Dark2")
monthCol <- monthCol[7:1]
shedNames <- unique(covidestim_output$shedName)
shedIDs <- unique(covidestim_output$shedID)

p <- vector("list")
for(i in unique(covidestim_output$shedID)){
  covidestim_output %>%
    filter(shedID == i) %>%
    filter(ww_var == "N1") %>%
    filter(date > as.Date("2020-08-10")) %>%
    mutate(minval = case_when(shedID %in% c("Total", "1") ~ 1,
                              TRUE ~ 2)) %>%
    filter(month > minval) %>%
    ggplot(aes(x = date, y = log(infections*pop), col = factor(month))) +
    geom_line()+ 
    geom_ribbon(aes(ymin = log(infections_p2_5*pop),
                    ymax = log(infections_p97_5*pop),
                    fill = factor(month)),
                col = NA,
                alpha = .2) +
    theme_bw() +
    scale_color_manual("", values = monthCol)+
    scale_x_date("", date_labels = "%b'%y", minor_breaks = '1 month')+
    scale_fill_manual("", values = monthCol)+
    scale_y_continuous("Estimated log(infections)") + 
    ggtitle(shedNames[which(shedIDs == i)]) +
    facet_wrap(~Model,nrow = 1) -> p[[i]]
}
tiff("figures/sup3-total.tiff", 
     units = "in", 
     res = 1000,
     height = 15.48,
     width = 7.48,
     type = "cairo")
ggpubr::ggarrange(p[["Total"]],
                  p[["1"]],
                  p[["2"]],
                  p[["3"]],
                  p[["4"]],
                  p[["5"]],nrow = 6, ncol =1, labels = "auto"
)
dev.off()

# Figure s4: predictive powerRt ----------------------------------------------
monthCol <- brewer.pal(7, "Dark2")
monthCol <- monthCol[7:1]
shedNames <- unique(covidestim_output$shedName)
shedIDs <- unique(covidestim_output$shedID)

q <- vector("list")
for(i in unique(covidestim_output$shedID)){
  covidestim_output %>%
    filter(shedID == i) %>%
    filter(ww_var == "N1") %>%
    filter(date > as.Date("2020-08-10")) %>%
    mutate(minval = case_when(shedID %in% c("Total", "1") ~ 1,
                              TRUE ~ 2)) %>%
    filter(month > minval) %>%
    ggplot(aes(x = date, y = log(r_t), col = factor(month))) +
    geom_line()+ 
    geom_ribbon(aes(ymin = log(r_t_p2_5),
                    ymax = log(r_t_p97_5),
                    fill = factor(month)),
                col = NA,
                alpha = .2) +
    theme_bw() +
    scale_color_manual("", values = monthCol)+
    scale_x_date("", date_labels = "%b'%y", minor_breaks = '1 month')+
    scale_fill_manual("", values = monthCol)+
    scale_y_continuous("Estimated log(Rt)") + 
    ggtitle(shedNames[which(shedIDs == i)]) +
    facet_wrap(~Model,nrow = 1) -> q[[i]]
}
tiff("figures/sup4-total.tiff", 
     units = "in", 
     res = 1000,
     height = 15.48,
     width = 7.48,
     type = "cairo")
ggpubr::ggarrange(q[["Total"]],
                  q[["1"]],
                  q[["2"]],
                  q[["3"]],
                  q[["4"]],
                  q[["5"]],nrow = 6, ncol =1, labels = "auto"
)
dev.off()


# Supplementary Figure 5 --------------------------------------------------

covidestim_output_deaths %>%
  transmute(date, shedID,shedName, Model, infections = infections*100000,
            pop, infections_p2_5=infections_p2_5*100000,
            infections_p97_5 = infections_p97_5  *100000,
            Deaths = "imputed proportional to SARS-CoV-2 mortality risk") %>%
  rbind(
    covidestim_output %>%
      filter(month == max(month)) %>%
      filter(ww_var == "N1") %>%          
      transmute(date, shedID, Model, shedName,
                infections = infections *100000, 
                pop, 
                infections_p2_5 = infections_p2_5 *100000, 
                infections_p97_5 = infections_p97_5 *100000,
                Deaths = "imputed proportional to population size")   ) %>%
  filter(shedID != "Total") %>%filter(date > as.Date("2020-08-16")) %>%
  ggplot(aes(x = date, y = infections, col = Deaths)) +
  geom_line() +
  scale_y_continuous("Estimated infections per 100K", limits = c(0,20000)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date("") +
  geom_ribbon(aes(ymin = infections_p2_5, ymax = infections_p97_5, fill = Deaths), col = NA, alpha = .2) +
  facet_grid(Model~shedName) -> ps5


tiff("figures/Figs5-deaths.tiff", 
     units = "in", 
     res = 1000,
     height = 7.48,
     width = 7.48,
     type = "cairo")
ps5
dev.off()


# Final supplement: case, deaths,ww ---------------------------------------

covidestim_input %>%
  filter(shedID != "total") %>%
  transmute(shedID, date, cases*100000, deaths = deaths/pop*100000, N1) %>%
  pivot_longer(-c(shedID, date), names_to = "outcome", values_to = "value") %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value, col = shedID)) +
  facet_wrap(~outcome, scale = "free_y") +
  geom_line()

# Figure S6: wastewater and cases smoothness -----------------

twoCol <- c("N1" = Col2[1],
            "cases" = Col2[2])

twoVar <- c("N1" = "Wastewater SARS-CoV-2 (N1)",
            "cases" = "Reported cases per 100K")

ww_by_week %>% 
  transmute(shedName, weekstart, N1) %>%
  pivot_longer(cols = c("N1", "cases"),
               names_to = "measure",
               values_to = "value") %>% 
  filter(weekstart > as.Date("2020-08-16")) %>%
  ggplot(aes(x = weekstart, y = value)) +
  geom_point(aes(col = measure), size = 1) +
  scale_x_date("")+
  geom_line(aes(col = measure)) + 
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        legend.position = "bottom",
        text = element_text(family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,400))+ 
  geom_ribbon(data = covidestim_output %>%
                filter(date > as.Date("2020-08-10")) %>%
                filter(ww_var %in% c("N1", "N1_ts")) %>%
                filter(shedID != "Total") %>%
                filter(model %in% c("cases-deaths-ww")) %>%
                group_by(shedID) %>%
                filter(month == max(month)) %>%
                ungroup() %>%
                mutate(weekstart = date, outliers = ww_var,
                       shedName = case_when(shedID == "Total"~ "Total",
                                            TRUE ~paste0("MSD0", shedID))),
              aes(x= weekstart, y = fitted_wastewater_prvl, ymin = fitted_wastewater_prvl_p2_5,
                  ymax = fitted_wastewater_prvl_p97_5, fill = outliers), alpha = .2) +
  facet_grid(~shedName) +
  scale_fill_manual("95% credible interval of model estimates", values = twoCol, labels = twoVar) +
  scale_color_manual("Input data", values = twoCol, labels = twoVar) +
  scale_y_continuous("SARS-CoV-2 (N1) (copies/ml)", limits = c(0,400)) +
  scale_x_date("", date_labels = "%b'%y", minor_breaks = '1 month') -> sup1
sup1

tiff("figures/supFig1-outlier.tiff", 
     units = "in", 
     res = 1000,
     height = 4.48,
     width = 10.48,
     type = "cairo")
sup1
dev.off()
