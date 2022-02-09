library(ggplot2)


### Code to create Figure 4.2 ###
cr_all <- cr_pp_all
cr_all$tp <- paste("01",cr_all$tp ,sep = "/")
cr_all$tp <- as.Date(cr_all$tp, format="%d/%Y/%m")
count_pp <- unique(cr_all$Precinct)
count_pp_s <- c(10,20,41,50,52,66,69,109,114,121)
cr_max_pp_s <- filter(cr_all, Precinct %in% count_pp_s)

cr_max_TSP <- ggplot(cr_max_pp_s) +
  geom_line(aes(x = tp, y = sumall), size=.3) +
  facet_wrap(~Precinct, ncol = 5) +
  xlab("Year") +
  ylab("Monthly arrest counts") +
  theme_bw() +
  theme(panel.spacing = unit (1, "lines")) +
  scale_x_date(date_labels = "%Y", date_minor_breaks = "1 year",date_breaks = "5 years")

print(cr_max_TSP)

### Code to create Figure 3.7 ###
cr_max_1w <- cr_pp_sum_w
cr_max_agg_1w <- aggregate(cr_max_1w["sumall"], by=cr_max_1w["tp"], sum)
cr_max_agg_1w$tp <- as.Date(cr_max_agg_1w$tp, format="%Y-%m-%d")

cr_max_1m <- cr_pp_sum_1m
cr_max_agg_1m <- aggregate(cr_max_1m["sumall"], by=cr_max_1m["tp"], sum)
cr_max_agg_1m$tp <- as.Date(cr_max_agg_1m$tp, format="%Y-%m-%d")

cr_max_3m <- cr_pp_sum_3m
cr_max_agg_3m <- aggregate(cr_max_3m["sumall"], by=cr_max_3m["tp"], sum)
cr_max_agg_3m$tp <- as.Date(cr_max_agg_3m$tp, format="%Y-%m-%d")

cr_max_6m <- cr_pp_sum_6m
cr_max_agg_6m <- aggregate(cr_max_6m["sumall"], by=cr_max_6m["tp"], sum)
cr_max_agg_6m$tp <- as.Date(cr_max_agg_6m$tp, format="%Y-%m-%d")

cr_max_y <- cr_pp_sum_y
cr_max_agg_y <- aggregate(cr_max_y["sumall"], by=cr_max_y["tp"], sum)
cr_max_agg_y$tp <- as.Date(cr_max_agg_y$tp, format="%Y")

cr_max_TSP_p1 <- ggplot() +
  #geom_line(data=cr_max_agg_1w, aes(x = tp, y = sumall), color="grey30", size=.3) +
  #geom_line(data=cr_max_agg_1m, aes(x = tp, y = sumall), color="grey30", size=.5) +
  #geom_line(data=cr_max_agg_6m, aes(x = tp, y = sumall), color="grey30", size=.5) +
  #geom_line(data=cr_max_agg_3m, aes(x = tp, y = sumall), color="grey30", size=.5) +
  geom_line(data=cr_max_agg_y, aes(x = tp, y = sumall), color="grey30", size=.5) +
  xlab("Year") +
  ylab("All arrest counts") +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  scale_x_date(date_labels = "%Y",date_minor_breaks = "1 month", date_breaks = "1 year")

print(cr_max_TSP_p1)


### Code to create residual maps of chapter 4 ###
pp_sf_poly <- mutate(pp, POLY_ID = row_number())

# STARMA
res_starma <- STARMA_y_all$residuals
starma_breaks <- scale(res_starma)
starma_breaks <- as.data.frame(t(starma_breaks))
res_g <- mutate(starma_breaks, POLY_ID = row_number())
res_g <- merge(y=res_g, x=pp_sf_poly, by="POLY_ID")
res_sf <- res_g %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
st_write(res_sf, "*/Outputs/R_Outputs/res_sf.shp", append=FALSE)
starma_max <- max(starma_breaks$V15)
starma_min <- min(starma_breaks$V15)
starma_breaks <- c(starma_min,-2,-1,starma_max)
mypal_starma <- c('#2166ac','#67a9cf','#d1e5f0','#f7f7f7','#fddbc7','#ef8a62','#b2182b')
mypal_starma <- c('#67a9cf','#d1e5f0','#f7f7f7')

tm_shape(res_sf) + 
            tm_fill("V15", title = "SD OF STARMA_y_all RES.", style = "fixed", breaks = starma_breaks, midpoint = 0 ,palette = mypal_starma) +
            tm_borders(alpha = 0.1) +
            tm_scale_bar(lwd = 0.5,text.size = 1) +
            tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2, legend.text.size = 1)

tmap_save(tm_shape(res_sf) + 
  tm_fill("V15", title = "SD OF STARMA_y_all RES.", style = "fixed", breaks = starma_breaks, midpoint = 0 ,palette = mypal_starma) +
  tm_borders(alpha = 0.1) +
  tm_scale_bar(lwd = 0.5,text.size = 1) +
  tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2, legend.text.size = 1),
  "*/Outputs/R_Outputs/Graphics/STARMA_y_all.jpg")


### ARIMA
arima_breaks <- scale(res_arima_w_p)
arima_breaks <- as.data.frame(arima_breaks)
res_g <- mutate(arima_breaks, POLY_ID = row_number())
res_g <- merge(y=res_g, x=pp_sf_poly, by="POLY_ID")
res_sf <- res_g %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
arima_max <- max(arima_breaks$X15)
arima_min <- min(arima_breaks$X15)
arima_breaks <- c(arima_min,-3,-2,-1,1,arima_max)
mypal_arima <- c('#2166ac','#67a9cf','#d1e5f0','#f7f7f7','#fddbc7','#ef8a62','#b2182b')
mypal_arima <- c('#2166ac','#67a9cf','#d1e5f0','#f7f7f7','#fddbc7')

tm_shape(res_sf) + 
  tm_fill("X15", title = "SD OF ARIMA_y_all RES.", style = "fixed", breaks = arima_breaks, midpoint = 0 ,palette = mypal_arima) +
  tm_borders(alpha = 0.1) +
  tm_scale_bar(lwd = 0.5,text.size = 1) +
  tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2, legend.text.size = 1)

tmap_save(tm_shape(res_sf) + 
  tm_fill("X15", title = "SD OF ARIMA_y_all RES.", style = "fixed", breaks = arima_breaks, midpoint = 0 ,palette = mypal_arima) +
  tm_borders(alpha = 0.1) +
  tm_scale_bar(lwd = 0.5,text.size = 1) +
  tm_layout(legend.position = c("left", "top"), legend.title.size = 1.2, legend.text.size = 1),
  "*/Outputs/R_Outputs/Graphics/ARIMA_y_all.jpg")

### STARMA residual output to create LISA cluster maps in GeoDa
res_starma <- STARMA_w_p$residuals
starma_breaks <- res_starma
starma_breaks <- as.data.frame(t(starma_breaks))
res_g_starma <- mutate(starma_breaks, POLY_ID = row_number())
res_g_starma <- merge(y=res_g_starma, x=pp_sf_poly, by="POLY_ID")
res_sf_starma <- res_g_starma %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
st_write(res_sf_starma, "*/Outputs/R_Outputs/res_sf_starma.shp", append=FALSE)

### ARIMA residual output to create LISA cluster maps in GeoDa
arima_breaks <- res_arima_w_p
arima_breaks <- as.data.frame(arima_breaks)
res_g_arima <- mutate(arima_breaks, POLY_ID = row_number())
res_g_arima <- merge(y=res_g_arima, x=pp_sf_poly, by="POLY_ID")
res_sf_arima <- res_g_arima %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
st_write(res_sf_arima, "*/Outputs/R_Outputs/res_sf_arima.shp", append=FALSE)
