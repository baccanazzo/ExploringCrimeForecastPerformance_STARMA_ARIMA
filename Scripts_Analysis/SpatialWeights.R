
### transform data for GeoDa implementation ###
# load shape of police precicts with bridges
admin_data_b <- read_sf("*/Input_Data/NY_PP/nypp_bridges.shp")
pp_b_sf <- pp_b  %>% st_as_sf(wkt="geometry", crs=2263, remove=FALSE)
pp_b <- admin_data_b[,c(1,3,4)]
names(pp_b)[names(pp_b) == 'POLY_ID'] <- 'Precinct'

# transpose data for input in GeoDa (row-wise are precincts, column time)
cr_pp_all_ts_y <- dcast(cr_pp_all_y, Precinct~tp, value.var = "sumall") #transpose for crime type all
cr_pp_all_ts_y$Precinct <- 1:77
cr_pp_p_ts_y <- dcast(cr_pp_p_y, Precinct~tp, value.var = "sump") #transpose for crime type p
cr_pp_p_ts_y$Precinct <- 1:77
cr_pp_v_ts_y <- dcast(cr_pp_v_y, Precinct~tp, value.var = "sumv") #transpose for crime type v
cr_pp_v_ts_y$Precinct <- 1:77

# write output file
cr_pp_poly <- merge(cr_pp_all_ts_y,pp_b, by="Precinct")
cr_pp_poly <- merge(cr_pp_p_ts_y,pp_b, by="Precinct")
cr_pp_poly <- merge(cr_pp_v_ts_y,pp_b, by="Precinct")
st_write(cr_pp_poly, "*/Outputs/GeoDa_Xplore/cr_pp_w.shp", append=FALSE)


### Create neighborhood relationships and spatial weights ###

# Create neighbors based on distance
# min dist
# Calculation of the critical threshold, i.e. the furthest distance between the two most distant centroids
k1_nb <- knearneigh(pp_XY_mtx, k=1)
str(k1_nb)
k1 <- knn2nb(k1_nb)
critical.threshold <- max(unlist(nbdists(k1,pp_XY_mtx)))

dmin_nb <- dnearneigh(pp_XY_mtx, 0, critical.threshold)  # radius (in ft) can be changed (cases should: r=30000, r=40000 & r=50000)
dmin_nb <- nblag(dmin_nb, 2) #different order of neighbors are examined (cases that examined: n=2, n=3 & n=4)
dmin_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
              order1=nb2mat(dmin_nb[[1]], zero.policy=TRUE))


d40_nb <- dnearneigh(pp_XY_mtx, 0, 40000)  # radius (in ft) can be changed (cases should: r=30000, r=40000 & r=50000)
d40_nb <- nblag(d40_nb, 4) #different order of neighbors are examined (cases that examined: n=2, n=3 & n=4)
d40_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
                  order1=nb2mat(d40_nb[[1]], zero.policy=TRUE))

# nearest neighbor
k1_nb <- nblag(k1, 2)
k1_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
              order1=nb2mat(k1_nb[[1]], zero.policy=TRUE))

# 2 nearest neighbors
k2_nb <- knearneigh(pp_XY_mtx, k=2)
k2_nb <- knn2nb(k2_nb)
k2_nb <- nblag(k2_nb, 2)
k2_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
              order1=nb2mat(k2_nb[[1]], zero.policy=TRUE),
              order2=nb2mat(k2_nb[[2]], zero.policy=TRUE))

# 1st order rook contiguity based
rc_nb <- poly2nb(pp_b_sf, queen = FALSE)
rc_nb <- nblag(rc_nb, 4)
rc_list <- list(order0=diag(77), # the number corresponds to the amount of study zones that used
              order1=nb2mat(rc_nb[[1]], zero.policy=TRUE))



