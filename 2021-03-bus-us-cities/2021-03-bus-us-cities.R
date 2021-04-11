# find long-lat boxes using https://boundingbox.klokantech.com/
# cf. https://wiki.openstreetmap.org/wiki/Map_features


library("osmdata")
library("ggmap")




####################################
# city as a function of bus routes #
####################################

# San Francisco Bay Area

coord_bay <- rbind(c(-122.723, -121.708), c(37.167, 38.180))
colnames(coord_bay) <- c("min", "max")
rownames(coord_bay) <- c("x", "y")
coord_bay

q <- coord_bay %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

bay_bus <- osmdata_sf(q)

ggplot(bay_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-122.723, -121.708) +
	ylim(37.167, 38.180) +
	annotate(geom="text", x=-121.708, y=37.167, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("bay_bus.png")








# Los Angeles and surroundings

coord_la <- rbind(c(-118.659, -117.075), c(33.380, 34.345))
colnames(coord_la) <- c("min", "max")
rownames(coord_la) <- c("x", "y")
coord_la

q <- coord_la %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

la_bus <- osmdata_sf(q)

ggplot(la_bus $osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-118.659, -117.075) +
	ylim(33.380, 34.345) +
	annotate(geom="text", x=-117.075, y=33.380, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("la_bus.png")





# Seattle and surroundings

coord_sea <- rbind(c(-122.459, -122.076), c(47.424, 47.735))
colnames(coord_sea) <- c("min", "max")
rownames(coord_sea) <- c("x", "y")
coord_sea

q <- coord_sea %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

sea_bus <- osmdata_sf(q)

ggplot(sea_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-122.459, -122.076) +
	ylim(47.424, 47.735) +
	annotate(geom="text", x=-122.076, y=47.424, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("sea_bus.png")







# NYC

coord_nyc <- rbind(c(-74.348, -73.701), c(40.477, 40.917))
colnames(coord_nyc) <- c("min", "max")
rownames(coord_nyc) <- c("x", "y")
coord_nyc

q <- coord_nyc %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

nyc_bus <- osmdata_sf(q)

ggplot(nyc_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-74.348, -73.701) +
	ylim(40.477, 40.917) +
	annotate(geom="text", x=-73.701, y=40.477, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("nyc_bus.png")





# Chicagoland

coord_chi <- rbind(c(-88.176, -87.524), c(41.643, 42.138))
colnames(coord_chi) <- c("min", "max")
rownames(coord_chi) <- c("x", "y")
coord_chi

q <- coord_chi %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

chi_bus <- osmdata_sf(q)

ggplot(chi_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-88.176, -87.524) +
	ylim(41.643, 42.138) +
	annotate(geom="text", x=-87.524, y=41.643, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("chi_bus.png")







# Dallas + Fort Worth

coord_dfw <- rbind(c(-97.573, -96.464), c(32.516, 33.081))
colnames(coord_dfw) <- c("min", "max")
rownames(coord_dfw) <- c("x", "y")
coord_dfw

q <- coord_dfw %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

dfw_bus <- osmdata_sf(q)

ggplot(dfw_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-97.440, -96.464) +
	ylim(32.516, 33.081) +
	annotate(geom="text", x=-96.464, y=32.516, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("dfw_bus.png")






# Houston

coord_hou <- rbind(c(-95.909, -94.871), c(29.358, 30.234))
colnames(coord_hou) <- c("min", "max")
rownames(coord_hou) <- c("x", "y")
coord_hou

q <- coord_hou %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

hou_bus <- osmdata_sf(q)

ggplot(hou_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-95.800, -95.100) +
	ylim(29.531, 30.070) +
	annotate(geom="text", x=-95.100, y=29.531, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("hou_bus.png")








# Washington DC + Baltimore

coord_dc <- rbind(c(-77.297, -76.398), c(38.728, 39.456))
colnames(coord_dc) <- c("min", "max")
rownames(coord_dc) <- c("x", "y")
coord_dc

q <- coord_dc %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

dc_bus <- osmdata_sf(q)

ggplot(dc_bus $osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-77.297, -76.398) +
	ylim(38.728, 39.456) +
	annotate(geom="text", x=-76.398, y=38.728, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("dc_bus.png")








# Miami + Fort Lauderdale

coord_mia <- rbind(c(-80.486, -80.010), c(25.553, 26.422))
colnames(coord_mia) <- c("min", "max")
rownames(coord_mia) <- c("x", "y")
coord_mia

q <- coord_mia %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

mia_bus <- osmdata_sf(q)

ggplot(mia_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-80.486, -80.010) +
	ylim(25.553, 26.400) +
	annotate(geom="text", x=-80.010, y=25.553, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("mia_bus.png")






# Philadelphia
coord_phi <- rbind(c(-75.660, -74.628), c(39.654, 40.331))
colnames(coord_phi) <- c("min", "max")
rownames(coord_phi) <- c("x", "y")
coord_phi

q <- coord_phi %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

phi_bus <- osmdata_sf(q)

ggplot(phi_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-75.660, -74.628) +
	ylim(39.654, 40.331) +
	annotate(geom="text", x=-74.628, y=39.654, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("phi_bus.png")






# Atlanta
coord_atl <- rbind(c(-84.589, -84.126), c(33.527, 33.987))
colnames(coord_atl) <- c("min", "max")
rownames(coord_atl) <- c("x", "y")
coord_atl

q <- coord_atl %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

atl_bus <- osmdata_sf(q)

ggplot(atl_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-84.589, -84.126) +
	ylim(33.527, 33.987) +
	annotate(geom="text", x=-84.126, y=33.527, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("atl_bus.png")






# Phoenix + Mesa

coord_pho <- rbind(c(-112.731, -111.476), c(33.151, 33.823))
colnames(coord_pho) <- c("min", "max")
rownames(coord_pho) <- c("x", "y")
coord_pho

q <- coord_pho %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

pho_bus <- osmdata_sf(q)

ggplot(pho_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-112.631, -111.660) +
	ylim(33.220, 33.710) +
	annotate(geom="text", x=-111.660, y=33.220, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("pho_bus.png")






# Boston Cambridge Newton

coord_bos <- rbind(c(-71.291, -70.805), c(42.185, 42.514))
colnames(coord_bos) <- c("min", "max")
rownames(coord_bos) <- c("x", "y")
coord_bos

q <- coord_bos %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

bos_bus <- osmdata_sf(q)

ggplot(bos_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-71.291, -70.838) +
	ylim(42.185, 42.514) +
	annotate(geom="text", x=-70.838, y=42.185, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("bos_bus.png")







# Detroit Dearborn

coord_det <- rbind(c(-83.479, -82.796), c(42.112, 42.728))
colnames(coord_det) <- c("min", "max")
rownames(coord_det) <- c("x", "y")
coord_det

q <- coord_det %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

det_bus <- osmdata_sf(q)

ggplot(det_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-83.479, -82.796) +
	ylim(42.142, 42.708) +
	annotate(geom="text", x=-82.796, y=42.142, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("det_bus.png")






# Minneapolis St Paul

coord_min <- rbind(c(-93.578, -92.896), c(44.754, 45.151))
colnames(coord_min) <- c("min", "max")
rownames(coord_min) <- c("x", "y")
coord_min

q <- coord_min %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

min_bus <- osmdata_sf(q)

ggplot(min_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-93.578, -92.896) +
	ylim(44.754, 45.151) +
	annotate(geom="text", x=-92.896, y=44.754, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("min_bus.png")






# San Diego

coord_sd <- rbind(c(-117.466, -116.880), c(32.522, 33.263))
colnames(coord_sd) <- c("min", "max")
rownames(coord_sd) <- c("x", "y")
coord_sd

q <- coord_sd %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

sd_bus <- osmdata_sf(q)

ggplot(sd_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-117.466, -116.880) +
	ylim(32.522, 33.263) +
	annotate(geom="text", x=-116.880, y=32.522, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("sd_bus.png")







# Tampa

coord_tam <- rbind(c(-82.896, -82.235), c(27.680, 28.227))
colnames(coord_tam) <- c("min", "max")
rownames(coord_tam) <- c("x", "y")
coord_tam

q <- coord_tam %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

tam_bus <- osmdata_sf(q)

ggplot(tam_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-82.856, -82.235) +
	ylim(27.680, 28.227) +
	annotate(geom="text", x=-82.235, y=27.680, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("tam_bus.png")







# Denver Boulder

coord_den <- rbind(c(-105.359, -104.620), c(39.504, 40.081))
colnames(coord_den) <- c("min", "max")
rownames(coord_den) <- c("x", "y")
coord_den

q <- coord_den %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

den_bus <- osmdata_sf(q)

ggplot(den_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-105.339, -104.700) +
	ylim(39.504, 40.081) +
	annotate(geom="text", x=-104.700, y=39.504, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("den_bus.png")





# St Louis

coord_stl <- rbind(c(-90.610, -90.004), c(38.458, 38.838))
colnames(coord_stl) <- c("min", "max")
rownames(coord_stl) <- c("x", "y")
coord_stl

q <- coord_stl %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

stl_bus <- osmdata_sf(q)

ggplot(stl_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-90.610, -90.004) +
	ylim(38.458, 38.838) +
	annotate(geom="text", x=-90.004, y=38.458, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("stl_bus.png")







# Las Vegas

coord_lv <- rbind(c(-115.358, -114.932), c(35.982, 36.322))
colnames(coord_lv) <- c("min", "max")
rownames(coord_lv) <- c("x", "y")
coord_lv

q <- coord_lv %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

lv_bus <- osmdata_sf(q)

ggplot(lv_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-115.328, -114.962) +
	ylim(35.992, 36.292) +
	annotate(geom="text", x=-114.962, y=35.992, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("lv_bus.png")



# Portland

coord_por <- rbind(c(-123.025, -122.339), c(45.328, 45.738))
colnames(coord_por) <- c("min", "max")
rownames(coord_por) <- c("x", "y")
coord_por

q <- coord_por %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

por_bus <- osmdata_sf(q)

ggplot(por_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-123.025, -122.339) +
	ylim(45.328, 45.738) +
	annotate(geom="text", x=-122.359, y=45.328, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("por_bus.png")








# Honolulu

coord_oahu <- rbind(c(-158.316, -157.616), c(21.232, 21.734))
colnames(coord_oahu) <- c("min", "max")
rownames(coord_oahu) <- c("x", "y")
coord_oahu

q <- coord_oahu %>%
		opq() %>%
		add_osm_feature(key="route", value="bus")

oahu_bus <- osmdata_sf(q)

ggplot(oahu_bus$osm_line) + 
	geom_sf(color = "#bd2222", size = 0.1) +
	xlim(-158.216, -157.656) +
	ylim(21.252, 21.704) +
	annotate(geom="text", x=-157.656, y=21.252, label="@tszhm", color="#bd2222", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("oahu_bus.png")
