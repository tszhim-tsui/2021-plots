# find long-lat boxes using https://boundingbox.klokantech.com/

library("osmdata")
library("ggmap")
library("sf")




################################
# Central and Western District #
################################

coord_central <- rbind(c(114.107, 114.169), c(22.251, 22.298))
colnames(coord_central) <- c("min", "max")
rownames(coord_central) <- c("x", "y")
coord_central

# get district boundary

q0 <- coord_central %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Central and Western District")

central_boundary <- osmdata_sf(q0)

# extract boundary to polygon
central_polygon <- st_cast(central_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
central_box <- data.frame(
					lon = c(114, 114, 114.5, 114.5),
					lat = c(22.1, 22.4, 22.4, 22.1))

central_box_point <- central_box %>% 
				st_as_sf(coords = c("lon", "lat"))

central_box_polygon <- st_cast(st_combine(central_box_point$geometry), "POLYGON")
central_box_polygon <- st_set_crs(central_box_polygon, st_crs(central_polygon))

# subtract district from larger box to create overlay
central_overlay <- st_difference(central_box_polygon, central_polygon)



# retrieve walkable paths

q1 <- coord_central %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_central %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_central %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")


central1 <- osmdata_sf(q1)
central2 <- osmdata_sf(q2)
central3 <- osmdata_sf(q3)


central_walkways <- c(central1, central2, central3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = central_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = central_overlay, fill = "white", color = NA, lwd = 0) +
	xlim(114.112, 114.167) +
	ylim(22.251, 22.292) +
	annotate(geom="text", x=114.165, y=22.252, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("central_walkways.png")
ggsave("central_walkways.pdf")








####################
# Eastern District #
####################

coord_eastern <- rbind(c(114.184, 114.263), c(22.243, 22.303))
colnames(coord_eastern) <- c("min", "max")
rownames(coord_eastern) <- c("x", "y")
coord_eastern

q0 <- coord_eastern %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Eastern District")

eastern_boundary <- osmdata_sf(q0)

# check
eastern_boundary$osm_multipolygons

# extract boundary to polygon
eastern_polygon <- st_cast(eastern_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
eastern_box <- data.frame(
					lon = c(114, 114, 114.5, 114.5),
					lat = c(22.1, 22.4, 22.4, 22.1))

eastern_box_point <- eastern_box %>% 
				st_as_sf(coords = c("lon", "lat"))

eastern_box_polygon <- st_cast(st_combine(eastern_box_point$geometry), "POLYGON")
eastern_box_polygon <- st_set_crs(eastern_box_polygon, st_crs(eastern_polygon))

# subtract district from larger box to create overlay
eastern_overlay <- st_difference(eastern_box_polygon, eastern_polygon)




q1 <- coord_eastern %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_eastern %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_eastern %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

eastern1 <- osmdata_sf(q1)
eastern2 <- osmdata_sf(q2)
eastern3 <- osmdata_sf(q3)


eastern_walkways <- c(eastern1, eastern2, eastern3)

ggplot() + 
	geom_sf(data = eastern_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = eastern_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.191, 114.257) +
	ylim(22.246, 22.296) +
	annotate(geom="text", x=114.257, y=22.2475, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("eastern_walkways.png")
ggsave("eastern_walkways.pdf")







#####################
# Southern District #
#####################

coord_southern <- rbind(c(114.100, 114.310), c(22.190, 22.277))
colnames(coord_southern) <- c("min", "max")
rownames(coord_southern) <- c("x", "y")
coord_southern


q0 <- coord_southern %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Southern District")

southern_boundary <- osmdata_sf(q0)

# check
southern_boundary$osm_multipolygons

# extract boundary to polygon
southern_polygon <- st_cast(southern_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
southern_box <- data.frame(
					lon = c(114, 114, 114.4, 114.4),
					lat = c(22.1, 22.3, 22.3, 22.1))

southern_box_point <- southern_box %>% 
				st_as_sf(coords = c("lon", "lat"))

southern_box_polygon <- st_cast(st_combine(southern_box_point$geometry), "POLYGON")
southern_box_polygon <- st_set_crs(southern_box_polygon, st_crs(southern_polygon))

# subtract district from larger box to create overlay
southern_overlay <- st_difference(southern_box_polygon, southern_polygon)




q1 <- coord_southern %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_southern %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_southern %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")


southern1 <- osmdata_sf(q1)
southern2 <- osmdata_sf(q2)
southern3 <- osmdata_sf(q3)


southern_walkways <- c(southern1, southern2, southern3)

ggplot() + 
	geom_sf(data = southern_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = southern_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.119, 114.261) +
	ylim(22.200, 22.27561) +
	annotate(geom="text", x=114.260, y=22.203, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("southern_walkways.png")
ggsave("southern_walkways.pdf")







#####################
# Wan Chai District #
#####################

coord_wanchai <- rbind(c(114.163, 114.208), c(22.254, 22.294))
colnames(coord_wanchai) <- c("min", "max")
rownames(coord_wanchai) <- c("x", "y")
coord_wanchai


q0 <- coord_wanchai %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Wan Chai District")

wanchai_boundary <- osmdata_sf(q0)

# check
wanchai_boundary$osm_multipolygons

# extract boundary to polygon
wanchai_polygon <- st_cast(wanchai_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
wanchai_box <- data.frame(
					lon = c(114.1, 114.1, 114.22, 114.22),
					lat = c(22.2, 22.3, 22.3, 22.2))

wanchai_box_point <- wanchai_box %>% 
				st_as_sf(coords = c("lon", "lat"))

wanchai_box_polygon <- st_cast(st_combine(wanchai_box_point$geometry), "POLYGON")
wanchai_box_polygon <- st_set_crs(wanchai_box_polygon, st_crs(wanchai_polygon))

# subtract district from larger box to create overlay
wanchai_overlay <- st_difference(wanchai_box_polygon, wanchai_polygon)



q1 <- coord_wanchai %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_wanchai %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_wanchai %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

wanchai1 <- osmdata_sf(q1)
wanchai2 <- osmdata_sf(q2)
wanchai3 <- osmdata_sf(q3)


wanchai_walkways <- c(wanchai1, wanchai2, wanchai3)

ggplot() + 
	geom_sf(data = wanchai_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = wanchai_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.163, 114.208) +
	ylim(22.254, 22.288) +
	annotate(geom="text", x=114.207, y=22.255, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("wanchai_walkways.png")
ggsave("wanchai_walkways.pdf")



############################
# almost the whole kowloon #
############################

coord_kowloon <- rbind(c(114.13, 114.245), c(22.290, 22.350))
colnames(coord_kowloon) <- c("min", "max")
rownames(coord_kowloon) <- c("x", "y")
coord_kowloon



#########################
# Kowloon City District #
#########################

coord_klc <- rbind(c(114.172, 114.220), c(22.294, 22.350))
colnames(coord_klc) <- c("min", "max")
rownames(coord_klc) <- c("x", "y")
coord_klc

q0 <- coord_kowloon %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Kowloon City District")

klc_boundary <- osmdata_sf(q0)

# check
klc_boundary$osm_multipolygons

# extract boundary to polygon
klc_polygon <- st_cast(klc_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
klc_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.2, 22.45, 22.45, 22.2))

klc_box_point <- klc_box %>% 
				st_as_sf(coords = c("lon", "lat"))

klc_box_polygon <- st_cast(st_combine(klc_box_point$geometry), "POLYGON")
klc_box_polygon <- st_set_crs(klc_box_polygon, st_crs(klc_polygon))

# subtract district from larger box to create overlay
klc_overlay <- st_difference(klc_box_polygon, klc_polygon)



q1 <- coord_klc %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_klc %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_klc %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

klc1 <- osmdata_sf(q1)
klc2 <- osmdata_sf(q2)
klc3 <- osmdata_sf(q3)


klc_walkways <- c(klc1, klc2, klc3)

ggplot() + 
	geom_sf(data = klc_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = klc_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.172, 114.218) +
	ylim(22.30, 22.35) +
	annotate(geom="text", x=114.217, y=22.3005, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("klc_walkways.png")
ggsave("klc_walkways.pdf")





######################
# Kwun Tong District #
######################

coord_kwuntong <- rbind(c(114.201, 114.245), c(22.282, 22.335))
colnames(coord_kwuntong) <- c("min", "max")
rownames(coord_kwuntong) <- c("x", "y")
coord_kwuntong

q0 <- coord_kowloon %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Kwun Tong District")

kwuntong_boundary <- osmdata_sf(q0)

# check
kwuntong_boundary$osm_multipolygons

# extract boundary to polygon
kwuntong_polygon <- st_cast(kwuntong_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
kwuntong_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.15, 22.45, 22.45, 22.15))

kwuntong_box_point <- kwuntong_box %>% 
				st_as_sf(coords = c("lon", "lat"))

kwuntong_box_polygon <- st_cast(st_combine(kwuntong_box_point$geometry), "POLYGON")
kwuntong_box_polygon <- st_set_crs(kwuntong_box_polygon, st_crs(kwuntong_polygon))

# subtract district from larger box to create overlay
kwuntong_overlay <- st_difference(kwuntong_box_polygon, kwuntong_polygon)



q1 <- coord_kwuntong %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_kwuntong %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_kwuntong %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

kwuntong1 <- osmdata_sf(q1)
kwuntong2 <- osmdata_sf(q2)
kwuntong3 <- osmdata_sf(q3)


kwuntong_walkways <- c(kwuntong1, kwuntong2, kwuntong3)

ggplot() + 
	geom_sf(data = kwuntong_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = kwuntong_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.201, 114.245) +
	ylim(22.284, 22.335) +
	annotate(geom="text", x=114.245, y=22.284, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("kwuntong_walkways.png")
ggsave("kwuntong_walkways.pdf")




#########################
# Sham Shui Po District #
#########################

coord_shamshuipo <- rbind(c(114.116, 114.182), c(22.305, 22.350))
colnames(coord_shamshuipo) <- c("min", "max")
rownames(coord_shamshuipo) <- c("x", "y")
coord_shamshuipo

q0 <- coord_kowloon %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Sham Shui Po District")

shamshuipo_boundary <- osmdata_sf(q0)

# check
shamshuipo_boundary$osm_multipolygons

# extract boundary to polygon
shamshuipo_polygon <- st_cast(shamshuipo_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
shamshuipo_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.15, 22.45, 22.45, 22.15))

shamshuipo_box_point <- shamshuipo_box %>% 
				st_as_sf(coords = c("lon", "lat"))

shamshuipo_box_polygon <- st_cast(st_combine(shamshuipo_box_point$geometry), "POLYGON")
shamshuipo_box_polygon <- st_set_crs(shamshuipo_box_polygon, st_crs(shamshuipo_polygon))

# subtract district from larger box to create overlay
shamshuipo_overlay <- st_difference(shamshuipo_box_polygon, shamshuipo_polygon)



q1 <- coord_shamshuipo %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_shamshuipo %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_shamshuipo %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

shamshuipo1 <- osmdata_sf(q1)
shamshuipo2 <- osmdata_sf(q2)
shamshuipo3 <- osmdata_sf(q3)


shamshuipo_walkways <- c(shamshuipo1, shamshuipo2, shamshuipo3)

ggplot() + 
	geom_sf(data = shamshuipo_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = shamshuipo_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.130, 114.177) +
	ylim(22.315, 22.347) +
	annotate(geom="text", x=114.176, y=22.316, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("shamshuipo_walkways.png")
ggsave("shamshuipo_walkways.pdf")







#########################
# Wong Tai Sin District #
#########################

coord_wongtaisin <- rbind(c(114.179, 114.225), c(22.330, 22.359))
colnames(coord_wongtaisin) <- c("min", "max")
rownames(coord_wongtaisin) <- c("x", "y")
coord_wongtaisin

q0 <- coord_kowloon %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Wong Tai Sin District")

wongtaisin_boundary <- osmdata_sf(q0)

# check
wongtaisin_boundary$osm_multipolygons

# extract boundary to polygon
wongtaisin_polygon <- st_cast(wongtaisin_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
wongtaisin_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.15, 22.45, 22.45, 22.15))

wongtaisin_box_point <- wongtaisin_box %>% 
				st_as_sf(coords = c("lon", "lat"))

wongtaisin_box_polygon <- st_cast(st_combine(wongtaisin_box_point$geometry), "POLYGON")
wongtaisin_box_polygon <- st_set_crs(wongtaisin_box_polygon, st_crs(wongtaisin_polygon))

# subtract district from larger box to create overlay
wongtaisin_overlay <- st_difference(wongtaisin_box_polygon, wongtaisin_polygon)



q1 <- coord_wongtaisin %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_wongtaisin %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_wongtaisin %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

wongtaisin1 <- osmdata_sf(q1)
wongtaisin2 <- osmdata_sf(q2)
wongtaisin3 <- osmdata_sf(q3)


wongtaisin_walkways <- c(wongtaisin1, wongtaisin2, wongtaisin3)

ggplot() + 
	geom_sf(data = wongtaisin_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = wongtaisin_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.181, 114.224) +
	ylim(22.330, 22.359) +
	annotate(geom="text", x=114.223, y=22.331, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("wongtaisin_walkways.png")
ggsave("wongtaisin_walkways.pdf")





##########################
# Yau Tsim Mong District #
##########################

coord_yautsimmong <- rbind(c(114.149, 114.187), c(22.288, 22.327))
colnames(coord_yautsimmong) <- c("min", "max")
rownames(coord_yautsimmong) <- c("x", "y")
coord_yautsimmong

q0 <- coord_kowloon %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Yau Tsim Mong District")

yautsimmong_boundary <- osmdata_sf(q0)

# check
yautsimmong_boundary$osm_multipolygons

# extract boundary to polygon
yautsimmong_polygon <- st_cast(yautsimmong_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
yautsimmong_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.15, 22.45, 22.45, 22.15))

yautsimmong_box_point <- yautsimmong_box %>% 
				st_as_sf(coords = c("lon", "lat"))

yautsimmong_box_polygon <- st_cast(st_combine(yautsimmong_box_point$geometry), "POLYGON")
yautsimmong_box_polygon <- st_set_crs(yautsimmong_box_polygon, st_crs(yautsimmong_polygon))

# subtract district from larger box to create overlay
yautsimmong_overlay <- st_difference(yautsimmong_box_polygon, yautsimmong_polygon)



q1 <- coord_yautsimmong %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_yautsimmong %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_yautsimmong %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

yautsimmong1 <- osmdata_sf(q1)
yautsimmong2 <- osmdata_sf(q2)
yautsimmong3 <- osmdata_sf(q3)


yautsimmong_walkways <- c(yautsimmong1, yautsimmong2, yautsimmong3)

ggplot() + 
	geom_sf(data = yautsimmong_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = yautsimmong_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.156, 114.186) +
	ylim(22.293, 22.327) +
	annotate(geom="text", x=114.186, y=22.293, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("yautsimmong_walkways.png")
ggsave("yautsimmong_walkways.pdf")





####################
# Islands District #
####################

coord_islands <- rbind(c(113.80, 114.31), c(22.12, 22.33))
colnames(coord_islands) <- c("min", "max")
rownames(coord_islands) <- c("x", "y")
coord_islands


q0 <- coord_islands %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Islands District")

islands_boundary <- osmdata_sf(q0)

# check
islands_boundary$osm_multipolygons

# extract boundary to polygon
islands_polygon <- st_cast(islands_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
islands_box <- data.frame(
					lon = c(113.5, 113.5, 114.5, 114.5),
					lat = c(22.1, 22.4, 22.4, 22.1))

islands_box_point <- islands_box %>% 
				st_as_sf(coords = c("lon", "lat"))

islands_box_polygon <- st_cast(st_combine(islands_box_point$geometry), "POLYGON")
islands_box_polygon <- st_set_crs(islands_box_polygon, st_crs(islands_polygon))

# subtract district from larger box to create overlay
islands_overlay <- st_difference(islands_box_polygon, islands_polygon)



q1 <- coord_islands %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_islands %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_islands %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

islands1 <- osmdata_sf(q1)
islands2 <- osmdata_sf(q2)
islands3 <- osmdata_sf(q3)


islands_walkways <- c(islands1, islands2, islands3)

ggplot() + 
	geom_sf(data = islands_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = islands_overlay, fill="white", color=NA, lwd=0) +
	xlim(113.850, 114.300) +
	ylim(22.158, 22.320) +
	annotate(geom="text", x=114.295, y=22.160, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("islands_walkways.png")
ggsave("islands_walkways.pdf")






#######################
# Kwai Tsing District #
#######################

coord_kwaitsing <- rbind(c(114.060, 114.168), c(22.313, 22.390))
colnames(coord_kwaitsing) <- c("min", "max")
rownames(coord_kwaitsing) <- c("x", "y")
coord_kwaitsing


q0 <- coord_kwaitsing %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Kwai Tsing District")

kwaitsing_boundary <- osmdata_sf(q0)

# check
kwaitsing_boundary$osm_multipolygons

# extract boundary to polygon
kwaitsing_polygon <- st_cast(kwaitsing_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
kwaitsing_box <- data.frame(
					lon = c(113.5, 113.5, 114.5, 114.5),
					lat = c(22.1, 22.4, 22.4, 22.1))

kwaitsing_box_point <- kwaitsing_box %>% 
				st_as_sf(coords = c("lon", "lat"))

kwaitsing_box_polygon <- st_cast(st_combine(kwaitsing_box_point$geometry), "POLYGON")
kwaitsing_box_polygon <- st_set_crs(kwaitsing_box_polygon, st_crs(kwaitsing_polygon))

# subtract district from larger box to create overlay
kwaitsing_overlay <- st_difference(kwaitsing_box_polygon, kwaitsing_polygon)



q1 <- coord_kwaitsing %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_kwaitsing %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_kwaitsing %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

kwaitsing1 <- osmdata_sf(q1)
kwaitsing2 <- osmdata_sf(q2)
kwaitsing3 <- osmdata_sf(q3)


kwaitsing_walkways <- c(kwaitsing1, kwaitsing2, kwaitsing3)

ggplot() + 
	geom_sf(data = kwaitsing_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = kwaitsing_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.079, 114.151) +
	ylim(22.318, 22.384) +
	annotate(geom="text", x=114.147, y=22.319, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("kwaitsing_walkways.png")
ggsave("kwaitsing_walkways.pdf")





##################
# North District #
##################

coord_north <- rbind(c(114.078, 114.337), c(22.465, 22.569))
colnames(coord_north) <- c("min", "max")
rownames(coord_north) <- c("x", "y")
coord_north


q0 <- coord_north %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="North District")

north_boundary <- osmdata_sf(q0)

# check
north_boundary$osm_multipolygons

# extract boundary to polygon
north_polygon <- st_cast(north_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
north_box <- data.frame(
					lon = c(113.5, 113.5, 114.5, 114.5),
					lat = c(22.1, 22.6, 22.6, 22.1))

north_box_point <- north_box %>% 
				st_as_sf(coords = c("lon", "lat"))

north_box_polygon <- st_cast(st_combine(north_box_point$geometry), "POLYGON")
north_box_polygon <- st_set_crs(north_box_polygon, st_crs(north_polygon))

# subtract district from larger box to create overlay
north_overlay <- st_difference(north_box_polygon, north_polygon)



q1 <- coord_north %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_north %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_north %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

north1 <- osmdata_sf(q1)
north2 <- osmdata_sf(q2)
north3 <- osmdata_sf(q3)


north_walkways <- c(north1, north2, north3)

ggplot() + 
	geom_sf(data = north_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = north_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.078, 114.335) +
	ylim(22.465, 22.563) +
	annotate(geom="text", x=114.334, y=22.466, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("north_walkways.png")
ggsave("north_walkways.pdf")





#####################
# Sai Kung District #
#####################

coord_saikung <- rbind(c(114.221, 114.503), c(22.148, 22.467))
colnames(coord_saikung) <- c("min", "max")
rownames(coord_saikung) <- c("x", "y")
coord_saikung


q0 <- coord_saikung %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Sai Kung District")

saikung_boundary <- osmdata_sf(q0)

# check
saikung_boundary$osm_multipolygons

# extract boundary to polygon
saikung_polygon <- st_cast(saikung_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
saikung_box <- data.frame(
					lon = c(113.5, 113.5, 114.6, 114.6),
					lat = c(22.1, 22.6, 22.6, 22.1))

saikung_box_point <- saikung_box %>% 
				st_as_sf(coords = c("lon", "lat"))

saikung_box_polygon <- st_cast(st_combine(saikung_box_point$geometry), "POLYGON")
saikung_box_polygon <- st_set_crs(saikung_box_polygon, st_crs(saikung_polygon))

# subtract district from larger box to create overlay
saikung_overlay <- st_difference(saikung_box_polygon, saikung_polygon)



q1 <- coord_saikung %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_saikung %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_saikung %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

saikung1 <- osmdata_sf(q1)
saikung2 <- osmdata_sf(q2)
saikung3 <- osmdata_sf(q3)


saikung_walkways <- c(saikung1, saikung2, saikung3)

ggplot() + 
	geom_sf(data = saikung_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = saikung_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.228, 114.400) +
	ylim(22.235, 22.447) +
	annotate(geom="text", x=114.399, y=22.236, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("saikung_walkways.png")
ggsave("saikung_walkways.pdf")





####################
# Sha Tin District #
####################

# approx bbox

coord_shatin <- rbind(c(114.14, 114.26), c(22.34, 22.44))
colnames(coord_shatin) <- c("min", "max")
rownames(coord_shatin) <- c("x", "y")
coord_shatin

# get district boundary

q0 <- coord_shatin %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Sha Tin District")

shatin_boundary <- osmdata_sf(q0)

# extract boundary to polygon
shatin_polygon <- st_cast(shatin_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
shatin_box <- data.frame(
					lon = c(114.1, 114.1, 114.3, 114.3),
					lat = c(22.3, 22.45, 22.45, 22.3))

shatin_box_point <- shatin_box %>% 
				st_as_sf(coords = c("lon", "lat"))

shatin_box_polygon <- st_cast(st_combine(shatin_box_point$geometry), "POLYGON")
shatin_box_polygon <- st_set_crs(shatin_box_polygon, st_crs(shatin_polygon))

# subtract district from larger box to create overlay
shatin_overlay <- st_difference(shatin_box_polygon, shatin_polygon)



# retrieve walkable paths

q1 <- coord_shatin %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_shatin %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_shatin %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

shatin1 <- osmdata_sf(q1)
shatin2 <- osmdata_sf(q2)
shatin3 <- osmdata_sf(q3)

shatin_walkways <- c(shatin1, shatin2, shatin3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = shatin_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = shatin_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.141, 114.256) +
	ylim(22.345, 22.436) +
	annotate(geom="text", x=114.253, y=22.347, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("shatin_walkways.png")
ggsave("shatin_walkways.pdf")






###################
# Tai Po District #
###################

coord_taipo <- rbind(c(114.111, 114.456), c(22.393, 22.569))
colnames(coord_taipo) <- c("min", "max")
rownames(coord_taipo) <- c("x", "y")
coord_taipo

# get district boundary

q0 <- coord_taipo %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Tai Po District")

taipo_boundary <- osmdata_sf(q0)

# check
taipo_boundary$osm_multipolygons

# extract boundary to polygon
taipo_polygon <- st_cast(taipo_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
taipo_box <- data.frame(
					lon = c(114.1, 114.1, 114.5, 114.5),
					lat = c(22.3, 22.6, 22.6, 22.3))

taipo_box_point <- taipo_box %>% 
				st_as_sf(coords = c("lon", "lat"))

taipo_box_polygon <- st_cast(st_combine(taipo_box_point$geometry), "POLYGON")
taipo_box_polygon <- st_set_crs(taipo_box_polygon, st_crs(taipo_polygon))

# subtract district from larger box to create overlay
taipo_overlay <- st_difference(taipo_box_polygon, taipo_polygon)



# retrieve walkable paths

q1 <- coord_taipo %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_taipo %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_taipo %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

taipo1 <- osmdata_sf(q1)
taipo2 <- osmdata_sf(q2)
taipo3 <- osmdata_sf(q3)

taipo_walkways <- c(taipo1, taipo2, taipo3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = taipo_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = taipo_overlay, fill="white", color=NA, lwd=0) +
	xlim(114.116, 114.446) +
	ylim(22.394, 22.556) +
	annotate(geom="text", x=114.445, y=22.395, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("taipo_walkways.png")
ggsave("taipo_walkways.pdf")




######################
# Tsuen Wan District #
######################

coord_tsuenwan <- rbind(c(113.994, 114.170), c(22.312, 22.418))
colnames(coord_tsuenwan) <- c("min", "max")
rownames(coord_tsuenwan) <- c("x", "y")
coord_tsuenwan

# get district boundary

q0 <- coord_tsuenwan %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Tsuen Wan District")

tsuenwan_boundary <- osmdata_sf(q0)

# check
tsuenwan_boundary$osm_multipolygons

# extract boundary to polygon
tsuenwan_polygon <- st_cast(tsuenwan_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
tsuenwan_box <- data.frame(
					lon = c(113, 113, 114.5, 114.5),
					lat = c(22.1, 22.6, 22.6, 22.1))

tsuenwan_box_point <- tsuenwan_box %>% 
				st_as_sf(coords = c("lon", "lat"))

tsuenwan_box_polygon <- st_cast(st_combine(tsuenwan_box_point$geometry), "POLYGON")
tsuenwan_box_polygon <- st_set_crs(tsuenwan_box_polygon, st_crs(tsuenwan_polygon))

# subtract district from larger box to create overlay
tsuenwan_overlay <- st_difference(tsuenwan_box_polygon, tsuenwan_polygon)



# retrieve walkable paths

q1 <- coord_tsuenwan %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_tsuenwan %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_tsuenwan %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

tsuenwan1 <- osmdata_sf(q1)
tsuenwan2 <- osmdata_sf(q2)
tsuenwan3 <- osmdata_sf(q3)

tsuenwan_walkways <- c(tsuenwan1, tsuenwan2, tsuenwan3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = tsuenwan_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = tsuenwan_overlay, fill="white", color=NA, lwd=0) +
	xlim(113.994, 114.170) +
	ylim(22.312, 22.418) +
	annotate(geom="text", x=114.169, y=22.313, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("tsuenwan_walkways.png")
ggsave("tsuenwan_walkways.pdf")




#####################
# Tuen Mun District #
#####################

coord_tuenmun <- rbind(c(113.865, 114.067), c(22.324, 22.434))
colnames(coord_tuenmun) <- c("min", "max")
rownames(coord_tuenmun) <- c("x", "y")
coord_tuenmun

# get district boundary

q0 <- coord_tuenmun %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Tuen Mun District")

tuenmun_boundary <- osmdata_sf(q0)

# check
tuenmun_boundary$osm_multipolygons

# extract boundary to polygon
tuenmun_polygon <- st_cast(tuenmun_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
tuenmun_box <- data.frame(
					lon = c(113, 113, 114.5, 114.5),
					lat = c(22.1, 22.6, 22.6, 22.1))

tuenmun_box_point <- tuenmun_box %>% 
				st_as_sf(coords = c("lon", "lat"))

tuenmun_box_polygon <- st_cast(st_combine(tuenmun_box_point$geometry), "POLYGON")
tuenmun_box_polygon <- st_set_crs(tuenmun_box_polygon, st_crs(tuenmun_polygon))

# subtract district from larger box to create overlay
tuenmun_overlay <- st_difference(tuenmun_box_polygon, tuenmun_polygon)



# retrieve walkable paths

q1 <- coord_tuenmun %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_tuenmun %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_tuenmun %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

tuenmun1 <- osmdata_sf(q1)
tuenmun2 <- osmdata_sf(q2)
tuenmun3 <- osmdata_sf(q3)

tuenmun_walkways <- c(tuenmun1, tuenmun2, tuenmun3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = tuenmun_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = tuenmun_overlay, fill="white", color=NA, lwd=0) +
	xlim(113.880, 114.065) +
	ylim(22.328, 22.434) +
	annotate(geom="text", x=114.064, y=22.329, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("tuenmun_walkways.png")
ggsave("tuenmun_walkways.pdf")





######################
# Yuen Long District #
######################

coord_yuenlong <- rbind(c(113.874, 114.128), c(22.391, 22.524))
colnames(coord_yuenlong) <- c("min", "max")
rownames(coord_yuenlong) <- c("x", "y")
coord_yuenlong

# get district boundary

q0 <- coord_yuenlong %>%
		opq() %>%
		add_osm_feature(key="boundary", value="administrative") %>%
		add_osm_feature(key="admin_level", value=6) %>%
		add_osm_feature(key="name:en", value="Yuen Long District")

yuenlong_boundary <- osmdata_sf(q0)

# check
yuenlong_boundary$osm_multipolygons

# extract boundary to polygon
yuenlong_polygon <- st_cast(yuenlong_boundary$osm_multipolygons$geometry, "POLYGON")

# create larger box/polyogn to create overlay
yuenlong_box <- data.frame(
					lon = c(113, 113, 114.5, 114.5),
					lat = c(22.1, 22.6, 22.6, 22.1))

yuenlong_box_point <- yuenlong_box %>% 
				st_as_sf(coords = c("lon", "lat"))

yuenlong_box_polygon <- st_cast(st_combine(yuenlong_box_point$geometry), "POLYGON")
yuenlong_box_polygon <- st_set_crs(yuenlong_box_polygon, st_crs(yuenlong_polygon))

# subtract district from larger box to create overlay
yuenlong_overlay <- st_difference(yuenlong_box_polygon, yuenlong_polygon)



# retrieve walkable paths

q1 <- coord_yuenlong %>%
		opq() %>%
		add_osm_feature(key="sidewalk")

q2 <- coord_yuenlong %>%
		opq() %>%
		add_osm_feature(key="footway")

q3 <- coord_yuenlong %>%
		opq() %>%
		add_osm_feature(key="highway",
						value=c("primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link", 
								"unclassified", "residential", "living_street", "service", "pedestrian", 
								"steps", "track", "footway", "path", "crossing")) %>%
		add_osm_feature(key="access", value="!private")

yuenlong1 <- osmdata_sf(q1)
yuenlong2 <- osmdata_sf(q2)
yuenlong3 <- osmdata_sf(q3)

yuenlong_walkways <- c(yuenlong1, yuenlong2, yuenlong3)


# plot path plus white overlay

ggplot() + 
	geom_sf(data = yuenlong_walkways$osm_line, color = "#873e23", size = 0.1) +
	geom_sf(data = yuenlong_overlay, fill="white", color=NA, lwd=0) +
	xlim(113.935, 114.128) +
	ylim(22.391, 22.523) +
	annotate(geom="text", x=114.1265, y=22.393, label="@tszhm", color="#873e23", size=1, family="sans", hjust=1) +
	theme_void()

ggsave("yuenlong_walkways.png")
ggsave("yuenlong_walkways.pdf")
