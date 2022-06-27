# 0. Packages and options ------------------------------------------------
library(sf)
library(tidygraph)
library(sfnetworks)
library(ggplot2)
library(mapview)
library(purrr)
library(TSP)

# Adjust some options
options(sfn_max_print_inactive = 6L)

# 1. The sfnetwork data structure ----------------------------------------

# An sfnetwork object is the union of two sf objects that represent the nodes
# and the edges data of a geospatial network. For example

nodes <- st_sfc(
  st_point(c(7, 51)), 
  st_point(c(7, 52)), 
  st_point(c(8, 52)), 
  st_point(c(8, 51.5)), 
  crs = 4326
)
edges <- st_sf(
  data.frame(
    from = c(1, 1, 3), 
    to = c(2, 3, 2)
  ), 
  geometry = st_sfc(
    st_linestring(rbind(c(7, 51), c(7, 52))), 
    st_linestring(rbind(c(7, 51), c(8, 52))), 
    st_linestring(rbind(c(8, 52), c(7, 52)))
  ), 
  crs = 4326
)
sfn <- sfnetwork(nodes, edges)

# Let's print it
sfn

# Some key components: 
# 1) The first line reports the number of nodes and edges
# 2) The second line displays the CRS (Coordinate Reference System)
# 3) The third line briefly describe the sfnetwork object
# 4) The next group of text shows the active geometry object, while the last
# block is related to the inactive geometry.

# In fact, a sfnetwork is a multitable object in which the core network elements
# (i.e. nodes and edges) are embedded as sf objects. However, thanks to the neat
# structure of tidygraph, there is no need to first extract one of those
# elements before you are able to apply your favorite sf function or tidyverse
# verb. Instead, there is always one element at a time labeled as active.

# And let's plot it
par(mar = c(2.5, 2.5, 0.5, 0.5))
plot(sfn, axes = TRUE, graticule = TRUE, lwd = 2, cex = 2, col = "darkred")

# We can explore more sophisticated visualisations thanks to the ggplot2
# package. For example:
autoplot(sfn) + theme_light()

# More details on this point below. 

# Given an sfnework object, we can quickly extract the nodes: 
st_as_sf(sfn, "nodes")

# and edges (simple features) tables: 
st_as_sf(sfn, "edges")

# NB: By default, st_as_sf returns the active table: 
st_as_sf(sfn)
sfn %>% activate(edges) %>% st_as_sf()
sfn %E>% st_as_sf() 

# We can also just extract the geometries: 
st_geometry(sfn, "nodes")
st_geometry(sfn, "edges")

# while the single coordinates can be extracted running
st_coordinates(sfn)

# or
sfn %E>% st_coordinates()

# Finally, we can change the CRS running the usual st_transform(): 
st_transform(sfn, 32632)

# NB: The two sf tables in a sfnetwork object must always be defined using the
# same CRS.

# By default, sfnetwork() creates a directed graph, but we can change that: 
sfnetwork(nodes, edges, directed = FALSE)

# NB: This has relevant consequences on the routing algorithms (see below). 

# Clean ws
rm(nodes)

# On the other hand, in transport or road safety analysis, we usually create an
# sfnetwork object starting from a set of edges that represent the street
# segments of a given city (typically obtained from Open Street Map or similar
# providers). For example:
edges <- st_sfc(
  st_linestring(rbind(c(0, 0), c(1, 1), c(2, 0))), 
  st_linestring(rbind(c(0, 1), c(1, 0), c(2, 1))), 
  st_linestring(rbind(c(0, 0), c(0, 1))), 
  st_linestring(rbind(c(2, 0), c(2, 1)))
)

# Plot the edges
plot(edges, axes = TRUE, graticule = TRUE, lwd = 2, col = "darkred")

# Build the sfnetwork
sfn <- as_sfnetwork(edges, directed = FALSE)

# Again, print
sfn

# and plot
plot(sfn, axes = TRUE, graticule = TRUE, lwd = 2, cex = 2, col = "darkred")

# This operation works as follows: the provided lines form the edges of the
# network, and nodes are created at their endpoints. Endpoints that are shared
# between multiple lines become one single node.

# 2. Network pre-processing and cleaning ----------------------------------

# Now, let's import some data from OSM
agege <- st_read("agege.gpkg")

# and convert the data to sfnetwork structure
sfn <- as_sfnetwork(agege, directed = FALSE)

# Unfortunately, we get an error... As we can read, the function as_sfnetwork
# requires LINESTRING geometries:
agege <- st_cast(agege, "LINESTRING")

# Now
sfn <- as_sfnetwork(agege, directed = FALSE)

# As always, let's print it: 
sfn

# and plot it
plot(sfn, axes = TRUE, graticule = TRUE)

# In the next lines of code we will see several processing steps that are
# FUNDAMENTAL for the analysis of street network data (in particular when the
# data is obtained from OSM):

# >- 2.1 Rounding coordinates ---------------------------------------------

# As we said, the algorithm behind as_sfnetwork creates a node for each boundary
# point and connects LINESTRINGS that share at least one point. Unfortunately,
# this make creates unexpected problems: 

edges <- st_sfc(
  st_linestring(rbind(c(0, 0), c(1, 1), c(2 + 1e-8, 0))), 
  st_linestring(rbind(c(0, 1), c(1, 0), c(2 + 1e-8, 1))), 
  st_linestring(rbind(c(0, 0), c(0, 1))), 
  st_linestring(rbind(c(2, 0), c(2, 1)))
)

# The "edges" object looks the same as before, but
as_sfnetwork(edges)

# has more nodes than expected (with simingly duplicated coordinates). 

# We can fix this problem rounding the input coordinates: 
edges <- st_geometry(edges) %>%
  lapply(function(x) round(x, 6)) %>%
  st_sfc(crs = st_crs(edges))
as_sfnetwork(edges)

# See also https://imgs.xkcd.com/comics/coordinate_precision.png

# >- 2.2 Subdivide edges --------------------------------------------------

# When constructing a sfnetwork from a set of sf linestrings, the endpoints of
# those linestrings become nodes in the network. If endpoints are shared between
# multiple lines, they become a single node, and the edges are connected.
# However, a linestring geometry can also contain interior points that define
# the shape of the line, but are not its endpoints. It can happen that such an
# interior point in one edge is exactly equal to either an interior point or
# endpoint of another edge. In the network structure, however, these two edges
# are not connected, because they don’t share endpoints. If this is unwanted, we
# need to split these two edges at their shared point and connect them
# accordingly.

# For example
edges <- st_sfc(
  st_linestring(rbind(c(0, 0), c(1, 1), c(2, 2), c(0, 2), c(0, 3))), 
  st_linestring(rbind(c(0, 2), c(1, 1), c(2, 0)))
)

# Let's print it
edges

# and plot it
plot(edges, axes = TRUE, graticule = TRUE, lwd = 2, reset = FALSE)
plot(st_cast(edges[1], "POINT"), col = "darkred", pch = 16, add = TRUE, cex = 3)
plot(st_cast(edges[2], "POINT"), col = "darkblue", pch = 16, add = TRUE, cex = 1.5)

# On the other hand
(sfn = as_sfnetwork(edges, directed = FALSE))

# NB: THIS PROBLEM IS REALLY COMMON WHEN YOU ANALYSE DATA OBTAINED FROM OSM

# We can fix it running the "to_spatial_subdivision" morpher: 
(sfn = convert(sfn, to_spatial_subdivision, .clean = TRUE))

# We will see more examples of "morphers" in the next part of this tutorial.
# They are used to modify the structure of a given sfnetwork object.

# In particular, the "to_spatial_subdivision" morpher constructs a subdivision of
# the network by subdividing edges at each interior point that is equal to any
# other interior or boundary point in the edges table. 

# Let's plot the new network
plot(sfn, graticule = TRUE, axes = TRUE, lwd = 2, cex = 2)

# >- 2.3 Simplify network -------------------------------------------------

# A network may contain sets of edges that connect the same pair of nodes. Such
# edges can be called multiple edges. Also, it may contain an edge that starts
# and ends at the same node. Such an edge can be called a loop edge.

# For example
edges <- st_sfc(
  st_linestring(rbind(c(0, -0.5), c(0, 0.5))), 
  st_linestring(rbind(c(0, 0.5), c(0, 1.5))), 
  st_linestring(rbind(c(0, 0.5), c(1, 0.5), c(1, 1.5), c(0, 1.5))), 
  st_linestring(rbind(c(0, -0.5), c(-1, -0.5), c(-1, -1.5), c(0, -1.5), c(0, -0.5)))
)
plot(edges, axes = TRUE, lwd = 3, col = sf.colors(4, categorical = TRUE))

# We can convert this object to sfn representation
(sfn <- as_sfnetwork(edges, directed = FALSE))

# and plot it
plot(sfn, axes = TRUE, lwd = 2, cex = 2)

# We can remove these (possibly) problematic edges running the following
sfn = sfn %E>% 
  filter(!edge_is_multiple()) %>% 
  filter(!edge_is_loop())

# Print it
sfn

# and plot it
plot(sfn, axes = TRUE, lwd = 3, cex = 2)

# The same (and even more complex) operations can be performed running the
# to_spatial_simple morpher:
sfn = as_sfnetwork(edges, directed = FALSE)

sfn = sfn %>% 
  convert(to_spatial_simple, .clean = TRUE)

# We refer to https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#simplify-network
# for more details.

# >- 2.4 Smooth pseudo nodes ----------------------------------------------

# A network may contain nodes that have only one incoming and one outgoing edge.
# For tasks like calculating shortest paths, such nodes are redundant, because
# they don’t represent a point where different directions can possibly be taken.
# Sometimes, these type of nodes are referred to as pseudo nodes.

# For example: 
edges <- st_sfc(
  st_linestring(rbind(c(0, 0), c(0, 1))), 
  st_linestring(rbind(c(1, 0), c(1, 1), c(0, 1))), 
  st_linestring(rbind(c(0, 1), c(0, 2))), 
  st_linestring(rbind(c(0, 2), c(0, 3)))
)
sfn = as_sfnetwork(edges, directed = FALSE)

# Let's plot it
plot(sfn, axes = TRUE, lwd = 2, cex = 2)

# The "to_spatial_smooth" morpher can be used to remove these redundant nodes: 
sfn = sfn %>% convert(to_spatial_smooth, .clean = TRUE)
plot(sfn, axes = TRUE, lwd = 2, cex = 2)

# NB: We are now working on an extension of "to_spatial_smooth" morpher that takes
# into account the field of the input data and avoid merging different values:
# https://github.com/luukvdmeer/sfnetworks/issues/124

# 3. More advanced operations ---------------------------------------------

# As we noticed, there are several steps that should be performed before
# analysing a spatial network object with sfnetworks. So, let's apply some of
# morphers details before to agege data:

sfn = as_sfnetwork(agege, directed = FALSE)

sfn = sfn %>% 
  convert(to_spatial_subdivision) %>% 
  convert(to_spatial_simple, .clean = TRUE) %>% 
  st_transform(32631)

# NB: There are several more operations that are defined in sfnetworks, check
# out the introductory vignettes.

# >- 3.1 Spatial filters --------------------------------------------------

# The st_filter function can be used to filter from a network using spatial
# predicate functions. For example, given a rectangular polygon

poly <- st_bbox(c(xmin = 534000, xmax = 536000, ymin = 731000, ymax = 733500), crs = 32631)
poly <- st_as_sfc(poly)
plot(sfn, axes = TRUE)
plot(st_boundary(poly), add = TRUE, lwd = 5, col = "darkred")

# we can filter all the nodes inside that area
small_sfn = st_filter(sfn, poly, .pred = st_intersects)

# and plot it
plot(small_sfn, axes = TRUE)
plot(st_boundary(poly), add = TRUE, lwd = 5, col = "darkred")

# or
plot(sfn, axes = TRUE)
plot(st_boundary(poly), add = TRUE, lwd = 5, col = "darkred")
plot(small_sfn, add = TRUE, col = "darkred")

# NB: When applying sf::st_filter() to a sfnetwork, it is internally applied to
# the active element of that network. Although the filter is applied only to the
# active element of the network, it may also affect the other element. When
# nodes are removed, their incident edges are removed as well. However, when
# edges are removed, the nodes at their endpoints remain, even if they don’t
# have any other incident edges. 

# We can remove the isolated nodes running tidygraph::node_is_isolated(). For
# example:

small_sfn = sfn %>% 
  activate("edges") %>% 
  filter(edge_intersects(poly))

plot(small_sfn)
plot(small_sfn %N>% filter(!node_is_isolated()))

# These operations can be combined to detect overpasses (i.e.  edges that cross
# other edges but are not connected at that point))

overpasses <- sfn %E>%
  filter(edge_crosses(.E())) %N>% 
  filter(!node_is_isolated())

overpasses

# Let's plot it
plot(overpasses)

# We can also manually check our results using mapview(): 
mapview(st_as_sf(overpasses, "edges"))

# Clean ws
rm(small_sfn, overpasses, poly)

# >- 3.2 Routing ----------------------------------------------------------

# Calculating shortest paths between pairs of nodes is a core task in network
# analysis. The sfnetworks package offers wrappers around the path calculation
# functions of igraph, making it easier to use them when working with spatial
# data and tidyverse packages.

# First, let's add a column of weights (i.e. the geographical lengths of the
# edges) to the sfnetwork data: 
sfn = sfn %E>%
  mutate(weight = edge_length())

# These are the weights that will be used for shortest path calculations. 

# Let's start with the most basic example, but first let's check again the
# printing of sfn:
sfn

# We can see that "sfn" is composed by 20 components (i.e. clusters of connected
# edges). In fact, we can color each node according to each membership:
sfn %N>% 
  mutate(group = factor(group_components())) %>% 
  st_as_sf() %>% 
  plot(pch = 16)

# The missing links are probably due to some edges not included in the OSM extract. 

# Now, for simplicity, we will select only the nodes (and the corresponding
# edges) that belong to the main component (i.e. group = 1). The presence of
# multiple components may create problems for the routing facilities.
sfn = sfn %>% 
  convert(to_components, .clean = TRUE)

# The basic example: 
paths = st_network_paths(sfn, from = 1, to = c(10, 100))
paths

# The output is a tibble with one row per returned path. Let's check one of them

paths %>% slice(1) %>% pull(node_paths)

# Now we can plot these paths
cols <- sf.colors(3, categorical = TRUE)
plot_path = function(node_path) {
  sfn %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

plot(sfn, axes = TRUE, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
plot(sfn %N>% slice(1), col = cols[1], add = TRUE, pch = 16, cex = 2)
plot(sfn %N>% slice(c(10, 100)), col = cols[2:3], add = TRUE, pch = 16, cex = 2)

# The same operation can be performed using geocoded points instead of indices: 
p1 = st_sfc(st_point(c(535000, 730500)), crs = 32631)
p2 = st_sfc(st_point(c(533900, 733050)), crs = 32631)

st_network_paths(sfn, from = p1, to = c(p2, p3))

# Moreover, we can easily extract the shortest path (and not only the indices)
# using the "to_spatial_shortest_path" morpher:

sp <- sfn %>% 
  convert(
    .f = to_spatial_shortest_paths, 
    from = p1, 
    to = p2
  )

plot(sfn, axes = TRUE, col = "grey")
plot(p1, add = TRUE, col = cols[1], pch = 16, cex = 2)
plot(p2, add = TRUE, col = cols[2], pch = 16, cex = 2)
plot(sp, add = TRUE)

# As we said, by default st_network_paths and to_spatial_shortest_paths uses the
# geographical lengths as weights. Nevertheless, we can define a custom routing
# profile to give more or less weights to certain types of highways. For example
sfn

# and we may want to adopt the following scheme: 
weighting_profile = c(
  residential = 1, 
  secondary = 0.66, 
  secondary_link = 0.66, 
  tertiary = 0.8, 
  trunk = 0.5, 
  trunk_link = 0.5, 
  unclassified = 1
)

weighted_sfn = sfn %>%
  activate("edges") %>%
  mutate(multiplier = weighting_profile[highway]) %>%
  mutate(weight = edge_length() * multiplier)

# Now we can run the same algorithm as before... 

# Clear ws
rm(p1, p2, p3, paths, sp, cols, weighting_profile, plot_path, weighted_sfn)

# >- 3.3 Isodistances -----------------------------------------------------

# With respect to a given point p and a given travel time t, an isochrone is the
# line for which it holds that the travel time from any point on the line to or
# from p is equal to t. When using distances instead of time, it is called an
# isodistance.

# We can calculate isodistances from a given point as follows: 
p1 <- st_sfc(st_point(c(534550, 732000)), crs = 32631)
p1_index <- st_nearest_feature(p1, sfn %>% activate("nodes"))

sfn_small <- sfn %N>% 
  filter(
    node_distance_from(
      nodes = p1_index, 
      weights = weight
    ) <= 1000
  )

plot(sfn, axes = TRUE, col = "grey")
plot(sfn_small, add = TRUE, col = "darkred")
plot(p1, add = TRUE, col = "black", pch = 16, cex = 2)

# Clear ws
rm(p1, p1_index, sfn_small)

# Any question? Do you want more examples? We can try to code isochrones, an
# example of the Travelling Salesperson Problem, or briefly explore some of the
# spatial morphers (to_spatial_contracted, to_spatial_neighborhood, ...).
