###POLS 370 FINAL PROJECT
##ERGM and Alliance Formation


#Research Question:
#To what extent do power-related attributes such as nuclear capability and 
#military expenditure influence the formation of alliances?

##

#Data:

#From ''Correlates of War''

#https://correlatesofwar.org/data-sets/formal-alliances/

#This data set records all formal alliances among states between 1816 and 2012, including mutual defense pacts, non-aggression treaties, and ententes.
# I used this dataset to show the alliance network

#https://correlatesofwar.org/data-sets/national-material-capabilities/

#military expenditure, military personnel, energy consumption, iron and steel production, urban population, and total population –
#are included in this data set.It serves as the basis for the most widely used indicator of national capability,
#CINC (Composite Indicator of National Capability) and covers the period 1816-2016. 

#I used this dataset to show countries' material capabilities, the military expenditure data in this dataset has been used for now
# however, other values can also be used in the future work


#nuclear status and nato/warsaw bloc membership have been coded manually

###

####Research Design and Methodology

#Network constructed from 1965-1975-1985 mutual defense pacts.

# Nodes represent states
# Edges show whether there are a mutual military defense pact or not between two states/nodes





##Hypotheses

#H1 / Network Centrality Hypothesis:

# Nuclear weapon states (NWS) are more likely to form alliances than non-nuclear weapon states (NNWS).

#rationale: network centrality may correlate with power. either NWS are attractive allies, or they're strategically autonomous.

#Tested via: nodecov("nuclear")


#### please read 
# important note: to show states with particular military strength, most of which developed their own nuclear weapons, but lack mutual defense treaties
# I manually add them under the name of non-aligned countries. hence, they are seen in the 'non-aligned' bloc but this
# bloc does not refer to 'non-aligned' movement of the period but just names the countries with mutual defense treaties.
# since bloc affliation had not been used in testing the hypotheses, I dropped this variable at the very end of this study



#library
library(statnet)
library(igraph)
library(intergraph)
library(ergm)
library(readr)
library(dplyr)
library(tidyverse)




#datasets
alliances <- read_csv("alliance_v4.1_by_member_yearly.csv")

nmc <- read_csv("NMC-60-abridged.csv")



###1965

########## 1965 Context
# Filtering for 1965 Defense Pacts Only 
alliances_1965 <- alliances %>%
  filter(year == 1965, defense == 1) %>%
  select(version4id, state_name, year)

# Generating Edge List (pairwise ties in same alliance) ===
edge_list <- data.frame()
unique_ids <- unique(alliances_1965$version4id)

for (id in unique_ids) {
  subset <- alliances_1965[alliances_1965$version4id == id, ]
  members <- subset$state_name
  if (length(members) >= 2) {
    pairs <- t(combn(members, 2))
    edge_list <- rbind(edge_list, data.frame(from = pairs[,1], to = pairs[,2]))
  }
}

#Removing duplicate edges
edge_list <- distinct(edge_list)


### important note: generating edge list and removing duplicate edges sections (the two above) were made by AI. AI has been used
# especially in this part due to constant error and misrepresantation of nodes (some states' names did not appear in the graph)
# by the hand-written code; from here, I will indicate the AI made codes via the sign ***


# Defining non-aligned important countries manually (1965) 
non_aligned <- c("India", "South Africa", "Egypt", "Yugoslavia", "Indonesia", "Israel")

#important reminder: non-aligned does not refer to ''non-aligned'' movement but just refer to countries that did not have 
#mutual defense pact with other countries, 

#also, bloc affliation had not been considered in the hypothesis testing process

#*** === Create vertex list including nodes from edges and non-aligned countries ===
nodes_from_edges <- unique(c(edge_list$from, edge_list$to))
all_nodes <- unique(c(nodes_from_edges, non_aligned))
node_df <- data.frame(name = all_nodes)

# Creating the Graph
g65 <- graph_from_data_frame(edge_list, vertices = node_df, directed = FALSE)

# Nuclear Status (manually assigned for 1965)
nws <- c("United States of America", "United Kingdom", "France", "Russia", "China")
V(g65)$nuclear <- ifelse(V(g65)$name %in% nws, "NWS", "NNWS")

# Bloc Assignment for 1965
nato_1965 <- c("United States of America", "United Kingdom", "France", "Italy",
               "Canada", "Belgium", "Netherlands", "Luxembourg", "Portugal", 
               "Norway", "Denmark", "Greece", "Turkey", "German Federal Republic")

warsaw_pact_1965 <- c("Russia", "Poland", "East Germany", "Czechoslovakia", 
                      "Hungary", "Romania", "Bulgaria", "Albania")

V(g65)$bloc <- ifelse(V(g65)$name %in% nato_1965, "NATO",
                    ifelse(V(g65)$name %in% warsaw_pact_1965, "Warsaw",
                           ifelse(V(g65)$name %in% non_aligned, "Non-Aligned", "Other")))

# Visual Attributes
V(g65)$frame.color <- ifelse(V(g65)$bloc == "NATO", "blue",
                           ifelse(V(g65)$bloc == "Warsaw", "darkred",
                                  ifelse(V(g65)$bloc == "Non-Aligned", "green", "black")))

# Plot of the Graph 
plot(g65,
     vertex.color = ifelse(V(g65)$nuclear == "NWS", "red", "lightgray"),
     vertex.label.color = "black",
     vertex.size = 5,
     vertex.label.cex = 0.7,
     vertex.frame.color = V(g65)$frame.color,
     main = "1965 Alliance-Defense Network with Non-Aligned Countries")

### Adding National Material Capabilities - military expenditure

##National Material Capabilities
nmc <- read_csv("NMC-60-abridged.csv")
nmc
nmc_1965 <- nmc %>% filter(year == 1965)

#selecting military expenditure
nmc_1965 <- nmc_1965 %>% select(stateabb, milex)
nmc_1965


#*** matching country names with their abbreviations
country_map <- c(
  "AFG" = "Afghanistan",  "ALB" = "Albania",
  "ARG" = "Argentina",  "AUL" = "Australia",  "AUS" = "Austria",  "BEL" = "Belgium",  "BEN" = "Benin",
  "BFO" = "Burkina Faso",  "BOL" = "Bolivia",  "BRA" = "Brazil",  "BUL" = "Bulgaria",  "CAM" = "Cambodia",  "CAN" = "Canada",
  "CAO" = "Central African Republic",  "CDI" = "Côte d'Ivoire",  "CEN" = "Cameroon",  "CHA" = "Chad",  "CHL" = "Chile",  "CHN" = "China",
  "COL" = "Colombia",  "CON" = "Republic of the Congo",  "COS" = "Costa Rica",  "CUB" = "Cuba",  "CYP" = "Cyprus",
  "CZE" = "Czechoslovakia",  "DEN" = "Denmark",  "DOM" = "Dominican Republic",  "DRC" = "Democratic Republic of the Congo",  "DRV" = "North Vietnam",
  "ECU" = "Ecuador",  "EGY" = "Egypt",  "ETH" = "Ethiopia",  "FIN" = "Finland",  "FRN" = "France",  "GAB" = "Gabon",  "GDR" = "East Germany",
  "GFR" = "German Federal Republic",  "GHA" = "Ghana",  "GRC" = "Greece",  "GUA" = "Guatemala",  "GUI" = "Guinea",  "HAI" = "Haiti",
  "HON" = "Honduras",  "HUN" = "Hungary",  "ICE" = "Iceland",  "IND" = "India",  "INS" = "Indonesia",  "IRE" = "Ireland",  "IRN" = "Iran",  "IRQ" = "Iraq",  "ISR" = "Israel",  "ITA" = "Italy",
  "JOR" = "Jordan",  "JPN" = "Japan",  "LAO" = "Laos",  "LBR" = "Liberia",  "LEB" = "Lebanon",  "LIB" = "Libya",  "LUX" = "Luxembourg",  "MAA" = "Mali Federation",
  "MAG" = "Madagascar",  "MAL" = "Malaysia",  "MEX" = "Mexico",  "MLI" = "Mali",  "MON" = "Mongolia",  "MOR" = "Morocco",  "MYA" = "Myanmar",
  "NEP" = "Nepal",  "NEW" = "New Zealand",  "NIC" = "Nicaragua",  "NIG" = "Niger",  "NIR" = "Nigeria",
  "NOR" = "Norway",  "NTH" = "Netherlands", "PAK" = "Pakistan",  "PAN" = "Panama",  "PAR" = "Paraguay",
  "PER" = "Peru",  "PHI" = "Philippines",  "POL" = "Poland",  "POR" = "Portugal",  "PRK" = "North Korea",
  "ROK" = "South Korea",  "ROM" = "Romania",  "RUS" = "Russia",  "RVN" = "South Vietnam",  "SAF" = "South Africa",
  "SAL" = "El Salvador",  "SAU" = "Saudi Arabia",  "SEN" = "Senegal",  "SOM" = "Somalia", "SPN" = "Spain",
  "SRI" = "Sri Lanka", "SUD" = "Sudan","SWD" = "Sweden","SWZ" = "Switzerland", "TAW" = "Taiwan",
  "THI" = "Thailand", "TOG" = "Togo", "TUN" = "Tunisia","TUR" = "Turkey", "UKG" = "United Kingdom",
  "URU" = "Uruguay", "USA" = "United States of America", "VEN" = "Venezuela", "YAR" = "Yemen Arab Republic", "YUG" = "Yugoslavia")


nmc_1965$country_name <- country_map[nmc_1965$stateabb]

V(g65)$milex <- nmc_1965$milex[match(V(g65)$name, nmc_1965$country_name)]

####
## centrality

#military-weighted connectivity score — countries with both lots of allies and big budgets will score highest.

# Degree centrality
sort(degree(g65))
deg <- degree(g65)

# Multiply degree by military expenditure
deg_milex <- deg * V(g65)$milex

deg_milex

sort(deg_milex) #may work

#betweenness
sort(betweenness(g65))
bc <- betweenness(g65)
bc_milex <- bc * V(g65)$milex
bc_milex
sort(bc_milex) #meaningless



#pagerank
#PageRank centrality measures a node’s global importance in a network — not just by how many links it has, but by how important its neighbors are.
#In a defense alliance network, PageRank centrality can be read as:
#“Which states are structurally important in the alliance system — not just because they have many allies,
#but because their allies are themselves well-connected or influential.”

pr <- page_rank(g65)$vector
sort(pr)
pr_milex <- pr * V(g65)$milex
sort(pr_milex) #may work, most appropriate because the first 5 countries have nuclear weapons (5/5 for the time)

## rationale of centrality measurements

# what I do: Centrality × Military Power = “Weighted Strategic Influence”

#The rationale were:
#A country with many alliances but low military capability may not be as influential 

#A country with few alliances but high military capacity might be a lone power .

#A country with both high centrality and high military expenditure is likely a key strategic actor

#This multiplication lets us:

#Identifying "military-weighted central actors".

#Ranking states by structural position + power projection capacity.

#Interpreting influence not just as position in the network, but position with resources.

#in the cold-war environment both blocs/alignment and material capabilities(milex) matters


#### Conducting Exponential Random Grahp Model Analysis
library(statnet)
library(igraph)
library(intergraph)
library(ergm)
library(network)

vertex_attr(g65, "milex")
# Converting igraph to network object
net65 <- asNetwork(g65)


# Checking for missing military expenditure values

# Extracting the attribute from the igraph object
milex_vals <- igraph::vertex_attr(g65, "milex")

#Converting to network object
net65 <- intergraph::asNetwork(g65)

#*** Assigning the attribute explicitly using network package
network::set.vertex.attribute(net65, "milex", milex_vals)

#*** Removing nodes with missing values
missing_nodes <- which(is.na(network::get.vertex.attribute(net65, "milex")))
net65_clean <- network::delete.vertices(net65, missing_nodes)

network::list.vertex.attributes(net65)

#checking for NAs
network::get.vertex.attribute(net65, "na")
#deleting NAs
network::delete.vertex.attribute(net65, "na")

network::list.vertex.attributes(net65)

summary(net65_clean)

network.size(net65)
network.size(net65_clean) 


##models

#model0_65, the baseline model
m0_65 <- ergm(net65_clean ~ edges)
summary(m0_65)

#   term          Estimate      Std. Error  MCMC %      z value     Pr(>|z|)    
#   edges         -1.62107       0.05644      0          -28.72      <1e-04 ***


#This model tests the baseline probability of a tie (i.e., alliance) forming between any two nodes in our network, without including any covariates.

#This model serves as a foundation for more complex ERGMs.

#Coefficient (edges = -1.62107): This is the log-odds of a tie (an alliance) forming between two nodes in the absence of any covariates.
# this can be converted to probability via the formula : probability of a tie = e^coefficient / 1 + e^coefficient
#which is equal in this case to 16.5%

# p-value shows that this is highly significant

#thus:
#our 1965 alliance network has statistically significant structure.

#There’s a low baseline probability (~16.5%) of any two countries being tied.




##model1_65

#but before we need to define the terms

#nodecov("attr") — Node-level covariate (continuous or binary)
#Used when the value of a node’s attribute influences its general propensity to form ties, regardless of whom the tie is with.
#such as, States with higher milex (military spending) form more ties

#nodematch("attr") — Homophily / matching effect
#Used when ties are more likely between nodes that share the same value of an attribute.
#such as, States in the same bloc are more likely to ally.

#nodefactor("attr") — Factor-level tie tendency
#Estimates separate effects for each category of a factor attribute.
#such as, Nuclear states may have higher or lower general propensity to form ties, depending on category.

#my two hypotheses are which i am testing
#H1: NWS are more likely to form alliances than NNWS.
#H2: States with higher military expenditure are more likely to form alliances.

#for H1
#This is a node-level attribute (0 = NNWS, 1 = NWS)
#we're testing whether nuclear-capable states are generally more likely to form ties, regardless of whom they're tied to.

#so for H1 using nodecov("nuclear") would be more appropriate because it is a binary category

#for H2 nodecov("milex") would be appropriate since tests whether a node’s numeric value on milex influences the number of ties it forms.



model1_65 <- ergm(net65_clean ~ edges + nodefactor("bloc") + nodecov("nuclear") + nodecov("milex"))


#Error: In term ‘nodecov’ in package ‘ergm’: Attribute ‘"nuclear"’ is not a numeric or logical vector as required.
#so
nuclear_numeric <- as.numeric(network::get.vertex.attribute(net65_clean, "nuclear"))
network::set.vertex.attribute(net65_clean, "nuclear", nuclear_numeric)
#then re-run the model
#Error: In term ‘nodecov’ in package ‘ergm’: Attribute ‘"nuclear"’ has missing data, which is not currently supported by ergm.
# Identify nodes with missing nuclear data
missing_nuclear <- which(is.na(network::get.vertex.attribute(net65_clean, "nuclear")))

# Removing those nodes
net65_clean <- network::delete.vertices(net65_clean, missing_nuclear)

network.size(net65)         # before deletion #68
network.size(net65_clean)   # after deletion #0, problem

#
table(network::get.vertex.attribute(net65, "nuclear"))
missing_nuclear <- which(is.na(network::get.vertex.attribute(net65, "nuclear")))
net65_clean <- network::delete.vertices(net65, missing_nuclear)
network.size(net65_clean)  # Should now be > 0, problem

#trying again

nuclear_char <- network::get.vertex.attribute(net65, "nuclear")
nuclear_numeric <- ifelse(nuclear_char == "NWS", 1,
                          ifelse(nuclear_char == "NNWS", 0, NA))

network::set.vertex.attribute(net65, "nuclear", nuclear_numeric)
missing_nuclear <- which(is.na(network::get.vertex.attribute(net65, "nuclear")))
net65_clean <- network::delete.vertices(net65, missing_nuclear)

table(network::get.vertex.attribute(net65_clean, "nuclear"))  # Should show 0s and 1s, almost solved

nuclear_final <- as.numeric(as.character(network::get.vertex.attribute(net65_clean, "nuclear")))
network::set.vertex.attribute(net65_clean, "nuclear", nuclear_final)

###problem solved, in order to utilize nodecov(''nuclear'') ( lets say instead of nodefactor) we need to convert the term
#nuclear to 0s and 1s (0 showing NNWS, 1 showing NWS), with accomplishing this 
#task we can now begin to build our models

model1_65 <- ergm(net65_clean ~ edges + nodefactor("bloc") + nodecov("nuclear") + nodecov("milex"))

summary(model1_65)

#RESULTS
#for H1
#nodecov.nuclear estimate= -0.4947, p = 0.035
#This coefficient is statistically significant at the 0.05 level (★).

#It means: being a nuclear weapon state reduces the log-odds of forming alliances, controlling for bloc and military spending.

#Contrary to H1: NWS are less likely to form alliances in this model.

#this result rejects H1 as formulated — NWS are not more active in alliance formation; in fact, possibly more self-reliant.

#for H2
#nodecov.milex estimate= 3.425e-08, p < 0.0001
#This is positive and highly significant (★★★).

#It means: states with higher military spending are significantly more likely to form alliances.

#The coefficient is small because milex values are large (e.g., millions), but the direction and significance are clear.

#this supports H2: Military power is associated with higher alliance activity.




####1975 version
### 1975

# Filtering for 1975 Defense Pacts Only
alliances_1975 <- alliances %>%
  filter(year == 1975, defense == 1) %>%
  select(version4id, state_name, year)

#*** === Generate Edge List (pairwise ties in same alliance) ===
edge_list <- data.frame()
unique_ids <- unique(alliances_1975$version4id)

for (id in unique_ids) {
  subset <- alliances_1975[alliances_1975$version4id == id, ]
  members <- subset$state_name
  if (length(members) >= 2) {
    pairs <- t(combn(members, 2))
    edge_list <- rbind(edge_list, data.frame(from = pairs[,1], to = pairs[,2]))
  }
}

#*** Removing duplicate edges 
edge_list <- distinct(edge_list)

# Defining non-aligned important countries manually (1975) 
non_aligned <- c("India", "South Africa", "Egypt", "Yugoslavia", "Indonesia", "Israel")

#*** Creating vertex list including nodes from edges and non-aligned countries
nodes_from_edges <- unique(c(edge_list$from, edge_list$to))
all_nodes <- unique(c(nodes_from_edges, non_aligned))
node_df <- data.frame(name = all_nodes)

# the Graph 
g75 <- graph_from_data_frame(edge_list, vertices = node_df, directed = FALSE)

# Nuclear Status (manually assigned for 1975) 
nws <- c("United States of America", "United Kingdom", "France", "Russia", "China", "India", "Israel")

V(g75)$nuclear <- ifelse(V(g75)$name %in% nws, "NWS", "NNWS")

# Bloc Assignment for 1975 
nato_1975 <- c("United States of America", "United Kingdom", "France", "Italy",
               "Canada", "Belgium", "Netherlands", "Luxembourg", "Portugal", 
               "Norway", "Denmark", "Greece", "Turkey", "German Federal Republic")

warsaw_pact_1975 <- c("Russia", "Poland", "East Germany", "Czechoslovakia", 
                      "Hungary", "Romania", "Bulgaria")

V(g75)$bloc <- ifelse(V(g75)$name %in% nato_1975, "NATO",
                      ifelse(V(g75)$name %in% warsaw_pact_1975, "Warsaw",
                             ifelse(V(g75)$name %in% non_aligned, "Non-Aligned", "Other")))

# Visual Attributes 
V(g75)$frame.color <- ifelse(V(g75)$bloc == "NATO", "blue",
                             ifelse(V(g75)$bloc == "Warsaw", "darkred",
                                    ifelse(V(g75)$bloc == "Non-Aligned", "green", "black")))

# Plot of the Graph
plot(g75,
     vertex.color = ifelse(V(g75)$nuclear == "NWS", "red", "lightgray"),
     vertex.label.color = "black",
     vertex.size = 5,
     vertex.label.cex = 0.7,
     vertex.frame.color = V(g75)$frame.color,
     main = "1975 Alliance-Defense Network with Non-Aligned Countries")

### Adding Military Expenditure (milex) from NMC

nmc_1975 <- nmc %>% filter(year == 1975) %>%
  select(stateabb, milex)

nmc_1975$country_name <- country_map[nmc_1975$stateabb]

# Assign milex values to nodes
V(g75)$milex <- nmc_1975$milex[match(V(g75)$name, nmc_1975$country_name)]



## centrality
#pagerank
#PageRank centrality measures a node’s global importance in a network — not just by how many links it has, but by how important its neighbors are.
#In a defense alliance network, PageRank centrality can be read as:
#“Which states are structurally important in the alliance system — not just because they have many allies,
#but because their allies are themselves well-connected or influential.”

pr75 <- page_rank(g75)$vector
sort(pr75)
pr75_milex <- pr75 * V(g75)$milex
sort(pr75_milex)



####ERGM model
net75 <- intergraph::asNetwork(g75)

nuclear_char <- network::get.vertex.attribute(net75, "nuclear")
nuclear_numeric <- ifelse(nuclear_char == "NWS", 1,
                          ifelse(nuclear_char == "NNWS", 0, NA))
network::set.vertex.attribute(net75, "nuclear", nuclear_numeric)

# Remove NAs in nuclear or milex
missing_nuclear <- which(is.na(network::get.vertex.attribute(net75, "nuclear")))
missing_milex <- which(is.na(network::get.vertex.attribute(net75, "milex")))

# Combine and remove
missing_all <- unique(c(missing_nuclear, missing_milex))
net75_clean <- network::delete.vertices(net75, missing_all)


network.size(net75) #70
network.size(net75_clean)  # Should be > 0, it is 70 so no node is deleted

#to remind:
#H1: Nuclear Weapon States (NWS) are more likely to form alliances than Non-NWS

#H2: States with higher military expenditure are more likely to form alliances



model75 <- ergm(net75_clean ~ edges +
                  nodecov("nuclear") +     # H1: NWS vs. NNWS
                  nodecov("milex") +       # H2: military spending
                  nodefactor("bloc"))      # Control for bloc membership

summary(model75)

##RESULTS
#nodecov.nuclear: estimate = -0.455, p = 0.0399
#This result is statistically significant and negative.

#It means: nuclear states were less likely to form alliances in 1975

#H1 is rejected: Data shows the opposite — NWS were less likely to form alliances

#Possible interpretation:
#Nuclear states (e.g., US, USSR, China) may rely on self-defense capabilities or bilateral deterrence instead of multilateral alliances.
#OR
#lack of sufficient external securities and systemic insecurity against other powers, states develop nuclear weapons for deterrrence 


#H2: nodecov.milex: estimate = +1.52e-08, p < 0.0001
#This coefficient is positive and highly significant.


#The magnitude looks small due to the scale (military spending values are in millions), but the direction and effect are clear.

#Interpretation: States with higher military expenditure were more likely to form alliances.

#H2 is strongly supported.




###1985 context
# Filtering for 1985 Defense Pacts Only
alliances_1985 <- alliances %>%
  filter(year == 1985, defense == 1) %>%
  select(version4id, state_name, year)

#*** Generating Edge List (pairwise ties in same alliance) 
edge_list <- data.frame()
unique_ids <- unique(alliances_1985$version4id)

for (id in unique_ids) {
  subset <- alliances_1985[alliances_1985$version4id == id, ]
  members <- subset$state_name
  if (length(members) >= 2) {
    pairs <- t(combn(members, 2))
    edge_list <- rbind(edge_list, data.frame(from = pairs[,1], to = pairs[,2]))
  }
}

#*** Removing duplicate edges
edge_list <- distinct(edge_list)

# Defining non-aligned important countries manually (1985) 
non_aligned <- c("India", "South Africa", "Egypt", "Yugoslavia", "Indonesia", "Israel")

#*** Creating vertex list including nodes from edges and non-aligned countries 
nodes_from_edges <- unique(c(edge_list$from, edge_list$to))
all_nodes <- unique(c(nodes_from_edges, non_aligned))
node_df <- data.frame(name = all_nodes)

#  the Graph 
g85 <- graph_from_data_frame(edge_list, vertices = node_df, directed = FALSE)

# Nuclear Status (manually assigned for 1985) 
nws <- c("United States of America", "United Kingdom", "France", "Russia", "China", "India", "Israel", "South Africa")

V(g85)$nuclear <- ifelse(V(g85)$name %in% nws, "NWS", "NNWS")

#  Bloc Assignment for 1985 
nato_1985 <- c("United States of America", "United Kingdom", "France", "Italy",
               "Canada", "Belgium", "Netherlands", "Luxembourg", "Portugal", 
               "Norway", "Denmark", "Greece", "Turkey", "German Federal Republic", "Spain")

warsaw_pact_1985 <- c("Russia", "Poland", "East Germany", "Czechoslovakia", 
                      "Hungary", "Romania", "Bulgaria")

V(g85)$bloc <- ifelse(V(g85)$name %in% nato_1985, "NATO",
                      ifelse(V(g85)$name %in% warsaw_pact_1985, "Warsaw",
                             ifelse(V(g85)$name %in% non_aligned, "Non-Aligned", "Other")))

# Visual Attributes 
V(g85)$frame.color <- ifelse(V(g85)$bloc == "NATO", "blue",
                             ifelse(V(g85)$bloc == "Warsaw", "darkred",
                                    ifelse(V(g85)$bloc == "Non-Aligned", "green", "black")))

# Plot of the Graph 
plot(g85,
     vertex.color = ifelse(V(g85)$nuclear == "NWS", "red", "lightgray"),
     vertex.label.color = "black",
     vertex.size = 5,
     vertex.label.cex = 0.7,
     vertex.frame.color = V(g85)$frame.color,
     main = "1985 Alliance-Defense Network with Non-Aligned Countries")

### Adding Military Expenditure (milex) from NMC

nmc_1985 <- nmc %>% filter(year == 1985) %>%
  select(stateabb, milex)

nmc_1985$country_name <- country_map[nmc_1985$stateabb]

# Assigning the milex values to nodes
V(g85)$milex <- nmc_1985$milex[match(V(g85)$name, nmc_1985$country_name)]

###centrality

pr85 <- page_rank(g85)$vector
sort(pr85)
pr85_milex <- pr85 * V(g85)$milex
sort(pr85_milex)




####ERGM model
net85 <- intergraph::asNetwork(g85)

nuclear_char <- network::get.vertex.attribute(net85, "nuclear")
nuclear_numeric <- ifelse(nuclear_char == "NWS", 1,
                          ifelse(nuclear_char == "NNWS", 0, NA))
network::set.vertex.attribute(net85, "nuclear", nuclear_numeric)

# Remove NAs in nuclear or milex
missing_nuclear <- which(is.na(network::get.vertex.attribute(net85, "nuclear")))
missing_milex <- which(is.na(network::get.vertex.attribute(net85, "milex")))

# Combine and remove
missing_all <- unique(c(missing_nuclear, missing_milex))
net85_clean <- network::delete.vertices(net85, missing_all)


network.size(net85) #77
network.size(net85_clean)  # Should be > 0, 77

#to remind:
#H1: Nuclear Weapon States (NWS) are more likely to form alliances than Non-NWS

#H2: States with higher military expenditure are more likely to form alliances



model85 <- ergm(net85_clean ~ edges +
                  nodecov("nuclear") +     # H1: NWS vs. NNWS
                  nodecov("milex") +       # H2: military spending
                  nodefactor("bloc"))      # Control for bloc membership

summary(model85)

#RESULTS
#nodecov.nuclear: estimate = -0.608, p = 0.004

#This result is statistically significant and negative.

#It means: nuclear states were less likely to form alliances in 1985

#Possible interpretation:
#Nuclear states (e.g., US, USSR, China) may rely on self-defense capabilities or bilateral deterrence instead of multilateral alliances.
#OR
#lack of sufficient external securities and systemic insecurity against other powers, states develop nuclear weapons for deterrrence 

#H1 is rejected: Nuclear states formed fewer alliances, not more.


#nodecov.milex: estimate = +6.88e-09, p < 0.0001

#Highly significant and positive.

#States with higher military spending were more likely to form alliances.

#The small coefficient is due to large scale of milex (millions), but the direction and significance are robust.

#H2 is supported: Greater military power is associated with more alliance ties.








### post-presentation, revised models

##bloc variable has been dropped from the models since it was not essential for hypotheses testings
#Baseline probability of forming a tie (edges)

#Effect of nuclear status (nodecov.nuclear)

#Effect of military expenditure (nodecov.milex)

#for 1965
model65_2 <- ergm(net65_clean ~ edges + nodecov("nuclear") + nodecov("milex"))

summary(model65_2)

###RESULTS
#                   Estimate     Std. Error    MCMC %     z value     Pr(>|z|)    
#edges             -1.727e+00    6.280e-02      0         -27.499     < 1e-04 ***
#nodecov.nuclear   -1.242e-01    2.328e-01      0         -0.533        0.593726    
#nodecov.milex      2.408e-08    6.436e-09      0          3.742        0.000183 ***

#Edges: Low baseline probability of alliance formation (log-odds = −1.727 → probability ≈ 15%).

#Nuclear status: No significant effect. In 1965, NWS were neither more nor less likely to form alliances than NNWS.
# Comparing with the previous models, this was the most changed feature of the model results. this variable was significant in the previous
#1965 model but not in the revised version

#Military spending: Strong and significant positive effect. Countries with higher military expenditure were more likely to form alliances.

#for 1975
model75_2 <- ergm(net75_clean ~ edges + nodecov("nuclear") +  nodecov("milex"))  

summary(model75_2)

#                    Estimate       Std. Error   MCMC %      z value    Pr(>|z|)    
#edges               -1.717e+00    6.244e-02      0          -27.501    <1e-04 ***
#nodecov.nuclear     -4.717e-01    1.897e-01      0          -2.486      0.0129 *  
#nodecov.milex        1.130e-08    2.485e-09      0           4.550     <1e-04 ***

#Edges: Similar baseline tie probability (~15%).

#Nuclear status: Negative and significant. Nuclear states were less likely to form alliances — possibly because of self-reliance or deterrent status.

#Military spending: Again, a strong and significant positive effect on alliance formation.



#for 1985
model85_2 <- ergm(net85_clean ~ edges + nodecov("nuclear") + nodecov("milex"))    

summary(model85_2)

#                 Estimate     Std. Error    MCMC %      z value     Pr(>|z|)    
#edges           -1.730e+00    5.719e-02      0          -30.250     <1e-04 ***
#nodecov.nuclear -8.865e-01    1.909e-01      0          -4.644      <1e-04 ***
#nodecov.milex    6.682e-09    1.024e-09      0           6.525      <1e-04 ***

#Edges: Slightly lower baseline probability (~15%).

#Nuclear status: Effect is more strongly negative and highly significant than in 1975. Nuclear states were even less likely to form alliances in 1985.

#Military spending: Still a strong positive driver of alliance formation.

####

#CONCLUSION
#Military power/expenditure promotes alliance formation (realist logic).

#Nuclear power discourages alliance formation (deterrent autonomy, bloc structure).

#The effect of nuclear weapons becomes more pronounced (negative) over time.

#my hypotheses were:
# H1: Nuclear Weapon States (NWS) are more likely to form alliances than Non-Nuclear Weapon States (NNWS).

# H2: States with higher military expenditure are more likely to form alliances.


#Year	 Coefficient nodecov(nuclear)      p-value	        Effect
#1965	     −0.124	                        0.594	        Not significant
#1975   	 −0.472	                        0.013	        Significant negative effect
#1985   	 −0.887	                       <0.001	        Stronger negative effect

#Contrary to H1, nuclear states were not more likely to form alliances. In fact, from 1975 onward, they were significantly less likely to do so.

## thus, H1 is rejected

# Year    Coefficient nodecov(milex)  p-value  Effect                  
# 1965           +2.41e−08             <0.001  Significant and positive
# 1975           +1.13e−08             <0.001  Significant and positive
# 1985           +6.68e−09             <0.001  Significant and positive

#In all three years, military spending had a positive and highly significant effect on alliance formation.

#thus, H2 is confirmed
  

### Extra models with gwnsp
#using gwnsp

###due to errors and lack of time (I ran the models for hours but my computer was not able to finish) I tried simplier gwnsp models
#1985 gwnsp
test_model <- ergm(net85_clean ~ gwnsp(0.5, fixed = TRUE))

summary(test_model)

#RESULTS of test_model
#Monte Carlo Maximum Likelihood Results:
  
#                   Estimate    Std. Error MCMC %    z value    Pr(>|z|)    
#gwnsp.fixed.0.5    -0.90784    0.04252      1       -21.35     <1e-04 ***

#gwnsp.fixed.0.5 This term captures geometrically weighted non-shared partners — meaning:
#The tendency for ties to form between nodes that share partners, but have not yet formed a triangle

#Interpretation
# Coefficient: -0.91 (statistically significant)
#This negative and significant coefficient tells us that non-shared partner configurations
#(i.e., open two-paths) are less likely to lead to tie formation in our 1985 alliance network.

#meaning: Countries that share allies but haven’t yet allied themselves are less likely to form a direct defense pact
#— indicating a lack of bridging or transitive closure.

#This contradicts classic balance theories that suggest “the friend of my friend is my friend.” Instead, it suggests:

#i)Blocs may already be saturated

#ii)Triadic closure is not common

#Conclusion:
#results suggest that in 1985:
#Transitivity was not driving alliance formation.

#The alliance network may have been shaped more by dyadic strategic interests or structural polarization (e.g., NATO–Warsaw boundaries),
#rather than triadic logic.


## after testing the test_model for 1985, I also conducted same models for 1965 and 1975 for comparison

#1965gwnsp
test_model_65 <- ergm(net65_clean ~ gwnsp(0.5, fixed = TRUE))

summary(test_model_65)

#Monte Carlo Maximum Likelihood Results:
  
#                    Estimate    Std. Error     MCMC %     z value    Pr(>|z|)    
#gwnsp.fixed.0.5     -0.65500    0.04065        0          -16.11     <1e-04 ***


#1975 gwnsp
test_model_75 <- ergm(net75_clean ~ gwnsp(0.5, fixed = TRUE))

summary(test_model_75)

#Monte Carlo Maximum Likelihood Results:
  
#                  Estimate   Std. Error   MCMC %     z value   Pr(>|z|)    
#gwnsp.fixed.0.5   -0.67919    0.04116      1         -16.5     <1e-04 ***



###Comparsion of results

#    Year | Term            | Estimate    | Std. Error | z-value | p-value | Significance |
#  | ---- | --------------- | ----------- | ---------- | ------- | ------- | ------------ |
#  | 1965 | gwnsp.fixed.0.5 | **−0.6550** | 0.04065    | −16.11  | <0.0001 | ***          |
#  | 1975 | gwnsp.fixed.0.5 | **−0.6792** | 0.04116    | −16.50  | <0.0001 | ***          |
#  | 1985 | gwnsp.fixed.0.5 | **−0.9078** | 0.04252    | −21.35  | <0.0001 | ***          |
  

#The gwnsp term is negative and highly statistically significant in all three years.
  
# this means that countries that share mutual allies, but are not directly allied themselves, are less likely to form a direct alliance.
  
#This pattern supports the idea that:
# Alliance networks are shaped more by exclusivity and balance of power than transitive closure.
#Stronger negative estimate suggests that:
# Alliance networks had solidified; bridging was even more avoided or strategically unnecessary.



#### THANKS FOR EVERYTHING
### MÜKERREM SAİD ÖZDEMİR, 2020202165, POLS370, 15.05.2025





































