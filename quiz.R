set.seed(31)
library(igraph)

setwd("/Users/tushargupta/Downloads/Project/Quiz")

company_director <- read.csv("data/ft-interlocks.ne.incidence-matrix.csv",
                        header = TRUE, row.names = 1)

company_net  <- igraph::graph.incidence(company_director)

company_net

graph_attr(company_net)

igraph::V(company_net)$shape <- ifelse(igraph::V(company_net)$type == TRUE, "circle", "square")

plot(company_net,
     vertex.label.cex = 0.2,
     vertex.label.font = 1.9,
     vertex.label.color = "blue",
     vertex.size = ifelse(igraph::V(company_net)$type == TRUE, 5, 6))

# # -------------------- Question 2 -----------------------------
company_net <- readRDS('interlock_net_nl.rds')

company_one_net <- igraph::bipartite.projection(company_net)$proj1
plot(company_one_net)


# # -------------------- Question 4 -----------------------------

one_mode_company <- readRDS('interlock_one_mode_nl.rds')

one_mode_company

id <- names(sort(igraph::betweenness(one_mode_company), decreasing = TRUE)[1])

id


# ALTERNATIVELY:
# igraph::betweenness(one_mode_company)[order(igraph::betweenness(one_mode_company), decreasing=TRUE)]

# # -------------------- Question 5 -----------------------------
comp_attr <- read.csv("data/ft-interlocks.ne.attributes.csv",
                        header = TRUE, row.names = 1)
comp_attr
# 
# comp_attr1 <- read.csv("data/ft-interlocks.ne.attributes.csv",
#                       header = TRUE)
# comp_attr1
# # -------------------- Question 6 -----------------------------
one_mode_company <- readRDS('interlock_one_mode_nl.rds')
comp_attr <- read.csv("data/ft-interlocks.ne.attributes.csv",
                        header = TRUE, row.names = 1)
match(V(one_mode_company)$name, comp_attr$vertex.id)
V(one_mode_company)$employeesnum <- comp_attr$employeesnum[id]
V(one_mode_company)$industry <- comp_attr$industry[id]
V(one_mode_company)$sector <- comp_attr$sector[id]
plot(one_mode_company)
