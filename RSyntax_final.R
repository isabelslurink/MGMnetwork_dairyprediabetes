# R syntax used for the manuscript: Slurink, I. A., Corpeleijn, E., Bakker, S. J., Jongerling, J., Kupper, N., Smeets, T., & Soedamah-Muthu, S. S. (2023).
#Dairy consumption and incident prediabetes: prospective associations and network models in the large population-based Lifelines Study.
#The American journal of clinical nutrition, 118(6), 1077-1090.

set.seed(123)

# Load r packages
library("dplyr")
library("mgm")
library("qgraph")
library("bootnet")
library("igraph")

# Select variables from dataset to be used in the network; numeric or integers variables are possible
# Recode factor variables to integers; variables including 0 must be recoded +1

## Figure 2. Network structure of dairy intake, food groups and energy intake of the study population with complete data.

# Assign variable type
data <- data_final %>% dplyr::select(kcal, vegetables, fruit, bread, legumes, nuts, meatredprocessed, fish, coffee, tea, SSB, milk_hf, milk_lf, yogurt_hf, yogurt_lf, cheese_hf, cheese_lf, cream, icecream) %>% filter(complete.cases(.))

# Assign variable type: g for Gaussian or c for Categorical
varnames <- names(data)
vartype <- unname(unlist(lapply(data, typeof)))
vartype <- ifelse(vartype == "double", "g", "c")
levels = c(rep(1, 19))

# Estimate network
fit_mgm <- mgm(data = data,
               type = vartype,
               levels = levels,
               k = 2,
               lambdaSel = "EBIC",
               lambdaGam = 0.5,
               saveModels = TRUE,
               miter = 10000000,
               scale = TRUE,
               ruleReg = "AND",
               signInfo = FALSE,
               verbatim = TRUE)

# Indicate group level
groups_list <- list(c(1:11), c(12:19))
names(groups_list) <- c("Diet", "Dairy intake")
groups_color <- c("#DA70D6", "#0096FF")
Labels <- c("kcal", "Vegetables", "  Fruit  ", "  Bread  ", " Legumes ", "   Nuts   ", "   Meat   ", "   Fish   ", " Coffee ", "    Tea    ", "   SSBs   ", " HF milk ", " LF milk ", "HF yogurt", "LF yogurt", "HF cheese", "LF cheese", "  Cream  ", "Ice cream")

# Change edge color from red/green to orange/blue
fit_mgm$pairwise$edgecolor <- ifelse(fit_mgm$pairwise$edgecolor == "red", "DarkOrange", fit_mgm$pairwise$edgecolor)
fit_mgm$pairwise$edgecolor <- ifelse(fit_mgm$pairwise$edgecolor == "darkgreen", "blue", fit_mgm$pairwise$edgecolor)

# Making predictions from mixed graphical models to obtain errors
pred_mgm <- predict(fit_mgm,
                    data = data,
                    errorCOn = c("RMSE", "R2"), # Error functions continuous vars
                    errorCat = c("CC", "nCC")) # Error functions categorical vars

# Compute vector containing the nodewise errors to display
errors <- c(pred_mgm$errors[1:18, 3])

# Graph with layout spring = Fruchterman-Reingold algorithm
results <- qgraph(fit_mgm$pairwise$wadj,
                  layout = "spring", repulsion = 1.3,
                  edge.color = fit_mgm$pairwise$edgecolor,
                  nodeNames = varnames,
                  pie = errors,
                  pieColor = c("#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B",     "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B", "#00008B"),
                  color = groups_color,
                  groups = groups_list,
                  labels = Labels,
                  border.width = 1.2,
                  legend = FALSE,
                  vsize = 8, cut = 0, esize = 11)

# Figure 3. Network structure of dairy intake, food groups and energy intake, lifestyle risk factors, socio-demographic characteristics, clinical markers, and prediabetes of the study population with complete data.
data <- data_final %>% dplyr::select(prediabetes, sex, age, education, smoking, pa, alcohol,
                                     BL_ldl, BL_triglycerides, BL_hypertension, BL_waist, kcal, vegetables, fruit, bread, legumes, nuts, meatredprocessed, fish, coffee, tea, SSB, milk_hf, milk_lf, yogurt_hf, yogurt_lf, cheese_hf, cheese_lf, cream, icecream) %>% filter(complete.cases(.))

# Assign variable type: g for Gaussian or c for Categorical
varnames <- names(data)
vartype <- unname(unlist(lapply(data, typeof)))
vartype <- ifelse(vartype == "double", "g", "c")
levels = c(c(2, 2, 1, 3, 3, 1, 1, 1, 1, 2, 1), rep(1, 19))

# Estimate network
fit_mgm <- mgm(data = data,
               type = vartype,
               levels = levels,
               k = 2,
               lambdaSel = "EBIC",
               lambdaGam = 0.5,
               saveModels = TRUE,
               miter = 10000000,
               scale = TRUE,
               ruleReg = "AND",
               signInfo = FALSE,
               verbatim = TRUE)

# Indicate group level
groups_list <- list(c(1), c(2, 3, 4), c(5, 6, 7), c(8:11), c(12:22), c(23:30))
names(groups_list) <- c("Outcome", "Sociodemographics", "Risk factors", "Biomarkers", "Diet", "Dairy intake")
groups_color <- c("#566573", "#229954", "#F4D03F", "#DC7633", "#DA70D6", "#0096FF")
Labels <- c("  PD  ", "  Sex  ", "  Age  ", "Education", "Smoking", "  PA  ", "Alcohol", " LDL-C ", "  TAG  ", "   HT   ", "   WC   ", "  Kcal   ", "Vegetables", "   Fruit  ", "  Bread  ", " Legumes ", "   Nuts   ", "   Meat   ", "   Fish   ", " Coffee ", "    Tea    ", "   SSBs   ", " HF milk ", " LF milk ", "HF yogurt", "LF yogurt", "HF cheese", "LF cheese", "  Cream  ", "Ice cream")

# Change edge color from red/green to orange/blue
fit_mgm$pairwise$edgecolor <- ifelse(fit_mgm$pairwise$edgecolor == "red", "DarkOrange", fit_mgm$pairwise$edgecolor)
fit_mgm$pairwise$edgecolor <- ifelse(fit_mgm$pairwise$edgecolor == "darkgreen", "blue", fit_mgm$pairwise$edgecolor)

# Making predictions from mixed graphical models to obtain errors
pred_mgm <- predict(fit_mgm,
                    data = data,
                    errorCat = c("CC", "nCC", "CCmarg"), #error functions categorical vars
                    errorCon = c("R2")) #error functions continuous vars

# Compute list containing the nodewise errors to display
errors_list <- list() #List for ring-segments
errors_list[[1]] <-  c(pred_mgm$errors[1, 5], (pred_mgm$errors[1, 3] - pred_mgm$errors[1, 5]))
errors_list[[2]] <-  c(pred_mgm$errors[2, 5], (pred_mgm$errors[2, 3] - pred_mgm$errors[2, 5]))
errors_list[[3]] <-  pred_mgm$errors[3, 2]
errors_list[[4]] <-  c(pred_mgm$errors[4, 5], (pred_mgm$errors[4, 3] - pred_mgm$errors[4, 5]))
errors_list[[5]] <-  c(pred_mgm$errors[5, 5], (pred_mgm$errors[5, 3] - pred_mgm$errors[5, 5]))
for(i in 6:9) errors_list[[i]] <-  pred_mgm$errors[i, 2]
errors_list[[10]] <-  c(pred_mgm$errors[10, 5], (pred_mgm$errors[10, 3] - pred_mgm$errors[10, 5]))
for(i in 11:30) errors_list[[i]] <-  pred_mgm$errors[i, 2]

# Compute list with colors
color_list <- list()
color_list[[1]] <- c("#ffa500", "#ff4300")
color_list[[2]] <- c("#ffa500", "#ff4300")
color_list[[3]] <-  "#00008B"
color_list[[4]] <- c("#ffa500", "#ff4300")
color_list[[5]] <- c("#ffa500", "#ff4300")
for(i in 6:9) color_list[[i]] <- "#00008B"
color_list[[10]] <- c("#ffa500", "#ff4300")
for(i in 11:30) color_list[[i]] <- "#00008B"

# Graph with layout spring = Fruchterman-Reingold algorithm
results <- qgraph(fit_mgm$pairwise$wadj,
                  layout = "spring",
                  edge.color = fit_mgm$pairwise$edgecolor,
                  nodeNames = varnames,
                  pie = errors_list,
                  pieColor = color_list, label.cex = .9,
                  color = groups_color,
                  groups = groups_list,
                  labels = Labels,
                  border.width = 1.2,
                  legend = FALSE,
                  vsize = 7, cut = 0, esize = 11)

# Figure 4 & Supplemental Figure 5. Standardized centrality of each variable in the network.

# New dataset named centrality with closeness, betweenness and strength using centrality function
closeness <- centrality(results)[["Closeness"]]
betweenness <- as.numeric(centrality(results)[["Betweenness"]])

centrality <- as.data.frame(cbind(closeness, betweenness))
centrality <- tibble::rownames_to_column(centrality, var = "node")
centrality <- as.data.frame(apply(centrality, 2, function(x) gsub("\\s+", "", x)))
centrality$number <- seq.int(nrow(centrality))
centrality %>% arrange(node)

strength <- as.data.frame(centralityPlot(results, print = FALSE)[["data"]]) %>% dplyr::select(node, value)
strength <- as.data.frame(apply(strength, 2, function(x) gsub("\\s+", "", x)))
strength %>% arrange(node)

centrality <- merge(centrality, strength, all = TRUE)
centrality <- centrality %>% mutate(strength = as.numeric(value),
                                    closeness = as.numeric(closeness),
                                    betweenness = as.numeric(betweenness),
                                    zstrength  = (strength  - mean(strength))/sd(strength),
                                    zcloseness = (closeness - mean(closeness))/sd(closeness),
                                    zbetweenness = (betweenness - mean(betweenness))/sd(betweenness)) %>% arrange(number) %>% dplyr::select(!value)

LabelsS <- c("PD", "Sex", "Age", "Edu", "Smo", "PA", "Alc", "LDL", "TAG", "HT", "WC", "Kcal",
             "Veg", "Fru", "WG", "Leg", "Nut", "Meat", "Fish", "Cof", "Tea", "SSB",
             "HF M", "LF M", "HF Y", "LF Y", "HF C", "LF C", "Cre", "Ice")

# Plot results (the manuscript figures were made in GraphPad)
ggplot() +
  geom_line(data=centrality, aes(x = number, y = zstrength, colour = "Strength", linetype = "Strength"), size=1) +
  geom_line(data=centrality, aes(x = number, y = zbetweenness, colour = "Betweenness", linetype = "Betweenness"), size=1) +
  geom_line(data=centrality, aes(x = number, y = zcloseness, colour = "Closeness", linetype = "Closeness"), size=1) +
  scale_x_continuous(breaks = seq(1,31,1)) +
  theme_classic() +
  xlab("Node") + ylab("standardized Z-scores") +
  scale_color_manual(name = "Centrality Indices", values = c("Strength" = "darkblue", "Betweenness" = "red", "Closeness" = "black")) +
  scale_linetype_manual(name = "Centrality Indices", values = c("Strength" = "solid", "Betweenness" = "dashed", "Closeness" = "dotted")) +
  scale_x_discrete(name = "Variable", limits = LabelsS) +
  geom_vline(xintercept=(breaks = seq(1,31,1)), linetype="solid", colour = "lightgray")

# Supplemental figure 3. Clusters of variables that strongly connect in the network represented by color.

# Run the spinglass algorithm 100 times to get a mean of communities, max and min values
g = as.igraph(results, attributes=TRUE)
matrix_spinglass <- matrix(NA, nrow=1, ncol=100)
for (i in 1:100) {
  set.seed(i)
  spinglass <- spinglass.community(g)
  matrix_spinglass[1,i] <- max(spinglass$membership)
}
mean(as.vector(matrix_spinglass))
max(as.vector(matrix_spinglass))
min(as.vector(matrix_spinglass))
median(as.vector(matrix_spinglass))
table(matrix_spinglass)

# Most common solution is at seed 26, rerun to obtain the most common clustering
set.seed(26)
sgc <- spinglass.community(g)
groups_list <- list(c(12,  15,  17,  18,  22,  23,  24,  27,  28,  29,  30), c(25, 26), c(6, 13, 14, 16,  19), c(2, 4, 5, 7,  20,  21), c(1, 3, 8, 9,  10,  11))

names(groups_list) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5")
groups_color <- c("#DA70D6", "#6B8E23", "#D2691E", "#CC9900", "#9966ff")

results <- qgraph(fit_mgm$pairwise$wadj,
                  layout = "spring",
                  repulsion = 1.3,
                  edge.color = fit_mgm$pairwise$edgecolor,
                  nodeNames = varnames,
                  groups = groups_list,
                  color = groups_color,
                  labels = Labels,
                  border.width = 1.2,
                  legend = FALSE,
                  vsize = 7, cut = 0,
                  esize = 11)

set.seed(123)

# Supplemental figure 6. Plot of bootstrapped sampling variation around the edge weights reflecting stability of the network.

# Fits model to 100 bootstrapped samples
res_obj <- mgm::resample(object = fit_mgm,
                         data = data,
                         nB = 100,
                         quantiles = c(0.05, 0.95))

# Plot of sampling distributions
plotRes(object = res_obj_2, labels = LabelsS,
        quantiles = c(0.05, 0.95),
        cut = c(1:435),
        cex.label = 1.25,
        axis.ticks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75),
        cex.mean = 0.5)

## Supplemental figure 7. Average correlation between centrality indices of the networks within the case-dropped bootstrapped samples and the original sample reflecting the stability of the centrality indices.

data.npn <- huge.npn(data)

## Supplemental figure 8. Network structure of dairy intake, food groups and energy intake, lifestyle risk factors, socio-demographic characteristics, clinical markers and prediabetes of the study population with complete data for variables in the model with a nonparanormal transformation applied to non-normally distributed continuous variables.

# Estimate network using bootnet
fit_est <- estimateNetwork(data, default = "mgm",
                           type = vartype,
                           level = levels,
                           tuning = 0.5,
                           criterion = "EBIC",
                           rule = "AND")

# Centrality stability
boot_1000 <- bootnet(fit_est, nBoots = 1000, type = "case", statistics = c("strength", "closeness", #"betweenness"),
                 caseMin = 0.891, caseMax = 1)
