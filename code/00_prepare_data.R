load('data/Glasgow-friendship.RData')
load('data/Glasgow-substances.RData')
load('data/Glasgow-demographic.RData')

#load('data/Glasgow-lifestyle.RData')
#load('data/Glasgow-geographic.RData')
load('data/Glasgow-various.RData')


# 3 - Read Data

friendships_t2 <- as.matrix(friendship.2)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t2[friendships_t2==1] <- 1
friendships_t2[friendships_t2==2] <- 1
friendships_t2[friendships_t2==10] <- 0

friendships_t2 <- replace(friendships_t2, is.na(friendships_t2), 0)

tobacco <- as.matrix(tobacco)

student_smoking_t2 <- c(tobacco[,2])
student_smoking_t2 <- replace(student_smoking_t2, is.na(student_smoking_t2), 0)


friendships_t3 <- as.matrix(friendship.3)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t3[friendships_t3==1] <- 1
friendships_t3[friendships_t3==2] <- 1
friendships_t3[friendships_t3==10] <- 0

friendships_t3 <- replace(friendships_t3, is.na(friendships_t3), 0)


student_smoking_t3 <- c(tobacco[,3])
student_smoking_t3 <- replace(student_smoking_t3, is.na(student_smoking_t3), 0)

friendships_t1 <- as.matrix(friendship.1)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t1[friendships_t1==1] <- 1
friendships_t1[friendships_t1==2] <- 1
friendships_t1[friendships_t1==10] <- 0

friendships_t1 <- replace(friendships_t1, is.na(friendships_t1), 0)


student_smoking_t1 <- c(tobacco[,1])
student_smoking_t1 <- replace(student_smoking_t1, is.na(student_smoking_t1), 0)

## Wave 1

EL1 <-  melt(friendships_t1)

names(EL1)[1] <- "source"
names(EL1)[2] <- "target"

EL1$source <- as.character(EL1$source)
EL1$target <- as.character(EL1$target)
EL1 <- EL1[apply(EL1!=0, 1, all),]
# Now change the shapes of the vertices in the network 
# 4- stands for square, 3- stands for triangle
sex_gender_shapes <- as.numeric(sex.F)
sex_gender_shapes <- recode(sex_gender_shapes, `2` = 'Girl', `1` = 'Boy')

NL1 <- data.frame(source = c(rownames(friendships_t1)), smokes = student_smoking_t1)
rownames(NL1) <- c(1:nrow(friendships_t1))

g1 <- graph_from_data_frame(d=EL1, vertices = NL1, directed=TRUE)

V(g1)$label <- c(rownames(friendships_t2))

V(g1)$indegree   <- degree(g1, mode = "in")
V(g1)$outdegree  <- degree(g1, mode = "out")

V(g1)$size  <- degree(g1, mode = "out")
V(g1)$type <- sex_gender_shapes

family_smoking <- as.numeric(familysmoking[, 1])
parent_smoking <- recode(family_smoking, `2` = 'Smoking', `1` = 'Non-Smoking')

V(g1)$parent_smoking <- parent_smoking
V(g1)$sex_variable <- sex_gender_shapes

E(g1)$type <-  "arrow"
E(g1)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors_t1 <- vcolors_palette[as.character(V(g1)$smokes)]
V(g1)$color <- vcolors_t1

## Wave 2
EL2 <-  melt(friendships_t2)

names(EL2)[1] <- "source"
names(EL2)[2] <- "target"

EL2$source <- as.character(EL2$source)
EL2$target <- as.character(EL2$target)
EL2 <- EL2[apply(EL2!=0, 1, all),]
# Now change the shapes of the vertices in the network 
# 4- stands for square, 3- stands for triangle
sex_gender_shapes <- as.numeric(sex.F)
sex_gender_shapes <- recode(sex_gender_shapes, `2` = 'Girl', `1` = 'Boy')

NL2 <- data.frame(source = c(rownames(friendships_t2)), smokes = student_smoking_t2)
rownames(NL2) <- c(1:nrow(friendships_t2))

g2 <- graph_from_data_frame(d=EL2, vertices = NL2, directed=TRUE)

V(g2)$label <- c(rownames(friendships_t2))

V(g2)$indegree   <- degree(g2, mode = "in")
V(g2)$outdegree  <- degree(g2, mode = "out")

V(g2)$size  <- degree(g2, mode = "out")
V(g2)$type <- sex_gender_shapes

family_smoking <- as.numeric(familysmoking[, 2])
parent_smoking <- recode(family_smoking, `2` = 'Smoking', `1` = 'Non-Smoking')

V(g2)$parent_smoking <- parent_smoking
V(g2)$sex_variable <- sex_gender_shapes

E(g2)$type <-  "arrow"
E(g2)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors_t2 <- vcolors_palette[as.character(V(g2)$smokes)]
V(g2)$color <- vcolors_t2

## Wave 3
## 
EL3 <-  melt(friendships_t3)

names(EL3)[1] <- "source"
names(EL3)[2] <- "target"

EL3$source <- as.character(EL3$source)
EL3$target <- as.character(EL3$target)
EL3 <- EL3[apply(EL3!=0, 1, all),]
# Now change the shapes of the vertices in the network 
# 4- stands for square, 3- stands for triangle
sex_gender_shapes <- as.numeric(sex.F)
sex_gender_shapes <- recode(sex_gender_shapes, `2` = 'Girl', `1` = 'Boy')

NL3 <- data.frame(source = c(rownames(friendships_t3)), smokes = student_smoking_t3)
rownames(NL3) <- c(1:nrow(friendships_t3))

g3 <- graph_from_data_frame(d=EL3, vertices = NL3, directed=TRUE)

V(g3)$label <- c(rownames(friendships_t3))

V(g3)$indegree   <- degree(g3, mode = "in")
V(g3)$outdegree  <- degree(g3, mode = "out")

V(g3)$size  <- degree(g3, mode = "out")
V(g3)$type <- sex_gender_shapes

family_smoking <- as.numeric(familysmoking[, 3])
parent_smoking <- recode(family_smoking, `2` = 'Smoking', `1` = 'Non-Smoking')

V(g3)$parent_smoking <- parent_smoking
V(g3)$sex_variable <- sex_gender_shapes

E(g3)$type <-  "arrow"
E(g3)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors_t3 <- vcolors_palette[as.character(V(g3)$smokes)]
V(g3)$color <- vcolors_t3
