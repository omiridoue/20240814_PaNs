result1 <- rbind(
  format(round(vcount(g1)), nsmall = 0),
  format(ecount(g1), digits = 3),
  format(edge_density(g1), digits = 3),
  format(reciprocity(g1), digits = 3),
  format(centr_betw(g1)$centralization, digits = 3),
  format(mean_distance(g1), digits = 3)
)

##------------------------------------------------------------------------------

result2 <- rbind(
  format(round(vcount(g2)), nsmall = 0),
  format(ecount(g2), digits = 3),
  format(edge_density(g2), digits = 3),
  format(reciprocity(g2), digits = 3),
  format(centr_betw(g2)$centralization, digits = 3),
  format(mean_distance(g2), digits = 3)
)


##------------------------------------------------------------------------------
result3 <- rbind(
  format(round(vcount(g3)), nsmall = 0),
  format(ecount(g3), digits = 3),
  format(edge_density(g3), digits = 3),
  format(reciprocity(g3), digits = 3),
  format(centr_betw(g3)$centralization, digits = 3),
  format(mean_distance(g3), digits = 3)
)
result_df <- as.data.frame(cbind(result1, result2, result3))

rownames(result_df) <- c("Nodes", "Edges", "Density", "Reciprocity", "Centrality", "Average Path Length")
colnames(result_df) <- c("Wave1", "Wave2", "Wave3")

styled_table <- result_df %>% 
  htmlTable(.,theme = "scientific", css.table = "width:80%;border: none")

result1_df <- as.data.frame(result1)

rownames(result1_df) <- c("Nodes", "Edges", "Density", "Reciprocity", "Centrality", "Average Path Length")
colnames(result1_df) <- c("Wave1")

styled_table1 <- result1_df %>% 
  htmlTable(.,theme = "scientific", css.table = "width:80%;border: none")