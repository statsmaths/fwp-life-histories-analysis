source("zzz.R")

# LDA Topic Model, With Dialect
lda <- create_topic_data(REMOVE_DIALECT = FALSE)
write_rds(lda, file.path("output", "models", "lda_with_dialect.rds"))

# LDA Topic Model, No Dialect
lda <- create_topic_data(REMOVE_DIALECT = TRUE)
write_rds(lda, file.path("output", "models", "lda_no_dialect.rds"))

# Cluster Model, With Dialect
clust <- create_cluster_data(REMOVE_DIALECT = FALSE, BALANCE_CLUSTER = FALSE)
write_rds(clust, file.path("output", "models", "cluster_with_dialect.rds"))

# Cluster Model, No Dialect
clust <- create_cluster_data(REMOVE_DIALECT = TRUE, BALANCE_CLUSTER = FALSE)
write_rds(clust, file.path("output", "models", "cluster_no_dialect.rds"))
