source("zzz.R")

# LDA Topic Model, With Dialect
lda <- read_rds(file.path("output", "models", "lda_with_dialect.rds"))
json <- make_topic_data_json(lda)
write_json(json, file.path("output", "json", "topic_dialect.json"))

# LDA Topic Model, With Dialect
lda <- read_rds(file.path("output", "models", "lda_no_dialect.rds"))
json <- make_topic_data_json(lda)
write_json(json, file.path("output", "json", "topic_nodialect.json"))

# Cluster Model, With Dialect
clust <- read_rds(file.path("output", "models", "cluster_with_dialect.rds"))
json <- make_cluster_data_json(clust)
write_json(json, file.path("output", "json", "cluster_dialect.json"))

# Cluster Model, No Dialect
clust <- read_rds(file.path("output", "models", "cluster_no_dialect.rds"))
json <- make_cluster_data_json(clust)
write_json(json, file.path("output", "json", "cluster_nodialect.json"))
