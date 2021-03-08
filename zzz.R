# Load R packages
library(readr)
library(dplyr)
library(stringi)
library(ggplot2)
library(cleanNLP)
library(smodels)
library(forcats)
library(glmnet)
library(jsonlite)
library(tidyr)
library(topicmodels)

bp <- file.path("..", "fwp-life-histories", "csv")

get_interview_full <- function()
{
  interview <- read_csv(file.path("output", "csv", "interview_for_text.csv"))
  interview_full <- read_csv(file.path(bp, "interview.csv")) %>%
    semi_join(interview, by = "id")
  interview_full$doc_names <- interview_full$title
  these <- which(stri_length(interview_full$doc_names) > 25)
  interview_full$doc_names[these] <- sprintf(
    "%s...", stri_sub(interview_full$doc_names[these], 1, 22)
  )

  # get pdf links
  finding_aid <- read_csv(file.path("input", "finding_aid.csv"))
  u1 <- "https://dc.lib.unc.edu/utils/getdownloaditem/collection/03709/id/"
  u2 <- "%d/filename/folder_%04d.pdf/mapsto/pdf/type/singleitem/folder_%04d.pdf"
  url <- sprintf(
    paste0(u1, u2), finding_aid$pdf_id, finding_aid$folder, finding_aid$folder
  )
  index <- match(interview_full$folder, finding_aid$folder)
  interview_full$pdf_url <- url[index]

  return(interview_full)
}

access_tfidf_matrix <- function(REMOVE_DIALECT = FALSE)
{
  # Read in the processed data
  interview <- read_csv(file.path("output", "csv", "interview_for_text.csv"))
  interview_full <- get_interview_full()
  token <- read_csv(file.path("output", "csv", "interview_token.csv"))
  annotation <- read_csv(file.path(
    "output", "csv", "interview_annotation.csv.bz2"
  ))

  # determine dialect terms
  token_filter <- token %>%
    filter(spell == 0 & spell_letters == 0) %>%
    filter(stri_detect(token, regex = "\\w")) %>%
    filter(token == stri_trans_tolower(token))

  df_dialect <- token_filter %>%
    group_by(token) %>%
    summarize(n = n()) %>%
    arrange(desc(n))

  dialect_tokens <- df_dialect$token
  dialect_tokens <- c(
    dialect_tokens,
    "git", "jest", "fer", "reckon", "wuz", "o", "den", "sho", "jist"
  )

  temp <- annotation %>%
    filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
    mutate(lemma = stri_trans_tolower(lemma)) %>%
    filter(stri_length(lemma) >= 3L)

  if (REMOVE_DIALECT)
  {
    temp <- temp %>%
      filter(!(lemma %in% dialect_tokens)) %>%
      filter(!stri_detect(lemma, fixed = "'"))
  }

  # remove geo and name stopwords
  stop_tokens <- sort(unique(unlist(stri_split(stri_trans_tolower(c(
    interview_full$state, interview_full$county, interview_full$city,
    interview$interviewee,
    interview$writer, interview_full$typer
  )), fixed = " "))))
  stop_tokens <- stop_tokens[stop_tokens != "mill"]

  temp <- temp %>%
    filter(!(lemma %in% stop_tokens))

  X <- cnlp_utils_tf(temp, min_df = 0.01, max_df = 0.5)
  X <- X[apply(X, 1, sum) > 0,]

  return(X)
}

create_topic_data <- function(REMOVE_DIALECT = FALSE)
{
  X <- access_tfidf_matrix(REMOVE_DIALECT = REMOVE_DIALECT)
  lda_model <- LDA(x = X, k = 16, control = list(seed = 2811, verbose = 1))

  lda <- list(
    terms = lda_model@terms,
    beta = lda_model@beta,
    gamma = lda_model@gamma,
    top_k = get_terms(lda_model, k=5)
  )

  return(lda)
}

create_cluster_data <- function(REMOVE_DIALECT = FALSE, BALANCE_CLUSTER = FALSE)
{
  tf_raw <- access_tfidf_matrix(REMOVE_DIALECT = REMOVE_DIALECT)
  df_raw <- as.numeric(apply(tf_raw != 0, 2, sum))

  tf <- log(1 + tf_raw)
  idf <- log(nrow(tf_raw) / df_raw)
  tfidf <- t(t(tf) * idf)
  tfidf <- scale(tfidf, center=FALSE)
  A <- tcrossprod(tfidf) / ncol(tfidf)
  diag(A) <- 0
  Dinvroot <- diag(1 / sqrt(apply(A, 1, sum)))

  L <- diag(nrow(A)) - Dinvroot %*% A %*% Dinvroot

  # now, cycle over the second eigenvalues
  max_depth <- 5
  groups <- rep(0, nrow(L))
  for (depth in 1:max_depth) {

    new_groups <- groups
    second_vals <- rep(0, length(groups))

    for (g in unique(groups)) {

      index <- which(groups == g)
      e <- eigen(L[index,index])
      vals <- e$vector[,ncol(e$vector)-1]
      m <- median(vals)
      if (BALANCE_CLUSTER) { m <- median(vals) } else { m <- 0 }

      new_groups[index][vals > m]  <- g * 10 + 1
      new_groups[index][vals <= m] <- g * 10

      second_vals[index] <- vals

      cat(sprintf("group: %014s  depth: %02d\n", g, depth))
    }
    groups <- new_groups
  }

  ntopics <- length(unique(groups))
  docs <- matrix(0, nrow=nrow(L), ncol=ntopics)
  gnames <- sort(unique(groups))
  for (i in 1:length(gnames)) {
    docs[groups == gnames[i], i] <- 1
  }

  words <- apply(tfidf, 2, function(v) tapply(v, groups, sum))
  rownames(words) <- NULL

  top_terms_clust <- apply(words, 1,
                 function(v) {
                   paste(colnames(tf_raw)[order(v, decreasing = TRUE)[1:5]],
                   collapse = ", ")
                   })

  doc_df <- tibble(
      id = rownames(tf_raw), cluster = apply(docs, 1, which.max)
    ) %>%
    left_join(
      read_csv(file.path("output", "csv", "interview_for_text.csv")), by = "id"
    )

  doc_df$cluster_name <- top_terms_clust[doc_df$cluster]
  clust <- list(
    top_terms_clust = top_terms_clust,
    doc_df = doc_df,
    words = words
  )

  return(clust)
}

make_topic_data_json <- function(lda)
{
  terms <- lda$terms
  beta <- lda$beta
  gamma <- lda$gamma
  top_k <- lda$top_k

  # Read in the processed data
  interview <- read_csv(file.path("output", "csv", "interview_for_text.csv"))
  interview_full <- get_interview_full()

  all <- vector("list", nrow(beta))
  top_words <- as.character(apply(top_k, 2, paste, collapse=", "))
  top_words_4 <- as.character(apply(top_k[,1:4], 2, paste, collapse=", "))
  to_long <- which(stri_length(top_words) > 49)
  if (length(to_long))
  {
    top_words[to_long] <- top_words_4[to_long]
  }

  doc_largest <- apply(gamma, 1, which.max)
  corp_topics <- apply(gamma, 2, sum)
  corp_topics <- round(corp_topics / sum(corp_topics) * 100)
  for (i in seq_len(nrow(beta))) {
    all[[i]] <- list(
      num=i,
      name=sprintf("Topic %d", i),
      description=sprintf("Topic %d: %s", i, top_words[i]),
      proportion=corp_topics[i],
      proportion_black=round(mean(
        interview$race[doc_largest == i] == "Black", na.rm = TRUE
      ) * 100),
      proportion_women=round(mean(
        interview$gender[doc_largest == i] == "Female", na.rm = TRUE
      ) * 100),
      proportion_black_writer=round(mean(
        interview$race_writer[doc_largest == i] == "Black", na.rm = TRUE
      ) * 100),
      proportion_women_writer=round(mean(
        interview$gender_writer[doc_largest == i] == "Female", na.rm = TRUE
      ) * 100)
    )
  }

  topics <- vector("list", nrow(beta))
  for (i in seq_len(nrow(beta))) {
    top_docs_ids <- order(gamma[,i],decreasing=TRUE)[1:25]
    doc_perc <- gamma[top_docs_ids,i] * 100
    top_docs <- interview_full$doc_names[top_docs_ids]

    top_word <- order(beta[i,],decreasing=TRUE)[1:25]
    word_wgt <- exp(beta[i,top_word])
    top_word <- terms[top_word]

    topics[[i]] <- list(
      num=i,
      top_docs_ids=top_docs_ids - 1L,
      top_docs=top_docs,
      doc_perc=doc_perc,
      top_word=top_word,
      word_wgt=word_wgt
    )
  }

  docs <- vector("list", nrow(interview_full))

  interview_full$county[is.na(interview_full$county)] <- "(not available)"
  interview_full$city[is.na(interview_full$city)] <- "(not available)"

  for (i in seq_len(nrow(gamma))) {
    top_topics_ids <- order(gamma[i,],decreasing=TRUE)
    top_topics <- sprintf("Topic %d", top_topics_ids)
    topic_weights <- round(gamma[i,top_topics_ids] * 100)

    num_include <- sum(topic_weights > 0)

    docs[[i]] <- list(
      num=i,
      id=interview_full$id[i],
      pdf=interview_full$pdf_url[i],
      title=sprintf("%s (%d)",
                    interview_full$title[i], interview_full$interview_year[i]),
      location=sprintf("%s, %s, %s",
                       interview_full$state[i],
                       interview_full$county[i],
                       interview_full$city[i]),
      interviewee=sprintf("%s (%s, %s)",
                          interview$interviewee[i],
                          interview$race[i],
                          interview$gender[i]),
      writer=sprintf("%s (%s, %s)",
                     interview$writer[i],
                     interview$race_writer[i],
                     interview$gender_writer[i]),
      top_topics_ids=top_topics_ids[seq_len(num_include)] - 1L,
      top_topics=top_topics[seq_len(num_include)],
      topic_weights=topic_weights[seq_len(num_include)]
    )
  }

  json <- list(
    all=all,
    topics=topics,
    docs=docs
  )

  return(json)
}

make_cluster_data_json <- function(clust)
{
  top_terms_clust <- clust$top_terms_clust
  doc_df <- clust$doc_df
  words <- clust$words

  # Fix names that are too long
  to_long <- which(stri_length(top_terms_clust) > 49)
  if (length(to_long))
  {
    t4 <- sapply(stri_split(top_terms_clust, fixed = ", "), function(v) {
      paste(v[1:4], collapse = ", ")
    })
    top_terms_clust[to_long] <- t4[to_long]
    stop(top_terms_clust[to_long])
  }

  # Read in the processed data
  interview <- read_csv(file.path("output", "csv", "interview_for_text.csv"))
  interview_full <- get_interview_full()

  all <- vector("list", length(top_terms_clust))
  for (i in seq_len(length(top_terms_clust))) {
    all[[i]] <- list(
      num=i,
      name=sprintf("Cluster %d", i),
      description=sprintf("Cluster %d: %s", i, top_terms_clust[i]),
      proportion=round(mean(doc_df$cluster == i) * 100),
      proportion_black=round(mean(
        doc_df$race[doc_df$cluster == i] == "Black", na.rm = TRUE
      ) * 100),
      proportion_women=round(mean(
        doc_df$gender[doc_df$cluster == i] == "Female", na.rm = TRUE
      ) * 100),
      proportion_black_writer=round(mean(
        doc_df$race_writer[doc_df$cluster == i] == "Black", na.rm = TRUE
      ) * 100),
      proportion_women_writer=round(mean(
        doc_df$gender_writer[doc_df$cluster == i] == "Female", na.rm = TRUE
      ) * 100)
    )
  }

  topics <- vector("list", length(top_terms_clust))
  for (i in seq_len(length(top_terms_clust))) {
    top_docs_ids <- which(doc_df$cluster == i)
    doc_perc <- rep(100, length(top_docs_ids))
    top_docs <- interview_full$doc_names[top_docs_ids]

    top_word <- order(words[i,],decreasing=TRUE)[1:25]
    word_wgt <- as.numeric(words[i,top_word])
    top_word <- colnames(words)[top_word]

    topics[[i]] <- list(
      num=i,
      top_docs_ids=top_docs_ids - 1L,
      top_docs=top_docs,
      doc_perc=doc_perc,
      doc_race=doc_df$race[top_docs_ids],
      doc_gender=doc_df$gender[top_docs_ids],
      doc_race_writer=doc_df$race_writer[top_docs_ids],
      doc_gender_writer=doc_df$gender_writer[top_docs_ids],
      top_word=top_word,
      word_wgt=word_wgt
    )
  }

  docs <- vector("list", nrow(interview_full))

  for (i in seq_len(nrow(interview_full))) {
    docs[[i]] <- list(
      num=i,
      id=interview_full$id[i],
      pdf=interview_full$pdf_url[i],
      title=sprintf("%s (%d)",
                    interview_full$title[i],
                    interview_full$interview_year[i]),
      location=sprintf("%s, %s, %s",
                       interview_full$state[i],
                       interview_full$county[i],
                       interview_full$city[i]),
      interviewee=sprintf("%s (%s, %s)",
                       interview$interviewee[i],
                       interview$race[i],
                       interview$gender[i]),
      writer=sprintf("%s (%s, %s)",
                     interview$writer[i],
                     interview$race_writer[i],
                     interview$gender_writer[i]),
      top_topics_ids=doc_df$cluster[i] - 1L,
      top_topics=sprintf("Cluster %d", doc_df$cluster[i]),
      topic_weights=100
    )
  }

  json <- list(
    all=all,
    topics=topics,
    docs=docs
  )

  return(json)
}
