#' textEmbedReduceOne help function
#' @param embeddings (list) Embedding(s) - including, tokens, texts and/or word_types.
#' @param n_dim (numeric) Number of dimensions to reduce to.
#' @param scalar (matrix) Scalars from reference (see reference below for more info). Or set own matrix.
#' @param pca (matrix) PCA weights from reference (see reference below for more info). Or set own matrix.
#' @return Returns embeddings with reduced number (n_dim) of dimensions.
#' @examples
#' 5 + 5
#' @seealso see \code{\link{textEmbed}}
#' @importFrom tibble tibble as_tibble
#' @noRd
textEmbedReduceOne <- function(
    embeddings,
    n_dim,
    scalar = NULL,
    pca = NULL) {
  ### Normalize embedding
  n_embeddings <- nrow(embeddings)
  scalar_1 <- scalar[1, ][rep(1, n_embeddings), ]
  scalar_2 <- scalar[2, ][rep(1, n_embeddings), ]
  norm_emb <- (embeddings - scalar_1) / scalar_2

  ###
  # shape of model: (K, 768)
  # the %*% operator is used for matrix multiplication.
  transformed_user_emb <- as.matrix(norm_emb) %*% t(pca)

  transformed_user_emb <- tibble::as_tibble(transformed_user_emb,
    .name_repair = "minimal"
  )
  if (!is.null(n_dim)) {
    transformed_user_emb <- transformed_user_emb[, 1:n_dim]
  }

  # Sorting output + adding comment
  colnames(transformed_user_emb) <- paste0(colnames(norm_emb),
    "_reduced",
    rep = ""
  )


  comment_update <- paste(
    comment(embeddings),
    "; reduced with textEmbedReduce"
  )
  comment(transformed_user_emb) <- comment_update

  return(transformed_user_emb)
}

#' Check that the embeddings are suitable for the reduction based on its commented info.
#' @param embeddings (list) Embedding(s) - including, tokens, texts and/or word_types.
#' @return Returns error if incorrect embedding/layers are used.
#' @noRd
check_reduce <- function(embeddings) {
  ## Check for correct embedding type
  model_name <- extract_comment(
    comment(embeddings),
    "model"
  )
  layer_name <- extract_comment(
    comment(embeddings),
    "layers"
  )

  # stop if no support
  if (!model_name == "roberta-base" || !layer_name == "11") {
    stop("Embedding reduction is currently only supported for roberta-base layer 11")
  }
}

#' Pre-trained dimension reduction (experimental)
#' @param embeddings (list) Embedding(s) - including, tokens, texts and/or word_types.
#' @param n_dim (numeric) Number of dimensions to reduce to.
#' @param scalar (string or matrix) Name or URL to scalar for standardizing the embeddings. If a URL, the function
#' first examines whether it has been downloaded before. The string should be to a csv file containing a matrix with
#' the pca weights for matrix multiplication. For more information see reference below.
#' @param pca (string or matrix) Name or URL to pca weights. If a URL, the function first examines whether it has been
#' downlaoded before. The string should be to a csv file containing a matrix. For more information see reference below.
#' @return Returns embeddings with reduced number of dimensions.
#' @examples
#' \dontrun{
#' embeddings <- textEmbedReduce(word_embeddings_4$texts)
#' }
#' @seealso \code{\link{textEmbed}}
#' @details
#' To use this method please see and cite:\cr
#' Ganesan, A. V., Matero, M., Ravula, A. R., Vu, H., & Schwartz, H. A. (2021, June).
#' Empirical evaluation of pre-trained transformers for human-level nlp: The role of sample size and dimensionality.
#' In Proceedings of the conference. Association for Computational Linguistics. North American Chapter. Meeting
#' (Vol. 2021, p. 4515).
#' NIH Public Access.\cr\cr
#' See \href{https://adithya8.github.io/blog/paper/2021/04/15/Empirical-Evaluation.html}{Git-Hub Empirical-Evaluation}
#' @importFrom tibble as_tibble is_tibble
#' @importFrom utils read.csv
#' @importFrom dplyr bind_cols
#' @export
textEmbedReduce <- function(
    embeddings,
    n_dim = NULL,
    scalar = "fb20/scalar.csv",
    pca = "fb20/rpca_roberta_768_D_20.csv") {
  ### Get the right cvs library(text)

  if (scalar == "fb20/scalar.csv") {
    scalar <- paste0("https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/",
                     "master/models/fb20/scalar.csv")

   # scalar <- "https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/master/models/fb20/scalar.csv"
  }

  if (pca == "fb20/rpca_roberta_768_D_20.csv") {
    pca <- paste0("https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/",
                  "master/models/fb20/rpca_roberta_768_D_20.csv")

    #    pca <- "https://raw.githubusercontent.com/adithya8/ContextualEmbeddingDR/master/models/fb20/rpca_roberta_768_D_20.csv"

  }

  if (is.character(scalar)) {
    scalar_path <- path_exist_download_files(scalar)

    scalar <- utils::read.csv(
      file = scalar_path,
      header = FALSE
    )
  }
  if (is.character(pca)) {
    pca_path <- path_exist_download_files(pca)

    pca <- utils::read.csv(
      file = pca_path,
      header = FALSE
    )
  }

  ## Reduce tokens data
  if (!is.null(embeddings$tokens)) {
    check_reduce(embeddings$tokens[[1]])

    token_embeddings_inner <- list()
    token_embeddings_all <- list()


    for (i in seq_len(length(embeddings$tokens))) {
      for (j in seq_len(length(embeddings$tokens[[i]]))) {
        embeddings_dim <- embeddings$tokens[[i]][[j]] %>%
          select(starts_with("Dim"))

        embeddings_dim_reduced <- textEmbedReduceOne(
          embeddings_dim,
          n_dim = n_dim,
          scalar = scalar,
          pca = pca
        )
        embeddings_dim_reduced1 <- dplyr::bind_cols(
          embeddings$tokens[[i]][[j]][, 1],
          embeddings_dim_reduced
        )

        token_embeddings_inner[[j]] <- embeddings_dim_reduced1
      }
      token_embeddings_all[[i]] <- token_embeddings_inner
    }
    names(token_embeddings_all) <- names(embeddings$tokens)
    embeddings$tokens <- token_embeddings_all
  }

  ## Reduce text data
  if (!is.null(embeddings$texts)) {
    check_reduce(embeddings$texts[[1]])

    embeddings$texts <- mapply(textEmbedReduceOne,
      embeddings$texts,
      MoreArgs = list(
        n_dim = n_dim,
        scalar = scalar,
        pca = pca
      ),
      SIMPLIFY = FALSE
    )
  }

  ### Reduce word_types data
  if (tibble::is_tibble(embeddings$word_types)) {
    check_reduce(embeddings$word_types)

    embeddings_dim <- embeddings$word_types %>%
      select(starts_with("Dim"))

    embeddings_dim_reduced <- textEmbedReduceOne(
      embeddings_dim,
      n_dim = n_dim,
      scalar = scalar,
      pca = pca
    )
    embeddings_dim_reduced1 <- dplyr::bind_cols(
      embeddings$word_types[, 1:2],
      embeddings_dim_reduced
    )
    embeddings$word_types <- embeddings_dim_reduced1
  }

  return(embeddings)
}
