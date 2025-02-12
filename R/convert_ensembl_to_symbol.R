#' Convert Ensembl IDs to gene symbols
#'
#' This function is automatically converts human or mouse Ensembl IDs to
#'
#' @param ensembl_ids Vector of Ensembl IDS
#' @param species Species
#' @param attributes Additional attributes to retrieve
#' @return A dataframe of Ensembl IDs, gene symbols, and any additional
#' attributes (if chosen)
#' @export
convert_ensembl_to_symbol <- function(ensembl_ids, species = 'mouse',
                                      attributes = NULL) {
  # Choose the dataset based on species
  dataset <- switch(species,
                    "mouse" = "mmusculus_gene_ensembl",
                    "human" = "hsapiens_gene_ensembl",
                    stop("Species not supported. Use 'mouse' or 'human'."))

  # Connect to the Ensembl database
  ensembl <- biomaRt::useMart("ensembl", dataset = dataset)

  # Choose the appropriate symbol attribute
  species_attr <- if (species == "mouse") "mgi_symbol" else "hgnc_symbol"

  # Retrieve gene symbols
  gene_info <- biomaRt::getBM(
    attributes = c("ensembl_gene_id", species_attr, attributes),
    filters = "ensembl_gene_id",
    values = ensembl_ids,
    mart = ensembl
  )

  # Return the results
  return(gene_info)
}
