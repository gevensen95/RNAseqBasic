% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_gene_names.R
\name{convert_gene_names}
\alias{convert_gene_names}
\title{Convert Ensembl IDs to gene symbols}
\usage{
convert_gene_names(
  genes,
  species = "mouse",
  gene_filters = "ensembl_gene_id",
  extra_attributes = NULL
)
}
\arguments{
\item{genes}{Vector of genes}

\item{species}{Species}

\item{gene_filters}{Filters to query}

\item{attributes}{Additional attributes to retrieve}
}
\value{
A dataframe of Ensembl IDs, gene symbols, and any additional
attributes (if chosen)
}
\description{
This function is automatically converts human or mouse Ensembl IDs to
}
