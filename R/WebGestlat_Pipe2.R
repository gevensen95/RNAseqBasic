#' WebGestalt Enrichment
#'
#' This function calculates enrichments results using the DEG dataframe
#'
#' @param mode "ORA" or "GSEA". Note: GSEA mode runs GSEA on the log2FC values.
#' @param results A makeComp() output data.frame. Any data frame with Gene name, pAdj, and log2FC can also work. Gene names must be in column 7, and log2foldchange must be in column 2. Columns must be named: "Gene", "padj", and "log2FoldChange".
#' @param alpha pAdj cut-off for DEGs (ORA only)
#' @param FC absolute log2FC cut-off for DEGs (ORA only)
#' @param DB WebGestalt enrichDatabase to query against, see WebGestaltR::listGeneSet() for a full list.
#' @param species WebGestalt organism, see WebGestaltR::listOrganism() for a full list.
#' @param projectName Name of raw data folder.
#' @param geneID Column for genes to use for enrichment
#' @param outputTerms The threshold for the top method. The default is 100.
#' @return A dataframe of Ensembl IDs, gene symbols, and any additional
#' attributes (if chosen)
#' @export
WebGestalt_Pipe2 <- function(mode="ORA", results, alpha=0.05, FC=0.5, DB=NULL,
                            species="mmusculus", projectName=NULL,
                            geneID='Gene', outputTerms=100){

  if(is.null(DB)){
    DB <- ifelse(mode == "ORA", "geneontology_Biological_Process_noRedundant", "pathway_KEGG")
  }

  if(mode == "ORA"){

    if(is.null(projectName)){
      ORA_down <- WebGestaltR::WebGestaltR(enrichMethod="ORA", organism=species, enrichDatabase = DB,
                                           enrichDatabaseType = "genesymbol", isOutput=F,
                                           interestGene=results[[geneID]][results$padj <= alpha & results$log2FoldChange < -1*FC],
                                           interestGeneType="genesymbol", sigMethod="top", referenceGene = results[[geneID]],
                                           referenceGeneType = "genesymbol", topThr = outputTerms,
                                           hostName = "https://www.webgestalt.org")
      ORA_up <- WebGestaltR::WebGestaltR(enrichMethod="ORA", organism=species, enrichDatabase = DB,
                                         enrichDatabaseType = "genesymbol", isOutput=F,
                                         interestGene=results[[geneID]][results$padj <= alpha & results$log2FoldChange > 1*FC],
                                         interestGeneType="genesymbol", sigMethod="top", referenceGene = results[[geneID]],
                                         referenceGeneType = "genesymbol", topThr = outputTerms,
                                         hostName = "https://www.webgestalt.org")
    } else {
      ORA_down <- WebGestaltR::WebGestaltR(enrichMethod="ORA", organism=species, enrichDatabase = DB,
                                           enrichDatabaseType = "genesymbol", projectName = paste0(projectName, "_ORA_downreg"),
                                           interestGene=results[[geneID]][results$padj <= alpha & results$log2FoldChange < -1*FC],
                                           interestGeneType="genesymbol", sigMethod="top", referenceGene = results[[geneID]],
                                           referenceGeneType = "genesymbol", topThr = outputTerms,
                                           hostName = "https://www.webgestalt.org")
      ORA_up <- WebGestaltR::WebGestaltR(enrichMethod="ORA", organism=species, enrichDatabase = DB,
                                         enrichDatabaseType = "genesymbol", projectName = paste0(projectName, "_ORA_upreg"),
                                         interestGene=results[[geneID]][results$padj <= alpha & results$log2FoldChange > 1*FC],
                                         interestGeneType="genesymbol", sigMethod="top", referenceGene = results[[geneID]],
                                         referenceGeneType = "genesymbol", topThr = outputTerms,
                                         hostName = "https://www.webgestalt.org")
    }





    if(nrow(ORA_down) >0 & nrow(ORA_up) >0){
      ORA_down$enrichmentRatio <- ORA_down$enrichmentRatio*-1
      ORA <- rbind(ORA_down, ORA_up)
      ORA <- ORA[!abs(ORA$enrichmentRatio) <1,]
      ORA <- ORA[order(abs(ORA$enrichmentRatio), decreasing = T),]
      ORA <- ORA[order(abs(ORA$FDR)),]
      ORA <- ORA[! duplicated(ORA$geneSet),]
      ORA$ID <- paste0(ORA$geneSet, ": ", ORA$description)
      ORA <- ORA[c(ncol(ORA),1:(ncol(ORA)-1))]
    }

    if(nrow(ORA_down) == 0 & nrow(ORA_up) == 0){
      message("No ORA terms are returned!")
    } else {
      return_data <- ORA
      return(return_data)
    }


  } else if( mode == "GSEA" ){
    if(is.null(projectName)){
      GSEA_output <- WebGestaltR::WebGestaltR(enrichMethod="GSEA",  organism=species,  enrichDatabase = DB,
                                              isOutput=F,
                                              interestGene=results[c(7,2)],
                                              interestGeneType="genesymbol",  sigMethod="top",
                                              topThr = outputTerms,  perNum = 1000,   hostName = "https://www.webgestalt.org")
    } else{
      GSEA_output <- WebGestaltR::WebGestaltR(enrichMethod="GSEA",  organism=species,  enrichDatabase = DB,
                                              projectName = paste0("GSEA_",projectName),
                                              interestGene=results[c(7,2)],
                                              interestGeneType="genesymbol",  sigMethod="top",
                                              topThr = outputTerms,  perNum = 1000,   hostName = "https://www.webgestalt.org")
    }


    if(nrow(GSEA_output) == 0){
      message("No ORA terms are returned!")
    } else {
      GSEA_output$ID <- paste0(GSEA_output$geneSet, ": ", GSEA_output$description)
      GSEA_output <- GSEA_output[c(ncol(GSEA_output),1:(ncol(GSEA_output)-1))]
      return_data <- GSEA_output
      return(return_data)
    }

  } else {
    warning("mode not recognized")
  }

}

