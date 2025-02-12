#' Plots a Volcano Plot
#'
#' This function plots a volcano plot for RNAseq results
#'
#' @param results Dataframe of results
#' @param alpha alpha value for significance
#' @param FCsig Fold change value for significance
#' @param Gene Column for labeling genes
#' @return A dataframe of Ensembl IDs, gene symbols, and any additional
#' attributes (if chosen)
#' @export

eRupt2 <- function(results, alpha = 0.05, FCsig = 1, Gene = 'Gene'){

  #colour significance by p-adj:
  results$significance <- "NS"
  results$significance[results$padj < alpha & results$log2FoldChange > FCsig] <- "Upregulated"
  results$significance[results$padj < alpha & results$log2FoldChange < (FCsig*-1)] <- "Downregulated"
  results <- na.omit(results)
  results$significance <- factor(results$significance, levels=c("Downregulated", "NS", "Upregulated"))

  # changes FDR=0 to very small values based on the rest of the data, so log transformed FDR=0 values are still plotted
  min_val <- min(na.omit(results$padj[results$padj!=0]))
  results$padj[results$padj == 0] <- runif(length(na.omit(results$padj[results$padj == 0])), (min_val*1e-3), (min_val*1e-1))
  results[order(-log10(results$padj), decreasing = T),]->results
  results$label <- F
  results$label[1:10] <- T

  plot <- ggplot2::ggplot(data = results, ggplot2::aes(x = log2FoldChange, y = -log10(padj), color = significance))+
    ggplot2::geom_point(alpha=0.35)+
    Ol_Reliable()+
    ggplot2::scale_y_continuous( limits=c(0, (-log10(min_val)+(-log10(min_val)*0.1))) )+
    ggplot2::theme(legend.title = ggplot2::element_blank())+
    ggrepel::geom_text_repel(ggplot2::aes(label=ifelse(label, Gene, "")), size=2, max.overlaps = Inf)+
    ggplot2::scale_color_manual(values=c("blue","black","red"), drop=F)

  return(plot)

}
