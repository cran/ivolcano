## ----results='hide', echo=FALSE, message=FALSE--------------------------------
library(yulab.utils)


## ----eval = FALSE-------------------------------------------------------------
# library(airway)
# library(DESeq2)
# library(org.Hs.eg.db)
# library(AnnotationDbi)
# 
# data("airway")
# 
# counts_mat <- assay(airway)
# airway <- airway[rowSums(counts_mat) > 1, ]
# 
# dds <- DESeqDataSet(airway, design = ~ cell + dex)
# dds <- DESeq(dds)
# res <- results(dds, contrast = c("dex", "trt", "untrt"))
# 
# df <- as.data.frame(res)
# df$gene_id <- rownames(df)
# df$symbol <- mapIds(org.Hs.eg.db,
#                     keys = df$gene_id,
#                     column = "SYMBOL",
#                     keytype = "ENSEMBL",
#                     multiVals = "first")
# 
# df <- df[!is.na(df$symbol), ]
# df <- df[, c("log2FoldChange", "padj", "symbol")]
# 
# head(df)


## ----echo=FALSE---------------------------------------------------------------
f <- system.file('extdata/airway.rds', package='ivolcano')
df <- readRDS(f)
head(df)


## ----ivocano, fig.width=10, fig.height=6--------------------------------------
library(ivolcano)

ivolcano(df,
        logFC_col = "log2FoldChange",
        pval_col = "padj",
        gene_col = "symbol",
        top_n = 5,
        onclick_fun=onclick_genecards)

