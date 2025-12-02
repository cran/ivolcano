## ----results='hide', echo=FALSE, message=FALSE--------------------------------
library(yulab.utils)


## -----------------------------------------------------------------------------
#| echo: false
#| include: false
library(org.Hs.eg.db)


## -----------------------------------------------------------------------------
#| eval: false
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


## -----------------------------------------------------------------------------
#| echo: false
f <- system.file('extdata/airway.rds', package='ivolcano')
df <- readRDS(f)
head(df)


## -----------------------------------------------------------------------------
#| label: ivocano
#| fig-width: 10
#| fig-height: 6

library(ivolcano)

p <- ivolcano(df,
        logFC_col = "log2FoldChange",
        pval_col = "padj",
        gene_col = "symbol",
        top_n = 5,
        onclick_fun=onclick_genecards)

print(p)


## -----------------------------------------------------------------------------
library(org.Hs.eg.db)
df$gene_id <- rownames(df)
df$entrez <- mapIds(org.Hs.eg.db, 
                    keys = df$gene_id,
                    column = "ENTREZID", 
                    keytype = "ENSEMBL", 
                    multiVals = "first")

top_eg <- df$entrez[order(df$padj)][1:50]

library(fanyi)
gs <- gene_summary(top_eg)

# not run, as it required api key
# see also https://github.com/YuLab-SMU/fanyi
#
# gs$summary_cn <- tencent_translate(gs$summary)

head(gs)


## -----------------------------------------------------------------------------
#| label: ivolcano-fanyi
#| fig-width: 10
#| fig-height: 6

onclick_fun <- onclick_fanyi(gs, c("description", "summary"))

p2 <- ivolcano(df,
        logFC_col="log2FoldChange",
        pval_col="padj",
        gene_col="symbol",
        onclick_fun = onclick_fun)

print(p2)


## -----------------------------------------------------------------------------
#| label: ivolcano-point-size
#| fig-width: 10
#| fig-height: 6

p3 <- ivolcano(df,
        logFC_col="log2FoldChange",
        pval_col="padj",
        gene_col="symbol",
        size_by = "negLogP")

print(p3)


## -----------------------------------------------------------------------------
#| label: ivolcano-point-size-manual
#| fig-width: 10
#| fig-height: 6

p4 <- ivolcano(df,
        logFC_col="log2FoldChange",
        pval_col="padj",
        gene_col="symbol",
        pval_cutoff = 0.05,
        pval_cutoff2 = 0.01,
        logFC_cutoff = 1,
        logFC_cutoff2 = 2,
        size_by = "manual")

print(p4)


## -----------------------------------------------------------------------------
#| label: figureya-basic
#| fig-width: 10
#| fig-height: 6

# load example data
f1 <- system.file('extdata/easy_input_limma.rds', package='ivolcano')
df_limma <- readRDS(f1)

p5 <- ivolcano(df_limma, 
        logFC_col = "logFC",
        pval_col = "P.Value",
        pval_cutoff = 0.05,
        pval_cutoff2 = 0.01,
        logFC_cutoff = 1,
        logFC_cutoff2 = 2,
        gene_col = "X")
print(p5)


## -----------------------------------------------------------------------------
#| label: figureya-selected
#| fig-width: 10
#| fig-height: 6

# load selected gene data
f2 <- system.file('extdata/easy_input_selected.rds', package='ivolcano')
selected <- readRDS(f2)
genes <- selected[,1]
genes

# display selected genes
# here X is the column in our volcano plot data frame that contains gene names

p6 <- ivolcano(df_limma, 
        logFC_col = "logFC",
        pval_col = "P.Value",
        pval_cutoff = 0.05,
        pval_cutoff2 = 0.01,
        logFC_cutoff = 1,
        logFC_cutoff2 = 2,
        gene_col = "X",
        filter = 'X %in% genes')
print(p6)


## -----------------------------------------------------------------------------
#| label: figureya-extreme
#| fig-width: 10
#| fig-height: 6

# Mark genes with extreme logFC values
p7 <- ivolcano(df_limma, 
        logFC_col = "logFC",
        pval_col = "P.Value",
        pval_cutoff = 0.05,
        pval_cutoff2 = 0.01,
        logFC_cutoff = 1,
        logFC_cutoff2 = 2,
        gene_col = "X",
        filter = 'logFC > 8')
        
print(p7)

