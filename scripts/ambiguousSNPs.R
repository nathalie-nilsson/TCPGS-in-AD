# Ambiguous SNPs
# takes a data table with ID, A1, and A2 columns

library(data.table)

ambiguousSNPs <- function(df) {
    ambiguous.snps <- c("A/T", "T/A", "C/G", "G/C")
    bim <- df[, A1.A2 := list(paste(A1, A2, sep = "/"))]
    bim.f <- bim[A1.A2 %in% ambiguous.snps, ID]
    bim.f
}
