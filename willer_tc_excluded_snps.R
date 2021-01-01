library(tidyverse)
library(data.table)

willer <- fread("data/willer/raw_data/jointGwasMc_TC.txt")
colnames(willer)[9] <- "p.value"

willer.arr <- willer[, p.n := as.numeric(p.value)
                     ][order(p.n)]

common <- fread("data/willer/v1_2019-11-10/willer_tc_common_snps.txt")

excl <- willer.arr[!rsid %in% common[, SNP],
                   ][, c("chr", "BP") := tstrsplit(SNP_hg19, ":", fixed = TRUE)
                     ][, pos1 := gsub("chr", "", SNP_hg19)
                       ][, pos := as.numeric(gsub(":", ".", pos1))]
excl.sig <- excl[p.n <= 0.05]

source("D:/OneDrive - McGill University/Lab_stuff/Research/Methods/R Programming/Scripts/ggplot2/palettes.R")
ggplot(excl.sig, aes(pos, -log10(p.n), color = chr)) +
    geom_point() +
    scale_color_manual(values = rep(cbPalette[1:7], 4)) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position = "none")

common[, .N]/willer.arr[, .N]
