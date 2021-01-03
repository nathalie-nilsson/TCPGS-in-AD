# Functions ####

# Function to find longest common substring
source(paste(path.to.project, "scripts/longest_common_substring.R", sep = ""))

# Bootstrapped regression functions
source(paste(path.to.project, "scripts/boot_reg_functions.R", sep = ""))

# Function to read score data
my.read.f <- function(a, common_path) {
    a.name1 <- gsub(common_path, "", a)
    a.name2 <- gsub(".profile", "", a.name1)
    raw <- read.table(a, header = TRUE) %>%
        select(IID, SCORE, CNT, CNT2) %>%
        `colnames<-`(c('IID',
                       paste("SCORE", a.name2, sep = ""),
                       paste("CNT", a.name2, sep = ""),
                       paste("CNT2", a.name2, sep = "")))
    raw
}


# Regression data ####

# Scoring data
pgrs.files <- list.files(score.files, pattern = ".profile", full.names = TRUE)

# Merged data
common_path <- strsplit(comsub(pgrs.files), split = "[.]") [[1]][1] %>%
    paste(".", sep = "")

for(i in 1:length(pgrs.files)) {
    m.cc <- merge.data.frame(m.cc, my.read.f(pgrs.files[i], common_path))
}

write.table(m.cc, file = paste(output.dir, "/", output.name, "_data.txt", sep = ""),
            row.names = FALSE, quote = FALSE, sep = "\t")


# Range list ####
my.range.list <- read.table(score.range, header = TRUE) %>%
    mutate(SCORE = paste("SCORE", label, sep = ""))


# Regression bootstrap - all subjects ####
res.all <- data.frame(stringsAsFactors = FALSE)
df <- data.frame()

for(i in 1:nrow(my.range.list)) {

    ind.var <- paste("SCORE", my.range.list[i, 1], sep = "")
    res <- boot(m.cc, pgrs.linreg.simple, n.bootstrap)

    ci.1 <- boot.ci(res, index = 1, type = "norm")
    ci.2 <- boot.ci(res, index = 2, type = "norm")

    df <- rbind.data.frame(df, data.frame(SCORE = rep(ind.var, 2),
                     Statistic = c("R2.diff", "P"),
                     Original.value = res$t0,
                     Mean.value = apply(res$t, 2, mean),
                     SD.value = apply(res$t, 2, sd),
                     SE.value = apply(res$t, 2, function(x) sd(x)/sqrt(length(x))),
                     CI.lower = c(ci.1$normal[, 2], ci.2$normal[, 2]),
                     CI.upper = c(ci.1$normal[, 3], ci.2$normal[, 3])))

}

res.all.w <- merge(filter(df, Statistic == "R2.diff"), filter(df, Statistic == "P"),
                   by = "SCORE",
                   suffixes = c(".R2", ".P")) %>%
    select(-contains("Statistic"))


write.table(res.all.w,
            file = paste(output.dir, "/", output.name, "_Results.txt", sep = ""),
            row.names = FALSE, quote = FALSE, sep = "\t")
