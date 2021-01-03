# Function to read file within project ####

read.within.project <- function(file.dir.within.project, ...) {
    read.table(paste(path.to.project, file.dir.within.project, sep = ""), 
               header = FALSE, sep = "\t")
}

read.within.project.h <- function(file.dir.within.project, ...) {
    read.table(paste(path.to.project, file.dir.within.project, sep = ""), 
               header = TRUE, sep = "\t")
}

# Write function ####
my.write.t <- function(data, file.name, ...) {
    write.table(x = data, file = file.name, quote = FALSE, row.names = FALSE, sep = "\t")
}

# Summary statistics function ####
sum.stat <- function(data, strat_by, cont_variables, disc_variables) {
    library(psych)
    
    # strata labels
    strata1 <- levels(data[, strat_by])[1]
    strata2 <- levels(data[, strat_by])[2]
    
    # summary statistics
    d <- describeBy(data[, c(strat_by, cont_variables, disc_variables)], strat_by)
    s <- d[[1]] %>%
        round(3) %>%
        merge.data.frame(round(d[[2]], 3), by = "row.names", 
                         suffixes = c(paste("_", strata1, sep = ""), 
                                      paste("_", strata2, sep = ""))) %>%
        select(1,3,4,14,16,17,27) %>%
        rename(variable = "Row.names") %>%
        filter(variable %in% c(cont_variables, disc_variables))
    
    # t-test p-values
    t.list <- list()
    for (i in 1:length(cont_variables)) {
        t.list[[cont_variables[i]]] <- t.test(as.formula(paste(cont_variables[i], "~", strat_by)), data = data)$p.value 
    }
    t.df <- data.frame(p = unlist(t.list)) %>%
        rownames_to_column("variable")
    
    # chisq test p-values
    c.list <- list()
    for(i in 1:length(disc_variables)) {
        c.list[[disc_variables[i]]] <- chisq.test(data[, strat_by], data[, disc_variables[i]])$p.value 
    }
    c.df <- data.frame(p = unlist(c.list)) %>%
        rownames_to_column(var = "variable") %>%
        rbind.data.frame(t.df)
    
    # combined
    df <- merge.data.frame(s, c.df, by = "variable")
    
    return(df)
    
    detach("package:psych", unload=TRUE)
}

# Plotting parameters ####
my.theme <- theme(legend.position = "bottom",
                  legend.key = element_blank(),
                  axis.line = element_line(colour = "gray60"),
                  text = element_text(size = 9),
                  axis.text.x = element_text(size = 9),
                  panel.grid = element_blank(), 
                  panel.background = element_rect(fill = "#f7f7f7"), 
                  strip.background = element_blank(), 
                  strip.background.y = element_blank(), 
                  strip.placement = "inside", 
                  strip.text = element_text(angle = 0, hjust = 0, size = 9), 
                  strip.text.y = element_text(hjust = 0.5), 
                  plot.title.position = "plot")

viridis.e.20 <- c("#00204DFF", "#002A64FF", "#00336FFF", "#1F3C6DFF", "#35466BFF", "#444F6BFF", "#53596CFF", "#5F636EFF", "#6B6C71FF", "#777776FF", "#838079FF", "#908B79FF", "#9D9677FF", "#ABA074FF", "#B9AC70FF", "#C7B76BFF", "#D7C463FF", "#E5D05AFF", "#F5DD4DFF", "#FFEA46FF")
