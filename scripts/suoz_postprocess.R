library(zonator)

# SuoZ --------------------------------------------------------------------
variants <- paste0("^", c(36:43, 45:47))
root.path <- "C:/suoZ/suoajot"
setwd(root.path)

# ESMK --------------------------------------------------------------------
variants <- paste0("^", c(21:27))
root.path <- "C:/Data/ESMK/analyysi"
setwd(root.path)

# Penalize for scientific notation
options(scipen=500)

for (variant in variants) {
  variant.folder <- list.files(pattern=variant)
  output.folder <- file.path(variant.folder, "output")
  if (file.exists(output.folder)) {
    nwout.file <- file.path(getwd(),
                            output.folder, 
                            paste0("result_", variant.folder, 
                                   ".nwout.1.spp_data.txt"))
    dat <- read.ppa.lsm(nwout.file)
    output1 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout1.csv"))
    output2 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout2.csv"))
    output3 <- file.path(dirname(nwout.file), paste0("result_", variant.folder,
                                                     "nwout3.csv"))
    write.table(dat[[1]], file=output1, sep=";", row.names=FALSE)
    write.table(dat[[2]], file=output2, sep=";", row.names=FALSE)
    write.table(dat[[3]], file=output3, sep=";", row.names=FALSE)
  }
}

library(gridExtra)

project.suoz <- new("Zproject", root=root.path)

variant.36 <- getVariant(project.suoz, 1)
variant.39 <- getVariant(project.suoz, 4)
variant.48 <- getVariant(project.suoz, 13)

labels <- c("Peruspiirteet kytk", "Peruspiirteet", "Lisäpiirteet", "Corine",
            "Linnut", "Vaikea suo")

p1 <- plot(variant.36, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="36_abf_extra_w_cond_cmat_birds")
p1  <- p1 + theme(legend.position=c(.25, .2))

p2 <- plot(variant.39, monochrome=FALSE, group=TRUE, labels=labels,
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")
p2  <- p2 + theme(legend.position=c(.25, .2))

p3 <- plot(variant.48, monochrome=FALSE, group=TRUE, labels=labels[2:6],
          statistic="mean", invert.x=TRUE, main="48_abf_extra_w_cond_birds")
p3  <- p3 + theme(legend.position=c(.25, .2))

grid.arrange(p1, p2, p3, nrow=1, ncol=3)

p4 <- plot(variant.39, monochrome=FALSE, labels=labels, features=1:8,
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")

p5 <- plot(variant.39, monochrome=FALSE, group=TRUE, groups=1:2, 
           labels=c("CONN", "LHQ"),
           statistic="mean", invert.x=TRUE, main="39_abf_extra_w_cond_cmat_birds_plu")
p5