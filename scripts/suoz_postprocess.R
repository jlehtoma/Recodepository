library(zonator)

variants <- paste0("^", c(36:43, 45:47))
setwd("C:/suoZ/suoajot")

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
