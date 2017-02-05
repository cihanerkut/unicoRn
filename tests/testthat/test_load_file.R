samples <- data.frame(
  ID = c(
    "Manual run 10:1",
    "Manual run 9:1",
    "Manual run 6:1",
    "Manual run 4:1"
  ),
  Sample = c("SapA", "0:0.20", "25:0", "25:0.10")
)

chr_data1 <- read_unicorn("micro1.xls",
                          samples)

chr_data2 <- read_unicorn(
  "prime.xls",
  sample_names = "POT",
  hardware_autozero = T,
  single_channel = T,
  combined = F
)

chr_data3 <-
  read_unicorn("micro2.xls", combined = F, single_channel = F)

chr_data4 <- read_res("sample.res", verbose = T)