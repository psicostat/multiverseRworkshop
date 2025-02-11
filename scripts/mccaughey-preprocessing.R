library(dplyr)
library(tidyr)

# reading from OSF

dat <- read.csv("https://osf.io/download/93kmz/")

# subset of the pre-processing steps --------------------------------------
# here a subset of the pre-processing steps from the original script
# see https://osf.io/q9sjr

# delete non-binary

dat$gender.category[dat$id ==  12377391726] <- NA
dat$gender.category[dat$id ==  12170251464] <- NA
dat$gender.category[dat$id ==  12131230442] <- NA

# keep only relevant columns and remove NA

dat <- dat |>
    select(self.efficacy, asi, frost.com, frost.da, stat.anx.tc,
           stat.anx.i, stat.anx.ah, stat.anx.ws, stat.anx.fst, stat.anx.sc, math.anx,
           faculty, program.type, gender.category) |>
    drop_na()

# combining subscales -----------------------------------------------------

## stat.anx overall

dat$stat.anx.TOT <- with(dat, stat.anx.tc + stat.anx.i + stat.anx.ah + stat.anx.ws + stat.anx.fst + stat.anx.sc) / 6

## stat.anx overall by anxiety and feelings

dat$stat.anx.ANX <- with(dat, stat.anx.tc + stat.anx.i + stat.anx.ah) / 3
dat$stat.anx.FEEL <- with(dat, stat.anx.ws + stat.anx.fst + stat.anx.sc) / 3

# factors

dat$gender.category <- factor(ifelse(dat$gender.category == 1, "m", "f"))
dat$program.type <- factor(ifelse(dat$program.type == 0, "undergraduate", "graduate"))
dat$faculty <- factor(dat$faculty, labels = c("arts", "science", "other"))

# saving ------------------------------------------------------------------

saveRDS(dat, "data/ms_anxiety.rds")