getwd()
install.packages("dplyr")
library(dplyr)

lifetable <- read.csv("ps1_data_F2023.csv", col.names = c("x", "nNx", "nDx", "nax")) 

lifetable <- 
  lifetable |>
  select(nDx, nNx) |>
  mutate(lifetable,nmx = nDx/nNx)

lifetable <- 
  lifetable |>
  mutate(lifetable,n = c(1,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,NA))

lifetable <- 
  lifetable |>
  select(nmx, n, nax) |>
  mutate(lifetable,nqx = n*nmx/(1+(n-nax)*nmx))

lifetable$nqx[is.na(lifetable$nqx)] <- 1


lifetable <- 
  lifetable |>
  select(nqx) |>
  mutate(lifetable,npx = 1-nqx)

lifetable$cum_npx <- cumprod(lifetable$npx)

lifetable <- 
  lifetable |>
  select(cum_npx) |>
  mutate(lifetable,cum_npx2 = lag(cum_npx))

lifetable$cum_npx2[is.na(lifetable$cum_npx2)] <- 1

lifetable$lx <- 100000 * lifetable$cum_npx2

lifetable <- 
  lifetable |>
  select(nqx, lx) |>
  mutate(lifetable,ndx = nqx * lx)


lifetable <- 
  lifetable |>
  select(ndx, nmx) |>
  mutate(lifetable,nLx = ndx/nmx)


lifetable$nLx_ <- rev(lifetable$nLx)

lifetable$cum_Tx_ <- cumsum(lifetable$nLx_)

lifetable$Tx <- rev(lifetable$cum_Tx_)


lifetable <- 
  lifetable |>
  select(Tx, lx) |>
  mutate(lifetable,ex = Tx/lx)

lifetable$cum_npx <- NULL
lifetable$cum_npx2 <- NULL
lifetable$nLx_ <- NULL
lifetable$cum_Tx_ <- NULL


col_order <- c("x", "n", "nNx",
               "nDx", "nax", "nmx", "nqx", "npx", "lx", "ndx", "nLx",
               "Tx", "ex")
lifetable2 <- lifetable[, col_order]
lifetable2


plot(lifetable$x, lifetable$lx, type = "b", xlab = "age", ylab = "lx")

plot(lifetable$x, lifetable$ndx, type = "b", xlab = "age", ylab = "ndx")

plot(lifetable$x, lifetable$nmx, type = "b", xlab = "age", ylab = "nmx")


install.packages("sos")
library(sos)
findFn("lt.mx")

install.packages("LifeTables")
library(LifeTables)
lifetable3 <- lt.mx(lifetable$nmx, sex = "male", age = c(0,1,seq(5,85,5)), nax=NULL)
lifetable3





