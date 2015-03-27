library(data.table)

DT = data.table(x=1:10, y=11:20, z=rep(1:2, each=5))

DT

DT[, y := cumsum(y), by = z]

DT[x > 2, sum(y), by = z]

DT[x > 2 , y := cumsum(y), by = z]

DT[, if(any(x > 5L)) y[1L] - y[2L] else y[2L], by = z]

DT[, if(any(x > 5)) y[1] - y[2], by = z]

DT[, .(l = list(y)), by = z]

# data.table Syntax                    ## dplyr syntax
# case (a)
DT[, lapply(.SD, sum), by=z]            DF %>% group_by(z) %>% summarise_each(funs(sum))
DT[, (cols) := lapply(.SD, sum), by=z]  DF %>% group_by(z) %>% mutate_each(funs(sum))
# case (b)
DT[, c(lapply(.SD, sum),                DF %>% group_by(z) %>%
       lapply(.SD, mean)), by=z]            summarise_each(funs(sum, mean))
# case (c)
DT[, c(.N, lapply(.SD, sum)), by=z]     DF %>% group_by(z) %>%
                                            summarise_each(funs(n(), mean))

DT[, lapply(.SD, sum), by = z]

cols = c("x_1","y_1")

DT[, (cols) := lapply(.SD, sum), by = z]

DT <- data.table(x=1:10, y=11:20, z=rep(1:2, each=5))

DT[, c("x_sum","y_sum") := lapply(.SD, sum), by = z]

DT[, c(lapply(.SD, sum)), by = z]

DT[, c(lapply(.SD, sum), lapply(.SD, mean)), by = z]

DT[, c(.N, lapply(.SD, sum)), by = z]

