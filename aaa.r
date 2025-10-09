library(ChainLadder)

development_acum <- function(
    link_ratios,
    tri_development,
    complete_triangle = TRUE
) {
    if (complete_triangle == TRUE) {
        #Completo el triangulo de link ratios con el development
        na_idx <- which(is.na(link_ratios), arr.ind = TRUE)
        link_ratios[na_idx] <- tri_development[na_idx[, 2]]

        link_ratios_acum <- link_ratios
        link_ratios_col <- ncol(link_ratios_acum)
        for (i in (link_ratios_col - 1):1) {
            link_ratios_acum[, i] <- link_ratios_acum[, i + 1] *
                link_ratios[, i]
        }

        link_ratios_acum2 <- link_ratios
        for (i in 2:(link_ratios_col)) {
            link_ratios_acum2[, i] <- link_ratios_acum2[, i - 1] *
                link_ratios[, i]
        }

        return(list(
            link_ratios = link_ratios,
            link_ratios_acum = link_ratios_acum,
            link_ratios_acum2 = link_ratios_acum2
        ))
    }

    link_ratios_acum <- link_ratios
    link_ratios_col <- ncol(link_ratios_acum)
    for (i in (link_ratios_col - 1):1) {
        link_ratios_acum[
            link_ratios_col - i:link_ratios_col,
            i
        ] <- link_ratios_acum[
            link_ratios_col - i:link_ratios_col,
            i + 1
        ] *
            link_ratios[link_ratios_col - i:link_ratios_col, i]
    }

    link_ratios_acum2 <- link_ratios
    for (i in 2:(link_ratios_col)) {
        link_ratios_acum2[1:(link_ratios_col - i + 1), i] <- link_ratios_acum2[
            1:(link_ratios_col - i + 1),
            i - 1
        ] *
            link_ratios[1:(link_ratios_col - i + 1), i]
    }

    return(list(
        link_ratios = link_ratios,
        link_ratios_acum = link_ratios_acum,
        link_ratios_acum2 = link_ratios_acum2
    ))
}

cl_rgaa <- function(triang, tail) {
    # Calculo linkratios
    linkratios <- c(attr(ata(triang), "vwtd"), tail)

    #Calculo triangulo entero
    n <- ncol(triang)
    triang_full <- cbind(triang, Ult = rep(0, nrow(triang)))

    for (k in 1:n) {
        triang_full[(n - k + 1):n, k + 1] <- triang_full[(n - k + 1):n, k] *
            linkratios[k]
    }

    #Calculo tabla de resumen
    FDA <- rev(cumprod(rev(linkratios)))
    names(FDA) <- colnames(triang)
    diagonal_actual <- getLatestCumulative(triang)
    ultimate <- diagonal_actual * rev(FDA) #
    resumen <- data.frame(diagonal_actual, FDA = rev(FDA), ultimate)
    resumen <- rbind(
        resumen,
        data.frame(
            diagonal_actual = sum(diagonal_actual),
            FDA = NA,
            ultimate = sum(ultimate),
            row.names = "Total"
        )
    )
    list(resumen = resumen, triang_full = triang_full)
}


# Cargar el archivo
file_path <- ".\\sample\\ssn_20192020_desarrollo_siniestros_automotores.csv"
triangle_long <- read.csv(
    file_path,
    sep = ",",
    header = TRUE,
    fileEncoding = "latin1",
    encoding = "latin1"
)

# Armar los datasets
tri_RC <- subset(triangle_long, Ramo %in% "RC")
tri_RC$origin <- as.integer(substr(tri_RC$origin, 1, 4))
tri_RC.incr <- as.triangle(
    tri_RC,
    origin = "origin",
    dev = "development",
    value = "Incurridos"
)
tri_RC.pag <- as.triangle(
    tri_RC,
    origin = "origin",
    dev = "development",
    value = "Pagados"
)
tri_RC.pend <- as.triangle(
    tri_RC,
    origin = "origin",
    dev = "development",
    value = "Pendientes"
)
tri_RC.incr_cum <- incr2cum(tri_RC.incr)
tri_RC.pag_cum <- incr2cum(tri_RC.pag)
tri_RC.pend_cum <- incr2cum(tri_RC.pend)

# Armo graficos para ver la parte que quiero analizar
triangulo_analizar <- as.triangle(tri_RC.incr_cum[1:10, 1:10])
triangulo_analizar[lower.tri(triangulo_analizar)[, c(10:1), drop = FALSE]] <- NA
plot(tri_RC.incr)
plot(tri_RC.incr_cum)


RAA <- tri_RC.incr_cum
tri_RC.incr_cum.dev_prom_smple <- c(attr(ata(RAA), "vwtd")) # --> "vwtd" para E(Xi+1)/E(Xi+1)
tri_RC.incr_cum.dev_prom_pond <- c(attr(ata(RAA), "smpl")) # --> "smpl" para E(xi+1/xi)
tri_RC.incr_cum.link_ratios <- RAA[, 2:ncol(RAA)] / RAA[, 1:(ncol(RAA) - 1)]

dev_acum_list <- development_acum(
    tri_RC.incr_cum.link_ratios,
    tri_RC.incr_cum.dev_prom_pond,
    complete_triangle = FALSE
)
tri_RC.incr_cum.full_link_rt <- dev_acum_list$link_ratios
tri_RC.incr_cum.full_link_rt2 <- dev_acum_list$link_ratios_acum
tri_RC.incr_cum.full_link_rt3 <- dev_acum_list$link_ratios_acum2

plot(as.triangle(as.matrix(tri_RC.incr_cum.full_link_rt)), lattice = FALSE)
plot(
    as.triangle(as.matrix(tri_RC.incr_cum.full_link_rt2)[,
        c(ncol(tri_RC.incr_cum.full_link_rt2):1),
        drop = FALSE
    ]),
    lattice = FALSE
)
plot(as.triangle(as.matrix(tri_RC.incr_cum.full_link_rt3)), lattice = TRUE)


tri_RC.incr_cum.rgaa <- cl_rgaa(triang = tri_RC.incr_cum, tail = 1.05)

mack <- MackChainLadder(tri_RC.incr_cum, est.sigma = "Mack")

plot(mack)
plot(mack, lattice = TRUE)
