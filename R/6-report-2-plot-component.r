# color palette -------

#' @title compute complementary color
#' @description compute complementary color
#' @param color Color color
#' @return complementary color
#' @export
complementary_color <- function(color) {
        rgb(255 - col2rgb(color)[1],
                255 - col2rgb(color)[2],
                255 - col2rgb(color)[3],
                maxColorValue = 255
        )
}

max_val <- 255

## llins ------
pbo <- rgb(144, 164, 157, max = max_val)
llins <- rgb(103, 172, 204, max = max_val)
dual_ai <- rgb(251, 229, 132, max = max_val)
urban_pbo <- rgb(255, 126, 39, max = max_val)
urban_llins <- complementary_color(llins)
urban_dual_ai <- complementary_color(dual_ai)

## irs ------
irs <- "#528552"

## smc ------
smc <- rgb(239, 134, 50, max = max_val)

## pmc ------
pmc <- rgb(255, 228, 126, max = max_val)

## no intervention ----
no_intervention <- rgb(255, 255, 255, max = max_val)

## vaccine ------
vac1p <- rgb(46, 16, 71, max = max_val)
vac1 <- rgb(133, 1, 125, max = max_val)
vac2 <- rgb(255, 0, 129, max = max_val)
vac3 <- rgb(255, 127, 192, max = max_val)
vac4 <- rgb(0, 176, 36, max = max_val)
vac5 <- rgb(118, 217, 133, max = max_val)
no_vac <- rgb(191, 191, 191, max = max_val)

## combination ------
cm_iptp_llins <- rgb(164, 203, 158, max = max_val)
cm_iptp_llins_smc <- rgb(132, 1, 126, max = max_val)
cm_iptp_pbo <- rgb(255, 152, 67, max = max_val)
cm_iptp_pbo_smc <- rgb(0, 162, 199, max = max_val)
cm_iptp_dui_ai <- rgb(255, 228, 126, max = max_val)
cm_iptp_pbo_pmc <- rgb(145, 209, 226, max = max_val)
cm_iptp_pbo_smc <- rgb(0, 162, 199, max = max_val)
cm_iptp_urban_pbo <- rgb(255, 0, 29, max = max_val)
cm_iptp_urban_pbo_pmc <- rgb(255, 153, 155, max = max_val)
cm_iptp_urban_pbo_smc <- rgb(28, 153, 60, max = max_val)

vac_palette_7 <- RColorBrewer::brewer.pal(7, "Paired")
cm_iptp_pbo_pmc_vac <- vac_palette_7[1]
cm_iptp_pbo_pmc_vacpri <- vac_palette_7[3]
cm_iptp_pbo_smc_vac <- vac_palette_7[2]
cm_iptp_pbo_smc_vacpri <- vac_palette_7[4]
cm_iptp_urban_pbo_vac <- vac_palette_7[5]
cm_iptp_urban_pbo_pmc_vac <- vac_palette_7[6]
cm_iptp_urban_pbo_smc_vac <- vac_palette_7[7]

#' @title color palette
#'
#' @description color palette
#' @export
color <- list(
        itn = list(
                pbo = pbo,
                llins = llins,
                dual_ai = dual_ai,
                urban_pbo = urban_pbo,
                urban_llins = urban_llins,
                urban_dual_ai = urban_dual_ai
        ),
        smc = smc,
        pmc = pmc,
        irs = irs,
        vaccine = list(
                vac1p = vac1p,
                vac1 = vac1,
                vac2 = vac2,
                vac3 = vac3,
                vac4 = vac4,
                vac5 = vac5,
                no_vac = no_vac
        ),
        no_intervention = no_intervention,
        strategy = list(
                cm_iptp_llins = cm_iptp_llins,
                cm_iptp_llins_smc = cm_iptp_llins_smc,
                cm_iptp_pbo = cm_iptp_pbo,
                cm_iptp_pbo_smc = cm_iptp_pbo_smc,
                cm_iptp_dui_ai = cm_iptp_dui_ai,
                cm_iptp_pbo_pmc = cm_iptp_pbo_pmc,
                cm_iptp_pbo_smc = cm_iptp_pbo_smc,
                cm_iptp_urban_pbo = cm_iptp_urban_pbo,
                cm_iptp_urban_pbo_pmc = cm_iptp_urban_pbo_pmc,
                cm_iptp_urban_pbo_smc = cm_iptp_urban_pbo_smc,
                cm_iptp_pbo_pmc_vac = cm_iptp_pbo_pmc_vac,
                cm_iptp_pbo_pmc_vacpri = cm_iptp_pbo_pmc_vacpri,
                cm_iptp_pbo_smc_vac = cm_iptp_pbo_smc_vac,
                cm_iptp_pbo_smc_vacpri = cm_iptp_pbo_smc_vacpri,
                cm_iptp_urban_pbo_vac = cm_iptp_urban_pbo_vac,
                cm_iptp_urban_pbo_pmc_vac = cm_iptp_urban_pbo_pmc_vac,
                cm_iptp_urban_pbo_smc_vac = cm_iptp_urban_pbo_smc_vac
        )
)

# theme ------
#' @title theme
#' default theme for plots
#' @export
#' @importFrom cowplot theme_cowplot
theme_snt <- function(font_size = 12, font_family = "sans") {
        theme_cowplot(font_size = font_size, font_family = font_family)
}

# align plots ------
#' @title align plots
#' Align plots in a grid, with the same width and height in the plot area.
#' Use methods from the cowplot package.
#' @param ... plots to align
#' @param ncol number of columns, default is 1
#' @param align how to align, default is "v" (vertical)
#' @param axis axis to align, default is "l" (left)
#' @export
#' @importFrom cowplot plot_grid
#' @return a list of plots
align_plots <- function(...,
                        ncol = 1,
                        align = "v",
                        axis = "l") {
        plots <- plot_grid(...,
                ncol = ncol,
                nrow = nrow,
                align = align,
                axis = axis
        )
        return(plots)
}

# axis ------
#' @title Numbered axis
#' @description reformat axis to be numbered, examples is format from
#' 1e6 to 1,000,000
#' @param axis x or y axis to be reformatted, default is x
#' @importFrom scales label_comma
#' @export
scale_numbered <- function(axis = "x") {
        if (axis == "x") {
                scale_y_continuous(labels = label_comma())
        } else if (axis == "y") {
                scale_y_continuous(labels = label_comma())
        }
}
