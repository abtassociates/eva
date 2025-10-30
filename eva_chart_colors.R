
## System Inflow/Outflow --------------------------------------------------

bar_colors <- c(
  "Inflow" = get_brand_color('med_grey'), 
  "Outflow" = get_brand_color('med_grey'),
  "First-Time \nHomeless" = get_brand_color('med_grey'), 
  "Returned from \nPermanent" = get_brand_color('med_grey'),
  "Re-engaged from \nNon-Permanent" = get_brand_color('med_grey'),
  "Exited, \nNon-Permanent" = get_brand_color('light_coral'),
  "Exited, Non-Permanent" = get_brand_color('light_coral'),
  "Active at Start: Homeless" = get_brand_color("coral"),
  "Exited, \nPermanent" = get_brand_color('light_blue'),
  "Exited, Permanent" = get_brand_color('light_blue'),
  "Active at End: Housed" = get_brand_color("dark_blue"),
  "Enrolled, Housed" = get_brand_color('dark_blue2'),
  "Homeless" = get_brand_color('coral'),
  "Enrolled, Homeless" = get_brand_color('coral'),
  "Housed" = get_brand_color('dark_blue'),
  "Inactive" = get_brand_color('med_grey')
)

mbm_bar_colors <- bar_colors[c("Active at Start: Homeless", "Inflow", "Outflow", "Active at End: Housed")]

mbm_single_status_chart_colors <- bar_colors[c("First-Time \nHomeless","Inactive")]


## fill patterns from fillpattern package
mbm_pattern_fills <- c(
  "Housed" = '',
  "Homeless" = '',
  "Inflow" = 'grid_solid',
  "First-Time \nHomeless" = "grid_solid",
  "Active at Start: Homeless" = "grid_solid",
  "Returned from \nPermanent" = "grid_solid",
  "Re-engaged from \nNon-Permanent" = "grid_solid",
  "Exited, \nNon-Permanent" = "stripe120_longdash",
  "Exited, \nPermanent" = "stripe120_longdash",
  "Outflow" = 'stripe120_longdash',
  "Inactive" = 'stripe120_longdash',
  "Active at End: Housed" = 'stripe120_longdash'
)


## System Status Sankey chart ----------------------------------------------

sankey_bar_colors <- bar_colors[c("Housed","Homeless","Exited, Non-Permanent","Enrolled, Homeless","Inactive","Exited, Permanent",
                                  "Enrolled, Housed")]


sankey_border_colors <- c(
  "Exited, Non-Permanent" = get_brand_color('light_coral'),
  "Enrolled, Homeless" = get_brand_color('coral2'),
  "Inactive" = "black",
  "Exited, Permanent" = get_brand_color('light_blue'),
  "Enrolled, Housed" = get_brand_color('dark_blue2')
)

sankey_pattern <- 'stripe120_longdash'

## adapted version of fillpattern::scale_fill_pattern that 
## allows us to pass through the ... additional arguments to 
## ggplot2, which is useful for setting legend breaks and labels

scale_fill_pattern_eva <- function (
    patterns = seq_len, 
    fg       = NA, 
    bg       = ifelse(is.na(fg), "transparent", NA), 
    fade     = ifelse(is.na(fg), 1, 0.6), 
    alpha    = 1, 
    angle    = 0, 
    width    = unit(1/10, 'npc'), 
    height   = NA, 
    lwd      = 1, 
    lty      = "solid",
    fun      = NULL,
    min_size = 2,... ) {
  
  
  #________________________________________________________
  # Fetch the i-th value from val, subsetting/recycling.
  #________________________________________________________
  get_i <- function (val, i) {
    if (length(val) <= 1) return (val)
    return (val[[(i - 1) %% length(val) + 1]])
  }
  
  
  fill_palette <- function (n) {
    
    #________________________________________________________
    # Call any functions passed to parameters
    #________________________________________________________
    for (i in formalArgs(scale_fill_pattern))
      if (is.function(v <- get(i)))
        if (!identical(formalArgs(v), c('env', 'row')))
          assign(i, v(n))
    
    
    #________________________________________________________
    # Sets of fill_pattern() arguments for pattern_alpha.
    #________________________________________________________
    fills <- lapply(seq_len(n), function (i) {
      structure(
        class = c('GridFillPattern', 'GridPattern'),
        .Data = list(
          patterns = get_i(patterns, i), 
          fg       = get_i(fg,       i), 
          bg       = get_i(bg,       i), 
          fade     = get_i(fade,     i), 
          alpha    = get_i(alpha,    i), 
          angle    = get_i(angle,    i), 
          width    = get_i(width,    i), 
          height   = get_i(height,   i), 
          lwd      = get_i(lwd,      i), 
          lty      = get_i(lty,      i), 
          fun      = get_i(fun,      i),
          min_size = get_i(min_size, i) ))
    })
    
    
    #________________________________________________________
    # Preserve any names set by the user on `patterns`.
    #________________________________________________________
    if (!is.null(names(patterns)))
      names(fills) <- names(patterns)
    
    return (fills)
  }
  
  
  ggplot2::discrete_scale(aesthetics = "fill",  palette = fill_palette, ...)
}
