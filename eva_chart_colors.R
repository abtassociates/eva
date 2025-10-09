
## System Inflow/Outflow --------------------------------------------------

bar_colors <- c(
  "Inflow" = get_brand_color('med_grey'), 
  "Outflow" = get_brand_color('med_grey'),
  "Homeless" = get_brand_color('coral'),
  "Housed" = get_brand_color('dark_blue')
)

mbm_inflow_bar_colors <- c(
  "Active at Start: Homeless" = get_brand_color('coral'),
  "Inflow" = get_brand_color('med_grey')
)

mbm_outflow_bar_colors <- c(
  "Outflow" = get_brand_color('med_grey'),
  "Active at End: Housed" = get_brand_color('dark_blue')
)

mbm_bar_colors <- c(
  mbm_inflow_bar_colors,
  mbm_outflow_bar_colors
)

mbm_single_status_chart_colors <- c(
  "First-Time \nHomeless" = bar_colors[["Inflow"]],
  "Inactive" = bar_colors[["Outflow"]]
)

mbm_pattern_fills <- c(
  "Housed" = '',
  "Homeless" = '',
  "Inflow" = 'grid_longdash_xl',
  "First-Time \nHomeless" = 'grid_longdash_xl',
  "Active at Start: Homeless" = "grid_longdash_xl",
  "Outflow" = 'stripe45_dashed_xl',
  "Inactive" = 'stripe45_dashed_xl',
  "Active at End: Housed" = "stripe45_dashed_xl"
)
## System Status Sankey chart ----------------------------------------------

sankey_bar_colors <- c(
  "Housed" = get_brand_color('dark_blue'), 
  "Homeless" = get_brand_color('coral'),
  "Exited, Non-Permanent" = get_brand_color('light_coral'),
  "Enrolled, Homeless" = get_brand_color('coral'),
  "Inactive" = get_brand_color('med_grey'),
  "Exited, Permanent" = get_brand_color('light_blue'),
  "Enrolled, Housed" = get_brand_color('dark_blue')
)

sankey_border_colors <- c(
  "Exited, Non-Permanent" = get_brand_color('light_coral'),
  "Enrolled, Homeless" = get_brand_color('coral2'),
  "Inactive" = "black",
  "Exited, Permanent" = get_brand_color('light_blue'),
  "Enrolled, Housed" = get_brand_color('dark_blue2')
)
