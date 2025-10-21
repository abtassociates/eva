
## System Inflow/Outflow --------------------------------------------------

bar_colors <- c(
  "Inflow" = get_brand_color('med_grey'), 
  "Outflow" = get_brand_color('med_grey'),
  "First-Time \nHomeless" = get_brand_color('med_grey'), 
  "Returned from \nPermanent" = get_brand_color('med_grey'),
  "Re-engaged from \nNon-Permanent" = get_brand_color('med_grey'),
  "Exited, \nNon-Permanent" = get_brand_color('light_coral'),
  "Exited, \nPermanent" = get_brand_color('light_blue'),
  "Homeless" = get_brand_color('coral'),
  "Housed" = get_brand_color('dark_blue'),
  "Inactive" = get_brand_color('med_grey')
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

## fill patterns from fillpattern package
mbm_pattern_fills <- c(
  "Housed" = '',
  "Homeless" = '',
  "Inflow" = 'grid_solid',
  "First-Time \nHomeless" = "grid_solid",
  "Active at Start: Homeless" = "grid_solid",
  "Outflow" = 'stripe120_longdash',
  "Inactive" = 'stripe120_longdash',
  "Active at End: Housed" = 'stripe120_longdash'
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
