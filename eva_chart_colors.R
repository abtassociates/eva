
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

## fill patterns from ggpattern package
mbm_patterns <- c(
  "Housed" = NA,
  "Homeless" = NA,
  "Inflow" = 'pch',
  "First-Time \nHomeless" = 'pch',
  "Active at Start: Homeless" = NA,
  "Outflow" = 'regular_polygon',
  "Inactive" = 'regular_polygon',
  "Active at End: Housed" = NA
)

## pch = 3 is a plus sign, convex2 is effectively a vertical line
mbm_pattern_shapes <- c(
  "Housed" = NA,
  "Homeless" = NA,
  "Inflow" = 3,
  "First-Time \nHomeless" = 3,
  "Active at Start: Homeless" = NA,
  "Outflow" = 'convex2',
  "Inactive" = 'convex2',
  "Active at End: Housed" = NA
)

## rotate minus signs 90 degrees to be horizontal
mbm_pattern_angles <- c(
  "Housed" = NA,
  "Homeless" = NA,
  "Inflow" = 0,
  "First-Time \nHomeless" = 0,
  "Active at Start: Homeless" = NA,
  "Outflow" = 90,
  "Inactive" = 90,
  "Active at End: Housed" = NA
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
