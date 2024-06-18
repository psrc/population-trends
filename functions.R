# Charts ------------------------------------------------------------------

echarts4r::e_common(font_family = "Poppins")

tooltip_js <- "
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

format_opts <- function(e, esttype, dec, title) {
  if(esttype == "number") {
    e <- e |> e_tooltip(trigger = "item") 
    
  } else {
    
    if(esttype == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    e <- e |>
      e_y_axis(name = title, 
               nameLocation = "middle", 
               nameGap = 50,
               nameTextStyle = list(fontSize=14),
               axisLabel=list(margin=10, fontSize=16, fontWeight='bold'),
               formatter = e_axis_formatter(esttype, digits = dec)) |>
      e_x_axis(axisLabel=list(fontSize=20)) |>
      e_tooltip(trigger = "item",
                formatter =  e_tooltip_item_formatter(style = esttype, digits = 0, currency = curr)) |>
      e_tooltip(formatter =  htmlwidgets::JS(tooltip_js))
  }
  return(e)
}

e_basics <- function(e, top_padding, bottom_padding, legend, left_align) {
  e <- e |>
    e_grid(left = left_align, top = top_padding, bottom = bottom_padding) |>
    e_x_axis(axisTick=list(show = FALSE),
             axisLabel=list(fontSize=16, fontWeight = 'bold')) |>
    e_y_axis(axisTick=list(show = FALSE),
             axisLabel=list(fontSize=16, fontWeight = 'bold')) |>
    e_show_loading()
  
  e <- e |> e_legend(show = legend, bottom=0)
  
  return(e)
}

timeline_opts <- function(e, right_toggle, left_toggle) {
  e |>
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = right_toggle,
                               left = left_toggle,
                               currentIndex = 0,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
}

create_bar_chart <- function(df, x, y, fill, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=25, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) #|>
    #e_toolbox_feature("dataView") |>
    #e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}

create_bar_chart_toggle <- function(df, x, y, fill, toggle, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL, right_toggle = 200, left_toggle = 200) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y), all_of(toggle)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    group_by(.data[[toggle]]) |>
    e_charts_(x, timeline = TRUE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Add in the Timeseries Selector
  c <- timeline_opts(c, right_toggle, left_toggle)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}

create_line_chart <- function(df, x, y, fill, esttype="number", dec=0, color, legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_line_(fill_items, smooth = FALSE)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  return(c)
  
}

create_static_treemap_chart <- function(t, area, fill, title="", subtitle="", source="", alt="", est="percent", dec=0, color="psrc_light") {
  
  tot <- t |> select(all_of(area)) |> pull() |> sum()
  t <- t |> mutate(total_share = .data[[area]]/tot)
  
  # Estimate type determines the labels
  if (est=="percent") {
    factor=100
    p=""
    s="%"
    
  } else if (est=="currency") {
    factor=1
    p="$"
    s=""
    
  } else {
    factor=1
    p=""
    s=""
  }
  
  if (est=="percent") {
    c <- ggplot(t,
                aes(area = get(eval(area)),
                    fill = get(eval(fill)), 
                    label = paste(get(eval(fill)), 
                                  paste0(p, prettyNum(round(get(eval(area))*factor,dec), big.mark = ","), s),
                                  sep = "\n"))) +
      geom_treemap(colour = "white", size = 3) +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 16,
                        grow = FALSE,
                        fontface="bold") +
      psrc_style() +
      theme(legend.position = "none") +
      scale_fill_discrete_psrc(color) +
      labs(title=title, caption = source, alt = alt)
    
  } else {
    
    c <- ggplot(t,
                aes(area = get(eval(area)),
                    fill = get(eval(fill)), 
                    label = paste(get(eval(fill)), 
                                  paste0(p, prettyNum(round(get(eval(area))*factor,dec), big.mark = ","), s),
                                  sep = "\n"))) +
      geom_treemap(colour = "white", size = 3) +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 16,
                        grow = FALSE,
                        fontface="bold") +
      psrc_style() +
      theme(legend.position = "none") +
      scale_fill_discrete_psrc(color) +
      labs(title=title, caption = source, alt = alt)
    
  }
  
  c <- c + theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = element_text(family="Poppins", size=14),

    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    plot.caption =  element_text(family="Poppins",
                                 size=10,
                                 face="italic",
                                 color="#4C4C4C"))
  return(c)
}

echart_pie_chart <- function(t, lab, val, color, legend=TRUE) {
  
  p <- t |>
    e_charts_(lab) |>
    e_pie_(val, radius = c("25%", "50%")) |> 
    e_color(color) |>
    e_tooltip() |>
    e_legend(show = legend)
  
  return(p)
  
}
