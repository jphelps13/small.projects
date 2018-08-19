#' @import data.table
#' @import magrittr
#' @import stringr
#' @import plotly

#' @export
cleanData <- function(file = "data.csv"){
  dt <- fread(file)
  pattern <- "([a-zA-Z]{1})(\\S+)"
  replacement <- "\\U\\1\\L\\2"
  setnames(dt, gsub(" ", "", 
                    gsub(pattern, replacement, x = names(dt), perl = TRUE)
  ))
  setnames(dt, gsub("\\&", "And", names(dt)))
  dt <- dt[!is.na(SummaryIndex),]
  return(dt)
}

#' @export
preparePlotData <- function(dt, metrics){

  # spline the values & filter to years
  sdt <- dt[, lapply(.SD, FUN = function(i){
    list(splinefun(x = Year, y = i, method = "natural"))
  }), .SDcols =  metrics, keyby = Countries]
  
  ndt <- dt[, .(Year = min(Year):max(Year),
                Real = min(Year):max(Year) %in% Year),
             keyby = Countries]
  
  ndt <- merge(ndt, sdt, by = "Countries")
  
  ndt <- ndt[, lapply(.SD, FUN = function(i){
    max(0, round(i[[1]](Year), 2))
  }), .SDcols = metrics, by = .(Countries, Year)]
  
  ndt <- merge(ndt, dt, all.x = TRUE, by = c("Countries", "Year"))
  
  setnames(ndt, stringr::str_replace_all(names(ndt), "(.*)\\.x", "\\1"))
  setnames(ndt, stringr::str_replace_all(names(ndt), "(.*)\\.y", "\\1_raw"))

  setkeyv(ndt, c("Countries", "Year"))
  
  return(ndt)
}

#' @export
plotlyTimeTrackPlot <- function(pdt, countries, dis_metrics, metrics, years,
                                plot_control){
  
  setkeyv(pdt, "Countries")
  
  pdt[, Opacity := c(seq(0.05, 1, length.out = .N))^4,
      by = Countries]
  
  setnames(pdt, metrics, c("x", "y"))
  pdt <- pdt[!is.na(x) & !is.na(y),]
  
  # simple plot
  if(nrow(pdt) > 0){
    xlim <- range(pdt[, "x", with = FALSE][[1]])
    ylim <- range(pdt[, "y", with = FALSE][[1]])
    xlim[1] <- max(0, xlim[1]-0.1)
    xlim[2] <- min(10, xlim[2]+0.1)
    ylim[1] <- max(0, ylim[1]-0.1)
    ylim[2] <- min(10, ylim[2]+0.1)
    
    p <- plot_ly(data = pdt[J(countries[1])],
                 hoverinfo = "text",
                 text = ~sprintf('</br> Year: %s </br> %s: %s </br> %s: %s', 
                                 Year, metrics[1], x,
                                 metrics[2], y)) %>% 
      layout(
        xaxis = list(range = xlim, title = dis_metrics[1],
                     gridcolor = toRGB(plot_control$grid_colour),
                     color = plot_control$font_colour),
        yaxis = list(range = ylim, title = dis_metrics[2],
                     gridcolor = toRGB(plot_control$grid_colour),
                     color = plot_control$font_colour),
        plot_bgcolor = plot_control$bg_colour,
        paper_bgcolor = plot_control$paper_colour,
        legend = list(font = list(
          color = plot_control$font_colour
        )))
    
    for(i in 1:length(countries)){
      cdt <- pdt[J(countries[i])]
      p <- p %>% add_trace(data = cdt,
                           x = ~x, y = ~y, mode = 'lines',
                           name = countries[i],
                           legendgroup = countries[i],
                           color = I(plot_control$palette[i]))
      if(nrow(cdt) > 1){
        p <- p %>% add_trace(data = cdt,
                             x = ~x, y = ~y, mode = 'markers',
                             name = countries[i],
                             size = ~Opacity, 
                             legendgroup = countries[i],
                             showlegend = FALSE,
                             color = I(plot_control$palette[i])) 
      }else{
        p <- p %>% add_trace(data = cdt,
                             x = ~x, y = ~y, mode = 'markers',
                             name = countries[i],
                             legendgroup = countries[i],
                             showlegend = FALSE,
                             color = I(plot_control$palette[i])) 
      }
    }
    
    p
  }else{
    invisible()
  }
}

#' @export
plotlyRadarChart <- function(rdt, all_metrics, display_metrics2, countries,
                             plot_control){
  
  setkey(rdt, Countries)
  
  p <- plot_ly(
    type = 'scatterpolar',
    mode = "line"
  ) 
  for(i in seq_along(countries)){
    input <- unlist(rdt[J(countries[i]), all_metrics, with = FALSE])
    p <- p %>% add_trace(
      r = c(input, input[1]),
      theta = c(display_metrics2, display_metrics2[1]),
      name = countries[i],
      color = I(plot_control$palette[i])
    ) 
  } 
  p <- p %>% layout(polar = list(
    bgcolor = plot_control$bg_colour,
    angularaxis = list(
      tickwidth = 0,
      linewidth = 0
      # layer = 'below traces'
    ),
    radialaxis = list(
      side = 'counterclockwise',
      showline = T,
      linewidth = 0,
      tickwidth = 0,
      gridcolor = plot_control$grid_colour,
      gridwidth = 1,
      range = c(0,10)
    )
  ),
  paper_bgcolor = plot_control$paper_colour,
  # legend = list(font = list(
  #   color = plot_control$font_colour
  # )),
  showlegend = FALSE,
  font = list(color = plot_control$font_colour))
  p
}

#' @export
plotlySlopeGraph <- function(pdt, plot_control, countries,
                             metric = "SummaryIndex",
                             display_metric = "Summary Index"){
  setkeyv(pdt, "Countries")
  
  N_lookup <- pdt[, .N, keyby = Countries]
  
  setnames(pdt, metric, "y")
  
  # simple plot
  if(nrow(pdt) > 0){
    ylim <- range(pdt[, "y", with = FALSE][[1]])
    ylim[1] <- max(0, ylim[1]-0.1)
    ylim[2] <- min(10, ylim[2]+0.1)
    xlim <- range(pdt[, "Year", with = FALSE][[1]])
    xlim[1] <- xlim[1] - 1
    xlim[2] <- xlim[2] + 1
    
    p <- plot_ly(data = pdt,
                 hoverinfo = "text"
                 ,text = ~sprintf('</br> Year: %s </br> %s: %s',
                                  Year, metric, y)
    ) %>%
      layout(
        xaxis = list(title = "",
                     range = xlim,
                     gridcolor = "transparent",
                     color = plot_control$font_colour),
        yaxis = list(range = ylim, title = display_metric,
                     gridcolor = "transparent",
                     color = plot_control$font_colour),
        plot_bgcolor = plot_control$bg_colour,
        paper_bgcolor = plot_control$paper_colour,
        legend = list(font = list(
          color = plot_control$font_colour
        )))
    
    for(i in 1:length(countries)){
      cdt <- pdt[J(countries[i])]
      year_range <- range(cdt$Year)
      mdt <- cdt[Year %in% year_range]
      p <- p %>% add_trace(data = cdt,
                           x = ~Year, y = ~y, mode = 'lines', type = "scatter", 
                           name = countries[i],
                           legendgroup = countries[i],
                           showlegend = FALSE,
                           color = I(plot_control$palette[i]))  %>%
        add_trace(data = mdt,
                  x = ~Year,
                  y = ~y,
                  mode = 'markers', type = "scatter", name = countries[i],
                  legendgroup = countries[i],
                  showlegend = FALSE,
                  color = I(plot_control$palette[i]),
                  marker = list(size = 10))
    }
    
    p
  }else{
    invisible()
  }
}
