library(dplyr)
library(forcats)
library(scales)
library(labelled)
library(stringr)
library(ggplot2)

working_directory

### stacked Bar graphs and density/box plots
stacked_plot <- function(df, variable, fill_vars, facet_vars=NULL, facet_wrap=FALSE, title_label = TRUE, 
                         facet_ncol = 4, rotate_axis=FALSE, box_plot=TRUE, legend=TRUE, nrow_legend = 1,
                         y_axis_breaks = 10, expand_yaxis = c(0.01,0.05), title_label_wrap_width = 35,
                         x_axis_label_wrap_width = 10, x_axis_breaks = 10, bar_width = 0.9) {
  out <- lapply(variable, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_vars]]) & !is.na(df[[facet_vars]]),]
      index_fill <- df[[fill_vars]]
      index <- df[[x]]
      label <- if (is.null(labelled::var_label(index))) {x 
        } else {
      labelled::var_label(index)
          }
      
      p <- if (is.factor(index)| is.logical(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index))) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index))
            }
        
        p2 <- if (legend == TRUE) {
          p1 +
          geom_bar(aes(fill = forcats::fct_rev(forcats::fct_infreq(index_fill))), width=bar_width,
                 position="fill", stat="count", show.legend = TRUE) +
          guides(fill=guide_legend(nrow = nrow_legend)) } else {
            p1 +
              geom_bar(aes(fill = forcats::fct_rev(forcats::fct_infreq(index_fill))), width=bar_width,
                       position="fill", stat="count", show.legend = FALSE)
          }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
          
        } else {
          p4 <- if (box_plot == TRUE) {
            p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index)) +
                coord_flip() 
            } else {
              ggplot(data=df, aes(x = index_fill, y = index))
            }
            p6 <- if (legend == TRUE) { 
              p5 +
                geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                             outlier.shape = 1, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend)) } 
            else {
              p5 +
                geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                             outlier.shape = 1, show.legend = FALSE)
            }
            
            } else {
              ggplot(data=df, aes(x = as.numeric(index))) + 
                geom_density(fill = index_fill, alpha=0.4, show.legend = TRUE) +
                guides(fill=guide_legend(nrow = nrow_legend))
              }
          p7 <- p4 +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            scale_y_continuous(n.breaks = y_axis_breaks)
       
          p7
        }
      p_ <- p + 
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol)
      
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_vars]]),]
      index_fill <- df[[fill_vars]]
      index <- df[[x]]
      label <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      p <- if (is.factor(index)| is.logical(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index))) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index))
        }
        
        p2 <- if (legend == TRUE) {
          p1 +
            geom_bar(aes(fill = forcats::fct_rev(forcats::fct_infreq(index_fill))), width=bar_width,
                     position="fill", stat="count", show.legend = TRUE) +
            guides(fill=guide_legend(nrow = nrow_legend)) } else {
              p1 +
                geom_bar(aes(fill = forcats::fct_rev(forcats::fct_infreq(index_fill))), width=bar_width,
                         position="fill", stat="count", show.legend = FALSE)
            }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index)) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                           outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend)) } 
          else {
            p5 +
              geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                           outlier.shape = 1, show.legend = FALSE)
          }
          
        } else {
          ggplot(data=df, aes(x = as.numeric(index))) + 
            geom_density(fill = index_fill, alpha=0.4, show.legend = TRUE) +
            guides(fill=guide_legend(nrow = nrow_legend))
        }
        p7 <- p4 +
          scale_x_continuous(n.breaks = x_axis_breaks) +
          scale_y_continuous(n.breaks = y_axis_breaks)
        p7
      }
     p
    }
    
    plot1 <- if (title_label == TRUE) {
      plot +
        labs(x=NULL,y=NULL, fill = "", colour = "", title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot + 
        labs(x=NULL,y=NULL, fill = "", colour = "", title = "")
    }
    
    plot1
    
  })
  out
}

### Line graphs
line_count_plot <- function(df, x_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, facet_wrap=FALSE,
                            facet_ncol = 4, title_label = TRUE, legend=TRUE, nrow_legend = 1, y_axis_breaks =10,
                            x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL), facet_scales = "free_y",
                            y_axis_limits = c(NULL, NULL), line_width = 0.5, expand_xaxis = c(0.02,0.02),
                            expand_yaxis = c(0.02,0.05), title_label_wrap_width = 35, x_axis_date_label = "%Y-%m",
                            x_axis_label_wrap_width = 20) {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
        } else {
      labelled::var_label(index_colour)
          }
    
      p <- ggplot(data=df, aes(x=index))
      
      p1 <- if (legend == TRUE) {
        p +
          geom_line(aes(colour=index_colour, group=index_colour), linewidth = line_width,
                    stat="count", show.legend = TRUE) +
          guides(colour=guide_legend(nrow = nrow_legend))
      } else { 
        p +
          geom_line(aes(colour=index_colour, group=index_colour), linewidth = line_width,
                    stat="count", show.legend = FALSE)
        }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(2),
                                      to = max(as.Date(index))%m-%months(2),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(2),
                                          to = max(as.Date(index))%m-%months(2),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "character" | class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
        }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index))
      
      p1 <- if (legend == TRUE) {
        p +
          geom_line(aes(colour=index_colour, group=index_colour), linewidth = line_width,
                    stat="count", show.legend = TRUE) +
          guides(colour=guide_legend(nrow = nrow_legend))
      } else { 
        p +
          geom_line(aes(colour=index_colour, group=index_colour), linewidth = line_width,
                    stat="count", show.legend = FALSE)
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(2),
                                      to = max(as.Date(index))%m-%months(2),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(2),
                                          to = max(as.Date(index))%m-%months(2),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "character" | class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 
      
    }
    
    plot1 <- plot +
      scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                          n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    
    plot2 <- if (title_label == TRUE) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = "")
    }
    
    plot2
    
  })
  out
}

line_sum_plot <- function(df, x_vars, y_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, 
                          stat_fun_sum = TRUE, title_label = TRUE, facet_wrap=FALSE, legend=TRUE, nrow_legend = 1,
                          facet_ncol = 4, y_axis_breaks =10, x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL),
                          y_axis_label_percent = FALSE, y_axis_limits = c(NULL, NULL), facet_scales = "free_y",
                          line_width = 0.5, title_label_wrap_width = 35, expand_xaxis = c(0.02,0.02), 
                          x_axis_date_label = "%Y-%m", x_axis_label_wrap_width = 20, expand_yaxis = c(0.02,0.05)) {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
    )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y))
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
        p + 
          geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "sum",
                    linewidth = line_width, show.legend = TRUE) +
            guides(colour=guide_legend(nrow = nrow_legend))
        } else {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "sum",
                      linewidth = line_width, show.legend = FALSE)
          }
      } else {
        if (legend == TRUE) {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "mean",
                      linewidth = line_width, show.legend = TRUE) +
            guides(colour=guide_legend(nrow = nrow_legend))
        } else {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "mean",
                      linewidth = line_width, show.legend = FALSE)
        }
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(2),
                                      to = max(as.Date(index))%m-%months(2),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(2),
                                          to = max(as.Date(index))%m-%months(2),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "character" | class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y))
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "sum",
                      linewidth = line_width, show.legend = TRUE) +
            guides(colour=guide_legend(nrow = nrow_legend))
        } else {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "sum",
                      linewidth = line_width, show.legend = FALSE)
        }
      } else {
        if (legend == TRUE) {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "mean",
                      linewidth = line_width, show.legend = TRUE) +
            guides(colour=guide_legend(nrow = nrow_legend))
        } else {
          p + 
            geom_line(aes(color=index_colour, group=index_colour), stat = "summary", fun = "mean",
                      linewidth = line_width, show.legend = FALSE)
        }
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(2),
                                      to = max(as.Date(index))%m-%months(2),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(2),
                                          to = max(as.Date(index))%m-%months(2),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "character" | class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 
      
    }
    
    plot1 <- if (y_axis_label_percent == TRUE) {
      plot +
        scale_y_continuous( labels = scales::percent, limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    } else {
      plot +
        scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    }
    
    plot2 <- if (title_label == TRUE) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = "")
    }
    
    plot2
  })
  out
}


