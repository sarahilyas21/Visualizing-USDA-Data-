# Load required libraries
library(ggplot2)
library(gganimate)
library(dplyr)



TransitionReveal2 <- ggproto(
  "TransitionReveal2", TransitionReveal,
  expand_panel = function (self, data, type, id, match, ease, enter, exit, params, 
                           layer_index) {    
    row_vars <- self$get_row_vars(data)
    if (is.null(row_vars)) 
      return(data)
    data$group <- paste0(row_vars$before, row_vars$after)
    time <- as.numeric(row_vars$along)
    all_frames <- switch(type,
                         point = tweenr:::tween_along(data, ease, params$nframes, 
                                                      !!time, group, c(1, params$nframes),
                                                      FALSE, params$keep_last),
                         path = tweenr:::tween_along(data, ease, params$nframes, 
                                                     !!time, group, c(1, params$nframes),
                                                     TRUE, params$keep_last),
                         polygon = tweenr:::tween_along(data, ease, params$nframes, 
                                                        !!time, group, c(1, params$nframes),
                                                        TRUE, params$keep_last),
                         stop(type, " layers not currently supported by transition_reveal", 
                              call. = FALSE))
    all_frames$group <- paste0(all_frames$group, "<", all_frames$.frame, ">")
    all_frames$.frame <- NULL
    
    # added step to filter out transition rows with duplicated positions
    all_frames <- all_frames %>%
      filter(!(.phase == "transition" &
                 abs(x - lag(x)) <= sqrt(.Machine$double.eps) &
                 abs(y - lag(y)) <= sqrt(.Machine$double.eps)))
    
    all_frames
  }
)

transition_reveal2 <- function (along, range = NULL, keep_last = TRUE) {
  along_quo <- enquo(along)
  gganimate:::require_quo(along_quo, "along")
  ggproto(NULL, TransitionReveal2, # instead of TransitionReveal
          params = list(along_quo = along_quo, range = range, keep_last = keep_last))
}

# Load your blueberry price data (replace this with your actual data)
blueberries_data <- read_excel("Volume_Price.xlsx")

# Select rows 1 to 42 in the "Value" column and divide by 1,000,000
blueberries_data[1:42, "Value"] <- blueberries_data[1:42, "Value"] / 1000000

blueberries_data$Year <- as.factor(blueberries_data$Year)

# Define line colors
line_colors <- c("Price" = "#33d1ae", "Volume" = "#ff9326")

# Create an animated plot for weekly blueberry prices
p <- ggplot(blueberries_data, aes(x = Week)) +
  geom_line(data = filter(blueberries_data, Year == "Price"), aes(y = Value, color = "Price"), show.legend = TRUE) +
  geom_line(data = filter(blueberries_data, Year == "Volume"), aes(y = Value * 4, color = "Volume"), show.legend = TRUE) +
  geom_point(aes(y = ifelse(Year == "Volume", Value * 4, Value), group = seq_along(Week), color = Year), size = 2, alpha = 1) +
  labs(y = 'Price (USD)', color = "") +
  scale_color_manual(values = line_colors) +
  scale_y_continuous(
    name = "Price (USD)",
    sec.axis = sec_axis(~ ./4 , name = "Volume (KG)", labels = scales::number_format( suffix = "M")),
    labels = scales::dollar_format( prefix = "$")
  ) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "grey89"),
    panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(margin = margin(t = 10)),  # Adjust the top margin of the x-axis label
    axis.title.y = element_text(margin = margin(r = 10)),  # Adjust the right margin of the y-axis label
    axis.title.y.right = element_text(margin = margin(l = 10)),
    legend.key = element_blank() 
  )+ labs(
    title = "              Blueberries, Conventional | Prices and Volumes",
    x = "Weeks" )
 

anim_plot <- p + transition_reveal2(Week)

animate(anim_plot, fps = 10, duration = 8, end_pause = 8)
anim_save("Blueberry_Volumes_Prices.gif", last_animation())


















