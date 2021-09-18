library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")


my_server <- function(input_list, output_list) {

  # create output for airbnb vs housing rent plot
  output_list$airbnb_rent_price_plot <- renderPlot({

    year_input_range <- input_list$year_choice # year sliderInput
    month_input_range <- input_list$month_choice # month sliderInput

    # filter data for given input year and month
    filtered_airbnb_rent_data <- mean_median_airbnb_vs_rent %>%
      filter(year >= year_input_range[1],
             year <= year_input_range[2],
             month >= month_input_range[1],
             month <= month_input_range[2]
             )


    plot1 <- ggplot(data = filtered_airbnb_rent_data) +
      labs(title = "Price of monthly Airbnb in comparison to rentals",
           x = "Date",
           y = "Price ($)",
           fill = "Type") +
      theme(axis.text.x = element_text(angle = 90, size = 4))

      # show airbnb prices
      if(input_list$show_airbnb == TRUE) {
        plot1 <- plot1 + geom_col(mapping = aes_string(x = "date", y = "monthly_average_median_airbnb"), fill = "pink", na.rm = TRUE)
      }

      # show rent prices
      if(input_list$show_rent == TRUE) {
        plot1 <- plot1 + geom_col(mapping = aes_string(x = "date", y = "monthly_average_median_rent"), na.rm = TRUE)
      }

    # dynamic text analysis
    output_list$airbnb_analysis <- renderText({
      paste0("From ", month.abb[input_list$month_choice[1]], " ", as.character(input_list$year_choice[1]),
            " to ", month.abb[input_list$month_choice[2]], " ", as.character(input_list$year_choice[2]),
            " the minimum Airbnb monthly rent was $", round(min(filtered_airbnb_rent_data$monthly_average_median_airbnb), digits = 0), ",",
            " the median monthly rent was $", round(median(filtered_airbnb_rent_data$monthly_average_median_airbnb), digits = 0), ",",
            " and the maximum monthly rent was $", round(max(filtered_airbnb_rent_data$monthly_average_median_airbnb), digits = 0),
            ". Over this time, the Airbnb monthly rent averaged $", round(mean(filtered_airbnb_rent_data$monthly_average_median_airbnb), digits = 0), "."
            )
    })

    output_list$rent_analysis <- renderText({
      paste0("Conversely, for traditional home rental,",
             " the minimum monthly rent was $", round(min(filtered_airbnb_rent_data$monthly_average_median_rent), digits = 0), ",",
             " the median monthly rent was $", round(median(filtered_airbnb_rent_data$monthly_average_median_rent), digits = 0), ",",
             " and the maximum monthly rent was $", round(max(filtered_airbnb_rent_data$monthly_average_median_rent), digits = 0),
             ". Over this time, the monthly rent averaged $", round(mean(filtered_airbnb_rent_data$monthly_average_median_rent), digits = 0), "."
      )
    })

    output_list$multiplier <- renderText({
      paste0("Across these year segments, Airbnb rentals charge significantly more (on average, ",
             round((mean(filtered_airbnb_rent_data$monthly_average_median_airbnb) / mean(filtered_airbnb_rent_data$monthly_average_median_rent)), digits = 2),
             " times as much!), and have much higher month-to-month price volatility, where traditional housing rentals stay consistent.")
    })

    return(plot1)
  })

  # create plot for airbnb cost by location
  output_list$airbnb_price_location_plot <- renderPlot({
    # create year range input
    year_input_range <- input_list$airbnb_location_year_choice
    # list of major cities
    selected_cities <- input_list$city_choice

    # filter data for given year input
    filtered_summ_stats_all_years <- summary_stats_all_years %>%
      filter(
        year >= year_input_range[1],
        year <= year_input_range[2]
      )

    filtered_summ_stats_all_years <- filter(filtered_summ_stats_all_years, is.element(filtered_summ_stats_all_years$location, selected_cities))

    # plot airbnb price vs location
    plot2 <- ggplot(
        data = filtered_summ_stats_all_years,
        mapping = aes(x = year, y = annual_median_airbnb_per_night, color = location)
      ) +
      geom_point() +
      geom_line(mapping = aes(group = location)) +
      scale_color_brewer(palette = "Set3") +
      labs(title = "Price of Airbnb nightly stay in comparison to location", x = "Year", y = "Price ($)", color = "Location")

      # dynamic text analysis
      output_list$nightly_airbnb <- renderText({
        paste0("From ", as.character(input_list$airbnb_location_year_choice[1]), " ",
               " to ", as.character(input_list$airbnb_location_year_choice[2]), " ",
               " the minimum Airbnb nightly rent was $", round(min(filtered_summ_stats_all_years$annual_median_airbnb_per_night), digits = 0), ",",
               " the median nightly rent was $", round(median(filtered_summ_stats_all_years$annual_median_airbnb_per_night), digits = 0), ",",
               " and the maximum nightly rent was $", round(max(filtered_summ_stats_all_years$annual_median_airbnb_per_night), digits = 0),
               ". Over this time, the Airbnb nightly rent averaged $", round(mean(filtered_summ_stats_all_years$annual_median_airbnb_per_night), digits = 0), "."
        )
      })

    return(plot2)
  })

  # create plot for rent vs new airbnb listings plot
  output_list$rent_airbnb_count_plot <- renderPlot({
    # assign inputs
    cities_selected <- input_list$cities_choices
    with_trendline <- input_list$trendline_choice

    # filters data for selected cities
    filtered_data <- rent_airbnb_count_df %>%
      filter(is.element(rent_airbnb_count_df$location, cities_selected))

    # plot graph
    plot3 <- ggplot(data = filtered_data) +
      geom_point(mapping = aes(x = airbnb_monthly_count,
                               y = median_rent,
                               color = location)) +
      scale_color_brewer(palette = "Set3") +
      labs(title = "New Monthly Airbnb Listings versus Median Rent",
           x = "Number of New Airbnb Listings",
           y = "Rent price ($)",
           color = "City")

    # plot with trendline if input indicates to do so
    if (with_trendline == "Y") {
      plot3 <- plot3 +
        geom_smooth(mapping = aes(x = airbnb_monthly_count,
                                  y = median_rent,
                                  group = location,
                                  color = location))
    }

    # dynamic text analysis
    output_list$num_listings <- renderText({
      paste0("For the selected cities, the number of Airbnb listings per month and how much rent
              they charge have a correlation of ", round(cor(filtered_data$median_rent, filtered_data$airbnb_monthly_count), digits = 3), ". ",
              "Additionally, differences between how much Airbnbs rent for across major cities vary greatly, from a minimum night stay of $", round(min(filtered_data$median_rent), digits = 2), ", ",
             "to a maximum night stay of $", round(max(filtered_data$median_rent), digits = 2), "; ", "a range of $", round(max(filtered_data$median_rent) - min(filtered_data$median_rent), digits = 2), "."
             )
    
     
    })

    return(plot3)
  })

output_list$us_map_rent_cost_plot <- renderPlot({	
  year_selected <- input_list$map_year_choice		
  bins_selected <- input_list$bins_choice		
  
  filtered_data <- median_rent_all_states %>%		
    filter(year == year_selected)		
  
  us_rent <- map_data("state") %>%		
    mutate(location = state.abb[match(region, tolower(state.name))]) %>%		
    filter(region != "district of columbia") %>%		
    left_join(filtered_data, by = "location")		
  
  bin_breaks = c(-Inf, 900, 1100, 1300, 1500, 1700, 1900, Inf)		
  bin_labels = c("<$900",		
                 "$900 to $1100",		
                 "$1100 to $1300",		
                 "$1300 to $1500",		
                 "$1500 to $1700",		
                 "$1700 to $1900",		
                 ">$1900")		
  
  if (bins_selected == "W") {		
    bin_breaks = c(-Inf, 700, 1000, 1300, 1600, 1900, 2100, Inf)		
    bin_labels = c("<$700",		
                   "$700 to $1000",		
                   "$1000 to $1300",		
                   "$1300 to $1600",		
                   "$1600 to $1900",		
                   "$1900 to $2100",		
                   ">$2100")		
  } else if (bins_selected == "N") {		
    bin_breaks = c(-Inf, 1150, 1250, 1350, 1450, 1550, 1650, Inf)		
    bin_labels = c("<$1150",		
                   "$1150 to $1250",		
                   "$1250 to $1350",		
                   "$1350 to $1450",		
                   "$1450 to $1550",		
                   "$1550 to $1650",		
                   ">$1650")		
  }		
  
  rent_change_bins <- cut(us_rent$avg_median_rent,		
                          breaks = bin_breaks,		
                          labels = bin_labels)		
  
  us_rent <- us_rent %>%		
    mutate(change_labels = rent_change_bins)		
  
  
  plot4 <- ggplot(data = us_rent) +		
    coord_quickmap() +		
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = change_labels)) +		
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +		
    theme_void() +		
    ggtitle("Rent Prices Across the US") +		
    labs(fill = "Average Rent Cost")
  
  output_list$rent <- renderText({
    paste0("Across the United States for the selected year, average rent prices were around $", 
           round(mean(us_rent$avg_median_rent), digits = 2), ". ",
           "However, in some more rural parts of the country, rent prices sunk as low as $", 
           round(min(us_rent$avg_median_rent), digits = 2), ", ",
          "and in more metropolitan areas, soared as high as $", round(max(us_rent$avg_median_rent), digits = 2), "; ", "a range of $", 
          round(max(us_rent$avg_median_rent) - min(us_rent$avg_median_rent), digits = 2), "."
    )
  })
  
  return(plot4)		
  })		
}

