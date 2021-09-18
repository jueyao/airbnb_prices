library("shiny")

##### INTRO PAGE #####
intro_panel <- tabPanel(
  title = "Introduction",
  titlePanel("Airbnb and Housing Market Data Comparison"),
  h4("A1 - Jueyao Liu, Collin Tran, Chloe Lee, Michael Mok"),
  
  # Introduction of our app 
  p(
  "Airbnb is an online marketplace that connects travelers and renters to private 
  housing accomodations. It caters to users seeking short-term housing for temporary
  trips, similar to a hotel-booking experience. Similar to travel agency websites 
  and apps, users may browse rentals (listed by private landlords and real estate owners,
  rather than traditional hotels and apartment complexes) by location, dates available,
  and guest capacity. All housing features of a rental property (number of rooms available, 
  pet policies, internet connectivity, etc.) are provided and advertised by the property owner, 
  and Airbnb receives a commission whenever a user books a stay at any rental unit.
  Airbnb has been criticized for alledgedly resulting in increased housing prices, as landlords 
  find the short-term rental business model significantly more profitable than traditional 
  long-term rentals, and resultantly choose to keep properties off of the long-term market."
  ), 
  
  p(
  "Analyzing both Airbnb and housing market data can help inform 
  whether Airbnb's popularity in the lodging industry has affected real
  estate market performance in major U.S. cities. Key indicators of housing market performance
  include rent price, location of listing, count of listings, and date of listing. Results
  of this analysis could inform whether legislation should instate regulations on the 
  short-term rental business model, limiting the impact on the long-term housing market for 
  permanent residents."
  ), 

  # hyperlink for airbnb listings
  p(
    "Our", 
    a(
      href = "https://www.kaggle.com/rudymizrahi/airbnb-listings-in-major-us-cities-deloitte-ml", "airbnb listings data"
      ), 
    "was found on Kaggle, posted by Rudy Mizrahi."
    ),

  p(
  "The data set is a list of Airbnb listings in the United States. The data was compiled together
  for a Deloitte machine learning competition. The aim of this competition was to predict the price 
  of Airbnb listings in major U.S. cities. The set features a first_review column which is the date 
  of the first review of the Airbnb listing for that month. The price column is the cost per night in USD."
  ),
  
  # hyperlink for rent data
  p(
    "Our", 
    a(
      href = "https://www.kaggle.com/zillow/rent-index", "median rent prices data"), 
    "was found on Kaggle, posted by Zillow."
    ),

  p(
  "The Zillow Rent Index is the median estimated monthly rental price for a given area regardless of whether 
  they are currently listed for rent. Values under month and year (i.e. January.2011) are represented in USD."
  ),

  # hyperlink for data report 
  p(
    "More information about our report can be found", 
    a(href = "https://info201a-wi20.github.io/project-report-michaeldmok/", strong("HERE."))
  ) 

)

##### PAGE 1 #####

# year slider input for airbnb vs rent data
year_range <- as.numeric(range(mean_median_airbnb_vs_rent$year))
year_input <- sliderInput(inputId = "year_choice", 
                          label = "Year", 
                          min = year_range[1], 
                          max= year_range[2], 
                          value = year_range,
                          sep = "")
# month slider input for airbnb vs rent data
month_range <- range(mean_median_airbnb_vs_rent$month)
month_input <- sliderInput(inputId = "month_choice",
                           label = "Month",
                           min = month_range[1],
                           max = month_range[2],
                           value = month_range)

# input to show airbnb prices
airbnb_price_input <- checkboxInput(
  inputId = "show_airbnb",
  label = "Airbnb Prices",
  value = TRUE
)

# input to show rental prices
rental_price_input <- checkboxInput(
  inputId = "show_rent",
  label = "Housing Rental Prices",
  value = TRUE
)

## Airbnb Price vs Rent PANEL ##
airbnb_vs_rent_panel <- tabPanel(
  title = "Monthly Rent",
  p(
    h2("How does the monthly price of Airbnb listings compare to that of housing rental prices?")
    ),
  
  sidebarLayout(
    sidebarPanel(
      year_input,
      month_input,
      rental_price_input,
      airbnb_price_input
    ),
    mainPanel(
      plotOutput(outputId = "airbnb_rent_price_plot"),
      p(textOutput("airbnb_analysis")),
      p(textOutput("rent_analysis")),
      p(textOutput("multiplier"))
    )
  )
)


##### PAGE 2 #####

# year slider input for airbnb vs location
year_airbnb_location_range <- as.numeric(range(summary_stats_all_years$year))
year_airbnb_location_input <- sliderInput(
  inputId = "airbnb_location_year_choice",
  label = "Year",
  min = year_airbnb_location_range[1],
  max = year_airbnb_location_range[2],
  value = year_airbnb_location_range,
  sep = ""
)

cities_list <- pull(unique(select(summary_stats_all_years, location)))
city_input <- checkboxGroupInput(
  inputId = "city_choice",
  label = "Cities",
  choices = cities_list,
  selected = cities_list
)

## Airbnb prices compared to location panel
airbnb_fluctuate_over_us_panel <- tabPanel(
  title = "Airbnb Price and Location",
  p(
    h2("How do the prices of Airbnb listings fluctuate across the U.S.?"),
  ),
  sidebarLayout(
    sidebarPanel(
      city_input,
      year_airbnb_location_input
    ),
    mainPanel(
      plotOutput(outputId = "airbnb_price_location_plot"),
      p(textOutput("nightly_airbnb"))
    )
  )

)

##### PAGE 3 #####
# group checkboxes input for which cities' data to display
cities <- unique(rent_airbnb_count_df$location)
cities_input <- checkboxGroupInput(inputId = "cities_choices",
                                   label = "Cities",
                                   choices = as.list(cities),
                                   selected = cities)

# radio button for graph w/ trend line or w/o trend line
trendline_input <- radioButtons(inputId = "trendline_choice",
                                label = "Trendline",
                                choices = list("With Trendline" = "Y",
                                               "Without Trendline" = "N"))

# layout
airbnb_listings_affect_rent_panel <- tabPanel(
  title = "Airbnb Listings Count",
  p(
    h2("How has the number of new Airbnb listings affected rent prices in
             major U.S. cities?"),
  ),
  sidebarLayout(
    sidebarPanel(
      cities_input,
      trendline_input
    ),
    mainPanel(
      plotOutput(outputId = "rent_airbnb_count_plot"),
      p(textOutput("num_listings"))
    )
  )
)

##### PAGE 4 #####
years <- unique(median_rent_all_states$year)		
map_year_input <- selectInput(inputId = "map_year_choice",		
                          label = "Year",		
                          choices = as.list(years))		
bins_input <- radioButtons(inputId = "bins_choice",		
                           label = "Bin",		
                           choices = list("Wide" = "W",		
                                          "Default" = "D",		
                                          "Narrow" = "N"),		
                           selected = "D")		
housing_fluctuate_over_us_panel <- tabPanel(		
  title = "Rent Prices in the US",
  p(
    h2("How does traditional rent fluctuate across the U.S.?"),
  ),		
  sidebarLayout(		
    sidebarPanel(		
      map_year_input,		
      bins_input		
    ),		
    mainPanel(		
      plotOutput(outputId = "us_map_rent_cost_plot"),
      p(
        textOutput("rent")
      )
    )		
  )
)

##### UI #####
my_ui <- navbarPage(
  title = "Airbnb Housing Comparison",
  # includeCSS(""), # maybe include styling code from a style.CSS file
  
  intro_panel,
  airbnb_vs_rent_panel,
  airbnb_fluctuate_over_us_panel,
  airbnb_listings_affect_rent_panel,
  housing_fluctuate_over_us_panel
)

# second plot stuff 

