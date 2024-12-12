#read in libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(countrycode)
library(DT)
library(shinya11y)
#set wd

#read in items
items <- read.csv("Item_Groups.csv")
#read in data
load("Food_Balance_Global.RData")

#some data tidying to run prior to running app
#tab 1 tidying
#food item groupings - can't merge until overlap resolved
items.count <- items %>% filter(Item.Group== "Grand Total") #98 types of food
items.count


groups.count <- unique(items$Item.Group)
print(groups.count) #24 groups

#check how many times each food type is duplicated
count_types <- table(items$Item)
print(count_types) #each comes up three times (so twice excl Grand total)

#set up hierachy 
#animal products, vegetal products overarching groups, containing all other categories (can tick one or other or both, checkbox)

#remove animal and vegetal products category from df AND Grand Total
items <- items[!items$Item.Group %in% c("Animal Products", "Vegetal Products", "Grand Total"),]

#merge with data
data <- merge(data, items, by="Item", all.x=TRUE)
#check for missing categories
any(is.na(data$Item.Group)) #true
sum(is.na(data$Item.Group)) #46904 NAs

#doesn't match NAs from total supply
sum(is.na(data$total_supply)) #16351 (missing either domestic supply or population)

#check unique items - 116 in full dataset vs 98 in items dataset.
x <- unique(data$Item)
x 

#check which items don't overlap
z <- as.data.frame(unique(data$Item[!(data$Item %in% items$Item)]))

#19 are categories (i.e. Item.Groups) - put these in Item.Group column of data
data$Item.Group[is.na(data$Item.Group)] <- data$Item[is.na(data$Item.Group)]

data$Item.Group <- ifelse(
  is.na(data$Item.Group), z[match(data$Item, z),], data$Item.Group)

#check elements - cereals,other in items group, can come under vegetal products
j <- as.data.frame(unique(data$Item.Group))
k <- as.data.frame(unique(items$Item.Group))
#domestic supply
data$total_supply <- data$Domestic_supply_quantity/data$Population


#assign remaining items to either vegetal or animal groups
data$veg.animal <- ifelse(
  data$Item.Group %in% c("Alcoholic Beverages", "Cereals - Excluding Beer", 
                         "Cereals, Other", "Fruits - Excluding Wine", "Miscellaneous", 
                         "Stimulants", "Sugar & Sweeteners", "Sugar Crops", "Treenuts", "Vegetable Oils",
                         "Vegetables"), "Vegetal Products", "Animal Products"
)

#tab 2 tidying
#assign countries to continents
data$Continent <- countrycode(data$Area, origin ="country.name", destination="continent")
#check any missing
any(is.na(data$Continent)) #false, proceed

print(unique(data$Area))

#tab 3 tidying
#assign countries to UN groups 
african_states <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                    "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", 
                    "Chad", "Comoros", "Congo", "Democratic Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", 
                    "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                    "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", "Lesotho", 
                    "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
                    "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 
                    "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", 
                    "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
                    "United Republic of Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
asia_pacific_states <- c("Afghanistan", "Australia", "Bahrain", "Bangladesh", 
                         "Bhutan", "Brunei Darussalam", "Cambodia", "China", "China, mainland", "China, Macao SAR",
                         "China, Hong Kong SAR", "China, Taiwan Province of",
                         "Cyprus", "Fiji", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", 
                         "Japan", "Jordan", "Kazakhstan", "Kiribati", "Kuwait", 
                         "Kyrgyzstan", "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", 
                         "Marshall Islands", "Micronesia (Federated States of)", "Mongolia", "Myanmar", 
                         "Nauru", "Nepal", "New Zealand", "Democratic People's Republic of Korea", "Oman", 
                         "Pakistan", "Palau", "Papua New Guinea", "Philippines", 
                         "Qatar", "Samoa", "Saudi Arabia", "Singapore", "Solomon Islands", 
                         "Republic of Korea", "Sri Lanka", "Syrian Arab Republic", "Tajikistan", 
                         "Thailand", "Timor-Leste", "Tonga", "Turkmenistan", 
                         "Tuvalu", "United Arab Emirates", "Uzbekistan", "Vanuatu", 
                         "Viet Nam", "Yemen")
eastern_european_states <- c("Albania", "Armenia", "Azerbaijan", "Belarus", 
                             "Bosnia and Herzegovina", "Bulgaria", "Croatia", 
                             "Czechia", "Estonia", "Georgia", "Hungary", 
                             "Latvia", "Lithuania", "Montenegro", "North Macedonia", 
                             "Poland", "Republic of Moldova", "Romania", "Russian Federation", "Serbia", 
                             "Slovakia", "Slovenia", "Ukraine")
latin_american_caribbean_states <- c("Antigua and Barbuda", "Argentina", "Bahamas", 
                                     "Barbados", "Belize", "Bolivia (Plurinational State of)", "Brazil", 
                                     "Chile", "Colombia", "Costa Rica", "Cuba", 
                                     "Dominica", "Dominican Republic", "Ecuador", 
                                     "El Salvador", "Grenada", "Guatemala", "Guyana", 
                                     "Haiti", "Honduras", "Jamaica", "Mexico", 
                                     "Nicaragua", "Panama", "Paraguay", "Peru", 
                                     "Saint Kitts and Nevis", "Saint Lucia", 
                                     "Saint Vincent and the Grenadines", "Suriname", 
                                     "Trinidad and Tobago", "Uruguay", "Venezuela (Bolivarian Republic of)")
western_european_other_states <- c("Andorra", "Australia", "Austria", "Belgium", 
                                   "Canada", "Denmark", "Finland", "France", 
                                   "Germany", "Greece", "Iceland", "Ireland", 
                                   "Israel", "Italy", "Liechtenstein", "Luxembourg", 
                                   "Malta", "Monaco", "Netherlands (Kingdom of the)", "New Zealand", 
                                   "Norway", "Portugal", "San Marino", "Spain", 
                                   "Sweden", "Switzerland", "Turkiye", 
                                   "United Kingdom of Great Britain and Northern Ireland", "United States of America")
un_groups <-list("African State" = african_states,
                 "Asia-Pacific State" = asia_pacific_states,
                 "Eastern European State" = eastern_european_states,
                 "Latin American and Caribbean State" = latin_american_caribbean_states,
                 "Western European and Other State" = western_european_other_states)
un_df <- as.data.frame(stack(un_groups))
colnames(un_df) <- c("Area", "UN Group")

#check for crossover of country names for UN groups and dataset
test <- setdiff(un_df$Area, data$Area)

test2 <- setdiff(data$Area, un_df$Area)


#assign UN groupings
data <- data %>%
  mutate(
    UN = case_when(
      Area %in% african_states ~ "African States",
      Area %in% asia_pacific_states ~ "Asia-Pacific States",
      Area %in% eastern_european_states ~ "Eastern European States",
      Area %in% latin_american_caribbean_states ~ "Latin American and Caribbean States",
      Area %in% western_european_other_states ~ "Western European and Other States",
      TRUE ~ "Non-member"
    )
  )
 
#Netherlands Antilles (former) dissolved in 2010, very little info in df - remove.
data <- data[data$Area!="Netherlands Antilles (former)",]


#user interface
ui <- dashboardPage(
  skin= "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      icon("carrot", style = "font-size: 1.5em; margin-right: 10px;"),  
      "FAO"
    )
    
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName="welcome"),
      menuItem("Domestic Supply by Country", tabName="tab1"),
      menuItem("Worldwide Production and Import", tabName="tab2"),
      menuItem("UN Regional Groups", tabName="tab3"),
      menuItem("Download Data", tabName="tab4")
    )
    
  ),
  dashboardBody(
    #checking accessibility 
    use_tota11y(),
    tags$html(lang="en"),
    #edit aesthetics, included in CSS file
    includeCSS("www/style.css"),
    tabItems(
      tabItem(
        tabName="welcome",
        tags$div(
          style= "display: flex; justify-content: flex-end;",
          img(src="uni_logo.jpg",alt="University of Strathclyde logo", 
              width=200)),
        tags$h1("Welcome!"),
        tags$p(
          "This dashboard was created using the Food and Agriculture Organisation (FAO) Food Balances dataset,
          which recorded data related to food production, import and use for countries within the UN from 2010-2022. 
          This was created in collaboration with the University of Strathclyde. Explore the dashboard for more information
          and visualisations of the FAO data!"
        )
      ),
      tabItem(
        tabName="tab1",
        tags$h1("Domestic Supply by Country"),
        tags$h2("The below shows the total domestic supply per population, 
                by country."),
        fluidRow(
          column(width=6,
          box(
          selectInput(inputId="country",
                      label="Select country",
                      choices=unique(data$Area)), width=NULL)
          ),
          column(width=6,
                 box(
                   radioButtons(inputId="veg.animal",
                                      label="Select Item Group Type",
                                      choices=unique(data$veg.animal)),width=NULL)
                 )
        ),
        fluidRow(
          column(width=6,
            box(withSpinner(
              plotlyOutput("tot.supply.plot", width=NULL),
              image= "spinner.gif",
              image.height = "200px",
              image.width = "200px"), width=NULL
              )),
          column(width=6,
            box(withSpinner(
              plotlyOutput("item.plot"), 
              image= "spinner.gif",
              image.height ="200px",
              image.width ="200px"), width=NULL
            )))
          
      ),
      tabItem(
        tabName="tab2",
        tags$h1("Worldwide Production and Import"),
        tags$h2("The below plot shows imported vs produced domestic food supply for each continent."),
        fluidRow(
          box(
            checkboxGroupInput(inputId = "continent",
                               label = "Select Continent(s)",
                               choices = unique(data$Continent),
                               selected="Europe")
          ),
          box(withSpinner(plotlyOutput("continent.plot"),
                          image="spinner.gif",
                          image.height = "200px",
                          image.width= "200px"), width=12
            
          )
        )
      ),
      tabItem(
        tabName="tab3",
        tags$h1("UN Regional Groups"),
        tags$h2("The below table compares factors relating to food supply between UN regional groups"),
        fluidRow(
          box(
            sliderInput(inputId="years",
                        label = "Choose range of years below",
                        min = 2010,
                        max = 2022,
                        value = c(2010, 2022),
                        step=1,
                        sep="")
        
          ),
          box(
            checkboxGroupInput(inputId = "un",
                               label = "Select UN regional groups below.",
                               choices = unique(data$UN),
                               selected = unique(data$UN))
          ),
          fluidRow(
            box(withSpinner(dataTableOutput("table"),
                            image="spinner.gif",
                            image.width="200px",
                            image.height="200px"), 
                width=12), width=NULL
          )
        )
      ),
      tabItem(
        tabName="tab4",
        tags$h1("Download Data"),
        tags$h2("Click the button below to download the raw dataset."),
        downloadButton(outputId = "download", label = "Download Dataset")
      )
    )
    
  )
  )


server <- function(input, output, session){
  #bookmarking
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  observe(session$doBookmark())

  
#tab 1 
  #reactive element, choose by country
  country_data <- reactive({
    #subset by input
    subset(data, Area==input$country) 
  })

  #total domestic supply plot
output$tot.supply.plot <-renderPlotly({
  Sys.sleep(0.5)
  
  country_data() %>% group_by(Year) %>% 
    summarise(total = sum(total_supply, na.rm=TRUE))%>%
    plot_ly(x=~Year, y=~total, type="scatter", mode="lines") %>%
    layout(title = "Domestic Supply Per Population Over Time",
           xaxis= list(title="Year"),
           yaxis= list(title="Domestic Supply (1000 tonnes/pop)"))
  })

#reactive element, choose vegetal or animal input
item_data <- reactive({
  filtered <- country_data()[country_data()$veg.animal %in% input$veg.animal, ]
  return(filtered)
})

#reactive title
reactive.title <- reactive({switch(input$veg.animal,
                                   "Vegetal Products"="Vegetal Products",
                                   "Animal Products" = "Animal Products")
})

#item group plot - to note, no NAs included in plot
output$item.plot <- renderPlotly({
  Sys.sleep(0.5)
  
  item_data() %>% plot_ly(x=~Item.Group, y=~total_supply, type="bar") %>%
    layout(title="Domestic Supply by Item Group",
           xaxis=list(title=reactive.title()),
           yaxis=list(title="Domestic Supply (1000 tonnes/pop)"))
  
})

#tab 2
#get % production and import for each continent

#reactive based on continent input
continent_data <- reactive({
   data %>%
    filter(Continent %in% input$continent)
})

#%breakdown by continent
output$continent.plot <- renderPlotly({

  
  validate(
    need(length(input$continent) >0, 
         "Please select at least one continent to display the plot.")
  )
  
  totals <- continent_data() %>% 
    group_by(Continent)%>%
    summarise(
      production=sum(Production, na.rm=TRUE),
      import = sum(Import_quantity, na.rm=TRUE),
      total = sum(production + import)
    )
  
  percentages <- totals %>%
    mutate(perc_production = (production/total)*100,
           perc_import = (import/total)*100
    )
  
  perc_table <- percentages %>%
    select(Continent, perc_production, perc_import) %>%
    pivot_longer(
      cols= starts_with("perc_"),
      names_to = "Import.Production",
      values_to = "Percentage"
    ) %>%
    mutate(
      Import.Production = recode(Import.Production,
                                 perc_production = "% produced",
                                 perc_import = "% imported")
    )
  
  Sys.sleep(0.5)
  perc_table %>% plot_ly(x=~Continent, y=~Percentage, 
                         color=~Import.Production,
                         type="bar",
                         #color blind friendly plot
                         colors="Dark2") %>%
    layout(
      title="Produced and Imported Supply by Continent",
      yaxis = list(title="Percentage (%)"),
      legend = list(title = list(text="")),
      barmode="group"
    )
   
  
}
  )

#tab 3 - table of UN groups

output$table <- renderDataTable({
  
  
  filtered <- data[data$UN %in% input$un,] %>% 
    filter(Year>=input$years[1],
           Year <= input$years[2])
 
    
  table <- filtered %>% group_by(Year) %>% 
    group_by(UN) %>% 
    select(-c(total_supply,veg.animal, Continent, Area)) %>%
    summarise(production = round(mean(Production, na.rm=TRUE),1),
              import_quantity = round(mean(Import_quantity, na.rm=TRUE),1),
              stock_variation = round(mean(Stock_Variation, na.rm=TRUE),1),
              export_quantity = round(mean(Export_quantity, na.rm=TRUE),1),
              domestic_supply = round(mean(Domestic_supply_quantity, na.rm=TRUE),1),
              feed = round(mean(Feed, na.rm=TRUE),1),
              seed = round(mean(Seed, na.rm=TRUE),1),
              processing = round(mean(Processing, na.rm=TRUE),1),
              losses = round(mean(Losses, na.rm=TRUE),1),
              other_uses = round(mean(Other_uses_non_food, na.rm=TRUE),1),
              food = round(mean(Food, na.rm=TRUE),1),
              tourist_consumption= round(mean(Tourist_consumption, na.rm=TRUE),1))
    
    colnames(table) <- c("UN Group", "Produced", "Imported", 
                         "Stock Variation", "Exported", "Domestic Supply",
                         "Feed", "Seed", "Processing", "Losses",
                         "Other Uses", "Food", "Tourist Consumption")
    Sys.sleep(0.5)
  
  datatable(table, options=list(scrollX=TRUE)) 
      
}
)

  
#download dataset (tab4)
output$download <- downloadHandler(
  filename = function() {
    paste("dataset-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data, file, row.names = FALSE)
  }
)
}



#run app
shinyApp(ui, server, enableBookmarking = "url")
