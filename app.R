library(shiny)
library(shinythemes)
library(factoextra) 
library(rworldmap)
library(rworldxtra)


require(gdata)
df = read.xls (("CountryData.xlsx"), sheet = 1, header = TRUE)
# Removing blank rows generated
df<- df[c(1:21)]

names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")

set.seed(0)

require(caret)
#we impute missing values with a random forest
imputation_model = preProcess(x = df[,-c(1,2)],method = "bagImpute")
imputated_data = predict(object = imputation_model,newdata=df[,-c(1,2)])

# Adding country names to the rows
rownames(imputated_data)<-df[,2] 

mydata.scaled<-scale(imputated_data)

pca.out<-prcomp(mydata.scaled)

library(gridExtra)
plot1 <- fviz_contrib(pca.out, choice="var", axes = 1, top = 19)
plot2 <- fviz_contrib(pca.out, choice="var", axes = 2, top = 19, color = "lightgrey")

#my_palette <- palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

#my_palette <- palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628"))
### Shiny app starts here
my_palette <- rainbow(7)

# Define UI ----
ui <- fluidPage( theme = shinytheme("superhero"),
                 titlePanel("Case study: Countries through clustering"),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     #h1("Cluster control"),
                     p("Use the controls below to change the appearance of the cluster plot"),
                     sliderInput("clusters", "Number of clusters",
                                 min = 2, max = 7, value = 3),
                     radioButtons(inputId="choice", label="What ellipse type should you use?", 
                                  choices=c("euclid","convex")),
                     p("Understanding the dimensions of the plot"),
                     plotOutput('plotPca', width = "100%", height = "510px")
                   ),
                   mainPanel(
                     h3("Countries in respective clusters"),
                     plotOutput('plotClusters'),
                     h3("World Map country plot"),
                     plotOutput('plotMap')
                   )
                 )
)
# Define server logic ----
server <- function(input, output, session) {
  
  
  clusters <- reactive({
    kmeans(mydata.scaled, input$clusters, nstart =50)
  })
  
  ellipse_type <- reactive({
    input$choice
  })
  
  output$plotPca <- renderPlot({
    grid.arrange(plot1, plot2, nrow=2)
  })
  
  output$plotClusters <- renderPlot({
    fviz_cluster(clusters(), 
                 data = mydata.scaled,
                 palette = my_palette,
                 ellipse.type = ellipse_type(), 
                 #labelsize = labels_size(),
                 #pointsize = 0.1,
                 #choose.vars = c("GDP", "UrbanPopulation"),
                 #geom="point",
                 star.plot = TRUE, # Add segments from centroids to items
                 repel = TRUE, # Avoid label overplotting (slow)
                 #labelsize = 0.3,
                 ggtheme = theme_minimal()
    )
  },height = function() {
    session$clientData$output_plotClusters_width*0.5
  })
  
  output$plotMap <- renderPlot({

    cluster = as.numeric(clusters()$cluster)
    spdf = joinCountryData2Map(data.frame(cluster,df$CountryName), joinCode="NAME", nameJoinColumn="df.CountryName",verbose = TRUE,mapResolution = "low")
    mapCountryData(spdf, nameColumnToPlot="cluster", catMethod="fixedWidth",colourPalette=my_palette, addLegend = FALSE, lwd = 0.5)
    
  },height = function() {
    session$clientData$output_plotMap_width*0.5
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)