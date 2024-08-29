library(shiny)

plot1 <- function(input){
  req(input$file_path)  # Require a file path input(call the file path)
  
  # Read the CSV file
  data <- read.csv(input$file_path, stringsAsFactors = FALSE)
  
  #compare age and sum of total spending
  library("dplyr")
  age_total <- data %>% group_by(age) %>% summarise(Total = sum(total))
  plot(
    age_total,
    ylab = "sum of total spending",xlab = "age",
    main = "compare age and sum of total spending",
    type = "b",
    col = "#144000",
    lwd = 2
  )
}

plot2 <- function(input){
  req(input$file_path)
  data=read.csv(input$file_path, stringsAsFactors = FALSE)
  
  groupedPayment <- data %>% group_by(paymentType) %>% summarise(Total = sum(total))
  pie(
    groupedPayment$Total,
    main = "Comparison between cash and credit totals",
    #getting the persentage of each section
    labels = paste(groupedPayment$paymentType, ": ", round(groupedPayment$Total / sum(groupedPayment$Total) * 100, 2), "%"),
    col = c("#00ff72", "#7500ff"),  # Colors for cash and credit
  )
  legend("topright", legend = groupedPayment$paymentType,  fill =c("#00ff72", "#7500ff"))
}

plot3 <- function(input){
  req(input$file_path)
  data=read.csv(input$file_path, stringsAsFactors = FALSE)
  
  groupedCity <- data %>% group_by(city) %>% summarise(Total = sum(total))
  # Assuming your data frame is named 'groupedCity'
  sorted_groupedCity <- groupedCity[order(groupedCity$Total, decreasing = TRUE), ]
  colors <- c('#00ff72','#00ffaf','#59ffc0','#00fffd','#00cdff','#0087ff','#004aff','#0d00ff','#5200ff','#7500ff')#nice Color gradation😎
  barplot(main = "Comparison between each city and its total sum", 
          height = sorted_groupedCity$Total,
          name = sorted_groupedCity$city,
          xlab = "City", 
          ylab = "Total Sum",
          col = colors
  )
  
  
}

# 144000
plot4 <- function(input){
  req(input$file_path)
  data=read.csv(input$file_path, stringsAsFactors = FALSE)
  # Assuming you have already created your box plot
  boxplot(x = data$total,
          main = "Distribution of Total Spending",
          xlab = "Total Spending",
          col = "#7500ff")
  
  # Calculate the statistics
  max_value <- max(data$total)
  min_value <- min(data$total)
  median_value <- median(data$total)
  
  # Add a legend
  legend("topright",  # Position of the legend
         legend = c(paste("Max:", max_value), paste("Min:", min_value), paste("Median:", median_value)),  # Legend labels
         fill = c("#2da609", "#2da609", "#2da609"),  # Colors (same as boxplot color)
         title = "Statistics",  # Legend title
         bty = "n",  # Remove the legend box
         text.col = "black",  # Text color
         cex = 0.8)  # Legend size
  
  
}
#collect all the visualization usage in a one dashboard
plot5 <- function(input)
{
  par(mfrow=c(2,2))
  plot1(input)
  plot2(input)
  plot3(input)
  plot4(input)
}

# Define UI for application
# changing some colors in the window using the CSS or HTML 
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        /* Change background color of the entire window */
        body {
            background: linear-gradient(to right, #59C173 , #a17fe0 ,#5D26C1); /* Adjust colors as needed */        }
    /* Define custom CSS for slider handle background color */
    .irs-single, .irs-handle {
      background-color: #5D26C1 !important; /* Change to your desired color */
    }
    div{color : black;}
    /* Change tab name color */
      .nav-tabs > li > a {
        color: white; /* You can use color names or hex values */
      }
      ")
    )
  ),
  div(id = "Team 36",
      h2("Impossible is not the title of Team 36’s story"),
      p("Thank you from the bottom of my heart for this wonderful effort🤩")
  ),
  # Application title
  titlePanel("Team #36"),
  
  # Sidebar layout with input and output
  sidebarLayout(
    sidebarPanel(
      # Text input for file path
      textInput("file_path", "Enter File Path:"),
      
      # Slider inputs
      sliderInput("clus", "Select Number of Clusters :", 
                  min = 2, max = 4, value = 3),
      sliderInput("supp", "Select The Minimum Support :", 
                  min = 0.001, max = 1, value = 0.05),
      sliderInput("conf", "Select The Minimum Confidence :", 
                  min = 0.001, max = 1, value = 0.3)
    ),
    mainPanel(
      textOutput("result"),
      # Output for file path input
      verbatimTextOutput("file_path_output"),
      #create a right area to display your input
      verbatimTextOutput("clus_output"),
      verbatimTextOutput("supp_output"),
      verbatimTextOutput("conf_output"),
      
      tabsetPanel(
        #tabPanel("the text that will appear on the buttom",plotOutput("the plot name"))
        tabPanel("Age Vs Total Spending📈", plotOutput("plot1")),
        tabPanel("Cash Vs Credit💸", plotOutput("plot2")),
        tabPanel("City Total Spending📊", plotOutput("plot3")),
        tabPanel("Distrib of Total T.spending🧩", plotOutput("plot4")),
        tabPanel("Dashboard📋", plotOutput("plot5")),
        tabPanel("Association rules 👀" , dataTableOutput("plot6")),
        tabPanel("Kmeans 🍃", dataTableOutput("table1")),
        tabPanel("About Us 👩‍💻", dataTableOutput("about"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Output for file path input
  output$file_path_output <- renderPrint({
    
    input$file_path
  })
  
  # Output for clusters
  output$clus_output <- renderPrint({
    paste("Number of Clusters = ", input$clus)
    
  })
  
  # Output for support
  output$supp_output <- renderPrint({
    paste("Minimum Support = ", input$supp)
  })
  
  # Output for confidence
  output$conf_output <- renderPrint({
    paste("Minimum Confidence = ", input$conf)
  })
  
  #plot1🥰
  output$plot1 <- renderPlot({
    plot1(input)
  })
  #plot2🤯
  output$plot2 <-renderPlot({
    plot2(input)
  })
  
  #plot3🤩
  output$plot3 <-renderPlot({
    plot3(input)
  })
  
  #plot4🤟
  output$plot4 <-renderPlot({
    plot4(input)
  })
  #plot5🥶
  output$plot5 <-renderPlot({
    plot5(input)
  })
  
  output$table1 <- renderDataTable({
    req(input$clus)
    library("dplyr")
    req(input$file_path)
    data=read.csv(input$file_path, stringsAsFactors = FALSE)
    data_summary<- data %>% group_by(customer,age) %>% summarise(Total = sum(total))
    #get the summary of the data without the customer colomn
    without_customer<-data_summary[,-c(1)]
    set.seed(1)
    #create the kmeans-clusterig 
    k_means<-kmeans(without_customer,centers =input$clus)
    #show the clusters
    k_means
    
    #adding the cluster vector to the data summary
    data_summaryy<-cbind(data_summary,k_means$cluster)
    #rename the forth colomn to "group" instade of default name
    colnames(data_summaryy)[4]="group"
    data_summaryy
    #convert the table to data frame 
    LEVA=as.data.frame(data_summaryy)
    
  })
  
  output$plot6 <- renderDataTable({
    req(input$file_path)
    #read the csv path as a string without adding double qoutations 
    data=read.csv(input$file_path, stringsAsFactors = FALSE)
    data=clean_data_KOL(data)
    #converting the first column into transaction data for the apriori algorithm
    trans <- unlist(data[, "items"])
    # to write the data into a text file
    writeLines(trans, "transactions.txt")
    #text connection allow you to convert the colomn to text file 
    trans = read.transactions("transactions.txt", sep = ',')
    #using apriori
    apororo = apriori(trans,parameter = list(support= input$supp, confidence=input$conf ,minlen=2))
    #display the assosiation rules in the output area
    
    #convert to dataframe to use it in table (this function to convert the arules to df(as.dataframe()doesn't work))
    df=DATAFRAME(apororo)
    
  })
  #our team information
  output$about <- renderDataTable({
    df=data.frame(
      names=c("Abdelrahmaen Ahmed Abdelmonem","Ahmed Mohamed Abdullatif","Adham Ibrahim Farouk","Khaled Nabil Fathy","Manar Mohamed Abdelkarim","Lamia Araby AboZaid Ahmed"),
      id=c(23011311,23011214,23011219,23011259,23011560,23011127)
    )
  })
  
  
}
#clean data without removing the outliers
clean_data_KOL <- function(data) {
  sum(is.na(data)) # 0
  data <- na.omit(data) # no need
  sum(duplicated(data)) # 2
  data <- unique(data) # must
  return(data)
}

# Run the application
shinyApp(ui = ui, server = server)