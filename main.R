library(shiny)
library(randomForest)

model <- readRDS("model.rds")

fallback_weather <- list(temperature = 25, humidity = 60)
fallback_pollutants <- list(aqi = 100)

ui <- fluidPage(
  titlePanel("Respiratory Disease Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "district",
        "Select Your District:",
        choices = c(
          "Achham", "Arghakhanchi", "Baglung", "Baitadi", "Bajhang", "Bajura", 
          "Banke", "Bara", "Bardiya", "Bhaktapur", "Bhojpur", "Chitwan", 
          "Dadeldhura", "Dailekh", "Dang", "Darchula", "Dhading", "Dhankuta", 
          "Dhanusha", "Dolakha", "Dolpa", "Doti", "Gorkha", "Gulmi", "Humla", 
          "Ilam", "Jajarkot", "Jhapa", "Jumla", "Kailali", "Kalikot", 
          "Kanchanpur", "Kapilvastu", "Kaski", "Kathmandu", "Kavrepalanchok", 
          "Khotang", "Lalitpur", "Lamjung", "Mahottari", "Makawanpur", 
          "Manang", "Morang", "Mugu", "Mustang", "Myagdi", "Nawalpur", 
          "Nuwakot", "Okhaldhunga", "Palpa", "Panchthar", "Parasi", "Parbat", 
          "Parsa", "Pyuthan", "Ramechhap", "Rasuwa", "Rautahat", "Rolpa", 
          "Rukum", "Rupandehi", "Salyan", "Sankhuwasabha", "Saptari", 
          "Sarlahi", "Sindhuli", "Sindhupalchok", "Siraha", "Solukhumbu", 
          "Sunsari", "Surkhet", "Syangja", "Tanahun", "Taplejung", "Terhathum", 
          "Udayapur"
        )
      ),
      actionButton("fetch_data", "Fetch Real-Time Data"),
      checkboxGroupInput(
        "symptoms", 
        "Select Symptoms You Are Experiencing:",
        choices = c("Cough", "Wheezing", "Shortness_of_breath", "Chest_tightness",
                    "Trouble_in_sleeping", "Fever", "Sweating_and_chills", "Chest_pain",
                    "Difficulty_breathing", "Chronic_cough", "Fatigue", "Bleeding_cough",
                    "Weight_loss", "Loss_of_appetite", "Weakness", "Night_sweats", 
                    "Eye_Change", "Hoarseness")
      ),
      actionButton("predict", "Predict Disease")
    ),
    mainPanel(
      verbatimTextOutput("real_time_data"),
      verbatimTextOutput("prediction_result")
    )
  )
)

server <- function(input, output, session) {
  
  fetch_real_time_data <- function(district) {
    return(list(temperature = fallback_weather$temperature, 
                humidity = fallback_weather$humidity, 
                aqi = fallback_pollutants$aqi))
  }
  
  observeEvent(input$fetch_data, {
    withProgress(message = "Fetching real-time data...", value = 0, {
      real_time_data <- fetch_real_time_data(input$district)
      
      output$real_time_data <- renderText({
        paste("Failed to fetch real-time data. Using fallback values:\n",
              "Temperature: ", real_time_data$temperature, "°C\n",
              "Humidity: ", real_time_data$humidity, "%\n",
              "AQI: ", real_time_data$aqi)
      })
    })
  })
  
  observeEvent(input$predict, {
    if (length(input$symptoms) > 0) {
      real_time_data <- fetch_real_time_data(input$district)
      
      features <- data.frame(
        temperature = real_time_data$temperature,
        humidity = real_time_data$humidity,
        aqi = real_time_data$aqi,
        symptoms_count = length(input$symptoms) 
      )
      
      colnames(features) <- c("Temperature", "Humidity", "AQI", "symptoms_count")
      
      tryCatch({
        prediction <- predict(model, newdata = features, type = "prob")
        

        max_prob <- max(prediction)
        disease <- colnames(prediction)[which.max(prediction)]
        
        if (max_prob > 0.45) {
          output$prediction_result <- renderText({
            paste("Predicted Disease:", disease, "\nProbability:", round(max_prob * 100, 2), "%", "\nPlease consult the doctor soon.")
          })
        } else {
          output$prediction_result <- renderText("No major risks detected. Please take care and monitor your health.")
        }
      }, error = function(e) {
        output$prediction_result <- renderText(paste("An error occurred while making the prediction: ", e$message))
      })
    } else {
      output$prediction_result <- renderText("Please select symptoms.")
    }
  })
}


shinyApp(ui, server)

