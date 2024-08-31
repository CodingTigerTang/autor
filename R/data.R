#' Create Text data file for exploration
#'
#' Internal Function
#' @keywords internal
text_data <- function() {
  text_data <- data.frame(
    Text = c(
      "The service at the restaurant was exceptional. The staff were attentive and the food was delicious. I had the best steak I've ever tasted. I'll definitely be coming back!",
      "I bought a new smartphone last week and it's already malfunctioning. The battery drains quickly, and the screen freezes often. I'm very disappointed with this purchase.",
      "I recently watched a movie that left me in awe. The storyline was captivating and the actors delivered stellar performances. It's one of the best films I've seen this year.",
      "The library offers a wide selection of books across different genres. It is open from 9 AM to 8 PM on weekdays and from 10 AM to 6 PM on weekends.",
      "The weather forecast for today predicts partly cloudy skies with a high of 75 degrees and a low of 60 degrees. There is a slight chance of rain in the evening.",
      "Our vacation to Hawaii was a dream come true. The beaches were pristine, and we enjoyed numerous water activities. The highlight was a sunset cruise that offered breathtaking views.",
      "My experience at the hotel was horrible. The room was dirty, the bed was uncomfortable, and the service was terrible. I wouldn't recommend this place to anyone.",
      "The concert was a disaster. The sound system kept failing, and the band seemed unprepared. It was a waste of time and money.",
      "The meeting started at 10 AM and covered various topics, including the annual budget and upcoming projects. It lasted for two hours and concluded with a Q&A session.",
      "The new office building is located downtown, close to several public transportation options. It features modern amenities and open-plan workspaces."
    ),
    stringsAsFactors = FALSE
  )
  text_data |> readr::write_csv("inst/shiny_label/text.csv")
}
