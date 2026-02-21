#' Create a traffic light HTML span
#' @param status Character: "green", "amber", or "red"
#' @return shiny.tag
#' @noRd
traffic_light <- function(status) {
  htmltools::tags$span(class = paste("traffic-light", status))
}
