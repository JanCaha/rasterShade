# helper function by Eldar Agalarov from
# https://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r
is.convertible.to.date <- function(x) {
  return(!is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d')))
}
