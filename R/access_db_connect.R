#' Connect to MS access database
#'
#' @description
#' This function is a wrapper for dbConnect and differs in that it takes a standard file
#' path as input.
#'
#' @import DBI
#' @import odbc
#' @export
#' @param db_file_path path to a MS Access database
#' @return
#' * A connection to a MS access database
#' @export
access_db_connect <- function(db_file_path)  {
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }
  # Assemble connection strings
  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  myconn <- DBI::dbConnect(odbc::odbc(),
                      .connection_string = db_connect_string)
  return(myconn)
}

