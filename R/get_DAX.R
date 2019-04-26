#' Function to download the current components of the DAX index from Wikipedia
#'
#' This function scrapes the stocks that constitute the DAX index from the wikipedia page at https://en.wikipedia.org/wiki/FTSE_100_Index#List_of_FTSE_100_companies.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the DAX index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.DAX <- get_DAX()
#' print(df.DAX$tickers)
#' }
get_DAX <- function(do.cache = TRUE,
                    cache.folder = 'BGS_Cache'){

    if (do.cache) {
        # check if cache folder exists, create if not
        ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
        cache.file <- file.path(cache.folder, paste0('DAX_Composition_', Sys.Date(), '.rds') )

        # check if file exists
        flag <- file.exists(cache.file)

        if (flag) {
            df.DAX <- readRDS(cache.file)
            return(df.DAX)
        }
    }

    my.url <- 'https://en.wikipedia.org/wiki/DAX'

    read_html <- 0 # fix for global variable nagging from BUILD
    my.xpath <- '//*[@id="constituents"]'
    df.DAX <- my.url %>%
        read_html() %>%
        html_nodes(xpath = my.xpath) %>%
        html_table()

    df.DAX <- df.DAX[[1]]
    df.DAX[1] = NULL

    colnames(df.DAX) <- c('company','prime_standard_industry','ticker','index_weight','employees','founded')

    if (do.cache) {
        saveRDS(df.DAX, cache.file)
    }

    return(df.DAX)
}
