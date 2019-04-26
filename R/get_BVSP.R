#' Function to download the current components of the Ibovespa (BVSP) index from Wikipedia
#'
#' This function scrapes the stocks that constitute the BVSP index from the wikipedia page at https://en.wikipedia.org/wiki/List_of_companies_listed_on_Ibovespa.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the BVSP index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.BVSP <- get_BVSP()
#' print(df.BVSP$tickers)
#' }
get_BVSP <- function(do.cache = TRUE,
                             cache.folder = 'BGS_Cache'){

    if (do.cache) {
        # check if cache folder exists, create if not
        ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
        cache.file <- file.path(cache.folder, paste0('BVSP_Composition_', Sys.Date(), '.rds') )

        # check if file exists
        flag <- file.exists(cache.file)

        if (flag) {
            df.BVSP <- readRDS(cache.file)
            return(df.BVSP)
        }
    }

    my.url <- 'https://en.wikipedia.org/wiki/List_of_companies_listed_on_Ibovespa'

    read_html <- 0 # fix for global variable nagging from BUILD
    my.xpath <- '//*[@id="mw-content-text"]/div/table[1]'
    df.BVSP <- my.url %>%
        read_html() %>%
        html_nodes(xpath = my.xpath) %>%
        html_table()

    df.BVSP <- df.BVSP[[1]]

    colnames(df.BVSP) <- c('company','ticker','industry','headquarters')

    if (do.cache) {
        saveRDS(df.BVSP, cache.file)
    }

    return(df.BVSP)
}
