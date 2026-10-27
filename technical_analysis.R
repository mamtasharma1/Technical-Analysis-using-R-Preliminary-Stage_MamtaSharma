# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Student: [Your Name]
# Purpose: Load stock data, calculate basic statistics, and display results.
# Note: Stock prices are downloaded from Yahoo Finance via quantmod.
# Internet access is required when running this script.

# 1. Install packages (run once; uncomment if packages are not installed)
# install.packages(c("quantmod", "TTR", "ggplot2", "dplyr"))

# 2. Load packages
required_packages <- c("quantmod", "TTR", "ggplot2", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(quantmod)
library(TTR)
library(ggplot2)
library(dplyr)

# 3. Read portfolio symbols from portfolio.txt
read_portfolio <- function(file_path = "portfolio.txt") {
  if (!file.exists(file_path)) {
    stop("Cannot find portfolio.txt. Set the R working directory to the Assignment2 folder.")
  }

  symbols <- readLines(file_path, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]
  symbols <- unique(symbols)

  if (length(symbols) == 0) {
    stop("portfolio.txt is empty. Add at least one stock symbol.")
  }

  symbols
}

# 4. Download data for each symbol and store it as a separate data frame.
load_stock_data <- function(file_path = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(file_path)
  stock_data <- list()

  for (symbol in symbols) {
    message("Downloading data for ", symbol, " ...")
    downloaded <- tryCatch(
      suppressWarnings(
        getSymbols(symbol, src = "yahoo", from = from, to = to,
                   auto.assign = FALSE)
      ),
      error = function(e) {
        message("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(downloaded)) next

    df <- data.frame(
      Date = as.Date(index(downloaded)),
      Open = as.numeric(Op(downloaded)),
      High = as.numeric(Hi(downloaded)),
      Low = as.numeric(Lo(downloaded)),
      Close = as.numeric(Cl(downloaded)),
      Volume = as.numeric(Vo(downloaded)),
      Adjusted = as.numeric(Ad(downloaded))
    )

    df$SMA20 <- as.numeric(TTR::SMA(df$Adjusted, n = 20))
    stock_data[[symbol]] <- df
  }

  if (length(stock_data) == 0) {
    stop("No stock data was downloaded. Check internet access and stock symbols.")
  }

  stock_data
}

# 5. Calculate statistics using adjusted closing prices.
# The mode is the most frequent observed value. If all values occur once,
# the function returns NA because there is no unique most-frequent value.
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  frequencies <- table(x)
  highest_frequency <- max(frequencies)
  modes <- names(frequencies)[frequencies == highest_frequency]
  if (length(modes) == length(frequencies)) return(NA_real_)
  as.numeric(modes[1])
}

calculate_statistics <- function(stock_df, symbol = "Stock") {
  prices <- stock_df$Adjusted
  prices <- prices[is.finite(prices)]

  if (length(prices) == 0) {
    return(data.frame(
      Symbol = symbol, Observations = 0, Mean = NA_real_, Mode = NA_real_,
      Median = NA_real_, Standard_Deviation = NA_real_,
      Latest_Adjusted_Close = NA_real_, Latest_SMA20 = NA_real_
    ))
  }

  latest_sma <- stock_df$SMA20[!is.na(stock_df$SMA20)]
  data.frame(
    Symbol = symbol,
    Observations = length(prices),
    Mean = mean(prices),
    Mode = calculate_mode(round(prices, 2)),
    Median = median(prices),
    Standard_Deviation = if (length(prices) > 1) sd(prices) else NA_real_,
    Latest_Adjusted_Close = tail(prices, 1),
    Latest_SMA20 = if (length(latest_sma) > 0) tail(latest_sma, 1) else NA_real_,
    row.names = NULL
  )
}

# 6. Display stock data and summary statistics.
display_stock_data <- function(stock_data, rows = 6) {
  for (symbol in names(stock_data)) {
    cat("\n----------------------------------------\n")
    cat("Stock symbol:", symbol, "\n")
    cat("First", rows, "rows of imported data:\n")
    print(utils::head(stock_data[[symbol]], rows))
  }
}

display_statistics <- function(stock_data) {
  summary_table <- dplyr::bind_rows(
    lapply(names(stock_data), function(symbol) {
      calculate_statistics(stock_data[[symbol]], symbol)
    })
  )
  cat("\nSummary statistics (based on Adjusted closing price):\n")
  print(summary_table, row.names = FALSE)
  summary_table
}

# 7. Create a line chart of adjusted closing prices and the 20-day moving average.
plot_stock_data <- function(stock_data, symbol) {
  if (!symbol %in% names(stock_data)) {
    stop("Symbol not found in downloaded stock data: ", symbol)
  }

  chart_data <- stock_data[[symbol]]
  p <- ggplot(chart_data, aes(x = Date)) +
    geom_line(aes(y = Adjusted, colour = "Adjusted close"), linewidth = 0.7) +
    geom_line(aes(y = SMA20, colour = "20-day moving average"),
              linewidth = 0.7, na.rm = TRUE) +
    labs(
      title = paste(symbol, "Adjusted Closing Price and 20-Day Moving Average"),
      x = "Date", y = "Price (USD)", colour = "Series"
    ) +
    theme_minimal()

  print(p)
  invisible(p)
}

# 8. Run the analysis
portfolio_data <- load_stock_data("portfolio.txt")
display_stock_data(portfolio_data)
statistics_table <- display_statistics(portfolio_data)

# Create an output folder and save the statistics table.
if (!dir.exists("output")) dir.create("output")
write.csv(statistics_table, file = "output/stock_statistics.csv", row.names = FALSE)

# Show one chart for each successfully downloaded stock.
for (symbol in names(portfolio_data)) {
  print(plot_stock_data(portfolio_data, symbol))
}

# Optional: save stock data as CSV files for later inspection.
for (symbol in names(portfolio_data)) {
  write.csv(portfolio_data[[symbol]],
            file = file.path("output", paste0(symbol, "_data.csv")),
            row.names = FALSE)
}

cat("\nFinished. Check the output folder for CSV files.\n")
