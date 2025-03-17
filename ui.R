# ui.R

library(shiny)

ui <- fluidPage(
  titlePanel("Nifty 50 Stock Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock (Nifty 50):",
                  choices = c("ADANIPORTS.NS", "APOLLOHOSP.NS", "ASIANPAINT.NS", "AXISBANK.NS", "BAJAJ-AUTO.NS",
                              "BAJFINANCE.NS", "BAJAJFINSV.NS", "BHARTIARTL.NS", "BPCL.NS", "BRITANNIA.NS",
                              "CIPLA.NS", "COALINDIA.NS", "DIVISLAB.NS", "DRREDDY.NS", "EICHERMOT.NS",
                              "GRASIM.NS", "HCLTECH.NS", "HDFC.NS", "HDFCBANK.NS", "HDFCLIFE.NS",
                              "HEROMOTOCO.NS", "HINDALCO.NS", "HINDUNILVR.NS", "ICICIBANK.NS", "INDUSINDBK.NS",
                              "INFY.NS", "JSWSTEEL.NS", "KOTAKBANK.NS", "LT.NS", "M&M.NS",
                              "MARUTI.NS", "NESTLEIND.NS", "NTPC.NS", "ONGC.NS", "POWERGRID.NS",
                              "RELIANCE.NS", "SBILIFE.NS", "SBIN.NS", "SHREECEM.NS", "SUNPHARMA.NS",
                              "TATACONSUM.NS", "TATAMOTORS.NS", "TATASTEEL.NS", "TCS.NS",
                              "TECHM.NS", "TITAN.NS", "ULTRACEMCO.NS", "UPL.NS", "WIPRO.NS"),
                  selected = "RELIANCE.NS"),
      dateInput("start_date", "Start Date:", value = Sys.Date() - 365, max = Sys.Date()),
      dateInput("end_date", "End Date:", value = Sys.Date(), max = Sys.Date()),
      actionButton("fetch", "Fetch Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Table", DT::dataTableOutput("stock_table")),
        tabPanel("Stock Plot", plotOutput("stock_plot")),
        tabPanel("Comparison Plot", plotOutput("comparison_plot"))
      )
    )
  )
)
