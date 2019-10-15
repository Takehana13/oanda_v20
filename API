OANDA_V20 <- R6Class(
  "OANDA_V20",
  public = list(
    symbol_name = NULL, strat_magic = NULL,
    account_type = NULL, account_id = NULL, access_token = NULL,
    v20 = NULL, api = NULL, accounts = NULL, instruments = NULL,
    orders = NULL, requests = NULL, trades = NULL,
    # init ----
    initialize = function(symbol_name = "USD_JPY",
                          account_type = NA, account_id = NA, access_token = NA,
                          strat_magic = "777"){
      pacman::p_load(rlang, tidyverse, lubridate, reticulate, rlist, pipeR, plotly, R6)
      
      self$symbol_name = symbol_name
      self$account_type = account_type
      self$account_id = account_id
      self$access_token = access_token
      self$strat_magic = strat_magic
      
      # python obj
      use_python("/cloud/project/r-reticulate/bin/python3")
      py_discover_config()
      py_install("oandapyV20")
      
      self$v20 <- import("oandapyV20")
      self$api <- self$v20$API(access_token = self$access_token, environment = self$account_type)
      self$accounts <- self$v20$endpoints$accounts
      self$instruments <- self$v20$endpoints$instruments
      self$orders <- self$v20$endpoints$orders
      self$requests <- self$v20$contrib$requests
      self$trades <- self$v20$endpoints$trades
    },
    # instruments info ----
    pip_value = NULL,
    tick_value = NULL,
    get_instrument = function(){
      r <- self$accounts$AccountInstruments(accountID = self$account_id)
      rv <- self$api$request(r)
      
      pv <- rv$instruments %>%
        list.filter(name == self$symbol_name) %>%
        list.map(pipLocation)
      self$pip_value <- if_else(pv == -2, 100, if_else(pv == -4, 10000, 0))
      self$tick_value <- if_else(pv == -2, 0.01, if_else(pv == -4, 0.0001, 0))
    },
    # price data ----
    price_data = NULL,
    price_rv = NULL,
    get_data = function(n = "2000", tf = "M1", ba = "bid"){
      param <- list(count = n, price = "BA", granularity = tf)
      r <- self$instruments$InstrumentsCandles(instrument = self$symbol_name, params = param)
      rv <- self$api$request(r)
      self$price_rv <- rv
      
      if(ba == "bid"){
        data <- rv$candles %>% 
          map(list.remove("ask")) %>%
          map(~.x %>% flatten_df) %>%
          bind_rows()
      } else if(ba == "ask"){
        data <- rv$candles %>% 
          list.remove("bid") %>%
          map(~.x %>% flatten_df) %>%
          bind_rows()
      } else return("bid or ask?")
      
      data <- data %>% 
        mutate(dttm = as_datetime(time)) %>%
        select(-time, -complete)
      self$price_data <- data %>%
        select(dttm, h, c, l, o, volume) %>% 
        mutate(dttm = with_tz(dttm, "Asia/Tokyo")) %>% 
        rename(high = h, close = c, low = l, open = o) %>% 
        mutate(
          open = as.double(open),
          high = as.double(high),
          low = as.double(low),
          close = as.double(close)
        )
    },
    # plot price data ----
    plt_data = function(){
      self$price_data %>% 
        plot_ly(x = ~dttm, type="candlestick",
                open = ~open, close = ~close,
                high = ~high, low = ~low) %>%
        layout(title = "Series Chart")
    },
    # check spread data ----
    sp = NULL,
    hist_sp = NULL,
    chk_sp = function(mode = "current"){
      if(mode == "current"){
        b <- self$price_rv$candles[[1]]$bid$c %>% as.numeric
        a <- self$price_rv$candles[[1]]$ask$c %>% as.numeric
        self$sp <- (a - b) * self$pip_value
      }
      if(mode == "hist"){
        b <- self$price_rv$candles %>% 
          map(pluck("bid")) %>%
          map(pluck("c")) %>%
          map(as.double) %>%
          flatten_dbl
        a <- self$price_rv$candles %>% 
          map(pluck("ask")) %>% 
          map(pluck("c")) %>% 
          map(as.double) %>% 
          flatten_dbl
        ba <- tibble(bid = b, ask = a)
        self$hist_sp <- ba %>% mutate(sp = (ask - bid) * self$pip_value)
      }
    },
    # plot spread data ----
    plt_sp = function(){
      self$hist_sp %>% 
        mutate(id = row_number()) %>% 
        ggplot(aes(id, sp)) + 
        geom_line() +
        theme_tq()
    },
    # account summary data ----
    account_summary = NULL,
    get_account_summary = function(){
      r <- self$accounts$AccountSummary(accountID = account_id)
      rv <- self$api$request(r)
      self$account_summary <- rv$account
    },
    # account detail data ----
    account_orders = NULL,
    account_trades = NULL,
    account_positions = NULL,
    account_detail = NULL,
    get_account_detail = function(){
      r <- accounts$AccountDetails(accountID = account_id)
      rv <- api$request(r)
      
      self$account_orders <- rv$account$order
      self$account_trades <- rv$account$trades 
      self$account_positions <- rv$account$positions
      self$account_detail <- rv$account %>>%
        list.remove(c("trades", "orders", "positions"))
    },
    # make new order ----
    last_tiket_id = NULL,
    last_tiket_unit = NULL,
    magic_lst = NULL,
    trans_lst = NULL,
    new_order = function(units = "+10000", type = "MARKET", pos_fill = "DEFAULT", magic = self$strat_magic,
                         use_tpsl = FALSE, tp_dis = NULL, sl_dis = NULL){
      direction <- if_else(as.numeric(units) > 0, 1, -1)
      if(use_tpsl){
        p <- self$price_data %>% tail(1) %>% pull(close) %>% as.numeric
        if(direction > 0){
          sl <- as.character(p - sl_dis * self$tick_value)
          tp <- as.character(p + tp_dis * self$tick_value)
        } else {
          sl <- as.character(p + sl_dis * self$tick_value)
          tp <- as.character(p - tp_dis * self$tick_value)
        }
        tp_data <- list(timeinForce = "GTC", price = tp)
        sl_data <- list(timeinForce = "GTC", price = sl)
      }
      
      if(!use_tpsl){
        data <- list(order = list(
          instrument = self$symbol_name,
          units = units,
          type = type,
          positionFILL = pos_fill
        ))
      } else {
        data <- list(order = list(
          instrument = self$symbol_name,
          units = units,
          type = type,
          positionFILL = pos_fill,
          takeProfitOnFill = tp_data,
          stopLossOnFill = sl_data
        ))
      }
      
      r <- self$orders$OrderCreate(self$account_id, data)
      rv <- self$api$request(r)
      
      self$last_tiket_id <- rv$orderFillTransaction$id
      self$last_tiket_unit <- units %>% as.numeric %>% abs %>% as.character
      self$magic_lst <- self$magic_lst %>% c(list(list(id = self$last_tiket_id, magic = magic)))
      self$trans_lst <- self$trans_lst %>% c(list(mkt = list(data = rv, magic = magic)))
    },
    # close position ----
    close_order = function(units = self$last_tiket_unit, trade_id = self$last_tiket_id,
                           magic = self$strat_magic){
      check_magic <- self$magic_lst %>>% 
        list.any(id == trade_id && magic == magic)
      
      if(check_magic){
        data <- list(units = units)
        r <- self$trades$TradeClose(accountID = self$account_id, tradeID = trade_id, data = data)
        rv <- self$api$request(r)
        
        i <- self$magic_lst %>% list.findi(id == trade_id && magic == magic)
        self$magic_lst <- self$magic_lst %>% list.remove(i)
        self$trans_lst <- self$trans_lst %>% c(list(close = list(data = rv, magic = magic)))
      } else {
        stop("wrong magic")
      }
    },
    # make pending order ----
    last_order_id = NULL,
    pending_order = function(price = "111.000", units = "-10000", magic = self$strat_magic,
                             type = "LIMIT", pos_fill = "DEFAULT"){
      param <- list(count = "1", price = "BA", granularity = "M1")
      direction <- if_else(as.numeric(units) > 0, 1, -1)
      
      r <- self$instruments$InstrumentsCandles(instrument = self$symbol_name, params = param)
      rv <- self$api$request(r)
      
      if(type == "LIMIT"){
        if(direction < 0 && as.numeric(price) < as.numeric(rv$candles[[1]]$bid$c)) stop("wrong price")
        if(direction > 0 && as.numeric(price) > as.numeric(rv$candles[[1]]$ask$c)) stop("wrong price")
      } else if(type == "STOP"){
        if(direction < 0 && as.numeric(price) > as.numeric(rv$candles[[1]]$bid$c)) stop("wrong price")
        if(direction > 0 && as.numeric(price) < as.numeric(rv$candles[[1]]$ask$c)) stop("wrong price")
      } else {
        stop("wrong order type")
      }
      
      data <- list(order = list(
        price = price,
        instrument = self$symbol_name,
        units = units,
        type = type,
        positionFill = pos_fill
      ))
      r <- self$orders$OrderCreate(self$account_id, data = data)
      rv <- self$api$request(r)
      
      self$last_order_id <- rv$lastTransactionID
      self$magic_lst <- self$magic_lst %>% c(list(list(id = self$last_order_id, magic = magic)))
      self$trans_lst <- self$trans_lst %>% c(list(pending = list(data = rv, magic = magic)))
    },
    # cancel pending order ----
    last_cancel_id = NULL,
    cancel_order = function(order_id = self$last_order_id, magic = self$strat_magic){
      check_magic <- self$magic_lst %>>% 
        list.any(id == order_id && magic == magic)
      
      if(check_magic){
        r <- self$orders$OrderCancel(accountID = self$account_id, orderID = order_id)
        rv <- self$api$request(r)
        
        self$last_cancel_id <- rv$lastTransactionID
        i <- self$magic_lst %>% list.findi(id == order_id && magic == magic)
        self$magic_lst <- self$magic_lst %>% list.remove(i)
        self$trans_lst <- self$trans_lst %>% c(list(cancel = list(data = rv, magic = magic)))
      } else {
        "wrong magic"
      }
    },
    # get pending order info ----
    order_cnt = NULL,
    order_ids = NULL,
    get_order_info = function(symbol = self$symbol_name, all = F, magic = self$strat_magic){
      r <- self$orders$OrdersPending(self$account_id)
      rv <- self$api$request(r)
      
      order_sym <- rv$orders %>>% list.filter(instrument == symbol)
      ids <- order_sym %>% list.map(id)
      
      if(all == F){
        self$order_ids <- self$magic_lst %>>%
          list.filter(id %in% ids & magic %in% magic) %>%
          list.map(id)
      } else {
        self$order_ids <- ids
      }
      
      self$order_cnt <- self$order_ids %>>% list.count
    },
    # get position info ----
    pos_cnt = NULL,
    pos_ids = NULL,
    amounts = NULL,
    get_pos_info = function(symbol = self$symbol_name, all = F, magic = self$strat_magic){
      r <- self$trades$OpenTrades(accountID = self$account_id)
      pos <- self$api$request(r)
      
      pos_sym <- pos$trades %>>% list.filter(instrument == symbol)
      ids <- pos_sym %>% list.map(id)
      if(all == F){
        self$pos_ids <- self$magic_lst %>>%
          list.filter(id %in% ids & magic %in% magic) %>>%
          list.map(id)
      } else {self$pos_ids <- ids}
      self$pos_cnt <- self$pos_ids %>>% list.count
      amounts <- pos_sym %>>%
        list.filter(id %in% self$pos_ids) %>% 
        list.map(currentUnits %>% as.numeric %>% abs %>% as.character) %>% 
        map(~list(units = .x))
    },
    # close all position ----
    trans_rec = NULL,
    close_all = function(magic = self$strat_magic){
      if(pos_cnt != 0){
        rs <- seq_len(self$pos_cnt) %>% 
          map(~self$trades$TradeClose(
            accountID = self$account_id,
            tradeID = self$pos_ids[[.x]],
            data = self$amounts[[.x]])
          )
        self$trans_rec <- rs %>% map(self$api$request)
        
        i <- magic_lst %>>% list.which(id %in% pos_ids & magic %in% magic)
        self$magic_lst <- self$magic_lst %>>% list.remove(i)
        self$trans_lst <- self$trans_lst %>% c(list(close = list(data = self$trans_rec, magic = magic)))
      } else {
        stop("no position")
      }
    },
    # cancel all pending position ----
    cancel_all = function(magic = self$strat_magic){
      if(order_cnt != 0){
        rs <- seq_len(self$order_cnt) %>% 
          map(~orders$OrderCancel(
            accountID = self$account_id,
            orderID = self$order_ids[[.x]])
          )
        self$trans_rec <- rs %>% map(self$api$request)
        
        i <- self$magic_lst %>>% list.which(id %in% self$order_ids & magic %in% magic)
        self$magic_lst <- self$magic_lst %>>% list.remove(i)
        self$trans_lst <- self$trans_lst %>% c(list(cancel = list(data = self$trans_rec, magic = magic)))
      } else {
        stop("no order")
      }
    }
  )
)
