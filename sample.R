# get github code ----
git <- "https://raw.githubusercontent.com/Takehana13/"
path <- paste0(git, "oanda_v20/master/API.R")
downloader::source_url(path, prompt = FALSE, quiet = TRUE)

# search python path ----
reticulate::py_discover_config()
py_path <- reticulate::py_discover_config()

# input oanda account ----
access_token <- "afe8a885e437d6e6c70f19e325876ecd-014140dbb0175d5d710500c8e94f1c4d"
account_id <- "101-009-12281852-001" 
account_type <- "practice"

# make new instance ----
oanda <- OANDA_V20$new(
  symbol_name = "USD_JPY",
  account_type = account_type,
  account_id = account_id,
  access_token = access_token,
  python_path = py_path$python
)

# get data test ----
oanda$get_data(n = "5000", tf = "D")
oanda$price_data %>% mutate(dttm = as_date(dttm)) %>% tail(50) %>% 
  ggplot(aes(x = dttm, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  theme_tq()

# get symbol name ----
get_ticker <- function(env_obj, sym){
  env_obj$get_instrument()
  ins <- env_obj$instrument_lst$instruments %>%
    map(list.flatten) %>%
    map(flatten_df)
  
  ins <- ins %>% bind_rows()
  ins_nam <- env_obj$instrument_lst$instruments %>%
    list.select(name) %>%
    map_chr(as.character)
  
  sym_nam <- ins_nam %>% str_subset(sym)
  syms <- ins %>% filter(name %in% sym_nam) %>% pull(name)
}

syms <- get_ticker(oanda, "GBP")

# get multi price data ----
get_dataset <- function(env_obj, sym){
  env_obj$symbol_name <- sym
  env_obj$get_data(n = "5000", tf = "D")
  env_obj$price_data
}

sym_dat <- syms %>%
  map(get_dataset, env_obj = oanda) %>%
  set_names(syms) %>% 
  bind_rows(.id = "symbol")
  
sym_dat %>% 
  mutate(dttm = as_date(dttm)) %>% 
  group_by(symbol) %>% 
  do(tail(., 20)) %>% 
  ungroup() %>% 
  ggplot(aes(x = dttm, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  theme_tq() +
  facet_wrap(.~symbol, scale = "free")

# check historical spread data ----
get_sp <- function(env_obj, sym){
  env_obj$symbol_name <- sym
  env_obj$get_instrument()
  env_obj$get_data(n = "5000", tf = "M1")
  env_obj$chk_sp(mode = "hist")
  env_obj$hist_sp
}

sym_sp <- syms %>%
  map(get_sp, env_obj = oanda) %>% 
  set_names(syms) %>% 
  bind_rows(.id = "symbol")

sym_sp %>% 
  group_by(symbol) %>% 
  mutate(id = row_number()) %>% 
  ggplot(aes(id, sp)) +
  geom_line() +
  theme_tq() +
  facet_wrap(.~symbol, scale = "free")

# get account infomation ----
oanda$get_account_detail()
oanda$account_detail
oanda$account_orders
oanda$account_trades
oanda$account_positions

oanda$get_account_summary()
oanda$account_summary

# send new order ----
mkt_param <- list(
  units = "+10000",
  type = "MARKET",
  pos_fill = "DEFAULT",
  magic = oanda$strat_magic,
  use_tpsl = FALSE,
  tp_dis = NULL,
  sl_dis = NULL
)
lift_dl(oanda$new_order)(mkt_param)
oanda$trans_lst
oanda$magic_lst
oanda$last_tiket_id
oanda$last_tiket_unit

oanda$close_order()

# send new pending order ----
pend_param <- list(
  price = "111.000",
  units = "-10000",
  magic = oanda$strat_magic,
  type = "LIMIT",
  pos_fill = "DEFAULT"
)
oanda$pending_order()
oanda$trans_lst
oanda$last_order_id

oanda$cancel_order()

# cancel all position ----
oanda$get_order_info()
oanda$cancel_all()

# close all position ----
oanda$get_pos_info()
oanda$close_all()
