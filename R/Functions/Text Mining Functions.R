### TEXT MINING FUNCTIONS ##


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)


#' Directions
#' source this file to get access to all the functions
#' use the functions in your script, replacing your df to add the new column
#' example: df <- get_phase_num(df, "column title")




# PHASE NUM ----
# description: returns a dataframe w/ phase num column
get_phase_num <- function(df, column_name){
  df$phase_num <- NA
  df$phase_num = str_extract(df[[column_name]], "\\b(?<!1)(\\d[\u00BC-\u00BE\u2150-\u215E\u2189]?\\.\\d)(\\.\\d){0,2}(?=\\s)(?!\\d)")
  return(df)
}
# test <- get_phase_num(inc_data, "Short description")
# table(test$phase_num)
# testdf <- test %>% filter(phase_num == "8.1.2")




# TYPES ----
# description: returns a dataframe w/ incident type column
get_incident_types <- function(df, column_name){
  df$inc_type <- NA
  df$inc_type <- case_when(

    # higher priority searches
    str_detect(df[[column_name]], "(?i)plum|bizerba|ishida|\\bdigi\\b|department(al)?.?scale|pfds") ~ "OnePlum",
    
    # other searches
    str_detect(df[[column_name]], "(?i)bump.?bar") ~ "Aloha Bump Bar",
    str_detect(df[[column_name]], "(?i)aloha.?kitchen") ~ "Aloha Kitchen",
    str_detect(df[[column_name]], "(?i)apple") ~ "Apple Pay",
    str_detect(df[[column_name]], "(?i)ctrl.?alt.?del") ~ "Autoadminlogon",
    str_detect(df[[column_name]], "(?i)back.?up.?battery|bbu|ups|eaton") ~ "Backup Battery",
    str_detect(df[[column_name]], "(?i)pair") ~ "Bluetooth",
    str_detect(df[[column_name]], "(?i)boot.?loop") ~ "Bootlooping",
    str_detect(df[[column_name]], "(?i)blue.?screen.?of.?death|bsod") ~ "BSOD",
    str_detect(df[[column_name]], "(?i)cartnado") ~ "Cartnado",
    str_detect(df[[column_name]], "(?i)drawer") ~ "Cash Drawer",
    str_detect(df[[column_name]], "(?i)variance") ~ "Cash Up",
    str_detect(df[[column_name]], "(?i)touch.?screen|cashier.?display") ~ "Cashier Display",
    str_detect(df[[column_name]], "(?i)cashier.?product") ~ "Cashier Productivity",
    str_detect(df[[column_name]], "(?i)charging") ~ "Charging",
    str_detect(df[[column_name]], "(?i)cmrc") ~ "CMRC",
    str_detect(df[[column_name]], "(?i)cold.?start") ~ "Cold Start",
    str_detect(df[[column_name]], "(?i)coupon") ~ "Coupon",
    str_detect(df[[column_name]], "(?i)customer.?(display|facing.?scre|scre)") ~ "Customer Display",  # test
    str_detect(df[[column_name]], "(?i)stuck token") ~ "Deadletter",
    str_detect(df[[column_name]], "(?i)dead.?letter|stuck.?token") ~ "Deadletter",
    str_detect(df[[column_name]], "(?i)upgrade|deployment|manual.?update") ~ "Deployment",
    str_detect(df[[column_name]], "(?i)(case|tm|team.?member).?discount|bottle.?deposit") ~ "Discount",
    str_detect(df[[column_name]], "(?i)(double|multiple).?charge|charged?.?twice") ~ "Double Charge",
    str_detect(df[[column_name]], "(?i)ebt") ~ "EBT",
    str_detect(df[[column_name]], "(?i)e.?grind|honey.?well") ~ "eGrind",
    str_detect(df[[column_name]], "(?i)extended.?menu|menu.?option") ~ "Extended Menu Options",
    str_detect(df[[column_name]], "(?i)freez|froz|lane.?stuck|latency|unresponsive") ~ "Freezing/Latency",
    str_detect(df[[column_name]], "(?i)gift.?card|black.?hawk") ~ "Gift Card",
    str_detect(df[[column_name]], "(?i)hhu|hand.?held") ~ "Handheld Unit",
    str_detect(df[[column_name]], "(?i)hdd|ntfs|boot.?media") ~ "Hard drive",
    str_detect(df[[column_name]], "(?i)insuff?ici?ent") ~ "Insufficient Privileges",
    str_detect(df[[column_name]], "(?i)no.?price|plu") ~ "Item Data",
    str_detect(df[[column_name]], "(?i)key|button") ~ "Keycap",
    str_detect(df[[column_name]], "(?i)kiosk") ~ "Kiosk",
    str_detect(df[[column_name]], "(?i)conversi?on|initial.?setup|lane.?(decomm?iss?i?on|set.?up)") ~ "Lane Setup",
    str_detect(df[[column_name]], "(?i)line.?direct") ~ "Line Director",
    str_detect(df[[column_name]], "(?i)lock(ed)?.?session") ~ "Locked Session",
    str_detect(df[[column_name]], "(?i)main.?board|bit.?locker") ~ "Mainboard",
    str_detect(df[[column_name]], "(?i)TAB6") ~ "mPOS",
    str_detect(df[[column_name]], "(?i)clean(ing)?.?kit") ~ "MSR Cleaning Kit",
    str_detect(df[[column_name]], "(?i)st(i|y)lus") ~ "MSR Stylus",
    str_detect(df[[column_name]], "(?i)msr|(card|chip).?reader|eft.?approved|pin.?pad|signal.?caught|transaction.?decline|verify.?custom")
    ~ "MSR",
    str_detect(df[[column_name]], "(?i)opos.?tri|triad|opos.{2}error") ~ "NCR Loader",
    str_detect(df[[column_name]], "(?i)retalix|pick.?up") ~ "Office Client",
    str_detect(df[[column_name]], "(?i)offline") ~ "Offline",
    
    str_detect(df[[column_name]], "(?i)outage|store.?(down|network)|network.?down") ~ "Outage",
    str_detect(df[[column_name]], "(?i)crash|off|power(ed)?.?(down|issues|on)|shut.?down|turning.?on") ~ "Power",  # this safe enough?
    str_detect(df[[column_name]], "(?i)price.?checker|pck") ~ "Price Checker",
    str_detect(df[[column_name]], "(?i)Prime") ~ "Prime",
    str_detect(df[[column_name]], "(?i)print") ~ "Printer",
    str_detect(df[[column_name]], "(?i)pxe") ~ "PXE",
    str_detect(df[[column_name]], "(?i)rabbit") ~ "RabbitMQ",
    str_detect(df[[column_name]], "(?i)refund") ~ "Refund",
    str_detect(df[[column_name]], "(?i)re.?image|build") ~ "Reimage",
    str_detect(df[[column_name]], "(?i)(remote|custom(er)?).?image|qr") ~ "Remote Imager",
    str_detect(df[[column_name]], "(?i)Tidel") ~ "Revolution Tidel",
    str_detect(df[[column_name]], "(?i)robo.?log(on|in)") ~ "Robologin",
    str_detect(df[[column_name]], "(?i)register.?reset|sales.?audit|out.?of.?balance|missing.?transaction") ~ "Sales Audit",
    str_detect(df[[column_name]], "(?i)scale") ~ "Scale",
    str_detect(df[[column_name]], "(?i)scanner") ~ "Scanner",
    str_detect(df[[column_name]], "(?i)(SCO|REG)9") ~ "Self Checkout",
    str_detect(df[[column_name]], "(?i)connect to server|server.?connection") ~ "Server Connection",
    str_detect(df[[column_name]], "(?i)server.?down") ~ "Server Down",
    str_detect(df[[column_name]], "(?i)side.?car") ~ "Sidecar",
    str_detect(df[[column_name]], "(?i)SBP|stand.?beside") ~ "Stand Beside Payment",
    str_detect(df[[column_name]], "(?i)Balance.?Report") ~ "Store Balance Report",
    str_detect(df[[column_name]], "(?i)store.?tools") ~ "Store Tools",
    str_detect(df[[column_name]], "(?i)(Stuck|void).?(Transac?t?i?o?n|trx)") ~ "Stuck Transaction",
    str_detect(df[[column_name]], "(?i)Tax") ~ "Tax",
    str_detect(df[[column_name]], "(?i)touch.?point") ~ "Touchpoint",
    str_detect(df[[column_name]], "(?i)TPM") ~ "TPM Chip",
    str_detect(df[[column_name]], "(?i)unexpected|reboot") ~ "Unexpected Shutdown",
    str_detect(df[[column_name]], "(?i)unhandled? exception") ~ "Unhandled Exception",
    str_detect(df[[column_name]], "(?i)upgrade") ~ "Upgrade",
    str_detect(df[[column_name]], "(?i)access|account|pin.?reset|role") ~ "User Account",
    str_detect(df[[column_name]], "(?i)virus") ~ "Virus Defnition",
    str_detect(df[[column_name]], "(?i)NIC") ~ "Wireless Card"  # ! EXCLUDE COMMA LAST LINE
  )  
  return(df)
}
# test <- get_incident_types(test, "Short description")


get_affected_system <- function(df, column_name){
  df$affected_system <- NA
  df$affected_system <- case_when(
    
    # higher priority searches
    str_detect(df[[column_name]], "(?i)plum|bizerba|ishida|\\bdigi\\b|department.?scale") ~ "OnePlum",
    
    # other searches
    str_detect(df[[column_name]], "(?i)SCO9|REG9") ~ "SSCO",
    str_detect(df[[column_name]], "(?i)TAB63|REG63") ~ "mPOS",
    str_detect(df[[column_name]], "(?i)Honeywell|eGrind|XRAP|Instock") ~ "Honeywell",
    str_detect(df[[column_name]], "(?i)(^(?=.*Aloha)(?!.*Payments))|aha") ~ "Aloha",
    str_detect(df[[column_name]], "(?i)PCK") ~ "PCK",
    str_detect(df[[column_name]], "(?i)R10|reg2|reg3|reg4|reg60") ~ "R10"  # ! EXCLUDE COMMA LAST LINE
  )  
  return(df)
}
