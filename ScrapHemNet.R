# Inspired by https://www.r-bloggers.com/web-scraping-javascript-rendered-sites/

options(stringsAsFactors = FALSE)
require(rvest)
library(stringr)
library(xlsx)

cleanString <- function(str) {
    str <- str_replace_all(str, "\n", "")
    str <- str_replace_all(str, ",", "")
    str <- str_trim(str)
    str
}

parseLocation <- function(node) {
    street <- borrough <- city <- type <- NA
    nodes <- html_children(node)
    for(n in 1:length(nodes)) {
        attrs <- html_attrs(nodes[[n]])["class"]
        if(is.na(attrs)) {
            nodes2 <- nodes[[n]] %>% html_nodes("span")
            for(i in 1:length(nodes2)) {
                attrs <- html_attrs(nodes2[[i]])["class"]
                if(attrs == "hide-element") {
                    type <- nodes2[[i]] %>% html_text()
                    type <- cleanString(type)
                } else if(attrs == "item-link") {
                    borrough <- nodes2[[i]] %>% html_text()
                    borrough <- cleanString(borrough)
                }
            }
            tmp <- strsplit(nodes[[n]] %>% html_text(), "\n")
            for(i in length(tmp[[1]]):1) {
                if(str_length(cleanString(tmp[[1]][i])) > 0)
                    break
            }
            city <- cleanString(tmp[[1]][i])
        } else if(attrs == "sold-property-listing__heading") {
            street <- nodes[[n]] %>% html_nodes("span") %>% `[[`(3) %>% html_text()
            street <- strsplit(street, ",")[[1]][1]
            street <- cleanString(street)
        }
    }
    df <- data.frame(street, borrough, city, type)
    df
}

parseSize <- function(node) {
    size <- rooms <- tomt <- biarea <- NA
    nodes <- html_children(node)
    for(n in 1:length(nodes)) {
        attrs <- html_attrs(nodes[[n]])["class"]
        if(attrs == "clear-children") {
            nxt <- html_children(nodes[[n]])
            for(nn in 1:length(nxt)) {
                attrs <- html_attrs(nxt[[nn]])["class"]
                if(attrs == "sold-property-listing__subheading sold-property-listing--left") {
                    tmp <- strsplit(nxt[[nn]] %>% html_text(), "\n")[[1]]
                    sz <- FALSE
                    for(l in 1:length(tmp)) {
                        tmpstr <- str_trim(tmp[l])
                        if(str_length(tmpstr) > 0)
                            if(!sz) {
                                size <- as.numeric(sub(",", ".", strsplit(tmpstr, "[[:space:]]")[[1]][1]))
                                sz <- TRUE
                            } else {
                                rooms <- as.numeric(sub(",", ".", strsplit(tmpstr, "[[:space:]]")[[1]][1]))
                            }
                    }
                } else if(attrs == "sold-property-listing__fee") {
                    avgift <- nxt[[nn]] %>% html_text()
                    avgift <- str_replace_all(cleanString(avgift), "[[:space:]]", "")
                    avgift <- str_replace_all(avgift, "kr/mån", "")
                }
            }
        } else if(attrs == "sold-property-listing__land-area sold-property-listing--left") {
            tomt <- nodes[[n]] %>% html_text()
            tomt <- as.numeric(sub(",", ".", strsplit(cleanString(tomt), "[[:space:]]")[[1]][1]))
        } else if(attrs == "sold-property-listing__supplemental-area sold-property-listing--left") {
            biarea <- nodes[[n]] %>% html_text()
            biarea <- as.numeric(sub(",", ".", strsplit(cleanString(biarea), "[[:space:]]")[[1]][1]))
        }
    }
    df <- data.frame(size, rooms, avgift, tomt, biarea)
    df
}

parsePrice <- function(node) {
    slutpris <- soldDate <- NA
    nodes <- html_children(node)
    for(n in 1:length(nodes)) {
        nxt <- html_children(nodes[[n]])
        attrs <- html_attrs(nxt)[[1]]["class"]
        if(attrs == "sold-property-listing__subheading sold-property-listing--left") {
            slutpris <- nxt[[1]] %>% html_text()
            slutpris <- str_replace_all(cleanString(slutpris), "[[:space:]]", "")
            slutpris <- str_replace(slutpris, "Slutpris", "")
            slutpris <- as.numeric(str_replace(slutpris, "kr", ""))
        } else if(attrs == "sold-property-listing__sold-date sold-property-listing--left") {
            soldDate <- nxt[[1]] %>% html_text()
            soldDate <- cleanString(soldDate)
            soldDate <- str_replace(soldDate, "Såld", "")
            soldDate <- str_trim(soldDate)
        }
    }
    df <- data.frame(slutpris, soldDate)
    df
}

parsePriceChange <- function(node) {
    priceChange <- 0
    attrs <- html_attrs(node)
    if(attrs == "sold-property-listing__price-change") {
        priceChange <- node %>% html_text()   
        priceChange <- cleanString(priceChange)
        priceChange <- as.numeric(str_replace(priceChange, "%", ""))/100
        priceChange <- ifelse(is.na(priceChange), 0, priceChange)
    }
    df <- data.frame(priceChange)
    df
}

selling_price_max <- 4500000
living_area_min <- 60
hem <- data.frame()
page <- 0
cont <- TRUE

while(cont) {
    page <- page + 1
    # url <- paste0("http://www.hemnet.se/salda/bostader?location_ids%5B%5D=17798&location_ids%5B%5D=17892&location_ids%5B%5D=18027&location_ids%5B%5D=18028&location_ids%5B%5D=18031&page=", page)
    url <- paste0("http://www.hemnet.se/salda/bostader?item_types%5B%5D=fritidshus&item_types%5B%5D=villa&item_types%5B%5D=radhus&item_types%5B%5D=bostadsratt&location_ids%5B%5D=17798&location_ids%5B%5D=17951&location_ids%5B%5D=18027&location_ids%5B%5D=18028&location_ids%5B%5D=18042&location_ids%5B%5D=473449&location_ids%5B%5D=473464&location_ids%5B%5D=898740&location_ids%5B%5D=923781&page=", page)
    lines <- readLines("phantomjs.js")
    lines[1] <- paste0("var url ='", url ,"';")
    writeLines(lines, "phantomjs.js")
    
    system("~/Applications/phantomjs/bin/phantomjs phantomjs.js")
    
    pg <- read_html("1.html", encoding = "UTF-8")
    
    li <- html_children(pg)
    li <- html_children(li[[2]])
    
    for(i in 1: length(li)) {
        cont <- FALSE # not found
        if(html_name(li[[i]]) == "div")
            if(html_attrs(li[[i]])["id"] == "wallpaper-wrapper") {
                cont <- TRUE # found
                break
            }
    }
    
    if(cont) {
        li <- html_children(li[[i]])
        li <- html_children(li[[1]])
        
        for(i in 1: length(li)) {
            cont <- FALSE # not found
            if(html_name(li[[i]]) == "div")
                if(html_attrs(li[[i]])["class"] == "page-container") {
                    cont <- TRUE # found
                    break
                }
        }
    }

    if(cont) {
        li <- html_children(li[[i]])
        
        for(i in 1: length(li)) {
            cont <- FALSE # not found
            if(html_name(li[[i]]) == "div")
                if(!is.na(html_attrs(li[[i]])["id"]))
                    if(html_attrs(li[[i]])["id"] == "page-content") {
                        cont <- TRUE # found
                        break
                    }
        }
    }
    
    if(cont) {
        li <- html_children(li[[i]])
        li <- html_children(li[[1]])
        
        for(i in 1: length(li)) {
            cont <- FALSE # not found
            if(html_name(li[[i]]) == "div")
                if(!is.na(html_attrs(li[[i]])["id"]))
                    if(html_attrs(li[[i]])["id"] == "result") {
                        cont <- TRUE # found
                        break
                    }
        }
    }
    
    if(cont) {
        li <- html_children(li[[i]])
        
        for(i in 1: length(li)) {
            cont <- FALSE # not found
            if(html_name(li[[i]]) == "ul") {
                if(!is.na(html_attrs(li[[i]])["id"]))
                    if(html_attrs(li[[i]])["id"] == "search-results") {
                        cont <- TRUE # found
                        break
                    }
            }
        }
    }
    
    if(cont) {
        
        li <- html_children(li[[i]])
        
        for(i in 1:length(li)) {
            if(html_name(li[[i]]) == "li") {
                street <- borrough <- city <- type <- size <- rooms <- avgift <- tomt <- biarea <- slutpris <- soldDate <- propertyUrl <- NA
                priceChange <- 0
                loc <- data.frame(street, borrough, city, type)
                siz <- data.frame(size, rooms, avgift, tomt, biarea)
                price <- data.frame(slutpris, soldDate)
                prchange <- data.frame(priceChange)
                url <- data.frame(propertyUrl)
                
                children <- li[[i]] %>% html_children() %>% html_children()
                for(ch in 1:length(children)){
                    if(html_name(children[[ch]]) == "div") {
                        cssclass <- html_attrs(children[[ch]])["class"]
                        if(cssclass == "sold-property-listing__location") {
                            loc <- parseLocation(children[[ch]])
                        } else if(cssclass == "sold-property-listing__size") {
                            siz <- parseSize(children[[ch]])
                        } else if(cssclass == "sold-property-listing__price") {
                            price <- parsePrice(children[[ch]])
                        } else if(cssclass == "sold-property-listing__price-change") {
                            prchange <- parsePriceChange(children[[ch]])
                        }
                    } else if(html_name(children[[ch]]) == "a") {
                        propertyUrl <- html_attrs(children[[ch]])["href"]
                        url <- data.frame(propertyUrl)
                        rownames(url) <- NULL
                    }
                }
                df <- cbind(loc, siz, price, prchange, url)
                hem <- rbind(hem, df)
            }
        }
    }
    # if(page == 1)
    #     cont <- FALSE
}

write.xlsx(hem, file = "hemdata3.xlsx", col.names = T, row.names = F)

selector <- !is.na(str_locate(hem$propertyUrl, "solna")[,"start"])
subselector <- is.na(str_locate(str_to_lower(hem$borrough), "solna")[,"start"])
hem$city[selector] <- "Solna"
hem$borrough[selector & subselector] <- paste(hem$borrough[selector & subselector], "- Solna")

selector <- !is.na(str_locate(hem$propertyUrl, "sollentuna")[,"start"])
subselector <- is.na(str_locate(str_to_lower(hem$borrough), "sollentuna")[,"start"])
hem$city[selector] <- "Sollentuna"
hem$borrough[selector & subselector] <- paste(hem$borrough[selector & subselector], "- Sollentuna")

selector <- !is.na(str_locate(hem$propertyUrl, "jarfalla")[,"start"])
subselector <- is.na(str_locate(str_to_lower(hem$borrough), "järfälla")[,"start"])
hem$city[selector] <- "Järfälla"
hem$borrough[selector & subselector] <- paste(hem$borrough[selector & subselector], "- Järfälla")

selector <- !is.na(str_locate(hem$propertyUrl, "sundbyberg")[,"start"])
subselector <- is.na(str_locate(str_to_lower(hem$borrough), "sundbyberg")[,"start"])
hem$city[selector] <- "Sundbyberg"
hem$borrough[selector & subselector] <- paste(hem$borrough[selector & subselector], "- Sundbyberg")

selector <- !is.na(str_locate(hem$propertyUrl, "upplands")[,"start"])
subselector <- is.na(str_locate(str_to_lower(hem$borrough), "upplands")[,"start"])
hem$city[selector] <- "Upplands Väsby"
hem$borrough[selector & subselector] <- paste(hem$borrough[selector & subselector], "- Upplands Väsby")

hem$borrough2 <- hem$borrough

selector <- !is.na(str_locate(hem$propertyUrl, "bromma")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$propertyUrl, "hasselby")[,"start"])
hem$borrough2[selector] <- "Hässelby"

selector <- !is.na(str_locate(hem$propertyUrl, "spanga")[,"start"])
hem$borrough2[selector] <- "Spånga"

selector <- !is.na(str_locate(hem$propertyUrl, "kista")[,"start"])
hem$borrough2[selector] <- "Kista"

selector <- !is.na(str_locate(hem$propertyUrl, "vallingby")[,"start"])
hem$borrough2[selector] <- "Vällingby"

hem$borrough2[hem$city != "Stockholm"] <- hem$city[hem$city != "Stockholm"]

selector <- !is.na(str_locate(hem$borrough2, "Annedal")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Beckomberga")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Mariehäll")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Traneberg")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Riksby")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Ulvsunda")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Blackeberg")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Södra Ängby")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Bromsten")[,"start"])
hem$borrough2[selector] <- "Spånga"

selector <- !is.na(str_locate(hem$borrough2, "Norra Ängby")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Ulvsunda")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Abrahamsberg")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Johannesfred")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Stora Mossen")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Flysta")[,"start"])
hem$borrough2[selector] <- "Spånga"

selector <- !is.na(str_locate(hem$borrough2, "Nockebyhov")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Solhem")[,"start"])
hem$borrough2[selector] <- "Spånga"

selector <- !is.na(str_locate(hem$borrough2, "Råcksta")[,"start"])
hem$borrough2[selector] <- "Vällingby"

selector <- !is.na(str_locate(hem$borrough2, "Minneberg")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Bällsta")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Nockeby")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Nälsta")[,"start"])
hem$borrough2[selector] <- "Vällingby"

selector <- !is.na(str_locate(hem$borrough2, "Söderberga Allé")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Äppelviken")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Södra Villastad")[,"start"])
hem$borrough2[selector] <- "Hässelby"

selector <- !is.na(str_locate(hem$borrough2, "Kälvesta")[,"start"])
hem$borrough2[selector] <- "Vällingby"

selector <- !is.na(str_locate(hem$borrough2, "Ålsten")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Olovslund")[,"start"])
hem$borrough2[selector] <- "Bromma"
###
selector <- !is.na(str_locate(hem$borrough2, "Åkeshov")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Solhöjden")[,"start"])
hem$borrough2[selector] <- "Spånga"

selector <- !is.na(str_locate(hem$borrough2, "Johannelunds")[,"start"])
hem$borrough2[selector] <- "Vällingby"

selector <- !is.na(str_locate(hem$borrough2, "Smedslätten")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Vinsta")[,"start"])
hem$borrough2[selector] <- "Vällingby"

selector <- !is.na(str_locate(hem$borrough2, "Söderberga Park")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Alvik")[,"start"])
hem$borrough2[selector] <- "Bromma"

selector <- !is.na(str_locate(hem$borrough2, "Hässelby - Lambarö")[,"start"])
hem$borrough2[selector] <- "Hässelby"

write.xlsx(hem, file = "hemdata3cleaned.xlsx", col.names = T, row.names = F)


