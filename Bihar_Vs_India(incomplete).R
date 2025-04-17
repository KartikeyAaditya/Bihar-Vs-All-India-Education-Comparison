library(dplyr)
library(stringr)
library(pdftools)
library(tidyverse)
library(plotly)
library(tabulapdf)
library(tabulizerjars)
library(janitor)

pdf_path <- "E:/aadikartikey/USEB.pdf"
tables <- extract_tables(pdf_path, pages = 1)


df_raw <- as.data.frame(tables[1], stringsAsFactors= FALSE)

df_raw<- df_raw%>%
  janitor::remove_empty("cols")%>%
  janitor::remove_empty("rows")

df_raw

df_cleaned <- df_raw[-c(1:2), ]

df_cleaned <- df_cleaned[df_cleaned$...1 !="Percentage",]

df_cleaned <- df_cleaned[!(df_cleaned$...1 == "...1"& df_cleaned$...2 == "...2"& df_cleaned$...3 == "...3"& df_cleaned$Percentage == "Percentage"),]

row.names(df_cleaned) <- NULL


str_trim(df_cleaned)

colnames(df_cleaned)

head(df_cleaned)

colnames(df_cleaned) <- df_cleaned[1,]

df_cleaned <- df_cleaned[-1,]

colnames(df_cleaned)
head(df_cleaned)

df_slice <- df_cleaned%>%
  select(-`(%)`)

df_slice <- df_slice[complete.cases(df_slice[, c("Bihar","All India")]),]

df_slice

bihar_population <- 100000000
india_population <- 1400000000
options(scipen = 999)

df_slice$Bihar <- as.numeric(df_slice$Bihar)
df_slice$`All India` <- as.numeric(df_slice$`All India`)

df_slice$Bihar_to_Pop <- round(bihar_population / df_slice$Bihar, 2)
df_slice$India_to_Pop <- round(india_population / df_slice$`All India`, 2)

colnames(df_slice)

head(df_slice)


write_csv(df_slice, "E://Aadikartikey//Bihar_Vs_India.csv")

path <- "E://Aadikartikey//Bihar_Vs_India.csv"

data <- read_csv(path)

head(data)

print(data$Indicator)

Indicators <- data$Indicator
clean_indicator <- c()
last_main <- ""

Indicators <- str_trim(Indicators)

for (k in Indicators){
  if(str_starts(k, "-")){
  clean_indicator <- c(clean_indicator, paste(last_main, "-", str_remove(k,"^\\-\\s+")))
  } else if(k %in% c("Primary","Upper Primary","Secondary","Higher Secondary")){
    clean_indicator <- c(clean_indicator, paste(last_main, "-", k))
  }else{
    last_main <- k
    clean_indicator <- c(clean_indicator, k)
  }
    
  
}

length(clean_indicator)== nrow(data)

data$fixed_indicator <- clean_indicator

p <- plot_ly(
  data,
  x = ~fixed_indicator,
  y = ~Bihar,
  type = "bar",
  marker = list(color = "orangered", alpha = 0.80),
  name = "Bihar"
)%>%
  add_trace(
    y = ~`All India`,
    type = "bar",
    marker = list(color = "lightblue", alpha = 0.80),
    name = "All India"
  )%>%
  layout(
    title = list(text = "<b>Bihar Vs All India</b><br><sup>Comparative Educational Indicators (log scale)</sup>",
                 font = list(size =20), xanchor = "left", x = 0.01, y = 0.977),
    xaxis = list(title = "", tickangle = -60, tickfont = list(size=11), showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = "Score/Percentage(log wise scale)", tickangle = -90,tickfont = list(size = 11),showgrid= FALSE,zeroline= FALSE, type = "log"),
    barmode = "group",
    legend = list(orientation = "h",
                  x = 0.75,
                  y = 1.08,
                  font = list(size =12)),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
   
  )



p