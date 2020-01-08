library(shiny)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(plotly)
station<-c("南港","臺北","板橋","桃園","新竹","苗栗","臺中","彰化","雲林","嘉義","臺南","左營")
ui <- fluidPage(
  
  # Application title
  titlePanel("台灣高鐵使用資訊分析"),
  br(),
  p("說明：
        1.高速鐵路自96年1月5日開始營運。
        2.臺北站自96年3月2日開始通車。
        3.苗栗站、彰化站及雲林站自104年12月1日開始通車。
      4.南港站自105年7月1日開始通車。"),
  navbarPage(
    "索引",
    tabPanel("介紹",
             sidebarPanel(
               tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/TaiwanHighSpeedRail_Route_Map.png/270px-TaiwanHighSpeedRail_Route_Map.png")
             ),
             mainPanel(
               h3("身為每日通勤的學生，不論北漂或在地，生活中一定不乏搭乘大眾運輸的工具，"),
               h3("而我們的報告即是從生活中尋求靈感，以此為主題，將分析對象定為高鐵及其使用情形，"),
               h3("除了使用交通部的統計資料，我們也希望能夠搭配其他資料，"),
               h3("試圖從中推論出各個車站地理位置和人群的關係。"),
               br(),
               p("教授：謝舒凱"),
               p("組長：陳曼琳"),
               p("組員：王舒旻、吳東憲、楊婕"),
               
             )
             
    ),
    
    tabPanel("107年各站進出站人數加總",
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("長條圖", plotlyOutput("plot")),
                           tabPanel("摘要", verbatimTextOutput("summary")),
                           tabPanel("表格", tableOutput("table"))
               )  
             )
    ),
    tabPanel("107年各車站各月進出站人數加總折線圖",
             sidebarPanel(
               checkboxGroupInput("stop", 
                                  h3("選擇車站(至少一個)"), 
                                  choices = station,
                                  selected = 1)),
             mainPanel(
               #plotOutput("distPlot"),
               plotlyOutput("Plot"),
               p("可以發現一年之中，幾乎每個車站都有兩個小高峰，分別是三月到四月，以及八月，而十二月的使用量是全年最多，此外，進出人數佔比越高的幾個車站，如台北、台中、左營，更能明顯看出起伏。")
             )
    ),
    tabPanel("各車站歷年進出站人數",
             mainPanel(
               plotlyOutput("yyear")
             )
    ),
    tabPanel("各車站歷年進出站人數佔比",
             mainPanel(
               plotlyOutput("prop"),
               p("此圖中可以看出台北的佔比近年有明顯下降，推測應和南港站通車有關，台中也有類似現象，也應該是和彰化、雲林通車有關。此外，台南是六都裡頭佔比較低的，甚至低於新竹（此處將板橋和南港排除）。")
             )
    ),
    tabPanel("各車站歷年進出站人數成長率",
             sidebarPanel(
               selectInput("stop2", 
                           h3("選擇車站"), 
                           choices = station)),
             mainPanel(
               tableOutput("gr"),
               p("數據觀察結果："),
               p("南港：剛開通不久，初期成長迅速，目前趨緩"),
               p("台北、台中：近年已趨於穩定，成長率大多在五個百分點左右"),
               p("板橋、新竹：近年已趨於穩定，成長率大多在十個百分點以內"),
               p("桃園：仍有些為波動，一直以來持續成長"),
               p("苗栗：剛開通不久，初期成長迅速，目前趨緩，但仍有十個百分點"),
               p("彰化、雲林：剛開通不久，初期成長迅速，目前趨緩至十個百分點以內"),
               p("嘉義、台南、左營：近年已趨於穩定，成長率大多在五個百分點左右")
             )
    ),
    tabPanel("各車站使用率差異因素",
             sidebarPanel(
               h4("共同因素"),
               p("1.所在地點"),
               p("2.所在縣市人口數"),
               p("3.目的地易達性（市內、市外交通）"),
               br(),
               h4("特殊因素"),p( "4.高鐵站本身特色（緊鄰景點）")
             ),
             mainPanel(
               tableOutput("reason") 
             )
    ),
    tabPanel("結論",
             p("從歷年使用人數來看，各個車站每年使用人數都有成長，推測背後原因可能是因為高鐵速度較快、且設站更多，較先前方便。"),br(),
             p("有些新設車站的地點並沒有位在市區，或其座落的城市人口不多，使得在佔比的圖中，可以看到使用量相對其他車站較少，例如：彰化（設站地點偏南，雖然人口多，卻偏離人口密集處）、雲林（人口數不多）、苗栗（人口數不多）。同樣的也可以發現，雖然台南市人口數（188萬）較新竹縣人口數（56萬）多，台南站的佔比卻較新竹站少，原因可能是其設站地點較南，偏離人口密集處。"),br(),
             p("近年的資料中，可以看到某些車站的通車，造成人口有效分流，例如南港站舒緩了台北站運輸壓力。至於中部地區新增的彰化站及雲林站卻沒有減輕台中站的運輸壓力，可以對照說法，也許是因為設站地點不理想的關係。"),br(),
             p("對於設站地點不理想的情況，可以解決的方法可能有：增加地區公共運輸的便利性，或者是重點發展車站周圍區域。但回到問題的根本，我們認為政府還是應該在規劃的過程中妥善評估設站地點。"),
             br(),
             
    ),
    tabPanel("資料來源",
             h3("參考資料"),
             tags$a(href="https://zh.wikipedia.org/zh-tw/","維基百科"),br(),
             tags$a(href="https://house.udn.com/house/story/5889/3676983","台中站資訊"),br(),
             tags$a(href="https://news.ltn.com.tw/news/life/breakingnews/2999852","彰化站資訊"),br(),
             tags$a(href="https://www.wikiwand.com/zh-mo/%E9%AB%98%E9%90%B5%E9%9B%B2%E6%9E%97%E7%AB%99 ","雲林站資訊"),br(),
             tags$a(href="https://topic.udn.com/event/thsrchiayi","嘉義站資訊"),br(),
             tags$a(href=" https://topic.udn.com/event/tainam","台南站資訊"),br(),
             tags$a(href=" https://house.ettoday.net/news/1609914?redirect=1","左營站資訊"),br(),
             tags$a(href=" https://www.ris.gov.tw/app/portal/346","內政部戶政司"),br(),br(),
             h3("主要數據來源"),
             tags$a(href=" http://stat.motc.gov.tw/mocdb/stmain.jsp?sys=100# ","交通部統計查詢網")
             
             
    )
  )
)